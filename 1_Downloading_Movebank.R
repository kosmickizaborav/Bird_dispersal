library(move2)
library(tidyverse)
library(here)
library(units)



# Things of interest ------------------------------------------------------


# defining species of interest
target_sp <- c("Anas platyrhynchos", "Columba livia", 
               "Chroicocephalus ridibundus", "Sturnus vulgaris", 
               "Turdus merula", "Circus aeruginosus", "Accipiter gentilis", 
               "Passer domesticus")

tags_ids <- c("GPS", "Sigfox Geolocation")
tags_ids_deployments <- c("gps", "sigfox-geolocation")

# defining columns of interest
col_interest <- c("id", "name", "taxon_ids", "sensor_type_ids", 
                  "timestamp_first_deployed_location", 
                  "timestamp_last_deployed_location", 
                  "number_of_individuals", "number_of_tags", 
                  "number_of_deployed_locations", 
                  "i_am_owner", "is_test", "i_have_download_access", 
                  "sensor_type_ids", "contact_person_name", "main_location"
)


# checking all the studies that are available for the species
# and the tracking method of interest
# 6992 studies
complete_movebank <- movebank_retrieve(entity_type = "study")

movebank_filtered <- complete_movebank |>
  drop_units() |>
  filter(
    number_of_deployed_locations > 0 & !is.na(number_of_deployed_locations)
  ) |>
  filter(str_detect(sensor_type_ids, str_c(tags_ids, collapse = "|"))) |>
  filter(i_have_download_access == TRUE)

movebank_filtered |>
  write_csv(here("Data", "downloadable_studies.csv"))


# Define a safe version of the function to handle errors
safe_movebank_download <- safely(~ {
  movebank_download_deployment(
    .x, 'license-md5' = '51d5f89ddc94971109e50a17eb14f8be'
    )
})


deployments <- movebank_filtered |> 
  distinct(id) |>  # Select distinct IDs
  group_split(id) |> 
  map(~ {
    
    res <- safe_movebank_download(.x$id)
    
    if (!is.null(res$result)) {
      
      tibble(res$result)
      
    } else {
      
      tibble(study_id = .x$id, error_text = as.character(res$error))
      
    }
    
  }, 
  .progress = T
  ) |> 
  bind_rows()


deployments |> 
  write_csv(here("Data", "downloadable_studies_deployments.csv"))


deployments_filtered <- deployments |> 
  filter(taxon_canonical_name %in% target_sp) |> 
  filter(
    str_detect(sensor_type_ids, str_c(tags_ids_deployments, collapse = "|"))
  ) |> 
  filter(manipulation_type == "none" | is.na(manipulation_type))


deployments_filtered |> 
  write_csv(here("Data", "downloadable_studies_deployments_filtered.csv"))



# 02 - Downloading deployments of interest --------------------------------


deployments <- here("Data", "downloadable_studies_deployments_filtered.csv") |> 
  read_csv(show_col_type = F)

col_deploy <- c("taxon_canonical_name", "study_id", "deployment_id" , 
                  "sensor_type_ids", 
                  "individual_local_identifier", "individual_number_of_deployments", 
                  "deployment_local_identifier", "tag_local_identifier", 
                  "sex", "animal_life_stage",  "manipulation_type", "error_text"
                  )


# Define a custom function to handle multiple arguments
study_download <- function(
    study_id, sensor_type_id, individual_local_identifier
    ) {

  movebank_download_study(
    study_id, 
    sensor_type_id = sensor_type_id,
    individual_local_identifier = individual_local_identifier, 
    # remove_movebank_outliers = T, 
    # omit_derived_data = T, 
    # convert_spatial_columns = F, 
    # atributes = NULL
    ) 
  
}

# Wrap the custom function with safely
safe_study_download <- safely(study_download)
  
studies <- unique(deployments$study_id)
  
download_report <- deployments |> 
  select(all_of(col_deploy)) |> 
  group_split(study_id) |> 
  map(~{
    
    study_id <- unique(.x$study_id)
    individuals <- unique(.x$individual_local_identifier) |> 
      as.character()
    
    print(paste(study_id, which(study_id == studies), "|", length(studies)))
    
    res <- safe_study_download(study_id, c(653, 2299894820), individuals)
      
      if(!is.null(res$result)){ 
        
        study_file <- here("Data", "Studies", str_c(study_id, "_study.csv"))
                           
        tibble(res$result) |> 
          write_csv(study_file)
        
        tibble(study_id = study_id, file = study_file)
              
      } else {
        
        tibble(study_id = study_id, error_text = paste(res$error, collapse = " "))
        
      }
    
  }, 
  .progess = T
  ) |> 
  bind_rows() 

download_report |> 
  write_csv(here("Data", "Studies", "studies_download_report.csv"))

# Warning messages:                                                                                                                    
#   1: `vroom()` finds reading problems with the movebank specification.
# ℹ This might relate to the returned data not fitting the expectation of the movebank data format specified in the package.
# ℹ For retrieving the specific problem you can enable `global_entrace` using rlang::global_entrace() then run the command and use
# `rlang::last_warnings()[[1]]$problems` to retrieve the problems.
# ℹ The requested url can then be retrieved with: `rlang::last_warnings()[[1]]$url`
# ℹ Alternatively in some cases you might be able to retrieve the problems calling `vroom::problems()` on the result of the
# function call that produced the warning. 
          
