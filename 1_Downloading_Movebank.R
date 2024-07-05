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



checking all the studies that are available for the species
and the tracking method of interest
6992 studies
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
  group_split(id) |>  # Split the data by ID
  map(~safe_movebank_download(.x$id), .progress = T) # Apply the safe function to each group

# Warning messages:                                  
#   1: `vroom()` finds reading problems with the movebank specification.
# ℹ This might relate to the returned data not fitting the expectation of 
#  the movebank data format specified in the package.
# ℹ For retrieving the specific problem you can enable `global_entrace`
#  using rlang::global_entrace() then run the command and use
# `rlang::last_warnings()[[1]]$problems` to retrieve the problems.
# ℹ The requested url can then be retrieved with:
#  `rlang::last_warnings()[[1]]$url`
# ℹ Alternatively in some cases you might be able to retrieve the problems 
# calling `vroom::problems()` on the result of the
# function call that produced the warning. 

  
deployments |> 
  map("result") |> 
  bind_rows() |> 
  write_csv(here("Data", "downloadable_studies_deployments.csv"))


deployments |> 
  map(~{
    
    if(is.null(.x[['error']])) {
      .x[["error"]] <- "none"
    }
    
    tibble(error = as.character(.x[['error']]))
    
     }
  ) |>
  bind_rows() |> 
  bind_cols(tibble(study_id = movebank_filtered$id)) |> 
  write_csv(here("Data", "downloadable_studies_deployments_error.csv"))




  filter(taxon_canonical_name %in% target_sp) |> 
  filter(str_detect(sensor_type_ids, str_c(tags_ids, collapse = "|")))

