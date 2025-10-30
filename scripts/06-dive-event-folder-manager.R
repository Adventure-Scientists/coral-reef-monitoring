# Dive Event Folder Manager
library(tidyverse)
library(arcgis)
library(googledrive)
library(googlesheets4)

print("===================================================================")

Sys.time()

tryCatch({
  # Name of script, should match filename
  script_name <- "05-dive-event-folder-manager.R"
  project_name <- "coral-reef-monitoring"
  
  # Create my timestamp function
  timestamper <- function() {
    paste0(format(Sys.Date(), '%a %B %d'),
           " ",
           format(strptime(Sys.time(), format='%Y-%m-%d %H:%M:%S'), '%r'))
  }
  
  # Create a date variable for use later
  today <- format(Sys.Date(), "%Y%m%d")
  
  # Create filepath for data for this project
  data_folder <- paste0("~/Documents",
                        "/Adventure Scientists",
                        "/adventure_scientists_code",
                        "/coral-reef-monitoring",
                        "/data/")
  
  # Get trust sheet
  trust <- read.csv("~/Documents/Adventure Scientists/adventure_scientists_code/trust.csv")
  
  ######################################################################
  
  # Arcgis auth
  token <- auth_client(client = Sys.getenv("ARCGIS_CLIENT"),
                       secret = Sys.getenv("ARCGIS_SECRET"))
  
  set_arc_token(token)
  
  # This is the survey URL
  main_feature_service <- "https://services5.arcgis.com/jnYf2Emqbv8T64a0/arcgis/rest/services/service_864dbd3e5cb843abb951d9d55130c889/FeatureServer/0"
  
  # Create the Feature Layer object
  main_feature_layer <- arc_open(main_feature_service)
  
  # Extracts the data from the Feature Layer Object
  main_spatial_data <- arc_select(main_feature_layer)
  
  # Removes the spatial data for compatibility with tidyverse
  main_field_data <- data.frame(main_spatial_data) %>% 
    select(-geometry)
  
  # Get distinct site names from each record in the feature service
  site_names <- main_field_data %>% 
    mutate(event_name = str_replace_all(paste0(tolower(survey_date_formatted),"_", tolower(survey_site_name)), " ", "_")) %>% 
    select(event_name, country, objectid)
    
  # Mapping of country to parent folder ID in Google Drive
  country_folders <- tribble(
    ~country,       ~folder_id,
    "Honduras",     "1Bm1vZxqEbABxAB1-Jx0QAXOYmiVBvOFv",
    "Costa Rica",   "13PW2mVNvOk2OubJTy3u5HhNaIUtP0QOH",
    "Mexico",       "1cv0Ks-e_I5v1K4JCVifLcUkQZqcAsgrg",
    "Colombia",     "16jKH2zNzL88xqzKxuSdLsC45amDRzBJV"
  )
  
  # Join the folder ID for each country
  sites_with_parent <- site_names %>%
    left_join(country_folders, by = "country") %>%
    filter(!is.na(folder_id))  # keep only countries we have folder IDs for
  
  # Only proceed if there are rows
  if (nrow(sites_with_parent) > 0) {
    
    result <- sites_with_parent %>%
      rowwise() %>%
      mutate(
        folder = list({
          # list existing subfolders within that country's folder
          existing <- drive_ls(as_id(folder_id))
          
          if (!(event_name %in% existing$name)) {
            
            drive_mkdir(
              name = event_name, 
              path = as_id(folder_id)
              )
            
          } else {
            existing %>% filter(name == event_name)
          }
        })
      ) %>%
      ungroup()
    
  } else {
    result <- tibble(country = character(), event_name = character(), folder = list())
  }
  
  # Get the URLs from newly created folders
  new_urls <- result %>% 
    mutate(url = (result[[4]][[1]])[[3]][[1]][["webViewLink"]]) %>% 
    select(event_name, url)
  
  # Match with the data
  data_to_urls <- site_names %>% 
    left_join(new_urls, "event_name")

  
  ######################################################################
  
  trust <- mutate(trust,
                  Status = case_when(Name == script_name & Project == project_name ~ "Passing",
                                     TRUE ~ Status),
                  Last_Run = case_when(Name == script_name & Project == project_name ~ timestamper(),
                                       TRUE ~ Last_Run))
  
  return(trust)
  
}, error = function(e) {
  # Update trust sheet with status
  trust <- mutate(trust,
                  Status = case_when(Name == script_name & Project == project_name ~ "Failing",
                                     TRUE ~ Status),
                  Last_Run = case_when(Name == script_name & Project == project_name ~ timestamper(),
                                       TRUE ~ Last_Run))
  return(trust)
})

# Write trust csv to upload later
write.csv(trust,
          "~/Documents/Adventure Scientists/adventure_scientists_code/trust.csv",
          row.names = F)