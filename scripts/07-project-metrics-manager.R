# Project Metrics Manager
library(tidyverse)
library(googlesheets4)

print("===================================================================")

Sys.time()

tryCatch({
  # Name of script, should match filename
  script_name <- "07-project-metrics-manager.R"
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
  
  # Get the applicant data stored locally
  applicant_data <- read.csv(paste0(data_folder, "all-applicants.csv"))
  
  # Push to the applicant sheet in project metrics
  range_write(
    "13Ru9l9dpfdgFZKnG6r1zkipTKsEP4r8Gh97139SQwX0",
    applicant_data,
    "Applicants",
    "A1:"
    )
  
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