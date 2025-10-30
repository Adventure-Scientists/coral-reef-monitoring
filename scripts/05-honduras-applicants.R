# 05 Honduras Applicants
library(tidyverse)
library(googlesheets4)

print("===================================================================")

Sys.time()

tryCatch({
  # Name of script, should match filename
  script_name <- "05-honduras-applicants.R"
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
  sheet <- "1ErCKAgqX7HRoJriBXAG4EcCzStfoXRHeDNZkHmoX070"
  
  applicants <- read.csv(paste0(data_folder, "all-applicants.csv"))
  
  honduras_applicants <- applicants %>% 
    filter(Dive_Location == "Honduras") %>% 
    select(Name, Email, `Phone or WhatsApp` = Phone_Number__c, Recruited, Application_Bio, Training_Status, Best_Quiz_Score, Trained_Protocol = Specific_Training_Name__c)
  
  range_write(
    sheet,
    honduras_applicants,
    "Applicant Data",
    "A2",
    reformat = F
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