# 01 Applicant Data
library(tidyverse)
library(salesforcer)
library(googlesheets4)

print("===================================================================")

Sys.time()

tryCatch({
  # Name of script, should match filename
  script_name <- "01-applicant-data.R"
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
  
  # Create expedition query
  coral_exp_query <- 
    "SELECT Id, Starting_Location__c FROM Expedition__c WHERE Project__c = 'a0tHq00000Ke31HIAR'"
  
  # Run query
  coral_exp <- sf_query(coral_exp_query, "Expedition__c")
  
  # Create expedition members query
  coral_exp_mem_query <- 
    "SELECT  Id, Name, CreatedDate, Contact__c, Email__c, Expedition__c, Outdoor_Activity_Experience__c, Internal_Prompt__c, Training_Status__c FROM Expedition_Members__c WHERE Project__c = 'a0tHq00000Ke31HIAR'"
  
  # Run query
  coral_exp_mem <- sf_query(coral_exp_mem_query, "Expedition_Members__c")
  
  # Combine Expeditions with Expedition Members
  coral_exp_combined <- merge(coral_exp, coral_exp_mem, by.x = "Id", by.y = "Expedition__c")
  
  coral_exp_combined_renamed <- coral_exp_combined %>% 
    rename(Expedition_Id = Id,
           Expedition_Members__c = Id.y,
           Application_Date = CreatedDate)
  
  # Create contact query (sprintf from contact IDs)
  coral_contacts_query <- sprintf("SELECT Id, FirstName, LastName, Phone_Number__c FROM Contact WHERE Id IN ('%s')",
                                         paste0(coral_exp_mem$Contact__c, collapse = "','"))
  
  # Get contacts
  coral_contacts <- sf_query(coral_contacts_query, "Contact")
  
  # Merge with expedition data
  coral_contact_and_exp <- merge(coral_contacts, coral_exp_combined_renamed,
                            by.x = "Id", by.y = "Contact__c")
  
  # Get training data
  coral_training_query <- sprintf("SELECT Id, Expedition_Members__c, Specific_Training_Name__c, Quiz_Score__c, CreatedDate FROM Training_Log__c WHERE Expedition_Members__c IN ('%s')",
                                  paste0(coral_exp_mem$Id, collapse = "','"))
  
  coral_training <- sf_query(coral_training_query, "Training_Log__c")
  
  coral_training_formatted <- coral_training %>% 
    group_by(Expedition_Members__c) %>% 
    arrange(desc(Quiz_Score__c)) %>% 
    distinct(Expedition_Members__c, .keep_all = T) %>% 
    rename(Training_Log_Id = Id,
           Training_Completed_Date = CreatedDate,
           Best_Quiz_Score = Quiz_Score__c)
  
  # Final merge
  coral_applicants <- left_join(coral_contact_and_exp, coral_training_formatted, "Expedition_Members__c")
  
  # Format
  coral_applicants_formatted <- coral_applicants %>% 
    mutate(Name = paste0(FirstName, " ", LastName),
           Training_Status = case_when(
             Best_Quiz_Score >= 70 ~ "Training Completed",
             Best_Quiz_Score < 70 ~ "Training Attempted",
             is.na(Best_Quiz_Score) ~ "Training Not Started"
           )) %>% 
    select(Expedition_Id,
           Contact_ID = Id,
           Name,
           Email = Email__c,
           Phone_Number__c,
           Dive_Location = Starting_Location__c,
           Recruited = Internal_Prompt__c,
           Application_Bio = Outdoor_Activity_Experience__c,
           Training_Status,
           Best_Quiz_Score,
           Specific_Training_Name__c
           )
  
  # Save locally
  write.csv(coral_applicants_formatted, paste0(data_folder, "all-applicants.csv"), row.names = F)
  
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