# Script to clean-up screened full-text pdfs from Rayaan

#load in packages

library(readxl)
library(tidyverse)
library(ggpubr)

#read in excel spreadsheet

# Getting the data

# I need to first get the data from cols study until the col include. 

# Path to your Excel file
file_path <- "data/TD_BU_data_extraction.xlsx"

# List of sheet names to extract
sheets_to_get <- c("Ewaldo","Isabelle","Anika", "Heather", "Arianna", "Britt", "Emmy")


combined <- sheets_to_get %>%
  lapply(function(s) {
    read_excel(file_path, sheet = s) %>%
      select(study:include) %>%
      mutate(across(everything(), as.character))
  }) %>%
  bind_rows()

# View or save
print(combined)

# Check what categories there are for the include / exclude

unique(combined$include)

# Select out all of the nos

combined_clean <- combined %>%
  filter(!include %in% c("n", "No", "no"),
         !is.na(study)) %>%
  mutate(phyto_func_response = if_else(phyto_func_response == "yes", "1",
                                       if_else(phyto_func_response == "no", "0", phyto_func_response)))


# Yes only just to be sure

combined_yes_only <-combined_clean %>%
  filter(include %in% c("y", "yes", "Yes", "yes?")) %>%
  mutate(ecosystem = str_trim(tolower(ecosystem)))

str(combined_yes_only)

# Graph to check whether the number of publications on each system

# making a dataframe just for that
ecosystem_only <- combined_yes_only %>%
  select(ecosystem) %>%
  mutate(ecosystem = str_to_lower(ecosystem)) %>%              # lowercase
  separate_rows(ecosystem, sep = ";|,") %>%                   # split on ; OR ,
  mutate(ecosystem = str_trim(ecosystem)) %>%
  filter(ecosystem %in% c("freshwater", "marine", "estuary","brackish"))


ggplot(ecosystem_only, aes(x = ecosystem))+  
  geom_bar()+
  theme_pubr()
