# Load libraries
library(dplyr)
library(here)
library(stringr)
library(lubridate)
library(tidyr)

# Establish date of the system for file labelling
date <- Sys.Date()

# Path to "last processed" record
last_processed_path <- here("reports", "Eligiblity_table_2526", "dropbox_data", "last_processed.txt")

# Read last processed filename if it exists
last_processed <- if (file.exists(last_processed_path)) {
  trimws(readLines(last_processed_path, warn = FALSE))
} else {
  NA_character_
}
cat("Last processed file recorded:", last_processed, "\n")

# Specify the directory and list files
dir_path <- here("reports", "Eligiblity_table_2526", "dropbox_data")
files <- list.files(
  dir_path,
  pattern = "enrollments_\\d{8}\\.csv",
  full.names = TRUE
)

# Extract dates from filenames
dates <- as.Date(gsub(".*_(\\d{8})\\.csv", "\\1", files), format = "%Y%m%d")

# Get the most recent file
latest_file <- files[which.max(dates)]
latest_file_base <- basename(latest_file)

cat("Latest file based on filename date:", latest_file_base, "\n")

# If latest file is same as last processed, skip processing
if (!is.na(last_processed) && latest_file_base == last_processed) {
  cat("No new data file found. Skipping processing.\n")
  quit(save = "no")  # Exit the script early
}

# Otherwise, update last processed record (store only basename)
cat("Updating last processed file to:", latest_file_base, "\n")
writeLines(latest_file_base, last_processed_path)

# Continue with processing (read full path)
cat("Reading data from:", latest_file, "\n")
nwri_enrolled <- read.csv(latest_file, na.strings = c("NA", ""))
dataapp <- read.csv(latest_file, na.strings = c("NA", ""))


nwri_all<-readRDS("reports/Eligiblity_table_2526/base_data/nwri_all_table_08112025.rds")
nwri_eligible<-readRDS("reports/Eligiblity_table_2526/base_data/nwri_eligible_table_08112025.rds")
MSID<-readRDS("reports/Eligiblity_table_2526/base_data/MSID_08112025.rds")

#run the code below if you want to remove the unmatched people


#get rid of double spaces
nwri_enrolled <- nwri_enrolled %>%
  mutate(across(where(is.character), str_squish)) 

nwri_eligible <-nwri_eligible %>%
  mutate(across(where(is.character), str_squish)) %>%
  mutate(grade = str_remove(grade, "^0+")) 

  
nwri_all <-nwri_all %>%
  mutate(across(where(is.character), str_squish)) %>%
  mutate(grade = str_remove(grade, "^0+")) 

enrolled_students <- nwri_enrolled %>%
  group_by(DistrictID, SchoolID, Grade) %>%
  summarise(n_enrolled =  n()) %>%
  ungroup() 


eligible_students<-nwri_eligible %>%
  filter(!grade == "P") %>%
  group_by(district,school, grade) %>%
  summarise(n_eligible =  n()) %>%
  ungroup() 


all_students<-nwri_all %>%
  filter(!grade == "P") %>%
  group_by(district,school,grade) %>%
  summarise(n_total =  n()) %>%
  ungroup() 

df<-all_students %>%
  left_join(eligible_students, by = c("district" = "district", "school" = "school", "grade" = "grade")) %>%
  left_join(enrolled_students, by = c("district" = "DistrictID", "school" = "SchoolID", "grade" = "Grade")) %>%
  left_join(
    MSID %>% select(DISTRICT, SCHOOL, DISTRICT_NAME, SCHOOL_NAME_LONG),
    by = c("district" = "DISTRICT", "school" = "SCHOOL")
  )

df %>%
  filter(SCHOOL_NAME_LONG == "NEWBERRY ELEMENTARY SCHOOL") %>%
  mutate(n_enrolled = ifelse(is.na(n_enrolled), 0, n_enrolled),
         n_eligible = ifelse(is.na(n_eligible), 0, n_eligible))  %>%
  filter(!is.na(n_total)) %>%
  mutate(perc_enroll = n_enrolled/n_total,
         perc_eligible = n_eligible/n_total)

saveRDS(df, file =here("reports","Eligiblity_table_2526", "data",paste0("final_table",date,".rds")))


###setting up heat map for eligiblity table

enroll_heat_week<-dataapp %>%
  drop_na(AdmissionDate) %>%
  drop_na(DistrictID) %>%
  filter(AdmissionDate > "2025-06-22") %>%
  mutate(
    EnrollmentDate = as.Date(EnrollmentDate),
    AdmissionDate = as.Date(AdmissionDate),
    appdate_week = floor_date(EnrollmentDate, unit = "week"),
    enrolldate_week = floor_date(AdmissionDate, unit = "week")
  ) %>%
  # weekly labels instead of monthly
  mutate(
    app_week_label = format(appdate_week, "%Y-%m-%d"),
    enroll_week_label = format(enrolldate_week, "%Y-%m-%d")
  ) %>%
  left_join(
    MSID %>% select(DISTRICT, SCHOOL, DISTRICT_NAME, SCHOOL_NAME_LONG),
    by = c("DistrictID" = "DISTRICT", "SchoolID" = "SCHOOL")
  ) %>%
  group_by(DISTRICT_NAME, enroll_week_label) %>%
  count() %>%
  ungroup() %>%
  complete(DISTRICT_NAME, enroll_week_label, fill = list(n = 0)) %>%
  pivot_wider(names_from = "enroll_week_label", values_from = "n") %>%
  ungroup() %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  drop_na(DISTRICT_NAME)


enroll_heat_week1 <- enroll_heat_week %>%
  rename_with(
    ~ ifelse(.x %in% c("DISTRICT_NAME", "total"), .x, format(as.Date(.x), "%m-%d")),
    .cols = -DISTRICT_NAME
  ) 



saveRDS(enroll_heat_week1, file =here("reports","Eligiblity_table_2526", "heatmapdataweekly",paste0("heat_mapweekly",date,".rds")))


