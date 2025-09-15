require(dplyr)
require(here)
require(stringr)

#establish date of the system for file labelling
date<-Sys.Date()
#getting data ready for the table

last_processed_path <- here("reports","Eligiblity_table_2526", "dropbox_data","last_processed.txt")


#load most recent data

# Specify the directory
dir_path <- here("reports","Eligiblity_table_2526", "dropbox_data")
here()
files <- list.files(here("reports","Eligiblity_table_2526", "dropbox_data"), pattern = "enrollments_\\d{8}\\.csv", full.names = TRUE)

# Extract dates from filenames
dates <- as.Date(gsub(".*_(\\d{8})\\.csv", "\\1", files), format = "%Y%m%d")

# Get the most recent based on the date in the filename
latest_file <- files[which.max(dates)]

cat("Latest file based on filename date:", latest_file, "\n")

# Read last processed filename if exists
last_processed <- if (file.exists(last_processed_path)) {
  readLines(last_processed_path)
} else {
  NA_character_
}

# If latest file is same as last processed, skip processing
if (!is.na(last_processed) && latest_file == last_processed) {
  cat("No new data file found. Skipping processing.")
  quit(save = "no")  # Exit the script early
}

cat("writing new last processed path with this:",latest_file)
# Otherwise, update last processed record
writeLines(latest_file, last_processed_path)

# Print it
print(latest_file)
nwri_enrolled <- read.csv(latest_file, na.strings = c("NA", ""))
cat("Reading file from:", latest_file, "\n")


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

