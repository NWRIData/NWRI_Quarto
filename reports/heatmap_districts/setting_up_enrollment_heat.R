#load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(here)


#establish date of the system for file labelling
date<-Sys.Date()
#getting data ready for the table

last_processed_path <- here("reports","heatmap_districts", "dropbox_data","last_processed.txt")


#load most recent data

# Specify the directory
dir_path <- here("reports","heatmap_districts", "dropbox_data")
here()
files <- list.files(here("reports","heatmap_districts", "dropbox_data"), pattern = "enrollments_\\d{8}\\.csv", full.names = TRUE)

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
dataapp <- read.csv(latest_file, na.strings = c("NA", ""))
cat("Reading file from:", latest_file, "\n")

MSID<-readRDS("reports/heatmap_districts/data/MSID_08112025.rds")
cat("Reading file from:", "reports/heatmap_districts/data/MSID_08112025.rds", "\n")


#set up data processing


Enrollment_heat<-dataapp %>%
  drop_na(AdmissionDate) %>%
  drop_na(DistrictID) %>%
  filter(EnrollmentDate > "2025-06-23") %>%
  mutate(EnrollmentDate = as.Date(EnrollmentDate),
         AdmissionDate = as.Date(AdmissionDate)) %>%
  mutate(appdate_month = floor_date(EnrollmentDate, unit = "month")) %>%
  mutate(appdate_week = floor_date(EnrollmentDate, unit = "week")) %>%
  mutate(enrolldate_month = floor_date(AdmissionDate, unit = "month")) %>%
  mutate(enrolldate_week = floor_date(AdmissionDate, unit = "week")) %>%
  mutate(appdate_month = month(appdate_month, label = TRUE, abbr = TRUE)) %>%
  mutate(enrolldate_month = month(enrolldate_month, label = TRUE, abbr = TRUE)) %>%
  mutate(
    app_month_label = format(appdate_week, "%b %Y")
  ) %>%
  mutate(
    enroll_month_label = format(enrolldate_week, "%b %Y")
  ) %>%
  left_join(.,MSID %>% select(DISTRICT, SCHOOL, DISTRICT_NAME, SCHOOL_NAME_LONG),
            by = c("DistrictID" = "DISTRICT", "SchoolID" = "SCHOOL")) %>%
  group_by(DISTRICT_NAME, enroll_month_label) %>%
  count() %>%
  ungroup() %>%
  complete(DISTRICT_NAME, enroll_month_label, fill = list(n = 0)) %>%
  pivot_wider(names_from = 'enroll_month_label', values_from = 'n')  %>%
  ungroup() %>%
  mutate(total = rowSums(across(where(is.numeric))))

# extract just the month columns from your pivoted data
enroll_month_cols <- Enrollment_heat %>%
  select(-DISTRICT_NAME) %>%           # drop county
  select(-total) %>% # drop helper cols if they still exist
  colnames()

month_order_enroll <- enroll_month_cols[order(as.Date(paste0("01 ", enroll_month_cols), format = "%d %b %Y"))]

Enrollment_heat <- Enrollment_heat %>%
  select(DISTRICT_NAME, all_of(month_order_enroll)) %>%
  mutate(total = rowSums(across(all_of(month_order_enroll))))


saveRDS(Enrollment_heat, file =here("reports","heatmap_districts", "heatmapdata",paste0("heat_map",date,".rds")))

