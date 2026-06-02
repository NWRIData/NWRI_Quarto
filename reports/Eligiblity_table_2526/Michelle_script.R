library(tidyverse)



get_latest_rds <- function(dir_path, verbose = TRUE) {
  rds_files <- list.files(path = dir_path, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(rds_files) == 0) {
    if (verbose) cat("No .rds files found in", dir_path, "\n")
    return(NULL)
  }
  
  file_dates <- sub(".*?(\\d{4}-\\d{2}-\\d{1,2})\\.rds$", "\\1", basename(rds_files))
  file_dates <- as.Date(file_dates, format = "%Y-%m-%d")
  
  latest_file <- rds_files[which.max(file_dates)]
  
  if (verbose) cat("Loaded file from:", latest_file, "\n")
  readRDS(latest_file)
}
setwd("reports/Eligiblity_table_2526/")
Gradeleveltabs<-get_latest_rds("data/")
enroll_heat_week <- get_latest_rds("heatmapdataweekly/")
nonmatch_data<-get_latest_rds("nonmatch_data/")
app_data<-readRDS("data/app_data.rds")

#make a list of schools under average enrollment but over average eligibility
#change any NAs for enrollment or eligible numbers to 0 and
#remove any schools where we have no data on total enrollment
Gradeleveltabs<-Gradeleveltabs |>
  mutate(n_enrolled = ifelse(is.na(n_enrolled), 0, n_enrolled),
         n_eligible = ifelse(is.na(n_eligible), 0, n_eligible))  |>
  filter(!is.na(n_total)) |>
  mutate(perc_enroll = n_enrolled/n_total,
         perc_eligible = n_eligible/n_total) |>
  filter(!perc_enroll > 1) |>
  rename(DistrictID = district,
         SchoolID = school)

orangefilter = Gradeleveltabs |>
  group_by(DistrictID, SchoolID) |>
  summarise(n_total = sum(n_total),
            n_eligible = sum(n_eligible),
            n_enrolled = sum(n_enrolled)) |>
  ungroup() |>
  mutate(perc_enroll = n_enrolled/n_total,
         perc_eligible = n_eligible/n_total)


# Step 1: Build summary table (1 row per school)


school_summary2 <- Gradeleveltabs |>
  group_by(DistrictID, SchoolID,DISTRICT_NAME, SCHOOL_NAME_LONG) |>
  summarize(
    total_n_eligible = sum(n_eligible),
    total_n_enroll = sum(n_enrolled),
    total_school_pop = sum(n_total),
    avg_perc_eligible = total_n_eligible/total_school_pop,
    avg_perc_enroll = total_n_enroll/ total_school_pop,
    .groups = "drop") |>
  mutate(priorit_col = case_when(
    avg_perc_enroll <  mean(orangefilter$perc_enroll, na.rm = T) &
      avg_perc_eligible > mean(orangefilter$perc_eligible, na.rm = T) ~ "orange",
    TRUE ~ 'grey'
  )) |>
  relocate(avg_perc_eligible, .after =total_n_eligible) |>
  relocate(avg_perc_enroll, .after = avg_perc_eligible) |>
  mutate('School Type' = priorit_col)  |>
  select(-total_school_pop)



#calculate enrollment and elibility percentages using totals rather then
# averaging across schools per district 
district_level<-Gradeleveltabs |>
  group_by(DISTRICT_NAME) |>
  summarise(total_eligible_d= sum(n_eligible),
            total_enroll_d = sum(n_enrolled),
            total_district_pop = sum(n_total)) |>
  ungroup() 

district_level<-left_join(district_level, nonmatch_data |> select(DistrictName, n), by = c("DISTRICT_NAME"= "DistrictName")) |>
  mutate(total_eligiblity = total_eligible_d/total_district_pop, 
         total_enroll = total_enroll_d/(total_eligible_d+total_enroll_d),
         nonmatch_added_enroll = (total_enroll_d + n)/(total_eligible_d+total_enroll_d+n)
  ) %>%
  drop_na(DISTRICT_NAME) %>%
  select(-c(total_enroll_d,total_district_pop,n))
