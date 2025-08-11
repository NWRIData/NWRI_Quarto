require(dplyr)

#getting data ready for the table

nwri_enrolled<-read.csv("reports/Eligiblity_table_2526/dropbox_data/enrollments_20250805.csv")
nwri_all<-read.csv("reports/Eligiblity_table_2526/base_data/nwri_all_table_08112025.rds")
nwri_eligible<-read.csv("reports/Eligiblity_table_2526/base_data/nwri_eligible_table_08112025.rds")
MSID<-read.csv("reports/Eligiblity_table_2526/base_data/MSID_08112025.rds")

#run the code below if you want to remove the unmatched people
nwri_enrolled_clean<-nwri_enrolled %>%
  filter(!MappedFLEID == "NM") 

vpkenrolled<-nwri_enrolled %>%
  filter(!MappedFLEID == "NM") %>%
  filter(SchoolName == "Other (VPK Application)")



wtf<-nwri_enrolled_clean %>%
  select(SchoolName, MappedDOESchool)
#get rid of double spaces
nwri_enrolled_clean <- nwri_enrolled_clean %>%
  mutate(across(where(is.character), str_squish)) %>%
  mutate(DistrictName = toupper(DistrictName),
         SchoolName = toupper(SchoolName))

nwri_eligible <-nwri_eligible %>%
  mutate(across(where(is.character), str_squish)) %>%
  ~
  
  
  nwri_all <-nwri_all %>%
  mutate(across(where(is.character), str_squish)) %>%
  mutate(grade = str_remove(grade, "^0+")) 



enrolled_students <- nwri_enrolled_clean %>%
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

df<-enrolled_students %>%
  left_join(eligible_students, by = c("DistrictID" = "district", "SchoolID" = "school", "Grade" = "grade")) %>%
  left_join(all_students, by = c("DistrictID" = "district", "SchoolID" = "school", "Grade" = "grade")) %>%
  left_join(
    MSID %>% select(DISTRICT, SCHOOL, DISTRICT_NAME, SCHOOL_NAME_LONG),
    by = c("DistrictID" = "DISTRICT", "SchoolID" = "SCHOOL")
  )

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
df %>%
  count(grade)

saveRDS(df,"data/eligiblity_table_2526/final_table_data.rds")


NAs_invest<-df %>%
  filter(is.na(DISTRICT_NAME))
nwri_enrolled %>%
  filter(DistrictID == 1 & SchoolID == 532)

deeplook <- nwri_enrolled %>%
  semi_join(NAs_invest, by = c("SchoolID", "DistrictID"))
deeplook %>%
  count(SchoolName)
deeplook$SchoolName
nwri_all %>% 
  filter(district == 221 & school== 3)
