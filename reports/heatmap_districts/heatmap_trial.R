dataapp<-read.csv("reports/heatmap_districts/data/enrollments_20250818.csv",
                  na.strings = "")

MSID<-readRDS("reports/heatmap_districts/data/MSID_08112025.rds")


#load libraries
library(lubridate)
library(dplyr)
library(tidyr)
require(reactable)
require(reactablefmtr)
library(wesanderson)


#color palette
# Pick a palette, e.g. Moonrise1 has a nice red/blue/yellow
wes_cols <- wes_palette("Zissou1", 200, type = "continuous")
wes_cols <- viridis::viridis(n = 200)



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



reactable(Enrollment_heat,
          defaultSorted = 'total',defaultSortOrder = "desc",
          wrap = TRUE,defaultPageSize = 100,
          height = 600,
          bordered = TRUE,
          highlight = TRUE,
          resizable = TRUE,
          sortable = TRUE,
          pagination = FALSE,
          showSortIcon = FALSE,
          theme = void(
            centered = TRUE, 
            cell_padding = 0,
            header_font_color = 'black',
            font_color = 'black'
          ),
          defaultColDef = colDef(
            maxWidth = 50,
            align = 'center',
            cell = tooltip(),
            style = color_scales(
              data = Enrollment_heat,
              span = 2:(ncol(Enrollment_heat)),
              colors = wes_cols,
              bias = 1.4,
              opacity = 0.9,
              show_text = FALSE
            )
          ),
          columns = list(
            DISTRICT_NAME = colDef(name = "District", filterable = T,
                            maxWidth = 200),
            total = colDef(name = "Annual Total",
                           maxWidth = 225,
                           cell = data_bars(
                             data = Enrollment_heat,
                             fill_color = wes_cols,
                             bias = 1.4,
                             fill_opacity = 0.9,
                             background = 'transparent',
                             bar_height = 40,
                             text_position = 'outside-end'
                           ),
                           style = list(borderLeft = "2px solid #999999")
            )
          )
)




app_heat<-dataapp %>%
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
  group_by(DISTRICT_NAME, app_month_label) %>%
  count() %>%
  ungroup() %>%
  complete(DISTRICT_NAME, app_month_label, fill = list(n = 0)) %>%
  pivot_wider(names_from = 'app_month_label', values_from = 'n')  %>%
  ungroup() %>%
  mutate(total = rowSums(across(where(is.numeric))))

# extract just the month columns from your pivoted data
app_month_cols <- app_heat %>%
  select(-DISTRICT_NAME) %>%           # drop county
  select(-total) %>% # drop helper cols if they still exist
  colnames()

month_order_app <- app_month_cols[order(as.Date(paste0("01 ", app_month_cols), format = "%d %b %Y"))]

app_heat <- app_heat %>%
  select(DISTRICT_NAME, all_of(month_order_app)) %>%
  mutate(total = rowSums(across(all_of(month_order_app))))


reactable(app_heat,
          wrap = TRUE,defaultPageSize = 100,
          height = 600,
          bordered = TRUE,
          highlight = TRUE,
          resizable = TRUE,
          sortable = TRUE,
          pagination = FALSE,
          showSortIcon = FALSE,
          theme = void(
            centered = TRUE, 
            cell_padding = 0,
            header_font_color = 'black',
            font_color = 'black'
          ),
          defaultColDef = colDef(
            maxWidth = 50,
            align = 'center',
            cell = tooltip(),
            style = color_scales(
              data = app_heat,
              span = 2:(ncol(app_heat)),
              colors = wes_cols,
              bias = 1.4,
              opacity = 0.9,
              show_text = FALSE
            )
          ),
          columns = list(
            DISTRICT_NAME = colDef(name = "District", filterable = T,
                                   maxWidth = 200),
            total = colDef(name = "Annual Total",
                           maxWidth = 225,
                           cell = data_bars(
                             data = app_heat,
                             fill_color = wes_cols,
                             bias = 1.4,
                             fill_opacity = 0.9,
                             background = 'transparent',
                             bar_height = 40,
                             text_position = 'outside-end'
                           ),
                           style = list(borderLeft = "2px solid #999999")
            )
          )
)
