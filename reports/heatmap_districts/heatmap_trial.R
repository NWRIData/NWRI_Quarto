dataapp<-readRDS("reports/heatmap_districts/data/app_data.rds")


library(lubridate)
library(dplyr)
library(tidyr)
data_table_react<-dataapp %>%
  mutate(month = month(appdate_month, label = TRUE, abbr = FALSE)) %>%
  group_by(NMCNTY, month) %>%
  count() %>%
  ungroup() %>%
  complete(NMCNTY, month, fill = list(n = 0)) %>%
  pivot_wider(names_from = 'month', values_from = 'n')  %>%
  ungroup() %>%
  mutate(total = rowSums(across(where(is.numeric))))
  
require(reactable)
require(reactablefmtr)

reactable(data_table_react,
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
              data = data_table_react,
              span = 2:13,
              colors = c('#002347','#003366','#003F7D','#FF8E00','#FD7702','#FF5003'),
              bias = 1.4,
              opacity = 0.9,
              show_text = FALSE
            )
            ),
          columns = list(
            total = colDef(
              maxWidth = 225,
              cell = data_bars(
                data = data_table_react,
                fill_color = c('#002347','#003366','#003F7D','#FF8E00','#FD7702','#FF5003'),
                bias = 1.4,
                fill_opacity = 0.9,
                background = 'transparent',
                bar_height = 40,
                text_position = 'center'
              ),
              style = list(borderLeft = "2px solid #999999")
            )
          )
            
          
)
require(ggplot2)
data("txhousing")
data(txhousing)

txhousing %>% 
  filter(city == 'San Marcos' & year > 2004 & year < 2015) %>%
  group_by(year, month) %>% 
  summarize(sales = mean(sales, na.rm = TRUE)) %>%
  mutate(month = month.abb[month]) %>% 
  pivot_wider(names_from = 'month', values_from = 'sales') %>% 
  ungroup() %>% 
  mutate(year = as.character(year),
         total = rowSums(across(where(is.numeric))))
