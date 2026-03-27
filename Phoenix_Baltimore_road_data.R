library(data.table)
sheet_dt_list <- list()
for(sheet in 1:2){
  sheet_df <- readxl::read_xlsx("Phoenix_Baltimore_road_data.xlsx", sheet)
  sheet_dt_list[[sheet]] <- data.table(sheet_df)
}
(sheet_dt <- rbindlist(sheet_dt_list))
table(sheet_dt$city_key)
