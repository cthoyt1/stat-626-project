library(tidyverse)

file_list <- list.files("raw_data")
csvs <- !str_detect(file_list,"zip")
csvs <- file_list[csvs] 
csvs <- paste0("raw_data/",csvs)

for (csv_path in csvs){
  print(csv_path)
  data <- read_csv(csv_path)
  names(data) <- tolower(str_replace_all(names(data)," ",""))
  data$trip_day <- as.Date(data$starttime,"%Y-%m-%d %H:%M:%S")
  data <- data %>%
    select(
      trip_day,
      tripduration
      ) %>%
    group_by(trip_day) %>%
    summarise(
      trip_count = n(),
      mean_trip_length = mean(tripduration),
      trip_hours = sum(tripduration)/60
      )
  
  is_JC <- str_detect(csv_path,"JC")
  if (is_JC) data$city = "JC"
  if (!is_JC) data$city = "NYC"
  
  write_csv(data,"trips_data_by_day.csv", append = TRUE)
}