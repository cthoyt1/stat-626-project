setwd('~/Learning/stat-626-project')
# Load the data
library(readr)
library(tidyverse)
library(astsa)
library(scales)
ridership <- read_csv("trips_data_by_day.csv", 
                      col_names = c(
                        "trip_day",
                        "trip_count",
                        "mean_trip_length",
                        "trip_hours",
                        "city"
                      ),
                      col_types=cols(
                        trip_day = col_date(),
                        trip_count = col_integer(),
                        mean_trip_length = col_double(),
                        trip_hours = col_double(),
                        city = col_character()
                      ))

weather  <- read_csv("noaa_weather_central_park.csv")

nyc_ridership <- ridership %>% 
  filter(city == "NYC")%>%
  left_join(weather, by = c("trip_day"="DATE")) %>%
  mutate(
    ridership_average_7 = stats::filter(nyc_ridership$trip_count, sides=2, filter=rep(1/7,7))
    , ridership_average_31 = stats::filter(nyc_ridership$trip_count, sides=2, filter=rep(1/31,31))
  )

ggplot(data = nyc_ridership, mapping=aes(x=trip_day)) +
  geom_line(mapping = aes(y=trip_count), color = "orange") +
  geom_line(mapping =aes(y=ridership_average_31), color= "blue") +
  ggtitle("NYC Daily Citibike Trip Counts with 31 Day Moving Average") +
  scale_y_continuous(label=comma) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_blank()
  )

ggplot(data = nyc_ridership, mapping=aes(x=trip_day)) +
  geom_line(mapping = aes(y=trip_count), color = "orange") +
  geom_line(mapping =aes(y=ridership_average_7), color= "blue") +
  ggtitle("NYC Daily Citibike Trip Counts with 7 Day Moving Average") +
  scale_y_continuous(label=comma) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_blank()
  )

nyc_ridership$log_trip_count <- log(nyc_ridership$trip_count)  #y = log transformation

ggplot(data = nyc_ridership, mapping=aes(x=trip_day)) +
  geom_line(mapping=aes(y=log_trip_count), color= "blue") +
  ggtitle("NYC Daily Citibike Total Trip Counts: Log Transformation") +
  # scale_y_continuous(label=percent) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_blank())

attach(nyc_ridership)
differenced_log_trip_count <- diff(log_trip_count)
row_count <- length(log_trip_count)

ggplot(mapping=aes(x=trip_day[2:row_count])) +
  geom_line(mapping=aes(y=diff(log_trip_count)), color= "blue") +
  ggtitle("NYC Daily Citibike Total Trip Counts: Differenced Log Transformation") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_blank())

detach()

acf_differenced_log_trip_count = acf(differenced_log_trip_count, lag.max = 36, plot = FALSE)
plot(acf_differenced_log_trip_count, main = "ACF of Transformed Bike Trips")

attach(nyc_ridership)
tsplot(x=trip_day, y=trip_count/1000, main="NYC Total Daily Trips: KSmooth Transformation", col="orange", lwd=2, ylab = "Number of Trips/1000")
lines(ksmooth(x=trip_day, y=trip_count/1000, "normal", bandwidth = 12), col="blue", lwd=2)
detach()

pacf_differenced_log_trip_count = pacf(differenced_log_trip_count, lag.max = 36)
pacf_log_trip_count <- pacf(nyc_ridership$log_trip_count, lag.max = 36, plot = FALSE)
plot(pacf_log_trip_count, main = "PACF of Transformed Bike Trips")

attach(nyc_ridership)
trips.yw <- ar.yw(trip_day, order=7)
nyc_ts <- ts(nyc_ridership)
tsplot(y=trip_count, x=trip_day, col="orange", main="Yule Walker Series")
detach()

