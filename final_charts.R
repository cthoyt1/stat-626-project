# Load and prepare the data
library(readr)
library(tidyverse)
library(astsa)
library(scales)
library(ggplot2)
library(caret)
library(e1071)
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
  select(
    trip_day
    , trip_count
    , AWND
    , PRCP
    , SNOW
    , SNWD
    , TMAX
    , TMIN
    , WDF2
    , WDF5
    , WSF2
    , WSF5
  ) %>%
  mutate(
    ridership_average_7 = stats::filter(trip_count, sides=2, filter=rep(1/7,7))
    , ridership_average_31 = stats::filter(trip_count, sides=2, filter=rep(1/31,31))
    , day_of_week = weekdays(as.Date(trip_day))
    , weekend = if_else(day_of_week %in% c('Saturday','Sunday'),1,0)
    , days_line = trip_day - min(trip_day)
    , boxcox_trip_count = trip_count^0.6
    , covid = if_else(trip_day >= "2020-04-01", as.integer(days_line) - 2466, 0)
  )

filter(nyc_ridership, trip_day == "2020-04-01")$days_line

# Histogram of Untransformed Data
ggplot(data=nyc_ridership) +
  geom_density(mapping = aes(x=trip_count), color="orange") +
  geom_histogram(mapping = aes(x=trip_count, y = ..density..), alpha=0.25, color = "blue", fill = "blue") +
  geom_rug(aes(x = trip_count, y = 0), color = "blue") +
  ggtitle("Histogram of Untransformed Trip Counts") +
  xlab("Trip Count Bins") +
  theme(
    plot.title = element_text(hjust = 0.5)
    , axis.title.y = element_blank()
    , axis.text.y = element_blank()
  )


# Box-Cox power = 0.6
caret::BoxCoxTrans(nyc_ridership$trip_count)

differenced_boxcox_trip_count <- diff(nyc_ridership$boxcox_trip_count)
row_count <- length(log_trip_count)

ggplot(data = nyc_ridership, mapping=aes(x=trip_day)) +
  geom_line(mapping=aes(y=boxcox_trip_count), color= "blue") +
  ggtitle("NYC Daily Citibike Total Trip Counts \n Box-Cox Transformation (0.6)") +
  # scale_y_continuous(label=percent) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_blank())


# Differenced power transformation
ggplot(mapping=aes(x=trip_day[2:row_count])) +
  geom_line(mapping=aes(y=differenced_boxcox_trip_count), color= "orange") +
  geom_smooth(mapping=aes(y=differenced_boxcox_trip_count), color = "blue", method = "loess") +
  ggtitle("NYC Daily Citibike Differenced Trip Counts \n Box-Cox Transformation (0.6)") +
  # scale_y_continuous(label=percent) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_blank())

trans_pacf <- pacf(differenced_boxcox_trip_count, lag.max = 60, plot=FALSE)
trans_acf <- acf(differenced_boxcox_trip_count, lag.max = 60, plot=FALSE)

plot(trans_pacf, main = "PACF of Transformed Trip Counts")
plot(trans_acf, main = "ACF of Transformed Trip Counts")

ggplot() +
  geom_density(mapping = aes(x=differenced_boxcox_trip_count), color="orange") +
  geom_histogram(mapping = aes(x=differenced_boxcox_trip_count, y = ..density..), alpha=0.25, color = "blue", fill = "blue") +
  geom_rug(aes(x = differenced_log_trip_count, y = 0), color = "blue") +
  ggtitle("Histogram of Transformed Trip Counts") +
  xlab("Trip Count Bins") +
  theme(
    plot.title = element_text(hjust = 0.5)
    , axis.title.y = element_blank()
    , axis.text.y = element_blank()
  )

sarima_mod <- sarima(nyc_ridership$trip_count, p=1, d=1, q=1, P=0, D=1, Q=1, S=7, xreg = cbind(nyc_ridership$covid)) #, nyc_ridership$PRCP, nyc_ridership$TMAX, nyc_ridership$TMIN, nyc_ridership$weekend, nyc_ridership$SNWD)) 
sarima_mod$ttable
sarima_mod$BIC

nyc_ridership$mod_residuals <- sarima_mod$fit$residuals
nyc_ridership$mod <- nyc_ridership$trip_count - nyc_ridership$mod_residuals

ggplot(data = nyc_ridership, mapping = aes(x=trip_day)) +
  geom_line(mapping = aes(y=trip_count, alpha = 'Ride Counts', color = 'Ride Counts')) +
  geom_smooth(mapping = aes(y=trip_count, alpha = 'Smoothed Ride Counts', color = 'Smoothed Ride Counts'), method="loess", span = 0.01, se=FALSE) +
  geom_line(mapping=aes(y=mod, alpha = 'Predictions', color = 'Predictions')) +
  geom_smooth(mapping = aes(y=mod, alpha = 'Smoothed Predictions', color = 'Smoothed Predictions'), method="loess", span = 0.01, se = FALSE
              ) +
  ggtitle("Predicted vs Actual Trips") +
  scale_y_continuous(labels = scales::comma) +
  ylab("Daily Trip Counts") +
  scale_colour_manual(name = "",values = c('Ride Counts'='blue', 'Smoothed Ride Counts'='blue', 'Predictions' = 'red', 'Smoothed Predictions' = 'red')) +
  scale_alpha_manual(name="",values = c('Ride Counts'=0.3, 'Smoothed Ride Counts' = 1, 'Predictions' = 0.3, 'Smoothed Predictions' = 1)) +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill='white'),
        legend.key = element_rect(fill="white"))






sarima.for(nyc_ridership$trip_count, 24, p=1, d=1, q=1, P=0, D=1, Q=1, S=7, xreg = nyc_ridership$covid, newxreg = 61:84, par(main="TITLEMENOW"))

mean(abs(nyc_ridership$mod_residuals/nyc_ridership$trip_count))*100
