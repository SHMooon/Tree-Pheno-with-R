## Filling short gaps in daily records

library(chillR)
library(tidyverse)
weather <- KA_weather %>% make_all_day_table()

Tmin_int <- interpolate_gaps(weather[,"Tmin"])

weather <- weather %>% mutate(Tmin = Tmin_int$interp,
                              Tmin_interpolated = Tmin_int$missing)

Tmax_int <- interpolate_gaps(weather[,"Tmax"])

weather <- weather %>% mutate(Tmax = Tmax_int$interp,
                              Tmax_interpolated = Tmax_int$missing)


# add an extra day to the KA_weather dataset that is not connected to the days that are already there.
# this creates a large gap, which we can then interpolate
KA_weather_gap <- rbind(KA_weather, c(Year = 2011,
                                      Month = 3,
                                      Day = 3,
                                      Tmax = 26,
                                      Tmin = 14)) 

# fill in the gaps between Julian date 300 (late October) and 100 (early April), only returning data between 2000 and 2011
fixed_winter_days <- KA_weather_gap %>% fix_weather(start_year = 2000,
                                                    end_year = 2011,
                                                    start_date = 300,
                                                    end_date = 100)

# fill in all gaps
fixed_all_days <- KA_weather_gap %>% fix_weather()

fixed_winter_days$QC

fixed_all_days$QC

gap_weather <- KA_weather[200:305,]
gap_weather[,"Tmin_observed"] <- gap_weather$Tmin
gap_weather$Tmin[c(2,4:5,7:9,11:14,16:20,22:27,29:35,
                   37:44,46:54,56:65,67:77,79:90,92:104)] <- NA
fixed_gaps <- fix_weather(gap_weather)$weather

ggplot(data=fixed_gaps,
       aes(DATE,Tmin_observed)) +
  geom_line(lwd=1.3) +
  xlab("Date") +
  ylab("Daily minimum temperature (°C)") +
  geom_line(data=fixed_gaps,aes(DATE,Tmin),col="red",lwd=1.3)




fixed_gaps[,"error"] <- abs(fixed_gaps$Tmin - fixed_gaps$Tmin_observed)

ggplot(data=fixed_gaps,
       aes(DATE,error)) +
  geom_line(lwd=1.3) +
  xlab("Date") +
  ylab("Error introduced by interpolation (°C)") +
  geom_point(data=fixed_gaps[which(!fixed_gaps$no_Tmin),],
             aes(DATE,error),col="red",cex=3)


## Filling long gaps in daily records


Bonn <- read.csv("data/Bonn_chillR_weather.csv")

Bonn_QC <- fix_weather(Bonn)$QC

Bonn_QC

station_list <- handle_gsod(action="list_stations",
                            location=c(7.10,50.73),
                            time_interval=c(1990,2020))
station_list

patch_weather<-
  handle_gsod(action = "download_weather",
              location = as.character(station_list$chillR_code[c(2,3,6)]),
              time_interval = c(1990,2020)) %>%
  handle_gsod()

patched <- patch_daily_temperatures(weather = Bonn,
                                    patch_weather = patch_weather)

patched$statistics[[1]]


patched_monthly <- patch_daily_temps(weather = Bonn,
                                     patch_weather = patch_weather,
                                     max_mean_bias = 1,
                                     max_stdev_bias = 2,
                                     time_interval = "month")
monthly_bias_fixed <- fix_weather(patched_monthly)
# follow all... 
patched <- patch_daily_temperatures(weather = Bonn,
                                    patch_weather = patch_weather,
                                    max_mean_bias = 1,
                                    max_stdev_bias = 2)

write.csv(monthly_bias_fixed$weather,
          "data/Bonn_weather.csv")

