library(chillR)
library(tidyverse)

Temp <- KA_weather %>%
  temperature_generation(years = c(1998,2009),
                         sim_years = c(2001,2100))


Temperatures <- KA_weather %>% filter(Year %in% 1998:2009) %>%
  cbind(Data_source = "observed") %>%
  rbind(
    Temp[[1]] %>% select(c(Year,Month,Day,Tmin,Tmax)) %>%
      cbind(Data_source = "simulated")
    ) %>%
  mutate(Date=as.Date(ISOdate(2000,Month,Day)))



ggplot(data = Temperatures,
       aes(Date,Tmin)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")

ggplot(data = Temperatures,
       aes(Date,Tmax)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")


chill_observed <- Temperatures %>%
  filter(Data_source == "observed") %>%
  stack_hourly_temps(latitude = 50.4) %>%
  chilling(Start_JDay = 305,
           End_JDay = 59)


### adding a freezing model - to make this for April, you'll also have 
# to adjust the dates in the calculations
df<-data.frame(
  lower= c(-1000, 0),
  upper= c(    0, 1000),
  weight=c(    1, 0))

freezing_hours<-function(x) step_model(x,df)

freezing_hours(c(1,2,4,5,-10))

chill_observed <- Temperatures %>%
  filter(Data_source == "observed") %>%
  stack_hourly_temps(latitude = 50.4) %>%
  tempResponse(Start_JDay = 305,
               End_JDay = 59,
               models=list(Frost = freezing_hours,
                           Chill_portions = Dynamic_Model,
                           GDH = GDH))


####


chill_simulated <- Temperatures %>%
  filter(Data_source == "simulated") %>%
  stack_hourly_temps(latitude = 50.4) %>%
  tempResponse(Start_JDay = 305,
               End_JDay = 59,
               models=list(Frost = freezing_hours,
                           Chill_portions = Dynamic_Model,
                           GDH = GDH))

chill_comparison <-
  cbind(chill_observed,
        Data_source = "observed") %>%
  rbind(cbind(chill_simulated,
              Data_source = "simulated"))

chill_comparison_full_seasons <-
  chill_comparison %>%
  filter(Perc_complete == 100)


ggplot(chill_comparison_full_seasons,
       aes(x=Chill_portions)) + 
  geom_histogram(binwidth = 1,
                 aes(fill = factor(Data_source))) +
  theme_bw(base_size = 10) +
  labs(fill = "Data source") +
  xlab("Chill accumulation (Chill Portions)") +
  ylab("Frequency")

ggplot(chill_comparison_full_seasons,
       aes(x=Frost)) + 
  geom_histogram(binwidth = 25,
                 aes(fill = factor(Data_source))) +
  theme_bw(base_size = 10) +
  labs(fill = "Data source") +
  xlab("Frost incidence during winter (hours)") +
  ylab("Frequency")

chill_simulations <-
  chill_comparison_full_seasons %>%
  filter(Data_source == "simulated")
  
ggplot(chill_simulations,
       aes(x = Chill_portions)) +
  stat_ecdf(geom = "step",
            lwd = 1.5,
            col = "blue") +
  ylab("Cumulative probability") +
  xlab("Chill accumulation (in Chill Portions)") +
  theme_bw(base_size = 20)

ggplot(chill_simulations,
       aes(x = Frost)) +
  stat_ecdf(geom = "step",
            lwd = 1.5,
            col = "blue") +
  ylab("Cumulative probability") +
  xlab("Frost incidence during winter (hours)") +
  theme_bw(base_size = 20)

# Here's the amount of chill that is exceeded in 90% of all years.
quantile(chill_simulations$Chill_portions,0.1)

# and here's the 50% confidence interval (25th to 75th percentile)
quantile(chill_simulations$Chill_portions, c(0.25,0.75))


