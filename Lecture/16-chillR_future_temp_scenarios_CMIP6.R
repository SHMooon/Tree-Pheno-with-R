
#chapter 15
#as a scientist need different models 
#coupled model intercomparison project (high quality models are included) 
  #most of them for coop. 
#



#chapter 16
#install.packages('chillR/chillR_0.74.2.tar.gz', repos=NULL, type='source')
require(chillR)
require(kableExtra)
require(tidyverse)
require(ecmwfr)
require(REMAGEN)
library(purrr)


Temperature_scenarios<-read.csv("data/Temperature_scenarios.csv")
Temperature_scenarios[,"Date"]<-as.Date(ISOdate(2000, Temperature_scenarios$Month, Temperature_scenarios$Day))
Bonn_temps <- read_tab("data/Bonn_temps.csv")

location=c(7.1,50.8)

area <- c(52, 6, 50, 8)

#user: user ID from Copernicus climate 
#key: from the ID I will get
#ecmwfr: European Centre for Medium-Range Weather Forecasts
#we can only start from 2015, which it started before
#inthe cmip6_downloaded file there is blacklist.txt, which shows scenarios not is not working

download_cmip6_ecmwfr(
  scenarios = 'ssp126',
  area =  area,
  user = '269601',
  key = '0b082782-deda-4eab-87e0-06d7f2c7fa4f',
  model = 'default',
  frequency = 'monthly',
  variable = c('Tmin', 'Tmax'),
  year_start = 2015,
  year_end = 2100,
  path_download = "cmip6_downloaded"
  )


#ssp scenario 2.6, 4.5, 7.0, 8.5

download_cmip6_ecmwfr(
  scenarios = c("historical", "ssp126", "ssp245", "ssp370", "ssp585"),
  area = area,
  user = '269601',
  key = '0b082782-deda-4eab-87e0-06d7f2c7fa4f',
  model = 'default',
  frequency = 'monthly',
  variable = c('Tmin', 'Tmax'),
  year_start = 2015,
  year_end = 2100,
  path_download = "cmip6_downloaded"
  )

#2015 all simulated data. it will give us the data is not changed and shows past data too. 





download_baseline_cmip6_ecmwfr(
  area = area,
  user = '269601',
  key = '0b082782-deda-4eab-87e0-06d7f2c7fa4f',
  model = 'match_downloaded',
  frequency = 'monthly',
  variable = c('Tmin', 'Tmax'),
  year_start = 1986,
  year_end = 2014,
  month = 1:12,
  path_download = "cmip6_downloaded"
  )

#1986is baseline 



station <- data.frame(
  station_name = c("Bonn"),
  longitude = c(7.1),
  latitude = c(50.8))


#not working
extracted <- extract_cmip6_data(stations = station)




head(extracted$`ssp126_AWI-CM-1-1-MR`)



change_scenarios <- gen_rel_change_scenario(extracted) #if the temperature changed from the baseline
head(change_scenarios)


#write.csv(change_scenarios, "data/all_change_scenarios.csv", row.names = FALSE)
#save this is important! that we don't have to runs all again. 

change_scenarios <- read.csv("data/all_change_scenarios.csv")
#2000 reference year.: 2050 compare to 2000
head(change_scenarios)

scen_list <- convert_scen_information(change_scenarios)

scen_frame <- convert_scen_information(scen_list)

scen_list$Bonn$ssp126$`ACCESS-CM2`$'2050'




temps_1996 <- temperature_scenario_from_records(Bonn_temps,1996)
temps_2000 <- temperature_scenario_from_records(Bonn_temps,2000)
temps_1996
temps_2000


base <- temperature_scenario_baseline_adjustment(temps_1996,temps_2000)

base

#scenario list
scen_list <- convert_scen_information(change_scenarios, 
                                      give_structure = FALSE)


adjusted_list <- temperature_scenario_baseline_adjustment(base,scen_list,
                                              temperature_check_args=list(
                                                scenario_check_thresholds = c(-5, 15)))#more that 5 degree cooling or 15 degree warmer than it will find it 


Bonn_temps<-read_tab("data/Bonn_temps.csv")


temps <- temperature_generation(Bonn_temps, years = c(1973, 2019), 
                                sim_years = c(2001, 2100), 
                                adjusted_list, 
                                temperature_check_args=list( 
                                  scenario_check_thresholds = c(-5, 15)))

# important the baseline download



save_temperature_scenarios(temps,"data/future_climate","Bonn_future")
temps <- load_temperature_scenarios("data/future_climate","Bonn_future")




# now we have temepratrue scenarios
frost_model <- function(x)
  step_model(x,
             data.frame(
               lower=c(-1000,0),
               upper=c(0,1000),
               weight=c(1,0)))

models <- list(Chill_Portions = Dynamic_Model,
               GDH = GDH,
               Frost_H = frost_model)



chill_future_scenario_list <- tempResponse_daily_list(temps,
                                                    latitude = 50.8,
                                                    Start_JDay = 305,
                                                    End_JDay = 59,
                                                    models = models)
chill_future_scenario_list <- lapply(chill_future_scenario_list,
                                     function(x) x %>%
                                       filter(Perc_complete == 100))

save_temperature_scenarios(chill_future_scenario_list,"data/future_climate","Bonn_futurechill")



chill_future_scenario_list <- load_temperature_scenarios("data/future_climate","Bonn_futurechill")


chill_hist_scenario_list<-load_temperature_scenarios("data","Bonn_hist_chill_305_59")
observed_chill <- read_tab("data/Bonn_observed_chill_305_59.csv")

# prepare for plotting 
chills <- make_climate_scenario(
  chill_hist_scenario_list,
  caption = "Historic",
  historic_data = observed_chill,
  time_series = TRUE)

plot_climate_scenarios(
  climate_scenario_list = chills,
  metric = "Chill_Portions",
  metric_label = "Chill (Chill Portions)")



SSPs <- c("ssp126", "ssp245", "ssp585")
Times <- c(2050, 2085)

list_ssp <- 
  strsplit(names(chill_future_scenario_list), '\\.') %>%
  map(2) %>%
  unlist()

list_gcm <-
  strsplit(names(chill_future_scenario_list), '\\.') %>%
  map(3) %>%
  unlist()

list_time <-
  strsplit(names(chill_future_scenario_list), '\\.') %>%
  map(4) %>%
  unlist()


for(SSP in SSPs)
  for(Time in Times)
    {
    
    # find all scenarios for the ssp and time
    chill <- chill_future_scenario_list[list_ssp == SSP & list_time == Time]
    names(chill) <- list_gcm[list_ssp == SSP & list_time == Time]
    if(SSP == "ssp126") SSPcaption <- "SSP1"
    if(SSP == "ssp245") SSPcaption <- "SSP2"
    if(SSP == "ssp585") SSPcaption <- "SSP5"    
    if(Time == "2050") Time_caption <- "2050"
    if(Time == "2085") Time_caption <- "2085"
    chills <- chill %>% 
      make_climate_scenario(
        caption = c(SSPcaption,
                    Time_caption),
        add_to = chills)
}

