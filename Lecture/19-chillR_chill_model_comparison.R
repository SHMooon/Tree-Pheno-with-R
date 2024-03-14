
library(chillR)
library(dormancyR)
library(tidyverse)
library(colorRamps)
library(patchwork)
library(gganimate)

SSPs <- c("ssp126", "ssp245", "ssp585")
Times <- c(2050, 2085)

library(chillR)
library(devtools)
install_github("EduardoFernandezC/dormancyR")
#devtools::install_github("https://github.com/EduardoFernandezC/dormancyR")
library(dormancyR)


#mix of hourly models, 
hourly_models <- list(Chilling_units = chilling_units,
     Low_chill = low_chill_model,
     Modified_Utah = modified_utah_model,
     North_Carolina = north_carolina_model,
     Positive_Utah = positive_utah_model,
     Chilling_Hours = Chilling_Hours,
     Utah_Chill_Units = Utah_Model,
     Chill_Portions = Dynamic_Model)



#daily steps.
daily_models <- list(Rate_of_Chill = rate_of_chill,
                     Chill_Days = chill_days,
                     Exponential_Chill = exponential_chill,
                     Triangular_Chill_Haninnen = triangular_chill_1,
                     Triangular_Chill_Legave = triangular_chill_2)

metrics <- c(names(daily_models),
             names(hourly_models))
#all of them is 13 different models 

#replace the names with 'spaces' etc.
model_labels = c("Rate of Chill",
                 "Chill Days",
                 "Exponential Chill",
                 "Triangular Chill (Häninnen)",
                 "Triangular Chill (Legave)",
                 "Chilling Units",
                 "Low-Chill Chill Units",
                 "Modified Utah Chill Units",
                 "North Carolina Chill Units",
                 "Positive Utah Chill Units",
                 "Chilling Hours",
                 "Utah Chill Units",
                 "Chill Portions")


## data.frame(Metric=model_labels,'Function name'=metrics)


data.frame(Metric=model_labels,'Function name'=metrics)

Bonn_temps <- read_tab("data/Bonn_temps.csv")

Temps <- load_temperature_scenarios("data",
                                    "Bonn_hist_scenarios")



Start_JDay <- 305
End_JDay <- 59

daily_models_past_scenarios <-
  tempResponse_list_daily(Temps,
                          Start_JDay = Start_JDay,
                          End_JDay = End_JDay,
                          models=daily_models)

#remove the complicate seasons 
#x: Perc_complete>90 with the fuction 
daily_models_past_scenarios <- lapply(
  daily_models_past_scenarios,
  function(x) x[which(x$Perc_complete>90),])

hourly_models_past_scenarios<-
  tempResponse_daily_list(Temps,
                          latitude = 50.866,
                          Start_JDay = Start_JDay,
                          End_JDay = End_JDay,
                          models = hourly_models,
                          misstolerance = 10)

past_scenarios <- daily_models_past_scenarios

past_scenarios <- lapply(
  names(past_scenarios),
  function(x)
    cbind(past_scenarios[[x]],
          hourly_models_past_scenarios[[x]][,names(hourly_models)]))

names(past_scenarios) <- names(daily_models_past_scenarios)

daily_models_observed <-
  tempResponse_daily(Bonn_temps,
                     Start_JDay = Start_JDay,
                     End_JDay = End_JDay,
                     models = daily_models)


daily_models_observed <-
  daily_models_observed[which(daily_models_observed$Perc_complete>90),]

hourly_models_observed <-
  tempResponse_daily_list(Bonn_temps,
                          latitude=50.866,
                          Start_JDay = Start_JDay,
                          End_JDay = End_JDay,
                          models = hourly_models,
                          misstolerance = 10)

past_observed <- cbind(
  daily_models_observed,
  hourly_models_observed[[1]][,names(hourly_models)])

save_temperature_scenarios(past_scenarios,
                           "data/future_climate",
                           "Bonn_multichill_305_59_historic")
write.csv(past_observed,
          "data/future_climate/Bonn_multichill_305_59_observed.csv",
          row.names=FALSE)



future_temps <- load_temperature_scenarios("data/future_climate","Bonn_future_")

SSPs <- c("ssp126", "ssp245", "ssp585")
Times <- c(2050, 2085)


list_ssp <-
  strsplit(names(future_temps), '\\.') %>% 
  map(2) %>%
  unlist()

list_gcm <-
  strsplit(names(future_temps), '\\.') %>%
  map(3) %>%
  unlist()

list_time <-
  strsplit(names(future_temps), '\\.') %>%
  map(4) %>%
  unlist()




for(SSP in SSPs)
  for(Time in Times)
    {
    Temps <- future_temps[list_ssp == SSP & list_time == Time] #what we loaded, and SSP, 
    names(Temps) <- list_gcm[list_ssp == SSP & list_time == Time]
    daily_models_future_scenarios <- tempResponse_list_daily(
      Temps,
      Start_JDay = Start_JDay,
      End_JDay = End_JDay,
      models = daily_models)
    daily_models_future_scenarios<-lapply(
      daily_models_future_scenarios,
      function(x) x[which(x$Perc_complete>90),])
    hourly_models_future_scenarios<-
      tempResponse_daily_list(
        Temps,
        latitude = 50.866,
        Start_JDay = Start_JDay,
        End_JDay = End_JDay,
        models=hourly_models,
        misstolerance = 10)

    future_scenarios <- daily_models_future_scenarios

    future_scenarios <- lapply(
      names(future_scenarios),
      function(x)
        cbind(future_scenarios[[x]],
              hourly_models_future_scenarios[[x]][,names(hourly_models)]))
    names(future_scenarios)<-names(daily_models_future_scenarios)

    chill<-future_scenarios

    save_temperature_scenarios(
      chill,
      "data/future_climate",
      paste0("Bonn_multichill_305_59_",Time,"_",SSP))
} # all the hist code set in to the future... 



chill_past_scenarios <- load_temperature_scenarios(
  "data/future_climate",
  "Bonn_multichill_305_59_historic")

chill_observed <- 
  read_tab("data/future_climate/Bonn_multichill_305_59_observed.csv")


chills <- make_climate_scenario(chill_past_scenarios,
                                caption = "Historic",
                                historic_data = chill_observed,
                                time_series = TRUE)


for(SSP in SSPs)
  for(Time in Times)
    {
    chill <- load_temperature_scenarios(
      "data/future_climate",
      paste0("Bonn_multichill_305_59_",Time,"_",SSP))
    if(SSP == "ssp126") SSPcaption <- "SSP1"
    if(SSP == "ssp245") SSPcaption <- "SSP2"
    if(SSP == "ssp585") SSPcaption <- "SSP5"    
    if(Time == "2050") Time_caption <- "2050"
    if(Time == "2085") Time_caption <- "2085"
    chills <- make_climate_scenario(chill,
                                    caption = c(SSPcaption,
                                                Time_caption),
                                    add_to = chills)
}


#plot_climate_scenarios(chills, metric = "Rate_of_chill",
#                        metric_label = "Rate of chill",
#                        End_year = "End_year")


for(i in 1:length(chills))
   {ch <- chills[[i]]
   if(ch$caption[1] == "Historic")
     {GCMs <- rep("none",length(names(ch$data)))
      SSPs <- rep("none",length(names(ch$data)))
      Years <- as.numeric(ch$labels)
      Scenario <- rep("Historic",
                      length(names(ch$data)))} else
                        {GCMs <- names(ch$data)
                        SSPs <- rep(ch$caption[1],
                                    length(names(ch$data)))
                        Years <- rep(as.numeric(ch$caption[2]),
                                     length(names(ch$data)))
                        Scenario <- rep("Future",
                                        length(names(ch$data)))}
   for(nam in names(ch$data))
     {for(met in metrics)
       {temp_res <-
         data.frame(Metric = met,
                    GCM = GCMs[which(nam == names(ch$data))],
                    SSP = SSPs[which(nam == names(ch$data))],
                    Year = Years[which(nam == names(ch$data))],
                    Result = quantile(ch$data[[nam]][,met],0.1), 
                    Scenario = Scenario[which(nam == names(ch$data))])
       if(i == 1 & nam == names(ch$data)[1] & met == metrics[1])
         results <- temp_res else
           results <- rbind(results,
                            temp_res)
         }
     }
   }

for(met in metrics)
  results[which(results$Metric == met),"SWC"] <-
    results[which(results$Metric == met),"Result"]/
      results[which(results$Metric == met & results$Year == 1980),
              "Result"]-1

## Here we start

rng = range(results$SWC)
#rng: (decrease 78% amd increase 60% )


p_future <- ggplot(results[which(!results$GCM == "none"),],
                   aes(GCM, #y axis is with GCM
                       y = factor(Metric, #with metric column
                                  levels = metrics), #with factor we can specify levels 
                       fill = SWC)) + #fill winter chill
  geom_tile() #it will fill like a tile

p_future
# SWC safe winter chill


#different scenario (SSP and Years)
p_future <-
  p_future +
  facet_grid(SSP ~ Year) 

p_future

#different layout (theme_bw, or different form: theme_dark,, etc.), 
p_future <-
  p_future +
  theme_bw(base_size = 15) +
  theme(axis.text = element_text(size=8)) 

p_future

library(colorRamps)
p_future <-
  p_future +
  scale_fill_gradientn(colours = rev(matlab.like(15)),# different colors (gradientn). like a palette: matlab.like (15)
                       #rev (reverse): 
                       labels = scales::percent, #labels show with % 
                       limits = rng) #adjusted range (it was around -80 to 60%, but now it is adjusted)

p_future


p_future <-
  p_future  +
  theme(axis.text.x = element_text(angle = 75, #angle 75°
                                   hjust = 1, #horizontal
                                   vjust = 1)) + #vertical 
  labs(fill = "Change in\nSafe Winter Chill\nsince 1975") + #\n like in python next line
  scale_y_discrete(labels = model_labels) +
  ylab("Chill metric")

p_future

#Utah models show different than the others. 
#some of scenario show decrease... 
#it shows that how important to choose a model. 




p_past<-
  ggplot(results[which(results$GCM == "none"),],
         aes(Year,
             y = factor(Metric, 
                        levels=metrics),
             fill = SWC)) +
  geom_tile() #we will chose. y axis with year



p_past<-
  p_past +
  theme_bw(base_size = 15) +
  theme(axis.text = element_text(size = 8))


p_past<-
  p_past +
  scale_fill_gradientn(colours = rev(matlab.like(15)),
                       labels = scales::percent,
                       limits = rng)

p_past<-
  p_past +
  scale_x_continuous(position = "top") #the scale is put top of the plot

#some effects here: utah model shows difference from 1908 to 2010 


p_past<-
  p_past +
  labs(fill = "Change in\nSafe Winter Chill\nsince 1975") +
  scale_y_discrete(labels = model_labels) +
  ylab("Chill metric")

#p_past


#put it together with patchwork
chill_comp_plot<-
  (p_past +
     p_future +
     plot_layout(guides = "collect", #change layout, similar will be merged 
                 nrow = 2,
                 heights = c(1,3))) &
  theme(legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

chill_comp_plot



#how the different metrics is changing over time. 
#summarize all the climate model

hist_results <- results[which(results$GCM == "none"),] #GCM is none
hist_results$SSP <- "SSP1"
hist_results_2 <- hist_results
hist_results_2$SSP <- "SSP2"
hist_results_3 <- hist_results
hist_results_3$SSP <- "SSP5"
hist_results <- rbind(hist_results,
                      hist_results_2,
                      hist_results_3) #bind all together 

future_results <- results[which(!results$GCM == "none"),]

GCM_aggregate <- aggregate( #aggregate: look all values, average, 
  #(SWC value, and the list, which contains metric, ssp and each years)
  future_results$SWC,
  by=list(future_results$Metric,
          future_results$SSP,
          future_results$Year),
  FUN=mean) 

colnames(GCM_aggregate) <- c("Metric",
                             "SSP",
                             "Year",
                             "SWC") #change the col names

SSP_Time_series<-rbind(hist_results[,c("Metric",
                                       "SSP",
                                       "Year",
                                       "SWC")],
                       GCM_aggregate) # bind the colname and hist results



SSP_Time_series$Year <- as.numeric(SSP_Time_series$Year) #put as a number

chill_change_plot<-
  ggplot(data = SSP_Time_series,
         aes(x = Year,
             y = SWC,
             col = factor(Metric,
                          levels = metrics))) +
  geom_line(lwd = 1.3) +
  facet_wrap(~SSP,
             nrow = 3) +
  theme_bw(base_size = 15) +
  labs(col = "Change in\nSafe Winter Chill\nsince 1975") +
  scale_color_discrete(labels = model_labels) +
  scale_y_continuous(labels = scales::percent) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  ylab("Safe Winter Chill")

chill_change_plot


#for a presentation is cool 
library(gganimate) #animate a ggplot
library(gifski) #save gif
library(png)
ccp <- chill_change_plot + transition_reveal(Year) 
  animate(ccp, fps = 20, duration = 5) #+ #reveal the year

anim_save("data/chill_comparison_animation_fps_20_duration_5.gif",
          animation = last_animation())
