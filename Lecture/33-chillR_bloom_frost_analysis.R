

library(chillR)
library(leaflet)
library(dplyr)
library(reshape2)
library(kableExtra)
library(ggplot2)
library(Kendall)

# leaflet: map tool 
leaflet() %>%
  setView(lng = 6.99,
          lat = 50.625,
          zoom=12) %>%
  addTiles() %>% #shows area
  addMarkers(lng = 6.99, #marker
             lat = 50.625,
             popup = "Campus Klein-Altendorf")



#usual weather data in KA
CKA_Alexander_Lucas <- read_tab("data/Alexander_Lucas_bloom_1958_2019.csv")
CKA_weather <- read_tab("data/TMaxTMin1958-2019_patched.csv")

head(CKA_Alexander_Lucas)

library(tidyverse)

#we will focus on first-, and last bloom
Alexander_Lucas <- 
  CKA_Alexander_Lucas %>%
  pivot_longer(cols = "First_bloom":"Last_bloom",
               names_to = "variable",
               values_to="YEARMODA") %>%
  mutate(Year = as.numeric(substr(YEARMODA, 1, 4)), #split up year, month, day
         Month = as.numeric(substr(YEARMODA, 5, 6)),
         Day = as.numeric(substr(YEARMODA, 7, 8))) %>%
  make_JDay() 


head(Alexander_Lucas)

ggplot(data = Alexander_Lucas,
       aes(Pheno_year,
           JDay,
           col = variable)) + #different color, different values
  geom_line() +
  theme_bw(base_size = 15) +
  scale_color_discrete(
    name = "Phenological event",
    labels = c("First bloom",
               "Full bloom",
               "Last bloom")) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)")
#in this plot is not clear to see 


#combine linear regression 
ggplot(data = Alexander_Lucas,
       aes(Pheno_year,
           JDay,
           col = variable)) +
  geom_line() +
  theme_bw(base_size = 15) +
  scale_color_discrete(name = "Phenological event",
                       labels = c("First bloom",
                                  "Full bloom", 
                                  "Last bloom")) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)") +
  geom_smooth(method = "lm") #lm: linear regression


ggplot(data=Alexander_Lucas,aes(Pheno_year,JDay,col=variable)) +
  geom_smooth() +
  theme_bw(base_size=15) +
  scale_color_discrete(
    name = "Phenological event",
    labels = c("First bloom", "Full bloom", "Last bloom")) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)") 
#hard to say what is happen to here


require(Kendall)
#calculate first bloom date: 
Kendall_first <-
  Kendall(x = Alexander_Lucas$Pheno_year[
            which(Alexander_Lucas$variable == "First_bloom")],
          y = Alexander_Lucas$JDay[
            which(Alexander_Lucas$variable == "First_bloom")])

Kendall_full <- 
  Kendall(x = Alexander_Lucas$Pheno_year[
            which(Alexander_Lucas$variable == "Full_bloom")],
          y = Alexander_Lucas$JDay[
            which(Alexander_Lucas$variable == "Full_bloom")])

Kendall_last <- 
  Kendall(x = Alexander_Lucas$Pheno_year[
            which(Alexander_Lucas$variable == "Last_bloom")],
          y = Alexander_Lucas$JDay[
            which(Alexander_Lucas$variable == "Last_bloom")])

Kendall_first
Kendall_full
Kendall_last

#lm: linear modell function: 
#we can check with dollar sign more information: linear_trend_first$

linear_trend_first <- lm(
  Alexander_Lucas$JDay[
    which(Alexander_Lucas$variable == "First_bloom")]~
    Alexander_Lucas$Pheno_year[
      which(Alexander_Lucas$variable == "First_bloom")])

linear_trend_full <- lm(
  Alexander_Lucas$JDay[
    which(Alexander_Lucas$variable == "Full_bloom")]~
    Alexander_Lucas$Pheno_year[
      which(Alexander_Lucas$variable == "First_bloom")])

linear_trend_last <- lm(
  Alexander_Lucas$JDay[
    which(Alexander_Lucas$variable == "Last_bloom")]~
    Alexander_Lucas$Pheno_year[
      which(Alexander_Lucas$variable == "First_bloom")])

linear_trend_first
linear_trend_full
linear_trend_last


phenology_trends <-
  data.frame(Stage = c("First bloom",
                       "Full bloom", 
                       "Last bloom"),
             Kendall_tau = c(round(Kendall_first[[1]][1],3),
                             round(Kendall_full[[1]][1],3),
                             round(Kendall_last[[1]][1],3)),
             Kendall_p = c(round(Kendall_first[[2]][1],3),
                           round(Kendall_full[[2]][1],3),
                           round(Kendall_last[[2]][1],3)),
             Linear_trend_per_decade =
               c(round(linear_trend_first[[1]][2],2) * 10,
                 round(linear_trend_full[[1]][2],2) * 10,
                 round(linear_trend_last[[1]][2],2) * 10)
             )


phenology_trends

#weight function to calculate... frost 
frost_df = data.frame(
  lower = c(-1000, 0),
  upper = c(0, 1000),
  weight = c(1, 0)) #weight =1: frost problem


# identifing all the frost model 
frost_model <- function(x) step_model(x,
                                      frost_df)



hourly <- stack_hourly_temps(CKA_weather,
                             latitude = 50.625)

frost <- tempResponse(hourly,
                      models = c(frost = frost_model))


#past situation: how much frost in general in KA
#the frost hours per years is sinking: climate change and global warming
ggplot(frost,
       aes(End_year,
           frost)) +
  geom_smooth() +
  geom_point() +
  ylim(c(0, NA)) +
  ylab("Frost hours per year") +
  xlab("Year")


Kendall(x = frost$End_year,
        y = frost$frost)

lm(frost$frost ~ frost$End_year)
  #more than 5 hours per year lost 

#the model with out summ: (summ: Boolean parameter indicating whether calculated metrics 
  #should be provided as cumulative values over the entire record (TRUE) or as the actual 
  #accumulation for each hour (FALSE).) 

frost_model_no_summ <- 
  function(x) step_model(x, 
                         frost_df,
                         summ=FALSE)

hourly$hourtemps[, 
                 "frost"] <- frost_model_no_summ(hourly$hourtemps$Temp)

#frost on daily basis
Daily_frost_hours <- aggregate(hourly$hourtemps$frost,
                               by = list(hourly$hourtemps$YEARMODA), #this function we need to put in list
                               FUN = sum) #FUN: a fuction to compute the summary statistics

Daily_frost <- make_JDay(CKA_weather)

#from the aggrogate data set
Daily_frost[, 
            "Frost_hours"] <- Daily_frost_hours$x

#if there are 0 then it is difficult so replace the frost values by NA, 0= nodata
Daily_frost$Frost_hours[which(Daily_frost$Frost_hours == 0)] <- NA

ggplot(data = Daily_frost,
       aes(Year,
           JDay,
           size = Frost_hours)) +
  geom_point(col = "light blue",
             alpha = 0.8) + 
  scale_size(range = c(0, 3),
             breaks = c(1, 5, 10, 15, 20),
             labels = c("1", "5", "10", "15", "20"),
             name = "Frost hours") +
  theme_bw(base_size = 15)
  #the graph is not clear so need to cut the data that we don't have interest: 


#combine the linear model
ggplot(data = Alexander_Lucas,
       aes(Pheno_year,
           JDay,
           col = variable)) +
  geom_line() +
  theme_bw(base_size = 15) +
  scale_color_discrete(
    name = "Phenological event",
    labels = c("First bloom",
               "Full bloom",
               "Last bloom")) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)") +
  geom_point(data = Daily_frost,
             aes(Year,
                 JDay,
                 size = Frost_hours),
             col = "light blue",
             alpha = 0.8) + 
  scale_size(range = c(0, 3),
             breaks = c(1, 5, 10, 15, 20),
             labels = c("1", "5", "10", "15", "20"),
             name = "Frost hours") +
  theme_bw(base_size = 15)


#cut only what we need 
ggplot(data = Alexander_Lucas,
       aes(Pheno_year,
           JDay,
           col = variable)) +
  geom_line() +
  theme_bw(base_size = 15) +
  scale_color_discrete(
    name = "Phenological event",
    labels = c("First bloom",
               "Full bloom",
               "Last bloom")) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)") +
  geom_point(data = Daily_frost,
             aes(Year,
                 JDay,
                 size = Frost_hours),
             col = "light blue",
             alpha = 0.8) + 
  scale_size(range = c(0, 3),
             breaks = c(1, 5, 10, 15, 20),
             labels = c("1", "5", "10", "15", "20"),
             name = "Frost hours") +
  theme_bw(base_size = 15) +
  ylim(c(75, 140))



#Ribbon function: 

#with pivot wider function to go back where we want to be, only select the interesting column
Ribbon_Lucas <-
  Alexander_Lucas %>%
  select(Pheno_year, variable, JDay) %>%
  pivot_wider(names_from = "variable", values_from = "JDay")


ggplot(data = Ribbon_Lucas,
       aes(Pheno_year)) +
  geom_ribbon(aes(ymin = First_bloom,
                  ymax = Last_bloom),
              fill = "light gray") +  #it shows the range of the blooming date 
  geom_line(aes(y = Full_bloom)) + #full bloom date 
  theme_bw(base_size = 15) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)") +
  geom_point(data = Daily_frost,
             aes(Year,
                 JDay,
                 size = Frost_hours),
             col = "light blue",
             alpha = 0.8) + 
  scale_size(range = c(0, 3),
             breaks = c(1, 5, 10, 15, 20),
             labels = c("1", "5", "10", "15", "20"),
             name = "Frost hours") +
  theme_bw(base_size = 15) +
  ylim(c(75, 140))
  # there are frost hours in the blooming date



#different color: frost before and  during the bloom
# identify frost events that overlap with bloom
lookup_dates <- Ribbon_Lucas

row.names(lookup_dates) <- lookup_dates$Pheno_year 


#add blooming date in daily frost data set

#year name will act as a character not as a number; first and last bloom data
Daily_frost[, "First_bloom"]<-
  lookup_dates[as.character(Daily_frost$Year),
               "First_bloom"] 

Daily_frost[, "Last_bloom"]<-
  lookup_dates[as.character(Daily_frost$Year),
               "Last_bloom"]

#from Daily_frost$Frost_hours takes data: before bloom, during bloom, after bloom 
Daily_frost[which(!is.na(Daily_frost$Frost_hours)),
            "Bloom_frost"] <-
  "Before bloom"

Daily_frost[which(Daily_frost$JDay >= Daily_frost$First_bloom),
            "Bloom_frost"]<-
  "During bloom"

Daily_frost[which(Daily_frost$JDay > Daily_frost$Last_bloom),
            "Bloom_frost"]<-
  "After bloom"

Daily_frost[which(Daily_frost$JDay > 180),
            "Bloom_frost"]<-
  "Before bloom"

ggplot(data = Ribbon_Lucas,
       aes(Pheno_year)) +
  geom_ribbon(aes(ymin = First_bloom, 
                  ymax = Last_bloom),
              fill = "light gray") +
  geom_line(aes(y = Full_bloom)) +
  theme_bw(base_size = 15) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)") +
  geom_point(data = Daily_frost, #add daily frost
             aes(Year,
                 JDay, 
                 size = Frost_hours,
                 col = Bloom_frost), #new column bloom frost 
             alpha = 0.8) + 
  scale_size(range = c(1, 6),
             breaks = c(1, 5, 10, 15, 20),
             labels = c("1", "5", "10", "15", "20"),
             name = "Frost hours") +
  scale_color_manual(
    breaks = c("Before bloom", # lot of frost in winter: not surprising 
               "During bloom",
               "After bloom"), #NA will be removed
    values = c("light green",
               "red", #for during bloom
               "light blue"),
    name = "Frost timing") +
  theme_bw(base_size = 15) +
  ylim(c(75, 140))

  #size of dots makes difference by blooming 
  #we can see when there was problem 



Bloom_frost_trend <- 
  aggregate(
    Daily_frost$Frost_hours,
    by = list(Daily_frost$Year,
              Daily_frost$Bloom_frost),
    FUN = function(x) sum(x,
                          na.rm = TRUE))

colnames(Bloom_frost_trend) <- c("Year",
                                 "Frost_timing",
                                 "Frost_hours")

DuringBloom<-
  Bloom_frost_trend[
    which(Bloom_frost_trend$Frost_timing == "During bloom"),]

#frequency of the frost hour during the blooming days
ggplot(data = DuringBloom,
       aes(Year,
           Frost_hours)) +
  geom_col() 


Kendall(x = DuringBloom$Year,
        y = DuringBloom$Frost_hours)
  #p value is far from 0.5: 

lm(DuringBloom$Frost_hours ~ DuringBloom$Year)
  #in linear model: increase 0.08 hours of frost day 




#exercise 
# same location but different cultivar with roter~


#next time some of plots from own data 
# in logbook
