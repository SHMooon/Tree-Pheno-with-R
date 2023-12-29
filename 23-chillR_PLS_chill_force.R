library(chillR)
library(kableExtra)

temps_hourly <- read_tab("data/TMaxTMin1958-2019_patched.csv") %>%
  stack_hourly_temps(latitude = 50.6)


head(temps_hourly$hourtemps)



daychill <- daily_chill(hourtemps = temps_hourly,
                        running_mean = 1,
                        models = list(
                          Chilling_Hours = Chilling_Hours,
                          Utah_Chill_Units = Utah_Model,
                          Chill_Portions = Dynamic_Model,
                          GDH = GDH)
                        )

head(daychill$daily_chill)


dc <- make_daily_chill_plot2(daychill,
                             metrics = c("Chill_Portions"),
                             cumulative = FALSE,
                             startdate = 300,
                             enddate = 30,
                             focusyears = c(2008), 
                             metriclabels = "Chill Portions")
#the gap in Dec and Jan: there are not same days in every month 



#cumulative option: true out the season 
#we still see the gap 
#if we use na.rm=true, in sd 

dc <- make_daily_chill_plot2(daychill,
                             metrics = c("Chill_Portions"),
                             cumulative = TRUE, 
                             startdate = 300,
                             enddate = 30,
                             focusyears = c(2008),
                             metriclabels = "Chill Portions")


#we can also add more years 
dc <- make_daily_chill_plot2(daychill,
                             metrics = c("Chill_Portions"),
                             cumulative = TRUE, 
                             startdate = 300,
                             enddate = 30,
                             focusyears = c(2008,2018),
                             metriclabels = "Chill Portions")



library(tidyverse)

Alex_first <- read_tab("data/Alexander_Lucas_bloom_1958_2019.csv") %>%
  select(Pheno_year, First_bloom) %>%
  mutate(Year = as.numeric(substr(First_bloom, 1, 4)), #select with substring comment 
         Month = as.numeric(substr(First_bloom, 5, 6)),
         Day = as.numeric(substr(First_bloom, 7, 8))) %>%
  make_JDay() %>%
  select(Pheno_year, 
         JDay) %>%
  rename(Year = Pheno_year, #rename the columns 
         pheno = JDay)



plscf <- PLS_chill_force(daily_chill_obj = daychill,
                         bio_data_frame = Alex_first,
                         split_month = 6,
                         chill_models = "Chill_Portions", #we can add multiple models
                         heat_models = "GDH") #name of heat model 

head(plscf$Chill_Portions$GDH$PLS_summary)

#nrow((plscf$Chill_Portions$GDH$PLS_summary)): 626 because of end at pheno end = True 

plot_PLS(plscf,
         PLS_results_path = "data/plscf_outputs")


plscf <- PLS_chill_force(daily_chill_obj = daychill,
                         bio_data_frame=Alex_first,
                         split_month = 6,
                         chill_models = "Chill_Portions",
                         heat_models = "GDH",
                         runn_means = 11) # a running mean filter. This usually enhances the clarity of results




plot_PLS(plscf,
         PLS_results_path = "data/plscf_outputs_11days")



#chilling phase in light blue rectangle as background and force phase in red 
#grey rectangle shows bluming days 
plot_PLS(plscf,
         PLS_results_path = "data/plscf_outputs_11days_periods",
         add_chill = c(-48,62),
         add_heat = c(-5,105.5))



#work only with the result table 
PLS_gg <- plscf$Chill_Portions$GDH$PLS_summary %>%
  mutate(Month = trunc(Date/100),
         Day = Date - Month * 100,
         Date = ISOdate(2002,
                        Month,
                        Day))

PLS_gg[PLS_gg$JDay <= 0,"Date"]<-
  ISOdate(2001,
          PLS_gg$Month[PLS_gg$JDay <= 0],
          PLS_gg$Day[PLS_gg$JDay <= 0])

PLS_gg <- PLS_gg %>%
  mutate(VIP_importance = VIP >= 0.8,
         VIP_Coeff = factor(sign(Coef) * VIP_importance)) #sign(Coef): ±1, *VIP_importance true = 1, 

chill_start_JDay <- -48
chill_end_JDay <- 62
heat_start_JDay <- -5
heat_end_JDay <- 105.5

chill_start_date <- ISOdate(2001,#normal R day
                            12,
                            31) + chill_start_JDay * 24 * 3600 #(chill start day= 62)*24 (time of second)*3600(days))
chill_end_date <- ISOdate(2001,
                          12,
                          31) + chill_end_JDay * 24 * 3600
heat_start_date <- ISOdate(2001,
                           12,
                           31) + heat_start_JDay * 24 * 3600
heat_end_date <- ISOdate(2001,
                         12,
                         31) + heat_end_JDay * 24 * 3600


library(ggplot2)

temp_plot <- ggplot(PLS_gg,
                    x = Date) +
  annotate("rect", 
           xmin = chill_start_date,
           xmax = chill_end_date,
           ymin = -Inf,
           ymax = Inf,
           alpha = .1, #indicator about transparancy of something
           fill = "blue") +
  annotate("rect",
           xmin = heat_start_date,
           xmax = heat_end_date,
           ymin = -Inf,
           ymax = Inf,
           alpha = .1,
           fill = "red") +
  annotate("rect",
           xmin = ISOdate(2001,
                          12,
                          31) +
             min(plscf$pheno$pheno,
                 na.rm = TRUE) * 24 * 3600,
           xmax = ISOdate(2001,
                          12,
                          31) +
             max(plscf$pheno$pheno,
                 na.rm = TRUE) * 24 * 3600,
           ymin = -Inf,
           ymax = Inf,
           alpha = .1,
           fill = "black") +
  geom_vline(xintercept = ISOdate(2001,
                                  12,
                                  31) +
               median(plscf$pheno$pheno,
                      na.rm = TRUE) * 24 * 3600,
             linetype = "dashed") +
  geom_ribbon(aes(x = Date,
                  ymin = MetricMean - MetricStdev , #MetricMean & MetricStdev <-  from the Summary data
                  ymax = MetricMean + MetricStdev),
              fill="grey") +
  geom_ribbon(aes(x = Date,
                  ymin = MetricMean - MetricStdev * (VIP_Coeff == -1), #if the VIP Coeff is -1: then shows red bar from min to max
                  ymax = MetricMean + MetricStdev * (VIP_Coeff == -1)),
              fill = "red") +
  geom_ribbon(aes(x = Date,
                  ymin = MetricMean - MetricStdev * (VIP_Coeff == 1), #only shows the positive ones 
                  ymax = MetricMean + MetricStdev * (VIP_Coeff == 1)),
              fill = "dark green") +
  geom_line(aes(x = Date,
                y = MetricMean ))

temp_plot

temp_plot <- temp_plot +
  facet_wrap(vars(Type), #type variable
             scales = "free_y", #free_y means choose own scale
             strip.position = "left",
             labeller = #change the label for two different plots
               labeller(Type = as_labeller(
               c(Chill = "Chill (CP)", 
                 Heat = "Heat (GDH)")))) +
  ggtitle("Daily chill and heat accumulation rates") +
  theme_bw(base_size = 15) + 
  theme(strip.background = element_blank(), #change color of lable 
        strip.placement = "outside", # and put the lable outside of a plot 
        strip.text.y = element_text(size =12),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank() 
        )

temp_plot



VIP_plot<- ggplot(PLS_gg,
                  aes(x = Date,
                      y = VIP)) +
  annotate("rect",
           xmin = chill_start_date,
           xmax = chill_end_date,
           ymin = -Inf,
           ymax = Inf,
           alpha = .1,
           fill = "blue") +
  annotate("rect",
           xmin = heat_start_date,
           xmax = heat_end_date,
           ymin = -Inf,
           ymax = Inf,
           alpha = .1,
           fill = "red") +
  annotate("rect",
           xmin = ISOdate(2001,
                          12,
                          31) +
             min(plscf$pheno$pheno,
                 na.rm = TRUE) * 24 * 3600,
           xmax = ISOdate(2001,
                          12,
                          31) +
             max(plscf$pheno$pheno,
                 na.rm = TRUE) * 24 * 3600,
           ymin = -Inf,
           ymax = Inf,
           alpha = .1,
           fill = "black") +
  geom_vline(xintercept = ISOdate(2001,
                                  12,
                                  31) +
               median(plscf$pheno$pheno,
                      na.rm = TRUE) * 24 * 3600,
             linetype = "dashed") +
  geom_bar(stat = 'identity',
           aes(fill = VIP > 0.8))

VIP_plot

VIP_plot <- VIP_plot + 
  facet_wrap(vars(Type),
             scales = "free",
             strip.position = "left",
             labeller = 
               labeller(Type = as_labeller(
                 c(Chill = "VIP for chill",
                   Heat = "VIP for heat")))) +
  scale_y_continuous(
    limits = c(0,
               max(plscf$Chill_Portions$GDH$PLS_summary$VIP))) +
  ggtitle("Variable Importance in the Projection (VIP) scores") +
  theme_bw(base_size = 15) + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()
        )
  
VIP_plot

VIP_plot <- VIP_plot +
  scale_fill_manual(name = "VIP", 
                    labels = c("<0.8", ">0.8"), #change the legend and color
                    values = c("FALSE" = "grey",
                               "TRUE" = "blue")) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

VIP_plot

coeff_plot <- ggplot(PLS_gg,
                     aes(x = Date,
                         y = Coef)) +
  annotate("rect",
           xmin = chill_start_date,
           xmax = chill_end_date,
           ymin = -Inf,
           ymax = Inf,
           alpha = .1,
           fill = "blue") +
  annotate("rect",
           xmin = heat_start_date,
           xmax = heat_end_date,
           ymin = -Inf,
           ymax = Inf,
           alpha = .1,
           fill = "red") +
  annotate("rect",
           xmin = ISOdate(2001,
                          12,
                          31) +
             min(plscf$pheno$pheno,
                 na.rm=TRUE) * 24 * 3600,
           xmax = ISOdate(2001,
                          12,
                          31) +
             max(plscf$pheno$pheno,
                 na.rm = TRUE) * 24 * 3600,
           ymin = -Inf,
           ymax = Inf,
           alpha = .1,
           fill = "black") +
  geom_vline(xintercept = ISOdate(2001,
                                  12,
                                  31) +
               median(plscf$pheno$pheno,
                      na.rm = TRUE) * 24 * 3600,
             linetype = "dashed") +
  geom_bar(stat = 'identity',
           aes(fill = VIP_Coeff))

coeff_plot

coeff_plot <- coeff_plot +
  facet_wrap(vars(Type),
             scales = "free",
             strip.position = "left",
             labeller =
               labeller(
                 Type = as_labeller(
                   c(Chill = "MC for chill",
                     Heat = "MC for heat")))) +
  scale_y_continuous(
    limits = c(min(plscf$Chill_Portions$GDH$PLS_summary$Coef),
               max(plscf$Chill_Portions$GDH$PLS_summary$Coef))) +
  ggtitle("Model coefficients (MC)") +
  theme_bw(base_size = 15) + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()
        )
  
coeff_plot 

coeff_plot <- coeff_plot +  
  scale_fill_manual(name="Effect direction", #change the name of lagend and label and values
                    labels = c("Advancing",
                               "Unimportant",
                               "Delaying"), 
                    values = c("-1" = "red",
                               "0" = "grey",
                               "1" = "dark green")) +
  ylab("PLS coefficient") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

coeff_plot


library(patchwork)
#combine 
plot<- (VIP_plot +
          coeff_plot +
          temp_plot +
          plot_layout(ncol = 1,
            guides = "collect")
        ) & theme(legend.position = "right",
                  legend.text = element_text(size = 8),
                  legend.title = element_text(size = 10),
                  axis.title.x = element_blank()) #remove the date under 

plot


#we can also do this with a fuction, so that we don't have to run again and again. 

plot_PLS_chill_force <- function(plscf,
                                 chill_metric = "Chill_Portions",
                                 heat_metric = "GDH",
                                 chill_label = "CP",
                                 heat_label = "GDH",
                                 chill_phase = c(-48, 62),
                                 heat_phase = c(-5, 105.5)) #they are the only input 
{
  # and we need to chagne the chill metric so that we can use flexiable 
  PLS_gg <- plscf[[chill_metric]][[heat_metric]]$PLS_summary %>%
    mutate(Month = trunc(Date/100),
           Day = Date - Month * 100,
           Date = ISOdate(2002,
                          Month,
                          Day))
  
  PLS_gg[PLS_gg$JDay <= 0,"Date"]<-
    ISOdate(2001,
            PLS_gg$Month[PLS_gg$JDay <= 0],
            PLS_gg$Day[PLS_gg$JDay <= 0])
  
  PLS_gg <- PLS_gg %>%
    mutate(VIP_importance = VIP >= 0.8,
           VIP_Coeff = factor(sign(Coef) * VIP_importance))
  
  #and we need to change the chill date and ... 
  chill_start_date <- ISOdate(2001,
                              12,
                              31) + chill_phase[1] * 24 * 3600
  chill_end_date <- ISOdate(2001,
                            12,
                            31) + chill_phase[2] * 24 * 3600
  heat_start_date <- ISOdate(2001,
                             12,
                             31) + heat_phase[1] * 24 * 3600
  heat_end_date <- ISOdate(2001,
                           12,
                           31) + heat_phase[2] * 24 * 3600




  temp_plot <- ggplot(PLS_gg) +
    annotate("rect",
             xmin = chill_start_date,
             xmax = chill_end_date,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,
             fill = "blue") +
    annotate("rect",
             xmin = heat_start_date,
             xmax = heat_end_date,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,
             fill = "red") +
    annotate("rect",
             xmin = ISOdate(2001,
                            12,
                            31) +
               min(plscf$pheno$pheno,
                   na.rm = TRUE) * 24 * 3600,
             xmax = ISOdate(2001,
                            12,
                            31) +
               max(plscf$pheno$pheno,
                   na.rm = TRUE) * 24 * 3600,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,
             fill = "black") +
    geom_vline(xintercept = ISOdate(2001,
                                    12,
                                    31) +
                 median(plscf$pheno$pheno,
                        na.rm=TRUE) * 24 * 3600,
               linetype = "dashed") +
    geom_ribbon(aes(x = Date,
                    ymin = MetricMean - MetricStdev ,
                    ymax = MetricMean + MetricStdev ),
                fill = "grey") +
    geom_ribbon(aes(x = Date,
                    ymin = MetricMean - MetricStdev * (VIP_Coeff == -1),
                    ymax = MetricMean + MetricStdev * (VIP_Coeff == -1)),
                fill = "red") +
    geom_ribbon(aes(x = Date,
                    ymin = MetricMean - MetricStdev * (VIP_Coeff == 1),
                    ymax = MetricMean + MetricStdev * (VIP_Coeff == 1)),
                fill = "dark green") +
    geom_line(aes(x = Date,
                  y = MetricMean)) +
    facet_wrap(vars(Type),
               scales = "free_y",
               strip.position = "left",
               labeller = 
                 labeller(
                   Type =
                     as_labeller(c(Chill = paste0("Chill (",
                                                  chill_label,
                                                  ")"),
                                   Heat = paste0("Heat (",
                                                 heat_label,
                                                 ")"))))) +
    ggtitle("Daily chill and heat accumulation rates") +
    theme_bw(base_size = 15) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.y = element_text(size = 12),
          plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank()
          )
  
  VIP_plot <- ggplot(PLS_gg,
                     aes(x = Date,
                         y = VIP)) +
    annotate("rect",
             xmin = chill_start_date,
             xmax = chill_end_date,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,
             fill = "blue") +
    annotate("rect",
             xmin = heat_start_date,
             xmax = heat_end_date,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,
             fill = "red") +
    annotate("rect",
             xmin = ISOdate(2001,
                            12,
                            31) + min(plscf$pheno$pheno,
                                      na.rm = TRUE) * 24 * 3600,
             xmax = ISOdate(2001,
                            12,
                            31) + max(plscf$pheno$pheno,
                                      na.rm = TRUE) * 24 * 3600,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,
             fill = "black") +
    geom_vline(xintercept = ISOdate(2001,
                                    12,
                                    31) + median(plscf$pheno$pheno,
                                                 na.rm = TRUE) * 24 * 3600,
               linetype = "dashed") +
    geom_bar(stat = 'identity',
             aes(fill = VIP>0.8)) +
    facet_wrap(vars(Type), 
               scales = "free",
               strip.position = "left",
               labeller = 
                 labeller(
                   Type = as_labeller(c(Chill="VIP for chill",
                                        Heat="VIP for heat")))) +
    scale_y_continuous(
      limits=c(0,
               max(plscf[[chill_metric]][[heat_metric]]$PLS_summary$VIP))) +
    ggtitle("Variable Importance in the Projection (VIP) scores") +
    theme_bw(base_size = 15) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.y = element_text(size = 12),
          plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank()
          ) +
    scale_fill_manual(name = "VIP", 
                      labels = c("<0.8", ">0.8"), 
                      values = c("FALSE" = "grey",
                                 "TRUE" = "blue")) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  coeff_plot <- ggplot(PLS_gg,
                       aes(x = Date,
                           y = Coef)) +
    annotate("rect",
             xmin = chill_start_date,
             xmax = chill_end_date,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,
             fill = "blue") +
    annotate("rect",
             xmin = heat_start_date,
             xmax = heat_end_date,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,
             fill = "red") +
    annotate("rect",
             xmin = ISOdate(2001,
                            12,
                            31) + min(plscf$pheno$pheno,
                                      na.rm = TRUE) * 24 * 3600,
             xmax = ISOdate(2001,
                            12,
                            31) + max(plscf$pheno$pheno,
                                      na.rm = TRUE) * 24 * 3600,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,
             fill = "black") +
    geom_vline(xintercept = ISOdate(2001,
                                    12,
                                    31) + median(plscf$pheno$pheno,
                                                 na.rm = TRUE) * 24 * 3600,
               linetype = "dashed") +
    geom_bar(stat = 'identity',
             aes(fill = VIP_Coeff)) +
    facet_wrap(vars(Type),
               scales = "free",
               strip.position = "left",
               labeller =
                 labeller(
                   Type = as_labeller(c(Chill = "MC for chill",
                                        Heat = "MC for heat")))) +
    scale_y_continuous(
      limits = c(min(plscf[[chill_metric]][[heat_metric]]$PLS_summary$Coef),
                 max(plscf[[chill_metric]][[heat_metric]]$PLS_summary$Coef))) +
    ggtitle("Model coefficients (MC)") +
    theme_bw(base_size = 15) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.y = element_text(size = 12),
          plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank()
          ) +
    scale_fill_manual(name="Effect direction", 
                      labels = c("Advancing",
                                 "Unimportant",
                                 "Delaying"), 
                      values = c("-1" = "red",
                                 "0" = "grey",
                                 "1" = "dark green")) +
    ylab("PLS coefficient") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  library(patchwork)
  
  plot <- (VIP_plot +
             coeff_plot +
             temp_plot +
             plot_layout(ncol=1,
                         guides = "collect")
           ) & theme(legend.position = "right",
                     legend.text = element_text(size = 8),
                     legend.title = element_text(size = 10),
                     axis.title.x = element_blank())

plot

}

plot_PLS_chill_force(plscf)


daychill <- daily_chill(hourtemps = temps_hourly,
                        running_mean = 11,
                        models = list(Chilling_Hours = Chilling_Hours,
                                      Utah_Chill_Units = Utah_Model,
                                      Chill_Portions = Dynamic_Model,
                                      GDH = GDH)
                        )

plscf <- PLS_chill_force(daily_chill_obj = daychill,
                         bio_data_frame = Alex_first,
                         split_month = 6,
                         chill_models = c("Chilling_Hours",
                                          "Utah_Chill_Units",
                                          "Chill_Portions"),
                       heat_models = c("GDH"))

#chill metric: different model 

plot_PLS_chill_force(plscf,
                     chill_metric = "Chilling_Hours",
                     heat_metric = "GDH",
                     chill_label = "CH",
                     heat_label = "GDH",
                     chill_phase = c(0,0),
                     heat_phase = c(0,0))

plot_PLS_chill_force(plscf,
                     chill_metric = "Utah_Chill_Units",
                     heat_metric = "GDH",
                     chill_label = "CU",
                     heat_label = "GDH",
                     chill_phase = c(0,0),
                     heat_phase = c(0,0))

#like in jan stagnate so we can only say that there is something happen but not exactly what. 
#because at the end of Jan increase finally heat accumulation. 
