library(chillR)
library(ggplot2)
library(reshape2)
library(tidyr)
library(kableExtra)
library(RMAWGEN)
library(dplyr)
library(tidyverse)
library(knitr)
library(ggpmisc)
library(patchwork)
library(purrr)
#install_github("EduardoFernandezC/dormancyR")
library(dormancyR)
library(colorRamps)
library(gganimate)
library(devtools)
library(purrr)

knitr::include_graphics('KR_image/chunk70_PLS_chill_force.png')

kable(cleaned_weather_GWANGJU[[1]][1:20,], caption = 'cleaned_weather_GWANGJU') %>%
  kable_styling("striped", position = "left", font_size = 10)
  
  
  kable(head(A)) %>%
  kable_styling("striped", position = "left", font_size = 10)

  chunk52_GWANGJU_scenario_1996
GWANGJU_scenario_1996$'1996'$data
GWANGJU_scenario_1980


