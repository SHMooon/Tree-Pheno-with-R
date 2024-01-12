library(chillR)
library(devtools)
#install.packages("devtools")
devtools::install_github("https://github.com/EduardoFernandezC/dormancyR")
library(dormancyR)


hourly_models <- list(Chilling_units = chilling_units,
                      Low_chill = low_chill_model,
                      Modified_Utah = modified_utah_model,
                      North_Carolina = north_carolina_model,
                      Positive_Utah = positive_utah_model,
                      Chilling_Hours = Chilling_Hours,
                      Utah_Chill_Units = Utah_Model,
                      Chill_Portions = Dynamic_Model)

daily_models <- list(Rate_of_Chill = rate_of_chill,
                     Chill_Days = chill_days,
                     Exponential_Chill = exponential_chill,
                     Triangula_Chill_Haninnen = triangular_chill_1,
                     Triangular_Chill_Legave = triangular_chill_2)

metrics <- c(names(daily_models),
             names(hourly_models))

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

data.frame(Metric=model_labels,'Function name'=metrics)


Bonn_temps <- read_tab("data/Bonn_temps.csv")

Temps <- load_temperature_scenarios("data",
                                    "Bonn_hist_scenarios")
