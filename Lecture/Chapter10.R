
library(chillR)
station_list<-handle_gsod(action="list_stations",
                          location=c(7.10,50.73),
                          time_interval=c(1973,2020))
require(kableExtra)

kable(station_list) %>%
  kable_styling("striped", position = "left", font_size = 8)

weather<-handle_gsod(action="download_weather",
                     location=station_list$chillR_code[4],
                     time_interval=c(1973,2020))

weather[[1]][1:20,]

cleaned_weather<-handle_gsod(weather)
cleaned_weather[[1]][1:20,]



write.csv(station_list,"data/station_list.csv",row.names=FALSE)
write.csv(weather[[1]],"data/Bonn_raw_weather.csv",row.names=FALSE)
write.csv(cleaned_weather[[1]],"data/Bonn_chillR_weather.csv",row.names=FALSE)

