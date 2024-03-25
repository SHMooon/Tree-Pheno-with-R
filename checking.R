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

r,echo=FALSE, out.width="49%",out.height="50%", fig.align='center', fig.cap= "time series labels", fig.show='hold'

knitr::include_graphics('KR_image/chunk25_QC.jpeg')

kable(cleaned_weather_GWANGJU[[1]][1:20,], caption = 'cleaned_weather_GWANGJU') %>%
  kable_styling("striped", position = "left", font_size = 10)
  
  
kable(head(A)) %>%
  kable_styling("striped", position = "left", font_size = 10)






```{r, eval=FALSE}
library(chillR)

hourtemps<- Winters_hours_gaps[,c("Year", "Month", "Day", "Hour", "Temp")]

WarmHours2<- function(hourtemps)
{
  threshold_warm<- 25
  hourtemps[,"Warm_Hour"]<- hourtemps$Temp>threshold_warm
  return(hourtemps)
}

write.csv(WarmHours2(hourtemps),"KR_data/WarmHours2.csv", row.names = FALSE)

##function creation applied to Winter_hours_gaps; I made a new column that tells whether the temperatures are greater than the set threshold value: 25C
hourtemps<- Winters_hours_gaps[,c("Year", "Month", "Day", "Hour", "Temp")]

WarmHours<- function(hourtemps)
{
  threshold_warm<- 25
  hourtemps[,"Warm_Hour"]<- hourtemps$Temp>threshold_warm
  return(hourtemps)
}

WarmHours(hourtemps)[13:20,]


##sum of warm hours of start date and end year in YEARMODA

sum_WarmHours<-function(hourtemps, Start_YEARMODA, End_YEARMODA)
{
  Start_Year<-trunc(Start_YEARMODA/10000)
  Start_Month<- trunc((Start_YEARMODA-Start_Year*10000)/100)
  Start_Day<- Start_YEARMODA-Start_Year*10000-Start_Month*100
  Start_Hour<-12
  End_Year<-trunc(End_YEARMODA/10000)
  End_Month<- trunc((End_YEARMODA-End_Year*10000)/100)
  End_Day<- End_YEARMODA-End_Year*10000-End_Month*100
  End_Hour<-12
  
  Start_Date<-which(hourtemps$Year==Start_Year & hourtemps$Month==Start_Month & hourtemps$Day==Start_Day & hourtemps$Hour==Start_Hour)
  End_Date<-which(hourtemps$Year==End_Year & hourtemps$Month==End_Month & hourtemps$Day==End_Day & hourtemps$Hour==End_Hour)
  
  Warm_hours<- WarmHours(hourtemps)
  return(sum(Warm_hours$Warm_Hour[Start_Date:End_Date]))
}

```

```{r, eval=FALSE}
##sample content for checking. # of warm hours in set time interval by manual counting= using sum_WH function), so I confirmed there's no error anywhere
sum_WarmHours(hourtemps,20080517,20080518)
WarmHours(hourtemps)[1803:1827,]

```

```{r, echo=FALSE}
library(kableExtra)


WarmHours_sample<-read_tab("KR_data/WarmHours.csv")
kable(WarmHours_sample[1803:1827,]) %>%
  kable_styling("striped", position = "left",font_size = 10)
```

```{r, eval=FALSE}
##[1] 15
```