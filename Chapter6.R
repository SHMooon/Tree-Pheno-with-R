
library(chillR)
library(knitr)
library(pander)
library(kableExtra)

kable(Winters_hours_gaps[1:10,])  %>%
  kable_styling("striped", position = "left",font_size = 10)

hourtemps<-Winters_hours_gaps[,c("Year","Month","Day","Hour","Temp")]

hourtemps[3,]

hourtemps[3,"Temp"]

hourtemps$Temp[1:5]

hourtemps[1:5,]

1==2

1==1

c(1,2,3)>2

a<-1
b<-2
c<-3
c(a,b,c)>=2

c(a,b,c)>=2&c(a,b,c)<3


hourtemps[,"Chilling_Hour"]<-hourtemps$Temp>=0&hourtemps$Temp<=7.2

hourtemps[13:20,]

sum(hourtemps$Chilling_Hour[13:20])

Start_Date<-which(hourtemps$Year==2008 &
                    hourtemps$Month==10 &
                    hourtemps$Day==1 & 
                    hourtemps$Hour==12)
End_Date<-which(hourtemps$Year==2008 &
                  hourtemps$Month==10 &
                  hourtemps$Day==31 &
                  hourtemps$Hour==12)

sum(hourtemps$Chilling_Hour[Start_Date:End_Date])

CH<-function(hourtemps)
{
  hourtemps[,"Chilling_Hour"]<-hourtemps$Temp>=0&hourtemps$Temp<=7.2
  return(hourtemps)
}

CH(hourtemps)[13:20,]  # again restricted to rows 13-20,



sum_CH<-function(hourtemps, Start_YEARMODA, End_YEARMODA)
{
  Start_Year<-trunc(Start_YEARMODA/10000) # "trunc" removes all decimals
  Start_Month<-trunc((Start_YEARMODA-Start_Year*10000)/100)
  Start_Day<-Start_YEARMODA-Start_Year*10000-Start_Month*100
  Start_Hour<-12 # This could also be flexible, but let's skip this for now
  End_Year<-trunc(End_YEARMODA/10000)
  End_Month<-trunc((End_YEARMODA-End_Year*10000)/100)
  End_Day<-End_YEARMODA-End_Year*10000-End_Month*100
  End_Hour<-12 # This could also be flexible, but let's skip this for now
  
  Start_Date<-which(hourtemps$Year==Start_Year &
                      hourtemps$Month==Start_Month &
                      hourtemps$Day==Start_Day &
                      hourtemps$Hour==Start_Hour)
  End_Date<-which(hourtemps$Year==End_Year &
                    hourtemps$Month==End_Month &
                    hourtemps$Day==End_Day &
                    hourtemps$Hour==End_Hour)
  
  Chill_hours<-CH(hourtemps)
  
  return(sum(Chill_hours$Chilling_Hour[Start_Date:End_Date]))
  
}
sum_CH(hourtemps,20080401,20081011)
