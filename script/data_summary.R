library("tidyverse")
library("dplyr")
library("ggplot2")

### Times for pressure and tides are GMT

setwd("C:/Users/melimore86/Desktop/tide_pressure/data")

# Reading all of the pressure files that are .csv
pressure_temp <- list.files(pattern="*.csv")

# Combining all files into a data frame
pressure <- do.call(rbind, lapply(pressure_temp, function(x) read.csv(x, stringsAsFactors = FALSE)))

pressure$date<- as.POSIXct(pressure$DATE.TIME, format="%m/%d/%Y %H:%M")


# Reading all of the tidal files that are .txt
tidal_temp = list.files(pattern="*.txt")

# Combining all files into a data frame
tidal= do.call(rbind, lapply(tidal_temp, function(x) read.table(x, stringsAsFactors = FALSE, header=T)))


#For tides need to round to the nearest hour to match with the pressure

tidal$date<- as.POSIXct(paste(tidal$Date, tidal$Time), format="%Y/%m/%d %H:%M")

tidal$date<-format(round(tidal$date, units="hours"), format="%Y/%m/%d %H:%M")

tidal2<- tidal %>%
  group_by(date=as.Date(Date)) %>%
  summarise(Measure = mean(Pred)) %>%
  select(date, Measure)

tidal2$Date<- as.POSIXct((tidal2$date), format="%Y/%m/%d")

#plotting
 
ggplot() +
  geom_line (data= tidal, aes( x=as.Date(Date), y= Pred), color= "red") +
  geom_point(data= tidal2, aes( x=as.Date(Date), y= Measure), color= "blue")
  


ggplot(data= pressure, aes( x= date, y= BARO), color= "red") +
  #geom_line (data= pressure, aes( x=date, y= BARO), color= "black") +
  geom_line () +
  geom_line (aes(y= mean_measure), color= "blue")
