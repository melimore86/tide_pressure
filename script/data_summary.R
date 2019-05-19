library("tidyverse")
library("dplyr")
library("ggplot2")

### Times for pressure and tides are GMT

setwd("C:/Users/Mel/Desktop/tide_pressure/data")

# Reading all of the pressure files that are .csv
# Pressue is in millibars
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


# Sensor 3 data


sensor<-read.csv("wq/wq.csv", header=T)

#Pressure data are cm
site_3<- sensor %>% 
  filter(Site==3) %>% 
  select(Date, Pressure, Temperature)


site_3$Date<- as.POSIXct(site_3$Date)

# Need to convert sensor 3 cm to millibars, need to mulitply by 0.980665,

site_3$baro_pressure <- site_3$Pressure * 0.980665

# Changing column name

colnames(pressure)<- c("date_time", "windspeed", "direction", "gust", "at", "baro_pressure", "relhum", "vis", "date")
colnames(site_3)<- c("date", "pressure", "baro_pressure", "temperaure")

# Adding column to distinguish between sensor and buoy

pressure$type<- "buoy"
site_3$type<- "sensor"

#only leaving pressure and date

sensor_b<- site_3 %>% 
  dplyr::select(date, baro_pressure, type)

pressure_b<- pressure %>% 
  dplyr::select(date, baro_pressure, type)


#Combining together as per instruction 3

barometric<- rbind(sensor_b, pressure_b)

#Apply calculation based on https://www.vanessen.com/images/PDFs/Diver-ProductManual-en.pdf

sensor
baro_depth<- left_join(site_3, pressure, by = "date")



# Plotting barometric pressure 

ggplot(barometric, aes (x= date, y=baro_pressure, color= type))+
  geom_line() +
  labs(x= "Date", y= "Barometric pressure millibars", color = "Sensnor Types") +
  scale_colour_manual(values = c("orange", "blue")) +
  theme_minimal()



# Elevation 

epoch3_elevatiopn<- read.csv("data/elevation/epoch3_reprocess.csv", header=T)

# Temp from C to K
#0°C + 273.15 = 273.15K

site_3$kelvin<- (site_3$temperaure + 273.15)


#Elevation formula

#Ph= atmospheric pressure at elevation height at H
#P0= atmospheric pressure at reference height
#m= 28.8 *10^-3
#g= 9.81 m/s (standard gravity)
#R= 8.314 j/mol/k
#t= temperature in Kelvin 
#height is specified at 1 ft or 0.3048 in meters

require("standardize")

site_3$ph<- (site_3$baro_pressure * 2.17 ^-((0.0288*9.81*0.3048)/(8.314*site_3$kelvin)))

site_3$normph<- rnorm(site_3$ph)

ggplot(data= site_3, aes(x= date)) +
  geom_line (aes( y= rnorm(ph), color= "PH"), color= "blue") +
  geom_line(aes( y= standardize(site_3$kelvin, scale=1), color= "Temp K"), color= "orange")
