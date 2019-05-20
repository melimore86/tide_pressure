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

elevation_production<- read.csv("data/elevation/elevation_production.csv", header=T)

elevation_production <- elevation_production %>% 
  filter (station== "LCO3")

# Temp from C to K
#0°C + 273.15 = 273.15K

site_3$kelvin<- (site_3$temperaure + 273.15)


#Elevation formula as per Diver manual, below are the different variables needed

#Ph= atmospheric pressure at elevation height at H
#P0= atmospheric pressure at reference height
#m= 28.8 *10^-3
#g= 9.81 m/s (standard gravity)
#R= 8.314 j/mol/k
#t= temperature in Kelvin 
#H= from the RTK, height in meters

#Ph= P0*e^- (M*g*H)/(R*T)


site_3$ph<- (site_3$baro_pressure * 2.17 ^-((0.0288*9.81*elevation_production$elev_m)/(8.314*site_3$kelvin)))

#normalization using `normalize` from the package BBmisc
library("BBmisc")
#https://www.rdocumentation.org/packages/BBmisc/versions/1.10/topics/normalize

site_3$normalization<- normalize(site_3$ph, method= "standardize", range= c(0,1))

dat$normalization<- normalize(dat$TideHeight_NAVD_m, method= "standardize", range= c(0,1))

# Tidal
library("rtide") 
library("scales")


dat <- tide_height('Cedar Key',from = as.Date('2019-01-01'),
                   to = as.Date('2019-01-30'), minutes = 60, 
                   tz = 'America/New_York')

#the dates here are the dates you are interested in. So you enter a from and to date in YYYY-MM-DD format.  minutes are the minutes for the prediction so a value of 15 is a predicted tide every 15 minutes

x_conversion<- -0.687
dat$TideHeight_NAVD_m=dat$TideHeight + x_conversion 
#convert from MLLW to NAVD using conversion from Peter's table

#Dont neet conversion to feet 
#ft_conversion<- 3.281
#dat$TideHeight_NAVD_ft=dat$TideHeight_NAVD_m*ft_conversion

#Plotting

cols<- c("Predicted Tidal Height"=  "#0072B2","Target"="black", "Target +3"="#D55E00", "Target -6"="#E69F00", "Refrigerator"="#999999", "Ph" = "darkblue")

ggplot() +

  #geom_line(aes( y= kelvin/1583.735, color= "Temp K"), color= "orange") +
  geom_hline(aes(color = "Target",yintercept = -1.45),size=1.2,linetype = 2) +
  
  geom_line(data = dat, aes(x = DateTime, y = normalization, color= "Predicted Tidal Height"), size =1.2, linetype=1)  +
  
  scale_x_datetime(name = "January 1-15, 2019", #<- can be in labs(), also, but fine here, since we need to use scale_x_dateime anyways
                   labels = date_format("%d-%H:%M", tz="America/New_York"),
                   limits = c(
                     as.POSIXct("2019-01-01 00:00:00 CET"),
                     as.POSIXct("2019-01-15 00:00:00 CET"))) +
  
  geom_hline(aes(color = "Target +3",yintercept = -1.20),size=1.5, linetype = 3) +
  
  geom_hline(aes(color = "Target -6",yintercept = -1.95),size=1.5, linetype = 3) +
  
  geom_hline(aes(color = "Refrigerator",yintercept = -0.7),size=1.3, linetype = 1) +
  
  geom_line (data= site_3, aes( x= date, y= normalization, color= "Ph"), linetype = 1, size= 1.5) +
  
  #Rearranged the scale_color_manual, to show the legend in the order of the lines shown in "breaks=c()" instead of in alpahebtical order (which is the default)
  #scale_color_manual(values =cols,guide= 'legend', show) +
  
  #guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,2,3))))+
  
  
  #Added labs() and removed ggtitle  and scale_y_continuous (this is more for y-axis control such as using limits=c(start,end)), so I can rename the legend in color = (legend title) 
  labs(main= "Cedar Key", ylab= "Tide Height NAVD (ft)", color= "Tidal Lines") +
  
  #Added some themes, can change legend to "top", "bottom", and "left" if desired
  theme(legend.position=("top"),
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +#<- to make the a-axis ticks 90 degrees
  theme_minimal()
