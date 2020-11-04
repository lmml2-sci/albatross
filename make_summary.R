# This script assumes the folders Summary_outputs and Logger-Combined_data
# Are in the same directory. These are both downloads from the google drive
library(ggplot2)
library(lubridate)
library(sf)
library(shiny)
library(htmlwidgets)
library(gapminder)
library(plotly)
library(dplyr)
library(tidyr)
library(maptools)
library(data.table)
library(Imap)
library(httr)
library(adehabitatLT)
# summ_visualizations <- function (id){}


# set to fractional seconds
op <- options(digits.secs=3)



# the working directory is a folder where R reads and saves files.
# setwd('C:/Lucia/Datos/JOBS/Liverpool_Albatross_infrasounds_2020_2021/Analysis')
# the working directory should be ideally where you are now

# load in logger table
sensor_table <- read.csv('Logger-Combined_data/Logger_deployment_tbl.csv', stringsAsFactors=F)


# load in data
# set time in UTC
# set local time which in Crozet islands is + 5 hours from UTC and there is no daylight time saving. 
# convert UTC time to seconds

gps <- read.csv('Logger-Combined_data/gps_clean.csv', stringsAsFactors=F)
gps$timez <- ymd_hms(gps$gps_dt)# assign the Universal Coordinated Time Zone (UTC) to the parsed date
gps$timezlocal<-gps$timez+hours(5)
gps$timezlocal2<-with_tz(gps$timez, tz="Indian/Kerguelen")# This was just to check that with_tz function gives equal results as adding 5 hours. 
gps$time_sec<- seconds(gps$timez)# time in seconds period object. 



# Date_time_GMT <- as.POSIXct(strptime(paste(x$Date_GMT, x$Time_GMT, sep = " "), format = "%Y-%m-%d %H:%M:%S"),
#                                   tz = "GMT")
# x$Date_time_AEST<- wtsw$Date_time_GMT
# attr(x$Date_time_AEST, "tzone") <- "Australia/Queensland"

acc <- fread('Logger-Combined_data/acc_clean.csv', stringsAsFactors=F)
acc$timez <- ymd_hms(acc$acc_dt)
acc$timezlocal<-acc$timez+hours(5)
acc$time_sec<- seconds(acc$timez)

# anemo <- read.csv('Logger-Combined_data/anemo_clean.csv', stringsAsFactors=F)
# anemo$timez <- ymd_hms(anemo$anemo_dt)
# anemo$time_sec<- seconds(anemo$timez)
# anemo$timezlocal<-anemo$timez+hours(5)
# 
# 
# infra <- read.csv('Logger-Combined_data/infra_clean.csv', stringsAsFactors=F)
# infra$timez <- ymd_hms(infra$infra_dt)
# infra$time_sec<- seconds(infra$timez)
# infra$timezlocal<-infra$timez+hours(5)


magnet <- fread('Logger-Combined_data/magnet_clean.csv', stringsAsFactors=F)
magnet$timez <- ymd_hms(magnet$magnet_dt)
magnet$timezlocal<-magnet$timez+hours(5)
magnet$time_sec<- seconds(magnet$timez)


# Convert gps data into spatial `sf` object

#### crs is the coordinate reference systems which includes information about the projection +proj=longlat
### this one is a geographic CRS datum=WGS84 because GPS datasets are provided in this CRS by default.
##there are two types of CRSs: geographic ('lon/lat', with units in degrees 
###longitude and latitude) and projected (typically with units of meters from a datum). 

#WGS84, a geographic CRS well suited for web mapping
wsg <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
sf_gps <- st_as_sf(x = gps, coords = c('longitude','latitude'), crs = wsg)

# from Alex Corbeau who did fieldwork there. 46°21'31.1"S 51°42'25.1"E 
colony <-c(51.706972, 46.358639)
colony <- st_sfc(st_point(colony), crs = wsg)


# # If youd like to plot over a simple base map 
# library(OpenStreetMap)
# bbox <- st_bbox(sf_gps)
# 
# map_sm <- 
#   openmap(c(-25,35),c(-55, 75),
#           type='esri-topo',zoom=6)
# map_sm <- openproj(map_sm, wsg)
# 
# plot(map_sm)
# plot(sf_gps['id'], add = T)
################

# Basic ggplot
ggplot() +
  geom_sf(data = sf_gps, aes(fill = id,color=id)) + geom_sf_text(data = colony, label='Colony')


# summ_visualizations function to visuzlice raw data for each id. 
# dawn and dusk times are in UTC times. +5 hours would be local time. 
# they are an approximation (Now its only the mean time for the whole data deployment period). Once all data for the same individual is on
# the same dataframe then the closer GPS location will be 
# added to each acc and magnet value and so the time civil twilight will be updated for each datapoint. 
a<- c('ID101_1_IS_FU4','ID101_2_IS_BC1','ID103_1_wind_NT0','ID103_1_wind_W38','ID107_1_IS_EX9',
      'ID108_1_IS_JA3','ID108_1_wind_B64','ID110_1_IS_BY7','ID110_1_IS_JD1','ID111_1_wind_FK1','ID111_2_IS_DT5',
      'ID112_1_IS_BV2', 'ID112_2_wind_U92','ID116_1_wind_EK3', 'ID121_1_IS_BC4', '11 ID122_1_IS_K81', 'ID122_1_wind_BR3')
b<-c("16:31:00","16:20:00","16:34:00","16:35:00","16:02:00","16:59:00","16:24:00","16:55:00","16:59:00", "16:11:00",
     "16:50:00","16:12:00", "16:30:00", "16:33:00", "16:33:00", "16:32:00", "16:04:00")
d<- c("01:07:00","01:15:00","01:04:00","01:03:00","01:34:00", "00:33:00","01:14:00","00:39:00","00:33:00", "01:26:00", 
      "01:44:00","01:25:00", "01:08:00", "01:05:00", "01:05:00", "01:06:00", "01:33:00")

params<-cbind(a,b,d)

x=1
id_val=params[x,1]
duskam=params[x,2]
dawnam=params[x,3]  


  
gps_id1 <- gps[gps[,"id"]==id_val,]
acc_id1 <- subset(acc, id == id_val)
mag_id1 <- subset(magnet, id == id_val)

#sampling rate of each point 
GPSres<-diff(gps_id1$time_sec@.Data)
gps_id1$timeinterv <- c(GPSres,NA)

###find crepuscule times to diferenciate between day and night
long0<-c(gps_id1$longitude)
lat0<-c(gps_id1$latitude)
GPS.sub<-cbind(long0, lat0)
# solarDep numeric vector with the angle of the sun below the horizon in degrees Tommy suggested me to put 6.
# POSIXct.out=TRUE to include POSIXct output 
GPS.dawn <- crepuscule(GPS.sub, gps_id1$timez, solarDep=6, direction="dawn", POSIXct.out=TRUE)
GPS.dusk <- crepuscule(GPS.sub, gps_id1$timez, solarDep=6, direction="dusk", POSIXct.out=TRUE)
gps_id1$dawn<-GPS.dawn
gps_id1$dusk<-GPS.dusk
# add day and night to the gps_id1 table based on the GPS time and dawn and dusk time. 
#as the lat and long of the animal varies with its movement, the dawn and dusk time also varies. 
gps_id1$time_category <- ifelse(gps_id1$timez >= gps_id1$dawn$time & gps_id1$timez <= gps_id1$dusk$time, "Day","Night")
gps_id1$time_category <- as.factor(gps_id1$time_category)
table(gps_id1$time_category)

##To do!!!!
#However, it shouldn't shift too much in a single day.Test the effect of lon and lat by using the same input 
##date.time and changing the coordinates slightly and seeing how it affects dawn/dusk. Does this make sense?
##To do!!!!




# calculate the distance between GPS points in meters from the latitude and longitude GPS points
#gdist(lon.1, lat.1, lon.2, lat.2, units = "nm", a = 6378137.0, b = 6356752.3142, verbose = FALSE)
longlat <- gps_id1[,c("longitude","latitude")]
distances <- apply(cbind(longlat, rbind(longlat[2:nrow(longlat), 
                                                ], c(NA, NA))), 1, function(x) {
                                                  gdist(x[1], x[2], x[3], x[4], units = "m")
                                                })
gps_id1$distances <- distances


# calculate the speed in m/s experienced by the bird in each distance travelled.
gps_id1$speed<-gps_id1$distances/gps_id1$timeinterv


#acc and mag data ploted over time and colored by day and nigh. Day and night times are based on an approximation
duskam<-as.character(duskam)
dawnam<-as.character(dawnam)
acc_id1$time_category <- ifelse(acc_id1$time  >= dawnam & acc_id1$time  <= duskam, "Day","Night")
acc_id1$time_category <- as.factor(acc_id1$time_category)
#table(acc_id1$time_category)
mag_id1$time_category <- ifelse(mag_id1$time  >= dawnam & mag_id1$time  <= duskam, "Day","Night")
mag_id1$time_category <- as.factor(mag_id1$time_category)
#table(mag_id1$time_category)

##create gps dataframe with datapoints sampled at a sampling frequency bigger than 60Hz, i.e. with no more than 1 sample every minute
#### need to remove all the GPS points taken during 1Hz sampling bouts except the last point as this is the accurate one.
# 1st check that errorH and errorV is big for all fixes taken during 1Hz sampling bouts of 10 and 15 m respectively, except
# for the last point that is the accurate one. 

gps_id1_600Hz<-gps_id1[gps_id1[,"timeinterv"]<600,]
gps_id1_filt<-gps_id1[gps_id1[,"timeinterv"]>=600,]
results<-list(gps_id1=gps_id1, acc_id1=acc_id1, mag_id1=mag_id1, gps_id1_filt=gps_id1_filt) 






###########################
############# #############
###########################
#analysis taking into account all gps data points from all ids to identify trip bouts based on the time intervals
# from all ids. Tommy recomended that "one should interpolate to a time step that is between the median and the 
# 3rd quartile of observed time differences and then split into separate bout trips or groups wherever the step 
# is greater than the cutoff. 
#sampling rate of each point 



timeinterval<-function(id,gps) {
  test1<-c(diff(gps$time_sec@.Data[gps["id"]==id]),NA)
  test1 }
test2<-lapply(unique(gps$id),timeinterval,gps)

test3<-unlist(test2)
gps$timeinterv<-test3
gps_filt<-gps[gps[,"timeinterv"]>=600,]


hist(gps_filt$timeinterv/3600/24)
summary(gps_filt$timeinterv)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#602.2    603.7    604.9   1538.3    617.3 907793.8       17 

gps_filt2<-gps[gps[,"timeinterv"]>=1700,]
hist(gps_filt2$timeinterv/3600/24)
summary(gps_filt2$timeinterv)




aggregate(gps$id, by=test2, FUN=mean)


gps_id1$timeinterv <- c(GPSres,NA)
gps_id1_filt<-gps_id1[gps_id1[,"timeinterv"]>=600,]


# Here is some code to run linear interpolation in the adehabitatLT R package 
# (also projecting to a equal area projection to conduct the interpolation) :
  
#### first projecting from WGS84 to a crozet centred projection
proj.dec <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj.utm <- "+proj=laea +lat_0=-46.35 +lon_0=51.71 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#remove NaN from database
gps_id1_filt<-gps_id1_filt[-which(is.na(gps_id1_filt$latitude)),]
coordinates(gps_id1_filt) <- ~longitude + latitude
proj4string(gps_id1_filt) <- proj.dec
## changing projection
gps_id1_filt <- spTransform(gps_id1_filt, proj.utm)
gps_id1_filt <- as.data.frame(gps_id1_filt)




# rediscretize trajectory using adehabitatLT
# first turn gps data into adehabitat trajectory
# type II correspond to trajectories for which the time is available for each relocation
# should date be in local time or not?
gps.traj <- as.ltraj(xy = gps_id1_filt[,c("longitude","latitude")], 
                     date = gps_id1_filt$timezlocal,
                     id = gps_id1_filt$id, typeII = TRUE)

# the rediscretize every 1800 seconds = 30 mins in my case 
gps.interp <- redisltraj(na.omit(gps.traj), 1800, type="time")
# convert to dataframe and convert back to dec degrees to extract coordinates
inter.point <- ltraj2spdf(gps.interp)
inter.df <- as.data.frame(inter.point)
coordinates(inter.df) <- ~x+y
proj4string(inter.df) <- proj.utm
gps_out <- spTransform(inter.df, CRS(proj.dec))
gps_out <- as.data.frame(gps_out)







pspeed <- gps_id1_big600Hz %>%
  ggplot(aes(x =timezlocal, y = speed)) +
  geom_point(aes(color = 'blue'),size=0.5)+
  theme_minimal()
ggplotly(pspeed,
         dynamicTicks = TRUE)

plot(gps_id1_big600Hz$errorH, gps_id1_big600Hz$errorV)
plot(gps_id1_big600Hz$errorH)
plot(gps_id1_big600Hz$errorV)


sum_df <- data.frame(t(unclass(summary(GPSres))))# create a summary of the sampling frequencies
rownames(sum_df) <- NULL
ea=as.data.frame(GPSres)
pGPS<-ggplot(ea, aes(x=GPSres))+
  geom_histogram(breaks=seq(600,20000, by=10), fill="red") # supposed to be a 10 min increment so 600 seconds
gg_hist <- highlight(ggplotly(pGPS), "plotly_selected")
crosstalk::bscols(gg_hist, DT::datatable(sum_df))
