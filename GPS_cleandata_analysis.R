###clean GPS data and aligned it with IS and BP data
library(ggplot2)
library(lubridate)
library(sf)
library(htmlwidgets)
library(gapminder)
library(plotly)
library(dplyr)
library(tidyr)
library(maptools)
library(data.table)
library(Imap)
library(httr)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggmap)
library(spectralGP)
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
###remove data from the colony if any. Do buffer around crozet. 

#The thing is that the logger is set off when in the colony, so first I need to plot crozet island and check if there are any GPS 
#point overlap in the island. And so if any then yes apply a buffer around the colony would be enough. 
#But I guess I will need to set it up to 1, 2, 3 5 km around the island based on some facts. 

#Calculate the distance between each location and the colony (using the rdist.earth function in fields package in R). 
#Then remove locations with a distance below a particular value (need to define this value). 
#Although it is probably worth checking if it hasn't removed occasions when the bird might be near the colony during other sections of the trip 
#(i.e. not start or end). 

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

# summ_visualizations function to visuzlice raw data for each id. 
# dawn and dusk times are in UTC times. +5 hours would be local time. 
# they are an approximation (Now its only the mean time for the whole data deployment period). Once all data for the same individual is on the same dataframe then the closer GPS location will be 
# added to each acc and magnet value and so the time civil twilight will be updated for each datapoint. 
a<- c('ID101_1_IS_FU4','ID101_2_IS_BC1','ID103_1_wind_NT0','ID103_1_wind_W38','ID107_1_IS_EX9',
      'ID108_1_IS_JA3','ID108_1_wind_B64','ID110_1_IS_BY7','ID110_1_IS_JD1','ID111_1_wind_FK1','ID111_2_IS_DT5',
      'ID112_1_IS_BV2', 'ID112_2_wind_U92','ID116_1_wind_EK3', 'ID121_1_IS_BC4', 'ID122_1_IS_K81', 'ID122_1_wind_BR3')
b<-c("16:31:00","16:20:00","16:34:00","16:35:00","16:02:00","16:59:00","16:24:00","16:55:00","16:59:00", "16:11:00", "16:50:00","16:12:00", "16:30:00", "16:33:00", "16:33:00", "16:32:00", "16:04:00")
d<- c("01:07:00","01:15:00","01:04:00","01:03:00","01:34:00", "00:33:00","01:14:00","00:39:00","00:33:00", "01:26:00", "01:44:00","01:25:00", "01:08:00", "01:05:00", "01:05:00", "01:06:00", "01:33:00")

params<-cbind(a,b,d)


##plot map 
##some data formating
gps$id <- as.factor(gps$id)


ggplot(data = world) +  
  geom_sf() +
  geom_point(data = gps, aes(x = longitude, y = latitude, color=id)) +
  geom_text(x=51.706972, y=-46.358639, label='Colony')+
  geom_point(x=51.706972, y=-46.358639, color="red")+
  coord_sf(xlim = c(51.5, 51.9), ylim = c(-46.45, -46.2), expand = FALSE)


ggplot(data = world) +  
  geom_sf() +
  geom_point(data = gps, aes(x = longitude, y = latitude, color=id)) +
  geom_text(x=51.706972, y=-46.358639, label='Colony')+
  geom_point(x=51.706972, y=-46.358639, color="red")+
  coord_sf(xlim = c(33, 73), ylim = c(-52, -30), expand = FALSE)

# calculate the distance bw each location and the colonly. 
longlat <- cbind(gps$longitude, gps$latitude)
colony2 <-c(51.706972, -46.358639)
test1<-rep(colony2[1],nrow(longlat))
test2<-rep(colony2[2],nrow(longlat))
col_loglat<-cbind(test1,test2)

distances<-rdist.earth(col_loglat[1:2,], longlat, miles = FALSE, R = NULL)
distances<-distances[1,]
gps$dis2colony<-distances
#Remove locations with a distance below a particular value (need to define this value). 

#from the histogram 
hist(distances, breaks=10000, xlim = c(0,20))

removedata<-gps[which(gps$dis2colony<5),]



ggplot(data = world) +  
  geom_sf() +
  geom_point(data = removedata, aes(x = longitude, y = latitude, color=id)) +
  geom_text(x=51.706972, y=-46.358639, label='Colony')+
  geom_point(x=51.706972, y=-46.358639, color="red")+
  coord_sf(xlim = c(51.5, 51.9), ylim = c(-46.45, -46.2), expand = FALSE)




gps_filt<-gps[which(gps$dis2colony>5),]


library("maptools")
library("rgdal")
coast<-readOGR(dsn=paste0(getwd(),"/shapefile_Crozet"), layer="country_original") 



p_all <- ggplot() +
  geom_point(data = removedata, aes(x = longitude, y = latitude, color=id)) +
  geom_text(x=51.706972, y=-46.358639, label='Colony')+
  geom_point(x=51.706972, y=-46.358639, color="red")+
  coord_sf(xlim = c(51.5, 51.9), ylim = c(-46.45, -46.2), expand = FALSE)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group), fill="")
p_all

plot(coast,xlim = c(51.5, 52), ylim = c(-46.65, -46.2))



my_spdf<-readShapeSpatial (paste0(getwd(),"/shapefile_Crozet/country_original")) 
my_spdf <- readShapeSpatial(paste0(getwd(),"/shapefile_Crozet/country_original"),  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))  #readShapeSpatial is in maptools
proj4string(test)
proj4string(my_spdf)
ggplot(data = test) +  
  geom_sf() +
  geom_point(data = removedata, aes(x = longitude, y = latitude, color=id)) +
  geom_text(x=51.706972, y=-46.358639, label='Colony')+
  geom_point(x=51.706972, y=-46.358639, color="red")+
  coord_sf(xlim = c(51.5, 51.9), ylim = c(-46.45, -46.2), expand = FALSE)



# Basic plot of this shape file:
par(mar=c(0,0,0,0))
plot(my_spdf, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )



###estimate speed and directionality from GPS data.adehabitatLT or sftrack https://github.com/mablab/sftrack
library("sftrack")




#### find the point from BP and IS data that is close to the GPS point. 