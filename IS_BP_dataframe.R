##create dataframe for IS and barometric P data. 



library(lubridate)
library(sf)
library(gapminder)
library(dplyr)
library(tidyr)
library(data.table)
library(httr)
library(ncdf4) # to get what's needed from ncdf files





# let's do this around infrasound and barometric pressure
path_to_env_files <- "./DataBaseOllieNew/"
ID_folders <- dir(path_to_env_files, pattern = "ID")

# let's do this for the 1st bird
x <-7 # first ID
path_to_trips <- paste0(path_to_env_files, ID_folders[x], "/")
ID_trips_folders <- dir(path_to_trips)

x_trip <-2 # second trip for the ID
path_to_ncdf <- paste0(path_to_trips, ID_trips_folders[x_trip], "/netCDF/")

# open Barometric pressure file
path_to_ncdf_baro <- paste0(path_to_ncdf, "Baro/")
baro_name <- dir(path_to_ncdf_baro, pattern = "Barometerdata")
nc_file <- nc_open(paste0(path_to_ncdf_baro, baro_name))
print(nc_file) # getting all the characteristics of the file

Pressure_matrix <- t(ncvar_get(nc_file, "Barodata"))
nc_close(nc_file)
Pressure_df <- as.data.frame(Pressure_matrix)
colnames(Pressure_df) <- c("Time","Pressure_hPa")
str(Pressure_df)
Pressure_df$Time_o<-Pressure_df$Time #time original
Pressure_df$Time <- as.POSIXct(Pressure_df$Time, tz="GMT", origin = "1970-01-01 00:00:00") + hours(5) # adding time difference with Crozet, so to be in local time rather than UTC. 

##ID101_2_IS_BC1 x=1 trip=1
##select only data collected before the 17th of Feb
#Pressure_df<-Pressure_df[Pressure_df[,"Time"]<= "2020-02-17 04:00:00",] 


##ID101/Trip_FU4 x=1 trip=2
##select only data collected before the 12th of Feb
#Pressure_df<-Pressure_df[Pressure_df[,"Time"]<= "2020-02-12 18:00:00",]


# #statistic metric
# #create  an index fore each 5 minutes bout every hour.
Presres<-diff(Pressure_df$Time_o)#  estimate the time between each sample taken. 
Presres3<-c(Presres,0)
Pressure_df3<- cbind(Presres3,Pressure_df)
colnames(Pressure_df3) <- c("Time_bw_samples", "Time","Pressure_hPa", "Time_o")


# the logger is programmed to take10 samples within a second for about 1 consecutive minute (600 samples). However sometimes there is are less than 10 samples taken within a second, and they could be taken for longer than a minute.  To select when a new bout starts 
#we define this as when the time difference between each sample is bigger than 50, i.e., more than 5 seconds without taking any sample. 
bout_st<-c(0,which(Pressure_df3$Time_bw_samples>50))
diff(bout_st)# this gives the number of samples within each bout.
sum(diff(bout_st))# check that this is the same as the number of values within Pressure_df3
lastp<-length(Pressure_df3$Time_bw_samples)-sum(diff(bout_st))
idx<-c(diff(bout_st),lastp)# this is the last point taken for each bout. 
Pressure_df3$group=(rep(1:length(idx),times=idx))# we divide the data in bouts. 


##ID103_1_wind_NT0 x=2 trip=1
##group 52 and 60 should be removed. 
# Pressure_df3<-Pressure_df3 %>%
#   filter(!group == 52 & !group == 60)


##ID107_1_IS_EX9  x=3 trip=1 
## remove group 10. It could be that only the first subgroup of this bout should be deleted. Waiting for Ollies answer. 
# Pressure_df3<-Pressure_df3 %>%
#   filter(!group == 10)]

##ID108_1_IS_JA3 x=4 trip=2
## only keep group 41 abd 42.
# Pressure_df3<-Pressure_df3 %>%
#   filter(!group <42)

##ID111_2_IS_DT5  x=6 trip=1
## remove last group n 83
# Pressure_df3<-Pressure_df3 %>%
#   filter(!group == 83)

##ID112_1_IS_BV2  x=7 trip=1
##group 17 and 163 should be removed, but once Ollie tell us we may just then take the last point for this bout as it seems good!!!
Pressure_df3<-Pressure_df3 %>%
  filter(!group == 17 & !group == 163)


#provide number of points in each bout
tabla<-Pressure_df3%>%group_by(group)%>%summarise(median_BaromP=median(Pressure_hPa,na.rm = T),
                                                  mean_BaromP=mean(Pressure_hPa,na.rm = T),
                                                  min_BaromP=min(Pressure_hPa,na.rm = T),
                                                  max_BaromP=max(Pressure_hPa,na.rm = T),
                                                  first_Qu_BaromP=quantile(Pressure_hPa,probs = 0.25,na.rm = T),
                                                  third_Qu_BaromP=quantile(Pressure_hPa,probs = 0.75,na.rm = T),
                                                  count=n(),
                                                  first_time = first(na.omit(Time_o)),
                                                  median_time = median(na.omit(Time_o)),
                                                  median_time_UTC = median(as.POSIXct(Time_o, tz="GMT", origin = "1970-01-01 00:00:00")) + hours(5)
)

tabla$IBI_sec<- c(0,diff(tabla$first_time))# it should be approx one per hour
write.csv(tabla,paste0(baro_name,".csv"))




##IS data
path_to_ncdf_sound <- paste0(path_to_ncdf, "Infrasd/")
sound_name <- dir(path_to_ncdf_sound, pattern = "SPL")
nc_file <- nc_open(paste0(path_to_ncdf_sound, sound_name))
print(nc_file) # getting all the characteristics of the file

# getting the names of the columns
names_var <- names(nc_file$var)
names_var <- names_var[1:(length(names_var)-1)]
SPL_Matrix <- ncvar_get(nc_file, "SPLdata")
Freq<-ncvar_get(nc_file,"Frequencies")


nc_close(nc_file)
SPL_df <- as.data.frame(SPL_Matrix)
colnames(SPL_df) <- c("Time",Freq[1:length(Freq)])
str(SPL_df)

#Freq <- SPL_df[1,2:dim(SPL_df)[2]] # These are the frequencies for SPL - for ID111_2_IS_DT5 SPL_df[13,2:dim(SPL_df)[2]]
SPL_df <- SPL_df[-1,]#  remove first row
SPL_df$Time_o<-SPL_df$Time
SPL_df$Time <- as.POSIXct(SPL_df$Time, tz="GMT", origin = "1970-01-01 00:00:00") + hours(5)
# We may not be interested in each frequency for now
#

# We'll make two categories of SPL: 0.07-1 and 1-4
SPL_low <- apply(SPL_df[,which(Freq < 1 & Freq > 0.07)+1],1,mean)# what does the -inf mean?
SPL_high <- apply(SPL_df[,which(Freq > 1)+1],1,mean)
SPL_cat <- cbind.data.frame(Time=SPL_df$Time,Time_O=SPL_df$Time_o, SPL_low=SPL_low, SPL_high=SPL_high)

SPL_cat$IBI_sec<- c(0,diff(SPL_cat$Time_O))# it should be approx one per hour
write.csv(SPL_cat,paste0(sound_name,".csv"))






