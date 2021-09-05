setwd("D:/Liverpool_Albatross_infrasounds_2020_2021/Analysis/Crozet 2013 and modelled data/Cone_statistics")
data<-read.table("GPS_ID_cones_full_database.txt", header=T)

str(data)
unique(data)

library(lme4)
library(nlme)
library(MuMIn)

#reverse the coding for whether a code is the focal one or not. 
#This is because at present 1 - not focal and this needs to be the otherway around

#take all the ones and rename them 5 for now - this is a hack as I don't know how to replace the 0s with 1s 
data$cone_ID[data$cone_ID == 1] <- 5

#make zeros ones
data$cone_ID[data$cone_ID == 0] <- 1

#now make 5s zeros
data$cone_ID[data$cone_ID == 5] <- 0

#remove NAs
I <-  is.na(data$abs_SPL_2000dB)|
  is.na(data$Sex)|
  is.na(data$Trip_state)|
  is.na(data$TripID)|
  is.na(data$cone_ID)

datanoNA<- data[!I,]

#standardise continuous variables
datanoNA$abs_SPL_2000dBsc<-scale(datanoNA$abs_SPL_2000dB)

#run binary model 
model1<-glmer(cone_ID~abs_SPL_2000dBsc:Trip_state + 
                      abs_SPL_2000dBsc:Sex +
                      abs_SPL_2000dBsc + Trip_state + Sex + (1|TripID) +(1|counter_TripID), 
                      data = datanoNA, family = binomial, na.action=na.fail)
dredge(model1)
model1_abs_scSam_output<-dredge(model1)
write.csv(model1_abs_scSam_output, "glmer_output_post_scaled_absolute_SPL_dB.csv")

model1_simple<-glmer(cone_ID~abs_SPL_2000dBsc+ abs_SPL_2000dBsc:Sex  + (1|TripID) +(1|counter_TripID), 
              data = datanoNA, family = binomial, na.action=na.fail)

outputmodel1<-summary(model1_simple)
write.csv(outputmodel1, "outputmodel1.csv")


model_simple<-glm(cone_ID~abs_SPL_2000dBsc, 
                     data = datanoNA, family = binomial, na.action=na.fail)

outputmodel<-summary(model_simple)

model2_simple<-glmer(cone_ID~abs_SPL_2000dBsc + Sex+ (1|TripID) +(1|counter_TripID), 
                     data = datanoNA, family = binomial, na.action=na.fail)

outputmodel2<-summary(model2_simple)
outputmodel2
write.csv(outputmodel1, "outputmodel1.csv")


#run binary model 
model1_abs<-glmer(cone_ID~abs_SPL_2000dB:Trip_state + 
                abs_SPL_2000dB + Trip_state + Sex + (1|TripID) +(1|counter_TripID), 
              data = datanoNA, family = binomial, na.action=na.fail)
model1_abs_output<-dredge(model1_abs)

write.csv(model1_abs_output, "glmer_output_absolute_SPL_dB.csv")

#run binary model not scaled as it was previously scaled 
I_std <-  is.na(data$abs_SPL_2000dB_std)|
  is.na(data$Sex)|
  is.na(data$Trip_state)|
  is.na(data$TripID)|
  is.na(data$cone_ID)

datanoNA_std<- data[!I_std,]
model2<-glmer(cone_ID~abs_SPL_2000dB_std:Trip_state + 
                abs_SPL_2000dB_std + Trip_state + Sex + (1|TripID) +(1|counter_TripID), 
              data = datanoNA_std, family = binomial, na.action=na.fail)
dredge(model2)
model1_standarized_output<-dredge(model2)
write.csv(model1_standarized_output, "glmer_output_standardized_SPL_dB.csv")




#####Rather than having 12 cones we are going to have 4 cones, one would be the focal and the other non focals. 
### the focal cone would be cone 1, 12 and 2. and the non focal cones would be 
### cone 3-4-5  6-7-8  9-10-11 and we need to sum up the SPL in Pa across all three cones then scale them to run the model.  

data$cone_n[data$cone_n == 2] <- 1
data$cone_n[data$cone_n == 12] <- 1

data$cone_ID[data$cone_n == 1] <- 1

data$cone_n[data$cone_n == 3] <- 2
data$cone_n[data$cone_n == 4] <- 2
data$cone_n[data$cone_n == 5] <- 2

data$cone_n[data$cone_n == 6] <- 3
data$cone_n[data$cone_n == 7] <- 3
data$cone_n[data$cone_n == 8] <- 3

data$cone_n[data$cone_n == 9] <- 4
data$cone_n[data$cone_n == 10] <- 4
data$cone_n[data$cone_n == 11] <- 4


##we need to select all rows with the same counter_TripID and same cone_n and 
## state all variables the same (somehow such as counter, mapID..) do the 
## mean abs_SPL_2000, calculate the mean abs_SPL_2000dB by converting abs_SPL_2000 
## into dB, set cone_vert_left as the min of the cone_vert_left of all 3 cones, 
## set cone_vert_right as the max of the cone_vert_right of all 3 cones, 
## mean (meanGdist_45dB), and the min (maxGdist)
library("tidyverse")

#remove NAs
I <-  is.na(data$abs_SPL_2000dB)|
  is.na(data$Sex)|
  is.na(data$Trip_state)|
  is.na(data$TripID)|
  is.na(data$cone_ID)

datanoNA<- data[!I,]

four_cone_databasenoNA<-datanoNA%>%
  group_by(birdID,TripID,mapID,counter,Sex,State,Trip_state, counter_TripID,cone_n, cone_ID)%>%
  summarize(abs_SPL_2000=mean(abs_SPL_2000), 
            abs_SPL_2000dB_std=mean(abs_SPL_2000dB_std),
            cone_vert_lef=min(cone_vert_lef),
            cone_vert_rig=max(cone_vert_rig),
            meanGdist_45dB=mean(meanGdist_45dB),
            maxGdist=min(maxGdist),
            x_lon=mean(x_lon),
            y_lat=mean(y_lat),
            Dist_cro_shelf=mean(Dist_cro_shelf),
            max_dist=mean(max_dist),
            per_trav_dist=mean(per_trav_dist),
            per_trip_time=mean(per_trip_time))

four_cone_databasenoNA$abs_SPL_2000dB<-10 * log10(four_cone_databasenoNA$abs_SPL_2000/(20e-6^2))



#standardise continuous variables
four_cone_databasenoNA$abs_SPL_2000dBsc<-scale(four_cone_databasenoNA$abs_SPL_2000dB)

#run binary model 
model1_4cone<-glmer(cone_ID~abs_SPL_2000dBsc:Trip_state + 
                      abs_SPL_2000dBsc:Sex +
                abs_SPL_2000dBsc + Trip_state + Sex + (1|TripID) +(1|counter_TripID), 
              data = four_cone_databasenoNA, family = binomial, na.action=na.fail)
dredge(model1_4cone)
model1_4cone_abs_scSam_output<-dredge(model1_4cone)
write.csv(model1_4cone_abs_scSam_output, "glmer_output_post_scaled_absolute_SPL_dB_4cone.csv")

model1_4cone_simple<-glmer(cone_ID~abs_SPL_2000dBsc + abs_SPL_2000dBsc:Sex + (1|TripID) +(1|counter_TripID), 
                     data = four_cone_databasenoNA, family = binomial, na.action=na.fail)

summary(model1_4cone_simple)
write.csv(outputmodel1, "outputmodel1.csv")





######wind data for Rocio 

setwd("D:/Liverpool_Albatross_infrasounds_2020_2021/Analysis/Crozet 2013 and modelled data")
data<- read.csv('./Created_databses_from_Full_script_data2013GPS_IS_modelR/GPS_ID_cones_full_database_wind.csv', stringsAsFactors=F)


library(dplyr)

data_focal_spl_wind<-data%>%
  filter(cone_ID == 0)


write.csv(data_focal_spl_wind, "./Created_databses_from_Full_script_data2013GPS_IS_modelR/data_focal_spl_wind.csv")

