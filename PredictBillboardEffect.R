#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("XLConnect")
#install.packages("XLConnectJars")
#install.packages("rgeos")
#install.packages("stats")
#install.packages("geosphere")
library(stats)
library(dplyr)
library(ggplot2)
library(XLConnectJars)
library(rJava)
library(XLConnect)
library(rgeos)
library(geosphere)
#setwd("C:/Users/bavya/Desktop")
dat = read.csv(file.choose(), nrows = 1000000, header = F)
#install.packages('Rhipe')

colnames(dat) = c("AppName", "latitude", "longitude", "AppId", "carrier", "categoryAsn","connectionType", "timestamp", "deviceCategory", "deviceName", "deviceId", "IPAddress", "OSversion", "City", "Country", "State", "Zip", "Platform", "userAgent", "AppStoreUrl", "DMA")
#Issues with Data Cleaning
# 1. Time stamp
# 2. Device ID repeats
# 
dat_old = dat
dat_multtime = dat %>%
                      group_by(deviceId) %>%
                      summarise(CNT = n()) %>%
                      arrange(deviceId)
  

dat_devtime = dat %>%
  group_by(deviceId, timestamp) %>%
  summarise(CNT = n()) %>%
  arrange(deviceId)

#example for device ID with same time stamp having 16 records
ex = dat[which(dat$deviceId == "b05e5bf6-eea1-4b07-bb9a-80f373b32f9f" & dat$timestamp == "2017-08-15 05:40:27"),]
  
# 685530 have zero lat long
zeroLatLong = dat[which(dat$latitude == 0 & dat$longitude == 0),] 

## Remove 0 Lat Long
datToRmove = which(dat$latitude == 0 & dat$longitude == 0)
dat = dat[-datToRmove, ]

summary(dat$latitude)
summary(dat$longitude)

density(dat$latitude)

billboards = loadWorkbook(file.choose())
bbSheet = readWorksheet(billboards, sheet = "BBLocations")
museumSheet = readWorksheet(billboards, sheet = "Musuem")

#bbSheet[8, ] = rbind(c(60.172021,24.936654,75))

#ctrs <- gCentroid((bbSheet[, c('Lat','Long')]), byid = FALSE) 
#options(digits = 8)
#trimws(bbSheet$Lat, "l")
bbLat = mean(bbSheet$Lat)
bbLong = mean(bbSheet$Long)
bbCentroid = cbind(bbLat, bbLong)

## Clustering to identify user groups
dat_2 = dat

dat_devtime_many = dat_2 %>%
  group_by(deviceId, timestamp) %>%
  summarise(CNT = n()) %>%
  filter(CNT > 1) %>%
  select(deviceId, timestamp) %>%
  distinct(deviceId, timestamp) %>%
  arrange(deviceId)


dat_condensed = merge(dat_2, dat_devtime_many, by.x = c("deviceId","timestamp"), by.y = c("deviceId","timestamp")) %>%
      group_by(deviceId, timestamp) %>%
      mutate(lat_new = mean(latitude)) %>%
      mutate(long_new = mean(longitude))%>%
  distinct()

## Verify there are no 0 values
which(dat_condensed$latitude == 0 & dat_condensed$longitude == 0)

dat_condensed_old = dat_condensed

dat_condensed$latitude = dat_condensed$lat_new
dat_condensed$longitude = dat_condensed$long_new
dat_condensed = dat_condensed[, -c(22, 23)]

#dat_2$latitude[which(dat_2$deviceId == dat_condensed$deviceId & dat_2$timestamp == dat_condensed$timestamp)] = dat_condensed$lat_new[which(dat_2$deviceId == dat_condensed$deviceId & dat_2$timestamp == dat_condensed$timestamp)]
#dat_2$longitude[which(dat_2$deviceId == dat_condensed$deviceId & dat_2$timestamp == dat_condensed$timestamp)] = dat_condensed$long_new

#selectedRows <- dat_2[which(dat_2$deviceId %in% dat_condensed$deviceId & 
#                               dat_2$timestamp %in% dat_condensed$timestamp), ]



selectedRows_ <- which(dat_2$deviceId %in% dat_condensed$deviceId & 
                              dat_2$timestamp %in% dat_condensed$timestamp)

dat_old = dat_2[- selectedRows_, ]
dat_new = rbind(dat_old, as.data.frame(dat_condensed))
# Fitting K-Means to the dataset
# set.seed(29)
# kmeans = kmeans(x = dat_new[, c(2,3)], centers = 2)
# y_kmeans = kmeans$cluster
# 
# 
# dat_new$Visit = y_kmeans
# dat_new$Visit = as.factor(dat_new$Visit)
# 
# 
# library(cluster)
# op <- par(oma=c(5,7,1,1))
# par(op)
# clusplot(head(dat_new[, c(2,3)], 250),
#          head(dat_new$Visit, 250),
#          lines = 0,
#          shade = TRUE,
#          color = TRUE,
#          labels = 2,
#          plotchar = FALSE,
#          span = TRUE,
#          main = paste('Clusters of customers'),
#          xlab = 'Annual Income',
#          ylab = 'Spending Score')
# 
# ### Calculate the distance between billboard and user location
# 
# distm(dat_new[, c(2, 3)], c(bbLat, bbLong), fun = distHaversine)
# 
# uc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
# 
# library(foreach)
# foreach(i = 1:nrow(x1), .combine = c ) %do% euc.dist(x1[i,],x2[i,])
# 
# dat_new %>%
#   mutate(distance = gDistance(vec1, vec2, hausdorff = FALSE))


## Calculate distance between points and billboards, Museum

library(geosphere)

#device_latlong = data.frame(data_new$la)

#V = distHaversine(dat_new[, c('latitude','longitude')], bbSheet[1, 1:2]) / 1000 

#distm(dat_new[,c('latitude','longitude')], bbSheet[1, c('Lat','Long')], fun=distVincentyEllipsoid)

#Dist in kms

dist_from_BB_N = function(n)
{
  distm(dat_new[,c('longitude','latitude')], bbSheet[n, 2:1], fun=distGeo) / 1000
}

## Dist in kms
BB1 = dist_from_BB_N(1)
BB2 = dist_from_BB_N(2)
BB3 = dist_from_BB_N(3)
BB4 = dist_from_BB_N(4)
BB5 = dist_from_BB_N(5)
BB6 = dist_from_BB_N(6)
BB7 = dist_from_BB_N(7)
Museum = dist_from_BB_N(8)

DistMat = cbind(BB1,BB2,BB3,BB4,BB5,BB6,BB7,Museum)

dim(DistMat)

colnames(DistMat) = c("BB1","BB2","BB3", "BB4", "BB5", "BB6", "BB7", "Museum")

## Dist Threshold 
dist_threshold = function(n)
{

Bin_DistMat_1 = ifelse(DistMat[, 1:8] < n, 1, 0)

#Bin_DistMat[!which(BB1 == BB2 & BB2 == BB3 & BB3 == BB4 & BB4 == BB5 & BB7 == Museum),]

BD_1 = as.data.frame(Bin_DistMat_1)

BD_1 =  BD_1 %>%
    mutate(X1 = ifelse((any(BD_1[, 1:7] == 1) & BD_1[,8] == 1), 1,0))

return(BD_1)

}

D1 = dist_threshold(1) ## Dist Threshold = 1 KM
D2 = dist_threshold(0.75) ## Dist Threshold = 0.75 KM
D3 = dist_threshold(2) ## Dist Threshold = 2 KM