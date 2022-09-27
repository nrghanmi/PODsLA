library(apcluster) 
library(dplyr) 
library(caret) 
library(maxcovr)
library(ggplot2)
library(sf) 
library("writexl") 
 
rm(list=ls())
source('/PODsLA/weighted_distance.R')

#---------- Load the data -----------
setwd("/Data/")

area = read.csv("areas.csv")

#-----please select the required state----
area = area %>% filter(grepl('ONE', area$STATE))
#area = area %>% filter(grepl('TWO', area$STATE))
#area = area %>% filter(grepl('THREE', area$STATE))

head(area)
str(area)

#---------- Selecting the required attributes ----------

area$LANDAREA <- sqrt(area$LANDAREA)
colnames(area)[4]  <- "lat"
colnames(area)[5]  <- "long"
area <- area[,c(2,3,4,5)]


#-------------------------- Data preprocessing ------------------------- 
#------ 1. Converting the long/lat to Cartesian coordinate  -----
area["X"] = 6371000  *cos((area$lat * pi / 180)) *cos(area$long * pi / 180)
area["Y"] = 6371000 * cos(area$lat * pi / 180) *sin(area$long * pi / 180)
area["Z"]= 6371000 * sin(area$lat * pi / 180)

#----------------- 2. Normalization -------------------
#----- [-1,1] -----
preprocessParams<-preProcess(area[,5:7], rangeBounds = c(-1,1), method="range")
areaNormalized <- predict(preprocessParams, area[,5:7])
areaNormalized[is.na(areaNormalized)]<-0
head(areaNormalized)

#----- [0,1] -----
preprocessParams<-preProcess(as.data.frame(area[,1]), rangeBounds = c(0,1), method="range")
popNormalized <- predict(preprocessParams, as.data.frame(area[,1]))
head(popNormalized$`area[, 1]`)

#-------- PW-AP ---------
set.seed(10)
sim <-  - euc_dist(areaNormalized,popNormalized$`area[, 1]`)
apres <- apcluster(sim, details=TRUE)
show(apres) ## show details of clustering results

#------------- Selecting the coordinates of PODs location via the index of exemplar ------------
APdF= area[apres@exemplars,3:4]
APdF

#------------------- results ----------------------
#------ ( distance +  coverage) --------
prop_cov_list<- c()
dist <- c()
TolPOP <- c()
Weight_AP_all <- c()
listofDist<- c()
for  (i in 1:10){
  listofDist[i] <- i*1609.344
  APC= coverage(APdF, area,distance_cutoff =(i*1609.344))
  APC
  prop_cov_list[i]<- APC$prop_cov*100
  
  APT= facility_user_dist(APdF, area , coverage_distance=(i*1609.344))
  APT
  for (j in 1:nrow(area)){
    if (APT$distance[j]==0){
      APT$distance[[j]]= (APT$LANDAREA[[j]]/2)*1609.344
    }
  } 
  dist[i] <- sum(APT$distance)/nrow(area)
  
  coveredAP=APT %>% filter(grepl(TRUE, APT$is_covered))
  TolPOP[i]=sum(coveredAP$POP)/sum(area$POP) 
  TolPOP[i]<- TolPOP[i]*100
  Weight_AP_all[i] <- weighted.mean(APT$distance,APT$POP)
  
}

#------ ( distance ) --------
dist[1]
Weight_AP_all[1]

#------ ( coverage) --------
prop_cov_list
TolPOP

setwd("/Results/Excel/")

#-----please select the required state----

write_xlsx(as.data.frame(TolPOP),"pop_cov_PODsLA_ONE.xlsx")
#write_xlsx(as.data.frame(TolPOP),"pop_cov_PODsLA_TWO.xlsx")
#write_xlsx(as.data.frame(TolPOP),"pop_cov_PODsLA_THREE.xlsx")

#------------ Visualization -----------

data <- data.frame(
  ID = nrow(APdF),
  longitude = APdF$long,
  latitude = APdF$lat
)

points_sf <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
data_sf_utm <- sf::st_transform(points_sf, "+proj=longlat +ellps=WGS84 +datum=WGS84")

circle <- sf::st_buffer(data_sf_utm, dist = 8046.72)

setwd("/Results/Figures/")

#-----please select the required state----
png("PODsLA_ONE.png", units="in", width=10, height=10, res=150)
#png("PODsLA_TWO.png", units="in", width=10, height=10, res=150)
#png("PODsLA_THREE.png", units="in", width=10, height=12, res=150)

p <- ggplot()+ geom_sf(data=points_sf)+  xlab("Longitude") + ylab("Latitude") +
  geom_point(data=area,   aes(x=long, y= lat, fill = POP,size = LANDAREA), shape = 21, alpha = 1)+ 
  geom_point(data=data, aes(x=longitude,y=latitude), shape=2, colour="red")+
  scale_size_continuous(range = c(1, 4))  +
  scale_fill_viridis_c() +
  geom_sf(data = circle, color = "red", alpha = 0, inherit.aes = FALSE) + theme(text = element_text(size = 30),axis.text.x = element_text(angle=90, hjust=1)) # for states ONE and THREE
 # geom_sf(data = circle, color = "red", alpha = 0, inherit.aes = FALSE) + theme(text = element_text(size = 25)) # for TWO state

p$labels$size="Area"
p$labels$fill ="Population"
p
dev.off()

