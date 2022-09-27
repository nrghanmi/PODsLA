library(apcluster) 
library(dplyr) 
library(caret) 
library(maxcovr)
library(ggplot2)
library(sf) 
library("writexl") 

rm(list=ls())

#---------- Load the data -----------
setwd("/Data/")

area = read.csv("areas.csv")

area = area %>% filter(grepl('ONE', area$STATE))

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

sim <- negDistMat(areaNormalized,r=1)
for  (i in 1:nrow(area)){
  for (j in 1:nrow(area)){
    if(i!=j){
      sim[i,j] <- max(popNormalized$`area[, 1]`[j],popNormalized$`area[, 1]`[i]) * sim[i,j]
    } 
  }
}


apres <- apcluster(sim, details=TRUE)
show(apres) ## show details of clustering results

#------------- Selecting the coordinates of PODs location via the index of exemplar ------------
APdF= area[apres@exemplars,3:4]
APdF

#------------------- results ----------------------
#------ ( distance +  coverage) --------


TolPOP <- c()
Weight_AP_all <- c()

listofDist<- c()
for  (i in 1:10){
  listofDist[i] <- i*1609.344
  APC= coverage(APdF, area,distance_cutoff =(i*1609.344))
  APC

  APT= facility_user_dist(APdF, area , coverage_distance=(i*1609.344))
  APT
  for (j in 1:nrow(area)){
    if (APT$distance[j]==0){
      APT$distance[[j]]= (APT$LANDAREA[[j]]/2)*1609.344
    }
  } 
  coveredAP=APT %>% filter(grepl(TRUE, APT$is_covered))
  TolPOP[i]=sum(coveredAP$POP)/sum(area$POP) 
  TolPOP[i]<- TolPOP[i]*100
  Weight_AP_all[i] <- weighted.mean(APT$distance,APT$POP)
  
}

#------ ( distance ) --------
Weight_AP_all[1]

#------ ( coverage) --------

TolPOP
setwd("/Results/Excel/")
write_xlsx(as.data.frame(TolPOP),"pop_cov_PW-AP_Max.xlsx")

#------------ Visualization -----------

data <- data.frame(
  ID = nrow(APdF),
  longitude = APdF$long,
  latitude = APdF$lat
)

points_sf <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
data_sf_utm <- sf::st_transform(points_sf, "+proj=longlat +ellps=WGS84 +datum=WGS84")
circle <- sf::st_buffer(data_sf_utm, dist = 8046.72)

png("/Results/Figures/PW-AP-ONE-Max.png", units="in", width=10, height=10, res=300)

p <- ggplot()+ geom_sf(data=points_sf)+  xlab("Longitude") + ylab("Latitude") +
  geom_point(data=area,   aes(x=long, y= lat, fill = POP,size = LANDAREA), shape = 21, alpha = 1)+ 
  geom_point(data=data, aes(x=longitude,y=latitude), shape=2, colour="red")+
  scale_size_continuous(range = c(1, 4))  +
  scale_fill_viridis_c() +
  geom_sf(data = circle, color = "red", alpha = 0, inherit.aes = FALSE) + theme(text = element_text(size = 30),axis.text.x = element_text(angle=90, hjust=1)) 
p$labels$size="Area"
p$labels$fill ="Population"
p
dev.off()

