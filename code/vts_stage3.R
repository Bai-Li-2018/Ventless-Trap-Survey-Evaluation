setwd("D:/project/maine_vts")
#### Install and library packages ####
list_of_packages <- c("maptools", "rgdal", "mgcv", "classInt","RColorBrewer","maps", "mapdata",  "mgcv", "raster", "SDMTools", "dplyr", "akima", "viridis", "sp", "rgeos")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(maptools)
library(rgdal)
library(classInt) 
library(RColorBrewer)
library(maps)
library(mapdata)
library(mgcv)
library(raster)
library(SDMTools)
library(dplyr)
library(akima)
library(viridis)
library(sp)
library(rgeos)
#### Load bottom trawl survey data and map shapefile#### 
nao_temperature <- read.csv("./data/NAO_temperature.csv")
lobster_catch_data = read.csv("./data/bottom_trawl_survey/Lobster_MENHTrawl_Catch.csv")
lobster_catch_data_env = read.csv("./data/bottom_trawl_survey/Lobster_MENHTrawl_Catch_old.csv")
lobster_length_data = read.csv("./data/bottom_trawl_survey/Lobster_MENHTrawl_Length.csv")

lobster_catch_data_env <- lobster_catch_data_env[which(as.character(lobster_catch_data_env$catch_sum)!="#N/A"),]
lobster_catch_data <- lobster_catch_data[which(as.character(lobster_catch_data$catch_sum)!="#N/A"),]
lobster_catch_data <- lobster_catch_data[which(as.character(lobster_catch_data$catch_sum) != ""),]

length(unique(lobster_catch_data_env$UniqueTowID))
length(unique(lobster_catch_data$UniqueTowID))

for (i in 1:nrow(lobster_catch_data)){
  id <- which(lobster_catch_data_env$UniqueTowID==lobster_catch_data$UniqueTowID[i])
  print(i);print(length(id))
  if (length(id)==0) {
    lobster_catch_data$Water_Temp_DegC[i] <- NA
    lobster_catch_data$Salinity_psu[i] <- NA
  }
  else {
    lobster_catch_data$Water_Temp_DegC[i] <- as.numeric(as.character(lobster_catch_data_env$Water_Temp_DegC[id]))
    lobster_catch_data$Salinity_psu[i] <- as.numeric(as.character(lobster_catch_data_env$Salinity_psu[id]))
  }
}


#coastline <-  readShapePoly("./data/gis/ME_clip_NAD83.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
#grid <-  readShapePoly("./data/gis/grids2007.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
statistical_areas <- readShapePoly("./data/gis/Statistical_Areas_2010.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
sa511_513 <- statistical_areas[which(statistical_areas@data$Id %in% c(511:513)),]
vts_gom_strata <-  readShapePoly("./data/gis/GOM_VTS_Strata.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))

head(lobster_catch_data)
class_convert_var <- c("UniqueTowID", "TowNum", "Region", "Stratum", "Grid_ID", "NumberCaught", "catch_sum", "Weight", "weight_sum", "end_lat", "end_lon", "end_depth", "Tow_Time", "Tow_LengthNM", "EXP_Catch", "Year", "Water_Temp_DegC", "Salinity_psu")
for(i in 1:length(class_convert_var)){
  lobster_catch_data[,class_convert_var[i]] <- as.numeric(as.character(lobster_catch_data[,class_convert_var[i]]))
}

head(lobster_length_data)
class_convert_var <- c("TowNum", "Region", "Stratum", "Year", "Length", "end_lat", "end_lon")
for(i in 1:length(class_convert_var)){
  lobster_length_data[,class_convert_var[i]] <- as.numeric(as.character(lobster_length_data[,class_convert_var[i]]))
}
#### Check outliers ####
summary(lobster_catch_data)
summary(lobster_length_data)
identical(lobster_catch_data$NumberCaught, lobster_catch_data$catch_sum)
mismatch_lobster_catch <- lobster_catch_data[which(!(lobster_catch_data$NumberCaught %in% lobster_catch_data$catch_sum)),]
write.csv(mismatch_lobster_catch, file = "./output/mismatch_lobster_catch.csv")


if(FALSE){
  jpeg(filename = "./plot/outliers_lobster_catch.jpeg", width = 170, height = 80, res = 600, units="mm")
  par(mfrow=c(2,3), mar=c(4,2,1,1))
  dotchart(lobster_catch_data$NumberCaught, groups=as.factor(lobster_catch_data$Region), ylab="Region", xlab="Catch (#)/Tow")
  dotchart(lobster_catch_data$EXP_Catch, groups=as.factor(lobster_catch_data$Region), ylab="Region", xlab="Expect Catch (#)/Tow")
  dotchart(lobster_catch_data$end_depth, groups=as.factor(lobster_catch_data$Region), ylab="Region", xlab="Depth")
  dotchart(lobster_catch_data$Water_Temp_DegC, groups=as.factor(lobster_catch_data$Region), ylab="Region", xlab="Temperature")
  dotchart(lobster_catch_data$Salinity_psu, groups=as.factor(lobster_catch_data$Region), ylab="Region", xlab="Salinity")
  dev.off()
  
  lobster_catch_data[which(lobster_catch_data$NumberCaught>10000),]
  
  summary(lobster_length_data$Unit_Len)
  lobster_length_data[which(lobster_length_data$Unit_Len=="CM"),]
  
  jpeg(filename = "./plot/outliers_lobster_length.jpeg", width = 170, height = 40, res = 600, units="mm")
  par(mfrow=c(1,3), mar=c(4,2,1,1))
  dotchart(lobster_length_data$Length, groups=as.factor(lobster_length_data$Region), ylab="Region", xlab="Length")
  dotchart(lobster_length_data$end_lat, groups=as.factor(lobster_length_data$Region), ylab="Region", xlab="Latitude")
  dotchart(lobster_length_data$end_lon, groups=as.factor(lobster_length_data$Region), ylab="Region", xlab="Longitude")
  dev.off()
}

#outlier_catch=lobster_catch_data[which(lobster_catch_data$NumberCaught>10000),]
#write.csv(outlier_catch, file = "./output/outlier_catch.csv")
#lobster_catch_data<-lobster_catch_data[which(lobster_catch_data$NumberCaught<10000),]
#### Clean and aggregate lobster length dataset ####
outlier_length=lobster_length_data[which(lobster_length_data$Length>300),]
write.csv(outlier_length, file = "./output/outlier_length.csv")
lobster_length_data <- lobster_length_data[which(lobster_length_data$Length<300),]
lobster_length_data[which(is.na(lobster_length_data$Length)),]

juvenile_length <- 83
juvenile_length_data <- lobster_length_data[which(lobster_length_data$Length<juvenile_length),]
juvenile_per_tow <- aggregate(juvenile_length_data$Length, by=list(juvenile_length_data$Survey, juvenile_length_data$TowNum), length)
colnames(juvenile_per_tow) <- c("Survey", "TowNum", "JuvNum")
sum_per_tow <- aggregate(lobster_length_data$Length, by=list(lobster_length_data$Survey, lobster_length_data$TowNum), length)
colnames(sum_per_tow) <- c("Survey", "TowNum", "SumNum")
juvenile_ratio <- sum_per_tow
for (i in 1:nrow(juvenile_ratio)){
  juv_num <- juvenile_per_tow$JuvNum[which(juvenile_per_tow$Survey==juvenile_ratio$Survey[i] & juvenile_per_tow$TowNum==juvenile_ratio$TowNum[i])]
  if (length(juv_num)==0) juv_num=0
  juvenile_ratio$SumNum[i] <- juv_num/juvenile_ratio$SumNum[i]
}
colnames(juvenile_ratio) <- c("Survey", "TowNum", "JuvRatio")
#### Match lobster catch and length datasets ####
month_day <- strsplit(as.character(lobster_catch_data$End_Date), split = '/')
for (i in 1:length(month_day)){
  lobster_catch_data$Month[i] <- as.numeric(month_day[[i]][1])
  lobster_catch_data$Day[i] <- as.numeric(month_day[[i]][2])
}

lobster_catch_data <- lobster_catch_data[which(lobster_catch_data$Survey!="FL18"),]
lobster_catch_data$Survey <- factor(lobster_catch_data$Survey)
summary(lobster_catch_data)

for (i in 1:nrow(lobster_catch_data)){
  juv_ratio <- juvenile_ratio$JuvRatio[which(juvenile_ratio$Survey==lobster_catch_data$Survey[i] & juvenile_ratio$TowNum==lobster_catch_data$TowNum[i])]
  if (length(juv_ratio)==0) juv_ratio=0
  lobster_catch_data$JuvRatio[i] <- juv_ratio
}
lobster_catch_data$juv_num <- lobster_catch_data$EXP_Catch*lobster_catch_data$JuvRatio
lobster_catch_data$adu_num <-lobster_catch_data$EXP_Catch*(1-lobster_catch_data$JuvRatio)

#dotchart(lobster_catch_data$juv_num, groups=as.factor(lobster_catch_data$Region), ylab="Region", xlab="Juveniles (#)/Tow")

#### Plot distribution of raw data ####
lobster_catch_data <- lobster_catch_data[-which(is.na(lobster_catch_data[,c("end_depth", "Water_Temp_DegC", "Salinity_psu", "end_lat", "end_lon", "Year", "Month")])),]

map_data <- lobster_catch_data
map_data$log_data <- ifelse(map_data$juv_num==0, NA, log(map_data$juv_num))

var="log_data"
plotvar <- map_data[,var]
nclr=6
plotclr <- rev(brewer.pal(nclr,"RdBu"))
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]

jpeg(filename = paste("./plot/juv_catch_map.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
layout(matrix(c(1,1,1,3,2,2,2,3), 2, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month<7&is.na(map_data$log_data))], map_data$end_lat[which(map_data$Month<7&is.na(map_data$log_data))], pch=4, cex=0.3, col="gray")
points(map_data$end_lon[which(map_data$Month<7)], map_data$end_lat[which(map_data$Month<7)], pch=16, col=colcode[which(map_data$Month<7)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Spring", bty="n")

plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month>8&is.na(map_data$log_data))], map_data$end_lat[which(map_data$Month>8&is.na(map_data$log_data))], pch=4, cex=0.3, col="gray")
points(map_data$end_lon[which(map_data$Month>8)], map_data$end_lat[which(map_data$Month>8)], pch=16, col=colcode[which(map_data$Month>8)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Fall", bty="n")

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table")), "No lobster"), fill=c(attr(colcode, "palette"), "gray"), cex=0.9, bty="n", title="Log(#)/Tow")
dev.off()

#### Temperature map ####
var="Water_Temp_DegC"
plotvar <- map_data[,var]
nclr=6
plotclr <- rev(brewer.pal(nclr,"RdBu"))
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]

jpeg(filename = paste("./plot/temperature_map.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
layout(matrix(c(1,1,1,3,2,2,2,3), 2, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month<7)], map_data$end_lat[which(map_data$Month<7)], pch=16, col=colcode[which(map_data$Month<7)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Spring", bty="n")

plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month>8)], map_data$end_lat[which(map_data$Month>8)], pch=16, col=colcode[which(map_data$Month>8)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Fall", bty="n")

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9, bty="n", title="Temperature")
dev.off()
#### Salinity map ####
var="Salinity_psu"
plotvar <- map_data[,var]
nclr=6
plotclr <- rev(brewer.pal(nclr,"RdBu"))
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]

jpeg(filename = paste("./plot/salinity_map.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
layout(matrix(c(1,1,1,3,2,2,2,3), 2, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month<7)], map_data$end_lat[which(map_data$Month<7)], pch=16, col=colcode[which(map_data$Month<7)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Spring", bty="n")

plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month>8)], map_data$end_lat[which(map_data$Month>8)], pch=16, col=colcode[which(map_data$Month>8)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Fall", bty="n")

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9, bty="n", title="Salinity")
dev.off()

#### Depth map####
var="end_depth"
plotvar <- map_data[,var]
nclr=6
plotclr <- rev(brewer.pal(nclr,"RdBu"))
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]

jpeg(filename = paste("./plot/depth_map.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
layout(matrix(c(1,1,1,3,2,2,2,3), 2, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month<7)], map_data$end_lat[which(map_data$Month<7)], pch=16, col=colcode[which(map_data$Month<7)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Spring", bty="n")

plot(map_data$end_lon, map_data$end_lat, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, type="n", xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE)
#map(coastline, add =T, fill = TRUE, col = "gray95", border="black", lwd=0.2)
points(map_data$end_lon[which(map_data$Month>8)], map_data$end_lat[which(map_data$Month>8)], pch=16, col=colcode[which(map_data$Month>8)], cex=0.3)
box()
degAxis(1)
degAxis(2)
legend("topleft", "Fall", bty="n")

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.9, bty="n", title="Depth (Fathom)")
dev.off()

#### Seasonal GAM ####
summary(lobster_catch_data$juv_num)
spring_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(lobster_catch_data$Month<7),])
summary(spring_model)

jpeg(filename = paste("./plot/spring_gam_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
par(mfrow=c(3,2), mar=c(4,4,1,1))
plot.gam(spring_model, xlab=c("Depth"), ylab="Lobster density", select = 1)
plot.gam(spring_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
plot.gam(spring_model, xlab=c("Salinity"), ylab="Lobster density", select = 3)
plot.gam(spring_model, xlab=c("Latitude"), ylab="Lobster density", select = 4)
plot.gam(spring_model, xlab=c("Longitude"), ylab="Lobster density", select = 5)
dev.off()

fall_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(lobster_catch_data$Year<2014 & (lobster_catch_data$Month==6 | lobster_catch_data$Month>8)),])
summary(fall_model)

fall_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(lobster_catch_data$Month==6 | lobster_catch_data$Month>8),])
summary(fall_model)

jpeg(filename = paste("./plot/fall_gam_relationship.jpeg", sep=""), width=100, height=80, units = "mm", res = 600)
par(mfrow=c(2,2), mar=c(4,4,1,1))
plot.gam(fall_model, xlab=c("Depth"), ylab="Lobster density", select = 1)
plot.gam(fall_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
plot.gam(fall_model, xlab=c("Latitude"), ylab="Lobster density", select = 3)
plot.gam(fall_model, xlab=c("Longitude"), ylab="Lobster density", select = 4)
dev.off()

if(FALSE){
  gam_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data)
  summary(gam_model)
  
  jpeg(filename = paste("./plot/gam_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  jpeg(filename = paste("./plot/flowchart_gam_relationship.jpeg", sep=""), width=80, height=100, units = "mm", res = 600)
  par(mfrow=c(2,1), mar=c(4,4,1,1))
  
  plot.gam(gam_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  
  dev.off()
} ## use all data

if(FALSE){
  gam_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(lobster_catch_data$Year<2014 & (lobster_catch_data$Month==6 | lobster_catch_data$Month>8)),])
  #gam_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(lobster_catch_data$Year<2014 & (lobster_catch_data$Month==6 | lobster_catch_data$Month>8)),])
  summary(gam_model)
  
  jpeg(filename = paste("./plot/gam_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 800)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model, xlab=c("Temperature"), ylab="Lobster density", select = 3)
  plot.gam(gam_model, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
} ## use June and fall data

if(FALSE){
  gam_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(lobster_catch_data$Month==6 |lobster_catch_data$Month==9),])
  summary(gam_model)
  
  gam_model <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(lobster_catch_data$Month==6 |lobster_catch_data$Month==9),])
  summary(gam_model)
  
  jpeg(filename = paste("./plot/gam_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 800)
  par(mfrow=c(2,2), mar=c(4,4,1,1))
  plot.gam(gam_model, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model, xlab=c("Latitude"), ylab="Lobster density", select = 3)
  plot.gam(gam_model, xlab=c("Longitude"), ylab="Lobster density", select = 4)
  dev.off()
} ## use June and September data

if(FALSE){
  trawl_points <- SpatialPoints(cbind(lobster_catch_data$end_lon, lobster_catch_data$end_lat), proj4string=CRS("+proj=longlat +datum=WGS84")) 
  polygon_points <- over(trawl_points , sa511_513)
  
  gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(polygon_points$Id=="511"),])
  summary(gam_model_511)
  gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(polygon_points$Id=="511"),])
  summary(gam_model_511)
  
  jpeg(filename = paste("./plot/gam511_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(2,2), mar=c(4,4,1,1))
  plot.gam(gam_model_511, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_511, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_511, xlab=c("Latitude"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511, xlab=c("Longitude"), ylab="Lobster density", select = 4)
  dev.off()
  
  gam_model_512 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(polygon_points$Id=="512"),])
  summary(gam_model_512)
  
  jpeg(filename = paste("./plot/gam512_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_512, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_512, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_512, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_512, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_512, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_513 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(polygon_points$Id=="513"),])
  summary(gam_model_513)
  
  jpeg(filename = paste("./plot/gam513_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_513, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_513, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_513, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_513, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
} ## use all data but separate data by statistical area

if(FALSE){
  trawl_points <- SpatialPoints(cbind(lobster_catch_data$end_lon, lobster_catch_data$end_lat), proj4string=CRS("+proj=longlat +datum=WGS84")) 
  polygon_points <- over(trawl_points , sa511_513)
  
  gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(polygon_points$Id=="511" & lobster_catch_data$Month>8),])
  summary(gam_model_511)
  
  #gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(polygon_points$Id=="511"),])
  #summary(gam_model_511)
  
  jpeg(filename = paste("./plot/gam511_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_511, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_511, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_511, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_511, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_512 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(polygon_points$Id=="512"& lobster_catch_data$Month>8),])
  summary(gam_model_512)
  
  jpeg(filename = paste("./plot/gam512_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_512, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_512, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_512, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_512, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_512, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_513 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data <- lobster_catch_data[which(polygon_points$Id=="513"& lobster_catch_data$Month>8),])
  summary(gam_model_513)
  
  jpeg(filename = paste("./plot/gam513_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_513, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_513, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_513, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_513, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
} ## use fall data and separate data by statistical area

if(TRUE){
  trawl_points <- SpatialPoints(cbind(lobster_catch_data$end_lon, lobster_catch_data$end_lat), proj4string=CRS("+proj=longlat +datum=WGS84")) 
  polygon_points <- over(trawl_points , sa511_513)
  
  gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511" & lobster_catch_data$Year<2017 & (lobster_catch_data$Month==6 | lobster_catch_data$Month>8)),])
  summary(gam_model_511)
  
  jpeg(filename = paste("./plot/gam511_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(2,2), mar=c(4,4,1,1))
  plot.gam(gam_model_511, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_511, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  #plot.gam(gam_model_511, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511, xlab=c("Latitude"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511, xlab=c("Longitude"), ylab="Lobster density", select = 4)
  dev.off()
  
  gam_model_512 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="512"& lobster_catch_data$Year<2017 & (lobster_catch_data$Month==6 |lobster_catch_data$Month>8)),])
  summary(gam_model_512)
  
  jpeg(filename = paste("./plot/gam512_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(2,2), mar=c(4,4,1,1))
  plot.gam(gam_model_512, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_512, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  #plot.gam(gam_model_512, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_512, xlab=c("Latitude"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_512, xlab=c("Longitude"), ylab="Lobster density", select = 4)
  dev.off()
  
  gam_model_513 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="513"& lobster_catch_data$Year<2017 & (lobster_catch_data$Month==6 |lobster_catch_data$Month>8)),])
  summary(gam_model_513)
  
  jpeg(filename = paste("./plot/gam513_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(2,2), mar=c(4,4,1,1))
  plot.gam(gam_model_513, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_513, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  #plot.gam(gam_model_513, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Latitude"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Longitude"), ylab="Lobster density", select = 4)
  dev.off()
  
  save(gam_model_511, gam_model_512, gam_model_513, file="./output/gam_fit.RData")
} ## use June and Fall data but separate data by statisitcal area

if(FALSE){
  trawl_points <- SpatialPoints(cbind(lobster_catch_data$end_lon, lobster_catch_data$end_lat), proj4string=CRS("+proj=longlat +datum=WGS84")) 
  polygon_points <- over(trawl_points , sa511_513)
  
  gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511" & lobster_catch_data$Year<2014 & (lobster_catch_data$Month==6 | lobster_catch_data$Month==10)),])
  summary(gam_model_511)
  
  #gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511" & (lobster_catch_data$Month==6 | lobster_catch_data$Month==10)),])
  #summary(gam_model_511)
  
  jpeg(filename = paste("./plot/gam511_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_511, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_511, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_511, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_511, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_512 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="512"& lobster_catch_data$Year<2014 &(lobster_catch_data$Month==6 |lobster_catch_data$Month==10)),])
  summary(gam_model_512)
  
  jpeg(filename = paste("./plot/gam512_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_512, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_512, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_512, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_512, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_512, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_513 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="513"& lobster_catch_data$Year<2014 &(lobster_catch_data$Month==6 |lobster_catch_data$Month==9)),])
  summary(gam_model_513)
  
  jpeg(filename = paste("./plot/gam513_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_513, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_513, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_513, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_513, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
} ## use June and September data but separate data by statistical area

if(FALSE){
  trawl_points <- SpatialPoints(cbind(lobster_catch_data$end_lon, lobster_catch_data$end_lat), proj4string=CRS("+proj=longlat +datum=WGS84")) 
  polygon_points <- over(trawl_points , sa511_513)
  
  gam_model_511512 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which((polygon_points$Id=="511" | polygon_points$Id=="512") & lobster_catch_data$Year<2014 & (lobster_catch_data$Month==6 | lobster_catch_data$Month==10)),])
  summary(gam_model_511512)
  
  #gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511" & (lobster_catch_data$Month==6 | lobster_catch_data$Month==10)),])
  #summary(gam_model_511)
  
  jpeg(filename = paste("./plot/gam511512_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_511512, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_511512, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_511512, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511512, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_511512, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_513 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="513"& lobster_catch_data$Year<2014 &(lobster_catch_data$Month==6 |lobster_catch_data$Month==9)),])
  summary(gam_model_513)
  
  jpeg(filename = paste("./plot/gam513_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_513, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_513, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_513, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_513, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
} ## use June and September data but separate data by 2 statistical areas

if(FALSE){
  trawl_points <- SpatialPoints(cbind(lobster_catch_data$end_lon, lobster_catch_data$end_lat), proj4string=CRS("+proj=longlat +datum=WGS84")) 
  polygon_points <- over(trawl_points , sa511_513)
  
  gam_model_511512 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which((polygon_points$Id=="511" | polygon_points$Id=="512") & lobster_catch_data$Year<2014 & (lobster_catch_data$Month==6 | lobster_catch_data$Month>8)),])
  summary(gam_model_511512)
  
  #gam_model_511 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="511" & (lobster_catch_data$Month==6 | lobster_catch_data$Month==10)),])
  #summary(gam_model_511)
  
  jpeg(filename = paste("./plot/gam511512_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_511512, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_511512, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_511512, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_511512, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_511512, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
  
  gam_model_513 <- gam(juv_num~s(end_depth, k=5)+s(Water_Temp_DegC, k=5)+s(Salinity_psu, k=5)+s(end_lat, k=5)+s(end_lon, k=5), family = tw(), data = lobster_catch_data[which(polygon_points$Id=="513"& lobster_catch_data$Year<2014 &(lobster_catch_data$Month==6 |lobster_catch_data$Month>8)),])
  summary(gam_model_513)
  
  jpeg(filename = paste("./plot/gam513_relationship.jpeg", sep=""), width=100, height=100, units = "mm", res = 600)
  par(mfrow=c(3,2), mar=c(4,4,1,1))
  plot.gam(gam_model_513, xlab=c("Depth"), ylab="Lobster density", select = 1)
  plot.gam(gam_model_513, xlab=c("Temperature"), ylab="Lobster density", select = 2)
  plot.gam(gam_model_513, xlab=c("Salinity"), ylab="Lobster density", select = 3)
  plot.gam(gam_model_513, xlab=c("Latitude"), ylab="Lobster density", select = 4)
  plot.gam(gam_model_513, xlab=c("Longitude"), ylab="Lobster density", select = 5)
  dev.off()
} ## use June and fall data but separate data by 2 statistical areas

#### Grid time ID ####
#yrs<-c(2006:2013, 2015:2016)
yrs<-2006:2016
dates<-as.data.frame(matrix(NA,length(yrs)*3,3))
colnames(dates)<-c('y','m','d')
dates$y <- rep(yrs, each=3)
#dates$y<-c(rep(yrs[1],3),rep(yrs[2],3),rep(yrs[3],3),rep(yrs[4],3),rep(yrs[5],3),rep(yrs[6],3), rep(yrs[7],3),rep(yrs[8],3), rep(yrs[9],3), rep(yrs[10],3))
dates$d<-rep(16,length(yrs))
dates$m<-rep(c(6,7,8),length(yrs))
dates$date <- do.call(paste, list(dates$m, dates$d, dates$y))
dates$date <- as.Date(dates$date, format=c("%m %d %Y"))
dates$julian_date <- as.numeric(dates$date) + 2440588
dates$modified_julian_date <- floor(dates$julian_date-2400000.5)
dates$modified_julian_date <- dates$modified_julian_date+0.17 
#### Download FVCOM time ID ####
fvcom_data <-as.data.frame(read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?time[0:1:342347]"))
fvcom_time <- as.data.frame(fvcom_data$Dataset..[5:nrow(fvcom_data)])
names(fvcom_time) <-"modified_julian_date"
fvcom_time$modified_julian_date <- as.numeric(as.character(fvcom_time$modified_julian_date))
fvcom_time$id <- 0:(nrow(fvcom_time)-1)

#### Match time ####
time_id <- c()
for(i in 1:nrow(dates)){
  temp <- fvcom_time$id[which(round(fvcom_time$modified_julian_date,2)==round(dates$modified_julian_date[i],2))]
  if(length(temp)==0) time_id[i] <- NA
  else time_id[i] <- temp
}
#### Download FVCOM location data ####
lat <- read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?lat[0:1:48450]")
lon <- read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?lon[0:1:48450]")
names(lat) <- "lat"
names(lon) <- "lon"

latitude <- lat$lat[5:nrow(lat)]
longitude <- lon$lon[5:nrow(lon)]
latitude <- as.numeric(as.character(latitude))
longitude <- as.numeric(as.character(longitude))
#### Grid location####
start_x <- floor(range(lobster_catch_data$end_lon)[1])
end_x <- ceiling(range(lobster_catch_data$end_lon)[2])
start_y <- floor(range(lobster_catch_data$end_lat)[1])
end_y <- ceiling(range(lobster_catch_data$end_lat)[2])
my_mesh=expand.grid(seq(start_x, end_x, by=0.01), seq(start_y, end_y, by=0.01))
coordinates(my_mesh) <- ~Var1 + Var2
grid <- aggregate(vts_gom_strata)
grid <- disaggregate(grid)

boundary_polygon = list()
for(i in 1:length(grid@polygons)){
  boundary <- grid@polygons[[i]]@Polygons[[1]]@coords
  colnames(boundary) <- c("lon", "lat")
  boundary_polygon[[i]] <- Polygon(boundary)
  boundary_list <- Polygons(boundary_polygon, paste("boundary",i))
  boundary_line <- SpatialPolygons(list(boundary_list))
}
#plot(boundary_line, axes=T)
gom_grid=crop(my_mesh, boundary_line)
#gom_grid=crop(my_mesh, sa511_513)
#grid <- aggregate(vts_gom_strata)
#gom_grid=crop(my_mesh, vts_gom_strata)
#gom_grid <- intersect(my_mesh,extent(grid))
grid_data <-as.data.frame(gom_grid@coords)
colnames(grid_data) <-c("lon", "lat")
grid_data <- grid_data[which(grid_data$lat>42.9),]

#### Download grid depth/temperature/salinity data####
if(T){
  #### Download depth data ####
  depth_raster <- raster("./data/gis/ne_atl_crm_v1.asc")
  sub_depth_raster <- crop(depth_raster, gom_grid)
  grid_data$depth <- extract.data(grid_data, sub_depth_raster)
  grid_data$fathom <- grid_data$depth/1.8288
  write.csv(grid_data, file="./output/grid_depth_data.csv")
  
  #### Download temperature data ####
  temperature_fvcom_data<-list()
  for (i in 1:length(time_id)){
    print(i)
    temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?temp[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep=""))
    temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
    temperature_fvcom_data[[i]] <- cbind(longitude, latitude, temp_data)
    colnames(temperature_fvcom_data[[i]]) <- c("lon", "lat", "temperature")
  }
  save(temperature_fvcom_data, file="./data/temperature_data.RData")
  
  load("./data/temperature_data.RData")
  temperature_raster_data <- list()
  grid_data <-as.data.frame(gom_grid@coords)
  colnames(grid_data) <-c("lon", "lat")
  grid_data <- grid_data[which(grid_data$lat>42.9),]
  for(i in 1:length(time_id)){
    print(i)
    temp_data <- as.data.frame(temperature_fvcom_data[[i]])
    rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
    rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
    akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
    rast <- raster(akima.smooth)
    temperature_raster_data[[i]] <- extract.data(grid_data, rast)
  }
  save(temperature_raster_data, file="./output/temperature_raster_data.RData")
  #### Download salinity data ####
  salinity_fvcom_data<-list()
  for (i in 1:length(time_id)){
    print(i)
    temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?salinity[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep=""))
    temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
    salinity_fvcom_data[[i]] <- cbind(longitude, latitude, temp_data)
    colnames(salinity_fvcom_data[[i]]) <- c("lon", "lat", "salinity")
  }
  save(salinity_fvcom_data, file="./data/salinity_data.RData")
  
  load("./data/salinity_data.RData")
  salinity_raster_data <- list()
  grid_data <-as.data.frame(gom_grid@coords)
  colnames(grid_data) <-c("lon", "lat")
  grid_data <- grid_data[which(grid_data$lat>42.9),]
  for(i in 1:length(time_id)){
    print(i)
    temp_data <- as.data.frame(salinity_fvcom_data[[i]])
    rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
    rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
    akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
    rast <- raster(akima.smooth)
    salinity_raster_data[[i]] <- extract.data(grid_data, rast)
  }
  save(salinity_raster_data, file="./output/salinity_raster_data.RData")
}
#### Plot depth grid map ####
grid_data <- read.csv(file="./output/grid_depth_data.csv")
depth_grid_plot <- read.csv("./output/grid_depth_data.csv")
plot_data <- as.data.frame(cbind(depth_grid_plot$lon, depth_grid_plot$lat, -depth_grid_plot$fathom))
colnames(plot_data) <- c("Longitude", "Latitude", "Y")
depth_odd_id <- which(plot_data$Y<=0)
summary(plot_data)
plot_data <- na.omit(plot_data)
plot_data <- plot_data[which(plot_data$Y>0),]
summary(plot_data)

plotvar <- plot_data$Y
nclr=8
plotclr <- brewer.pal(nclr+1,"Blues")[2:length(brewer.pal(nclr+1,"Blues"))]
class <- classIntervals(plotvar, nclr, style="equal")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]
jpeg(filename = paste("./plot/grid_depth_map.jpeg", sep=""), width=100, height=50, units = "mm", res = 600)
layout(matrix(c(1,1,1,2), 1, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(plot_data$Longitude, plot_data$Latitude, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
box()
degAxis(1)
degAxis(2)
par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.75, bty="n", title="Depth (Fathom)")
dev.off()

#### Plot temperature grid map ####
load("./output/temperature_raster_data.RData")

jpeg(filename = paste("./plot/grid_temperature_map.jpeg", sep=""), width=120, height=140, units = "mm", res = 600)
m <- cbind(rep(34, 13), matrix(c(37, 37, 37, 1:33, 36, 36, 36), nrow=13, ncol=3, byrow=T), rep(35, 13))
layout(m)
par(mar=c(0,0,0,0))
plotvar=unlist(temperature_raster_data)
#na_id <- which(is.na(plotvar))
#plotvar <- plotvar[-na_id]
nclr=10
plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
class <- classIntervals(plotvar, nclr, style="equal")
fix_break<-round(class$brks, digits = 2)
for(i in 1:length(temperature_raster_data)){
  print(i)
  #na_id <- which(is.na(temperature_raster_data[[i]]))
  #plotvar <- temperature_raster_data[[i]][-na_id]
  plotvar <- temperature_raster_data[[i]]
  nclr=10
  plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="fixed", fixedBreaks=fix_break)
  colcode <- findColours(class, plotclr)
  
  start_x <- range(lobster_catch_data$end_lon)[1]
  end_x <- range(lobster_catch_data$end_lon)[2]
  start_y <- range(lobster_catch_data$end_lat)[1]
  end_y <- range(lobster_catch_data$end_lat)[2]
  plot(depth_grid_plot$lon, depth_grid_plot$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F)
  map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
  #plot(sa511_513, add=T)
  box()
  legend("topleft", paste(dates$y[i], "-", dates$m[i], sep=""), bty="n", cex=0.7)
  
  if (i == (length(temperature_fvcom_data)-3+1)) {
    axis(1, cex=0.5)
  }
  if (i == length(temperature_fvcom_data)) {
    axis(1, cex=0.5)
  }
  if (i == 2) axis(3, cex=0.5)
  else {
    if ((i+5)%%6 == 0) axis(2, cex=0.5)
    if (i%%6 == 0) axis(4, cex=0.5)
  }
  
  if (i == 16) mtext("Latitude (°)", side=2, line=2.2)
  if (i == 32) mtext("Longitude (°)", side=1, line=2.2)
}

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()

par(mar=c(0.1,2,0.1,0.1))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.7, bty="n", title="Temperature (°C)")
dev.off()

#### Plot salinity grid map ####
load("./output/salinity_raster_data.RData")

jpeg(filename = paste("./plot/grid_salinity_map.jpeg", sep=""), width=120, height=140, units = "mm", res = 600)
m <- cbind(rep(34, 13), matrix(c(37, 37, 37, 1:33, 36, 36, 36), nrow=13, ncol=3, byrow=T), rep(35, 13))
layout(m)
par(mar=c(0,0,0,0))

plotvar=unlist(salinity_raster_data)
nclr=10
plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
class <- classIntervals(plotvar, nclr, style="quantile")
fix_break<-round(class$brks, digits = 2)
for(i in 1:length(salinity_raster_data)){
  print(i)
  plotvar <- salinity_raster_data[[i]]
  
  nclr=10
  plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
  class <- classIntervals(plotvar, nclr, style="fixed", fixedBreaks=fix_break)
  colcode <- findColours(class, plotclr)
  
  start_x <- range(lobster_catch_data$end_lon)[1]
  end_x <- range(lobster_catch_data$end_lon)[2]
  start_y <- range(lobster_catch_data$end_lat)[1]
  end_y <- range(lobster_catch_data$end_lat)[2]
  plot(depth_grid_plot$lon, depth_grid_plot$lat, pch=16, col=colcode, cex=0.1, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F)
  map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
  #plot(sa511_513, add=T)
  box()
  legend("topleft", paste(dates$y[i], "-", dates$m[i], sep=""), bty="n", cex=0.7)
  
  if (i == (length(temperature_fvcom_data)-3+1)) {
    axis(1, cex=0.5)
  }
  if (i == length(temperature_fvcom_data)) {
    axis(1, cex=0.5)
  }
  if (i == 2) axis(3, cex=0.5)
  else {
    if ((i+5)%%6 == 0) axis(2, cex=0.5)
    if (i%%6 == 0) axis(4, cex=0.5)
  }
  
  if (i == 16) mtext("Latitude (°)", side=2, line=2.2)
  if (i == 32) mtext("Longitude (°)", side=1, line=2.2)
}

par(mar=c(0.5,0.5,0.5,0.5))
plot.new()

par(mar=c(0.1,2,0.1,0.1))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.7, bty="n", title="Salinity (psu)")
dev.off()


#### Prediction ####
grid_data <- read.csv(file="./output/grid_depth_data.csv")
depth_grid_plot <- read.csv("./output/grid_depth_data.csv")
load("./output/temperature_raster_data.RData")
load("./output/salinity_raster_data.RData")

if (FALSE){
  prediction_mean <- list()
  prediction_se <- list()
  for (i in 1:nrow(dates)){
    print(i)
    temp_data <- as.data.frame(cbind(depth_grid_plot$fathom[which(depth_grid_plot$fathom<0)], temperature_raster_data[[i]][which(depth_grid_plot$fathom<0)], salinity_raster_data[[i]][which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)], depth_grid_plot$lon[which(depth_grid_plot$fathom<0)]))
    colnames(temp_data) <- c("end_depth", "Water_Temp_DegC", "Salinity_psu", "end_lat", "end_lon")
    temp_data$end_depth <- -temp_data$end_depth
    prediction_model <- predict(gam_model, newdata <- temp_data, se.fit=T, type="response")
    rm(list="temp_data")
    gc()
    prediction_mean[[i]] <- prediction_model$fit
    prediction_se[[i]] <- prediction_model$se.fit
    rm(list="prediction_model")
    gc()
  }
} ## predict data without separating the statistical area

if(T){
  prediction_mean <- list()
  prediction_se <- list()
  for (i in 1:nrow(dates)){
    print(i)
    temp_data <- as.data.frame(cbind(depth_grid_plot$fathom[which(depth_grid_plot$fathom<0)], temperature_raster_data[[i]][which(depth_grid_plot$fathom<0)], salinity_raster_data[[i]][which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)], depth_grid_plot$lon[which(depth_grid_plot$fathom<0)]))
    colnames(temp_data) <- c("end_depth", "Water_Temp_DegC", "Salinity_psu", "end_lat", "end_lon")
    
    temp_points <- SpatialPoints(cbind(temp_data$end_lon, temp_data$end_lat), proj4string=CRS("+proj=longlat +datum=WGS84")) 
    polygon_points <- over(temp_points, sa511_513)
    
    temp_data$end_depth <- -temp_data$end_depth
    
    mean_val <- c()
    se_val <- c()
    for(j in 1:nrow(temp_data)){
      if (is.na(polygon_points$Id[j])) {
        mean_val[j] <-NA
        se_val[j] <- NA
      }
      else {
        if (polygon_points$Id[j]==511) {
          mean_val[j] <- predict(gam_model_511, newdata <- temp_data[j,], se.fit=T, type="response")$fit
          se_val[j] <- predict(gam_model_511, newdata <- temp_data[j,], se.fit=T, type="response")$se.fit
        }
        if (polygon_points$Id[j]=="512") {
          mean_val[j] <- predict(gam_model_512, newdata <- temp_data[j,], se.fit=T, type="response")$fit
          se_val[j] <- predict(gam_model_512, newdata <- temp_data[j,], se.fit=T, type="response")$se.fit
        }
        if (polygon_points$Id[j]=="513") {
          mean_val[j] <- predict(gam_model_513, newdata <- temp_data[j,], se.fit=T, type="response")$fit
          se_val[j] <- predict(gam_model_513, newdata <- temp_data[j,], se.fit=T, type="response")$se.fit
        }
      }
    }
    
    rm(list="temp_data")
    gc()
    prediction_mean[[i]] <- mean_val
    prediction_se[[i]] <- se_val
    rm(list="prediction_model")
    gc()
  }
} ## predict data with three models separting the statistical area;final choice

if(FALSE){
  prediction_mean <- list()
  prediction_se <- list()
  for (i in 1:nrow(dates)){
    print(i)
    temp_data <- as.data.frame(cbind(depth_grid_plot$fathom[which(depth_grid_plot$fathom<0)], temperature_raster_data[[i]][which(depth_grid_plot$fathom<0)], salinity_raster_data[[i]][which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)], depth_grid_plot$lon[which(depth_grid_plot$fathom<0)]))
    colnames(temp_data) <- c("end_depth", "Water_Temp_DegC", "Salinity_psu", "end_lat", "end_lon")
    
    temp_points <- SpatialPoints(cbind(temp_data$end_lon, temp_data$end_lat), proj4string=CRS("+proj=longlat +datum=WGS84")) 
    polygon_points <- over(temp_points, sa511_513)
    
    temp_data$end_depth <- -temp_data$end_depth
    
    mean_val <- c()
    se_val <- c()
    for(j in 1:nrow(temp_data)){
      if (is.na(polygon_points$Id[j])) {
        mean_val[j] <-NA
        se_val[j] <- NA
      }
      else {
        if (polygon_points$Id[j]==511 | polygon_points$Id[j]==512) {
          mean_val[j] <- predict(gam_model_511512, newdata <- temp_data[j,], se.fit=T, type="response")$fit
          se_val[j] <- predict(gam_model_511512, newdata <- temp_data[j,], se.fit=T, type="response")$se.fit
        }
        
        if (polygon_points$Id[j]=="513") {
          mean_val[j] <- predict(gam_model_513, newdata <- temp_data[j,], se.fit=T, type="response")$fit
          se_val[j] <- predict(gam_model_513, newdata <- temp_data[j,], se.fit=T, type="response")$se.fit
        }
      }
    }
    
    rm(list="temp_data")
    gc()
    prediction_mean[[i]] <- mean_val
    prediction_se[[i]] <- se_val
    rm(list="prediction_model")
    gc()
  }
} ## predict data with two models separting the statistical area

save(prediction_mean, prediction_se, file="./output/prediction6_fall.RData")

#### 1000 predicted population ####
sim_num=1000
load("./output/prediction6_fall.RData")
if (T){
  
  population_1000 <- list()
  
  for(i in 1:length(prediction_mean)){
    print(i)
    temp <- matrix(NA,nrow=length(prediction_mean[[i]]), ncol=sim_num)
    for (j in 1:nrow(temp)){
      temp_data <- rnorm(1500, mean=prediction_mean[[i]][j], sd=prediction_se[[i]][j])
      temp[j,] <- temp_data[which(temp_data>0)][1:sim_num]
    }
    population_1000[[i]] <- temp
  }
  save(population_1000, file="./output/population_1000.RData")
}

#### Load VTS individual lobster information ####
vts_lobster_data <- read.csv("./data/vts/VTS_biodata2006_2017.csv")
vts_trap_data <- read.csv("./data/vts/VTS_traps.csv")
#### VTS abundance index (<2017) ####
start_date <- as.Date(vts_lobster_data$TRIP_START_DATE, "%m/%d/%Y")
vts_lobster_data$month <- as.numeric(format(start_date, format="%m"))
vts_lobster_data$year <- as.numeric(format(start_date, format="%Y"))
for(i in 1:nrow(vts_lobster_data)){
  if(vts_lobster_data$month[i]==9) vts_lobster_data$month[i]=8
}
unique(vts_lobster_data$month)

trip_date <- as.Date(vts_trap_data$TRIP_START_DATE, "%m/%d/%Y")
vts_trap_data$month <- as.numeric(format(trip_date, format="%m"))
vts_trap_data$year <- as.numeric(format(trip_date, format="%Y"))
for(i in 1:nrow(vts_trap_data)){
  if(vts_trap_data$month[i]==9) vts_trap_data$month[i]=8
}
unique(vts_trap_data$month)
## lobster number per trap ##
sublegal_lobsters <- vts_lobster_data[which(vts_lobster_data$SAMPLE_LENGTH < 83 & vts_lobster_data$year < 2017 & (vts_lobster_data$TRAP_TYPE=="V" | vts_lobster_data$TRAP_TYPE=="v")),]

#lobster_per_trap <- aggregate(sublegal_lobsters$SAMPLE_SEQ_NO, by=list(sublegal_lobsters$TRAP_ID), length)

lobster_per_trap <- aggregate(sublegal_lobsters$SAMPLE_LENGTH, by=list(sublegal_lobsters$TRAP_ID, sublegal_lobsters$SET_OVER_DAYS), length)
lobster_per_trap$x <- round(lobster_per_trap$x/lobster_per_trap$Group.2)*3
lobster_per_trap <- as.data.frame(cbind(lobster_per_trap$Group.1, lobster_per_trap$x))
colnames(lobster_per_trap) <- c("Group.1", "x")

for (i in 1:nrow(vts_trap_data)){
  print(i)
  if (vts_trap_data$TRAP_ID[i] %in% lobster_per_trap$Group.1) vts_trap_data$quantity[i] <- lobster_per_trap$x[which(lobster_per_trap$Group.1==vts_trap_data$TRAP_ID[i])]
  else vts_trap_data$quantity[i] <- NA
}

vts_vtrap_data <- vts_trap_data[which(vts_trap_data$year<2017 & (vts_trap_data$TRAP_TYPE=="V" |vts_trap_data$TRAP_TYPE=="v")),]
vts_vtrap_data <- na.omit(vts_vtrap_data)
write.csv(vts_vtrap_data, "./data/vts/vts_vtrap_data_2006_2016.csv")

plot_data <- as.data.frame(cbind(vts_vtrap_data$LONGITUDE_DECIMAL, vts_vtrap_data$LATITUDE_DECIMAL, vts_vtrap_data$quantity))
colnames(plot_data) <- c("Longitude", "Latitude", "Y")


plotvar <- plot_data$Y
nclr=8
plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
class <- classIntervals(plotvar, nclr, style="quantile")
class$brks = round(class$brks, digits=2)
colcode <- findColours(class, plotclr)

start_x <- range(lobster_catch_data$end_lon)[1]
end_x <- range(lobster_catch_data$end_lon)[2]
start_y <- range(lobster_catch_data$end_lat)[1]
end_y <- range(lobster_catch_data$end_lat)[2]
jpeg(filename = paste("./plot/vts_lobster_quantity_map.jpeg", sep=""), width=100, height=50, units = "mm", res = 600)
layout(matrix(c(1,1,1,2), 1, 4, byrow = TRUE))
par(mar=c(4,4,1,1))
plot(plot_data$Longitude, plot_data$Latitude, pch=16, col=colcode, cex=0.3, xlim=c(start_x, end_x), ylim=c(start_y, end_y), axes=F, xlab="Longitude", ylab="Latitude")
map(database = "worldHires", ylim=c(start_y, end_y), xlim=c(start_x, end_x), col = "gray90", fill = TRUE, add = TRUE, lwd=0.1)
box()
degAxis(1)
degAxis(2)
par(mar=c(0.5,0.5,0.5,0.5))
plot.new()
legend("left", legend=c(names(attr(colcode, "table"))), fill=c(attr(colcode, "palette")), cex=0.7, bty="n", title="Lobster Density (#/trap)")
dev.off()

lobster_per_year_sa_depth <- aggregate(vts_vtrap_data$quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), sum)
colnames(lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 

lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), length)$x

lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data$SITE_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata), function(x) length(unique(x)))$x
summary(lobster_per_year_sa_depth)

lobster_per_year_sa_depth$Ave_Lob_Trap <- lobster_per_year_sa_depth$Lob_Quantity/lobster_per_year_sa_depth$Trap_Quantity

area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
rownames(area_per_sa_depth) <- c("511", "512", "513")
## substrat_mean
substrat_mean <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
year_id <- unique(lobster_per_year_sa_depth$Year)
colnames(substrat_mean) <- c("511", "512", "513")
rownames(substrat_mean) <- year_id
for (i in 1:length(year_id)){
  substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
  substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
  substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
}

## substrat_variance
lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data$quantity, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), sum)
colnames(lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 

lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data$TRAP_ID, by=list(vts_vtrap_data$year, vts_vtrap_data$STATAREA, vts_vtrap_data$assigneddepthstrata, vts_vtrap_data$SITE_ID), length)$x

lobster_per_year_sa_depth_site$Quantity_per_Trap <- lobster_per_year_sa_depth_site$Lob_Quantity/lobster_per_year_sa_depth_site$Trap_Quantity

lobster_per_year_sa_depth_site=merge(lobster_per_year_sa_depth_site, lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))

lobster_per_year_sa_depth_site$Sub <- (lobster_per_year_sa_depth_site$Ave_Lob_Trap - lobster_per_year_sa_depth_site$Quantity_per_Trap)^2

individual_value <- aggregate(lobster_per_year_sa_depth_site$Sub, by=list(lobster_per_year_sa_depth_site$Year, lobster_per_year_sa_depth_site$SA, lobster_per_year_sa_depth_site$Depth), sum)
colnames(individual_value) <- c("Year", "SA", "Depth", "x")
individual_value$individual_value <- individual_value$x/aggregate(lobster_per_year_sa_depth_site$Site, by=list(lobster_per_year_sa_depth_site$Year, lobster_per_year_sa_depth_site$SA, lobster_per_year_sa_depth_site$Depth), length)$x

substrat_var <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
colnames(substrat_var) <- c("511", "512", "513")
rownames(substrat_var) <- year_id

for (i in 1:length(year_id)){
  substrat_var[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
  
  substrat_var[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
  
  substrat_var[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
}

save(substrat_mean, substrat_var, file="./output/vts_lobster_substrat_mean_real_2006_2016.RData")
jpeg(filename = "./plot/vts_lobster_substrat_mean_real.jpeg", width=150, height=50, units = "mm", res = 600)
par(mfrow=c(1,3))
ylim_min <- min(substrat_mean-substrat_var)
ylim_max <- max(substrat_mean+substrat_var)

plot(year_id, substrat_mean[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"513"]+substrat_var[,"513"], rev(substrat_mean[,"513"]-substrat_var[,"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "513", bty="n")

plot(year_id, substrat_mean[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"512"]+substrat_var[,"512"], rev(substrat_mean[,"512"]-substrat_var[,"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "512", bty="n")

plot(year_id, substrat_mean[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"511"]+substrat_var[,"511"], rev(substrat_mean[,"511"]-substrat_var[,"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "511", bty="n")

dev.off()
#### simulated samples ####
depth_grid_plot <- read.csv("./output/grid_depth_data.csv")
prediction_points <- SpatialPoints(cbind(depth_grid_plot$lon[which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)]), proj4string=CRS("+proj=longlat +datum=WGS84")) 
vts_points <- SpatialPoints(cbind(vts_vtrap_data$LONGITUDE_DECIMAL, vts_vtrap_data$LATITUDE_DECIMAL), proj4string=CRS("+proj=longlat +datum=WGS84"))

if(T){
  nearest_pred_points <- c()
  # for (i in 1:length(vts_points)){
  #   print(i)
  #   nearest_pred_points[i] <- which.min(gDistance(vts_points[i], prediction_points, byid=T))
  #   gc()
  # }
  nearest_pred_points <- sapply(1:length(vts_points), function(x) which.min(gDistance(vts_points[x], prediction_points, byid=T)))
  save(nearest_pred_points, file="./data/vts/vts_nearest_prediction_point.RData")
}

if(T){
  load("./output/population_1000.RData")
  load("./data/vts/vts_nearest_prediction_point.RData")
  #load("./data/vts/vts_nearest_prediction_point.RData")
  #load("./output/s5_prediction_fall_sa.RData")
  #load("./output/s5_prediction_spring6_fall9_sa.RData")
  dates$id <- 1:nrow(dates)
  simulated_vts_samples <- matrix(NA, nrow = length(vts_points), ncol=sim_num)
  for (i in 1:length(vts_points)){
    print(i)
    if(vts_vtrap_data[i,"year"]==2017) simulated_vts_samples[i,] <- rep(NA, sim_num)
    #if(vts_vtrap_data[i,"year"]==2017 | vts_vtrap_data[i,"year"]==2014) simulated_vts_samples[i,] <- rep(NA, sim_num)
    else {
      if(vts_vtrap_data[i,"month"]==9) pop_time_id <- dates$id[which(dates$y==vts_vtrap_data[i,"year"] & dates$m == 8)]
      else {
        pop_time_id <- dates$id[which(dates$y==vts_vtrap_data[i,"year"] & dates$m == vts_vtrap_data[i,"month"])]
      }
      temp_value <- population_1000[[pop_time_id]][nearest_pred_points[i],]
      if (any(is.na(temp_value))) simulated_vts_samples[i,]<- rep(NA, sim_num)
      else simulated_vts_samples[i,]<- temp_value
    }
  }
  summary(simulated_vts_samples)
  save(simulated_vts_samples, file="./output/simulated_vts_samples_1000.RData")
  #vts_vtrap_data$sim_quantity <- simulated_vts_samples
  #summary(vts_vtrap_data$sim_quantity)
  #save(vts_vtrap_data, file="./output/vts_simulated_samples.RData")
}

#### Catchability set up ####
if (TRUE){
  
  load("./output/temperature_raster_data.RData")
  
  mean_temperature <- dates
  temp<- c()
  for(i in 1:length(temperature_raster_data)){
    temp[i]<-mean(temperature_raster_data[[i]])
  }
  mean_temperature$temperature <- round(temp, digits = 2)
  temperature_point <- c()
  for(i in 1:nrow(vts_vtrap_data)){
    if(vts_vtrap_data$month[i] == 9) temperature_point[i] <- mean_temperature$temperature[which(mean_temperature$y==vts_vtrap_data$year[i] & mean_temperature$m==8)]
    else temperature_point[i] <- mean_temperature$temperature[which(mean_temperature$y==vts_vtrap_data$year[i] & mean_temperature$m==vts_vtrap_data$month[i])]
    
  }
  vts_vtrap_data$temperature <- temperature_point
  
  load("./output/simulated_vts_samples_1000.RData")
  q_mean <- matrix(NA, nrow = nrow(vts_vtrap_data), ncol=sim_num)
  q_sd <-  matrix(NA, nrow = nrow(vts_vtrap_data), ncol=sim_num)
  for(sim in 1:sim_num){
    print(sim)
    vts_vtrap_data$catchability<- vts_vtrap_data$quantity/simulated_vts_samples[,sim]
    #vts_vtrap_data[which(vts_vtrap_data$catchability>1),]
    temp_data <- na.omit(vts_vtrap_data)
    temp_data <- temp_data[which(temp_data$catchability<=1),]
    #temp_data$month[(temp_data$month==9)] <- 8
    catchability_mean <- aggregate(temp_data$catchability, by=list(temp_data$temperature, temp_data$assigneddepthstrata), mean)
    catchability_sd <- aggregate(temp_data$catchability, by=list(temp_data$temperature,  temp_data$assigneddepthstrata), sd)
    colnames(catchability_mean) <- c("temperature", "assigneddepthstrata", "mean")
    colnames(catchability_sd) <- c("temperature", "assigneddepthstrata", "sd")
    
    catchability_data <- vts_vtrap_data
    
    #catchability_data$month[(catchability_data$month==9)] <- 8
    
    merge_data <- merge(catchability_data, catchability_mean, by=c("temperature","assigneddepthstrata"))
    q_mean[,sim] <- merge_data[match(catchability_data$TRAP_ID,merge_data$TRAP_ID),]$mean
    merge_data <- merge(catchability_data, catchability_sd, by=c("temperature","assigneddepthstrata"))
    q_sd[,sim] <- merge_data[match(catchability_data$TRAP_ID,merge_data$TRAP_ID),]$sd
    if (sim==1) {
      jpeg(filename = "./plot/catchability1_year_month_strata.jpeg", width=160, height=150, units = "mm", res = 600)
      par(mfrow=c(2,2))
      boxplot(catchability_mean$mean~catchability_mean$temperature, xlab="Temperature", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      #boxplot(catchability_mean$mean~catchability_mean$month, xlab="Month", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      #boxplot(catchability_mean$mean~catchability_mean$STATAREA, xlab="Statistical Area", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      boxplot(catchability_mean$mean~catchability_mean$assigneddepthstrata, xlab="Depth Strata", ylab="Catchability", ylim=c(0,0.3), xlim=c(1.5,4.5), pch=16, cex=0.5)
      dev.off()
    }
    if (sim==500) {
      jpeg(filename = "./plot/catchability500_year_month_strata.jpeg", width=160, height=150, units = "mm", res = 600)
      par(mfrow=c(2,2))
      boxplot(catchability_mean$mean~catchability_mean$temperature, xlab="Temperature", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      #boxplot(catchability_mean$mean~catchability_mean$month, xlab="Month", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      #boxplot(catchability_mean$mean~catchability_mean$STATAREA, xlab="Statistical Area", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      boxplot(catchability_mean$mean~catchability_mean$assigneddepthstrata, xlab="Depth Strata", ylab="Catchability", ylim=c(0,0.3), xlim=c(1.5,4.5), pch=16, cex=0.5)
      dev.off()
    }
    if (sim==1000) {
      jpeg(filename = "./plot/catchability1000_year_month_strata.jpeg", width=160, height=150, units = "mm", res = 600)
      par(mfrow=c(2,2))
      boxplot(catchability_mean$mean~catchability_mean$temperature, xlab="Temperature", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      #boxplot(catchability_mean$mean~catchability_mean$month, xlab="Month", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      #boxplot(catchability_mean$mean~catchability_mean$STATAREA, xlab="Statistical Area", ylab="Catchability", ylim=c(0,0.3), pch=16, cex=0.5)
      boxplot(catchability_mean$mean~catchability_mean$assigneddepthstrata, xlab="Depth Strata", ylab="Catchability", ylim=c(0,0.3), xlim=c(1.5,4.5), pch=16, cex=0.5)
      dev.off()
    }
  }
  summary(q_mean)
  summary(q_sd)
  save(q_mean, q_sd, file="./output/catchability_mean_sd_1000.RData")
  
  if(T){
    simulated_vts_samples_q <- matrix(NA, nrow=nrow(simulated_vts_samples), ncol=sim_num)
    
    for(i in 1:nrow(simulated_vts_samples)){
      print(i)
      if(is.na(simulated_vts_samples[i,])) simulated_vts_samples_q[i,] <- NA
      else{
        for (sim in 1:sim_num){
          simulated_vts_samples_q[i,sim] <- simulated_vts_samples[i,sim]*rnorm(1, mean=q_mean[i,sim], sd=q_sd[i,sim])
        }
        #simulated_vts_samples_q[i,] <- simulated_vts_samples[i,]*q_mean[i,]
        
        #if(simulated_vts_samples_q[i,]<0) simulated_vts_samples_q[i,]<-simulated_vts_samples[i,]*catchability_data$q_mean[i]
      }
    }
    
    save(simulated_vts_samples_q, file="./output/vts_simulated_samples_q.RData")
    
  }
  
} #### catchability with FVCOM temperature and depth
#### Temperature increasing rate (2006-2016) ####
year_id <- 2006:2016
jun_id <- seq(1, length(temperature_raster_data), by=3)
jul_id <- seq(2, length(temperature_raster_data), by=3)
aug_id <- seq(3, length(temperature_raster_data), by=3)

jun_annual_mean <- c()
for (i in 1:length(jun_id)){
  jun_annual_mean[i] <- mean(temperature_raster_data[[jun_id[i]]], na.rm = T)
}

jul_annual_mean <- c()
for (i in 1:length(jul_id)){
  jul_annual_mean[i] <- mean(temperature_raster_data[[jul_id[i]]], na.rm = T)
}

aug_annual_mean <- c()
for (i in 1:length(aug_id)){
  aug_annual_mean[i] <- mean(temperature_raster_data[[aug_id[i]]], na.rm = T)
}

jun_model <- lm(jun_annual_mean~year_id) #0.03903
jul_model <- lm(jul_annual_mean~year_id) #0.02211
aug_model <- lm(aug_annual_mean~year_id) #0.05453 

jpeg(filename = paste("./plot/fvcom_temperature_annual_Increase.jpeg", sep=""), width=120, height=100, units = "mm", res = 600)
plot(year_id, jun_annual_mean, pch=16, col="deepskyblue3", ylim=c(6,14), xlab="Year", ylab="Temperature (°C)")
points(year_id, jul_annual_mean, pch=16, col="coral3")
points(year_id, aug_annual_mean, pch=16, col="black")
abline(a=jun_model$coefficients[1], b=jun_model$coefficients[2], col="deepskyblue3")
abline(a=jul_model$coefficients[1], b=jul_model$coefficients[2], col="coral3")
abline(a=aug_model$coefficients[1], b=aug_model$coefficients[2], col="black")
legend("topleft", c("Jun", "Jul", "Aug"), lty=c(1,1,1), col=c("deepskyblue3", "coral3", "black"), bty="n", cex=0.8)
dev.off()

#### Predict temperature for 10 years ####
num_year <- 10
#num_year <- 20
increasing_rate <- 1
#increasing_rate <- 1.02 #2%
#increasing_rate <- 1.06 #6%
prediction_year <- rep(2017:(2017+num_year-1), each=3)
prediction_month <- rep(6:8, times=num_year)
prediction_id <- as.data.frame(cbind(prediction_year, prediction_month))
prediction_id$id <- rep(1:num_year, each=3)
temperature_prediction_10year <- list()

for (i in 1:nrow(prediction_id)){
  if (prediction_id$prediction_month[i]==6) temperature_prediction_10year[[i]]<-temperature_raster_data[[28]]*((1+jun_model$coefficients[2]*1)^prediction_id$id[i])
  if (prediction_id$prediction_month[i]==7) temperature_prediction_10year[[i]]<-temperature_raster_data[[29]]*((1+jul_model$coefficients[2]*1)^prediction_id$id[i])
  if (prediction_id$prediction_month[i]==8) temperature_prediction_10year[[i]]<-temperature_raster_data[[30]]*((1+aug_model$coefficients[2]*1)^prediction_id$id[i])
}
#### Predict lobster abundance for 10 years ####
statistical_areas <- readShapePoly("./data/gis/Statistical_Areas_2010.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
sa511_513 <- statistical_areas[which(statistical_areas@data$Id %in% c(511:513)),]
grid_data <- read.csv(file="./output/grid_depth_data.csv")
depth_grid_plot <- read.csv("./output/grid_depth_data.csv")
load("./output/salinity_raster_data.RData")
load("./output/gam_fit.RData")
if(T){
  prediction_mean <- list()
  prediction_se <- list()
  for (i in 1:nrow(prediction_id)){
    print(i)
    if (prediction_id$prediction_month[i]==6) temp_data <- as.data.frame(cbind(depth_grid_plot$fathom[which(depth_grid_plot$fathom<0)], temperature_prediction_10year[[i]][which(depth_grid_plot$fathom<0)], salinity_raster_data[[28]][which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)], depth_grid_plot$lon[which(depth_grid_plot$fathom<0)]))
    if (prediction_id$prediction_month[i]==7) temp_data <- as.data.frame(cbind(depth_grid_plot$fathom[which(depth_grid_plot$fathom<0)], temperature_prediction_10year[[i]][which(depth_grid_plot$fathom<0)], salinity_raster_data[[29]][which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)], depth_grid_plot$lon[which(depth_grid_plot$fathom<0)]))
    if (prediction_id$prediction_month[i]==8) temp_data <- as.data.frame(cbind(depth_grid_plot$fathom[which(depth_grid_plot$fathom<0)], temperature_prediction_10year[[i]][which(depth_grid_plot$fathom<0)], salinity_raster_data[[30]][which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)], depth_grid_plot$lon[which(depth_grid_plot$fathom<0)]))
    colnames(temp_data) <- c("end_depth", "Water_Temp_DegC", "Salinity_psu", "end_lat", "end_lon")
    
    temp_points <- SpatialPoints(cbind(temp_data$end_lon, temp_data$end_lat), proj4string=CRS("+proj=longlat +datum=WGS84")) 
    polygon_points <- over(temp_points, sa511_513)
    
    temp_data$end_depth <- -temp_data$end_depth
    
    mean_val <- c()
    se_val <- c()
    for(j in 1:nrow(temp_data)){
      if (is.na(polygon_points$Id[j])) {
        mean_val[j] <-NA
        se_val[j] <- NA
      }
      else {
        if (polygon_points$Id[j]=="511") {
          mean_val[j] <- predict(gam_model_511, newdata <- temp_data[j,], se.fit=T, type="response")$fit
          se_val[j] <- predict(gam_model_511, newdata <- temp_data[j,], se.fit=T, type="response")$se.fit
        }
        if (polygon_points$Id[j]=="512") {
          mean_val[j] <- predict(gam_model_512, newdata <- temp_data[j,], se.fit=T, type="response")$fit
          se_val[j] <- predict(gam_model_512, newdata <- temp_data[j,], se.fit=T, type="response")$se.fit
        }
        if (polygon_points$Id[j]=="513") {
          mean_val[j] <- predict(gam_model_513, newdata <- temp_data[j,], se.fit=T, type="response")$fit
          se_val[j] <- predict(gam_model_513, newdata <- temp_data[j,], se.fit=T, type="response")$se.fit
        }
      }
    }
    
    rm(list="temp_data")
    gc()
    prediction_mean[[i]] <- mean_val
    prediction_se[[i]] <- se_val
    rm(list="prediction_model")
    gc()
  }
}
#save(prediction_mean, prediction_se, file="./output/prediction_10year_t1.04.RData")
save(prediction_mean, prediction_se, file=paste("./output/prediction_", num_year, "year_t",increasing_rate, ".RData", sep=""))
load(paste("./output/prediction_", num_year, "year_t",increasing_rate, ".RData", sep=""))
#load("./output/prediction_10year_t1.04.RData")

sim_num=1000
if (T){
  
  population_1000 <- list()
  
  for(i in 1:length(prediction_mean)){
    print(i)
    temp <- matrix(NA,nrow=length(prediction_mean[[i]]), ncol=sim_num)
    for (j in 1:nrow(temp)){
      temp_data <- rnorm(1500, mean=prediction_mean[[i]][j], sd=prediction_se[[i]][j])
      temp[j,] <- temp_data[which(temp_data>0)][1:sim_num]
    }
    population_1000[[i]] <- temp
  }
  #save(population_1000, file="./output/population_1000_future.RData")
  save(population_1000, file=paste("./output/population_1000_future",num_year, "year_t",increasing_rate, ".RData", sep=""))
}


#### Population at 2017 stations ####
vts_lobster_data <- read.csv("./data/vts/VTS_biodata2006_2017.csv")
vts_trap_data <- read.csv("./data/vts/VTS_traps.csv")
#save(vts_lobster_data, file="./output/vts_trap_data.RData")
#load("./output/vts_trap_data.RData")

#summary(vts_lobster_data)
start_date <- as.Date(vts_lobster_data$TRIP_START_DATE, "%m/%d/%Y")
vts_lobster_data$month <- as.numeric(format(start_date, format="%m"))
vts_lobster_data$year <- as.numeric(format(start_date, format="%Y"))
for(i in 1:nrow(vts_lobster_data)){
  if(vts_lobster_data$month[i]==9) vts_lobster_data$month[i]=8
}
unique(vts_lobster_data$month)

trip_date <- as.Date(vts_trap_data$TRIP_START_DATE, "%m/%d/%Y")
vts_trap_data$month <- as.numeric(format(trip_date, format="%m"))
vts_trap_data$year <- as.numeric(format(trip_date, format="%Y"))
for(i in 1:nrow(vts_trap_data)){
  if(vts_trap_data$month[i]==9) vts_trap_data$month[i]=8
}
unique(vts_trap_data$month)


sublegal_lobsters <- vts_lobster_data[which(vts_lobster_data$SAMPLE_LENGTH < 83 & vts_lobster_data$year == 2017 & (vts_lobster_data$TRAP_TYPE=="V" | vts_lobster_data$TRAP_TYPE=="v")),]

lobster_per_trap <- aggregate(sublegal_lobsters$SAMPLE_LENGTH, by=list(sublegal_lobsters$TRAP_ID, sublegal_lobsters$SET_OVER_DAYS), length)
lobster_per_trap$x <- round(lobster_per_trap$x/lobster_per_trap$Group.2)*3
lobster_per_trap <- as.data.frame(cbind(lobster_per_trap$Group.1, lobster_per_trap$x))
colnames(lobster_per_trap) <- c("Group.1", "x")

for (i in 1:nrow(vts_trap_data)){
  print(i)
  if (vts_trap_data$TRAP_ID[i] %in% lobster_per_trap$Group.1) vts_trap_data$quantity[i] <- lobster_per_trap$x[which(lobster_per_trap$Group.1==vts_trap_data$TRAP_ID[i])]
  else vts_trap_data$quantity[i] <- NA
}
summary(vts_trap_data)

set.seed(9564)
year_sample <- sample(2006:2014, num_year-1, replace = F)
#year_sample <- sapply(1:(num_year-1), function(x) sample(2006:2014, 1, replace = T))
year_sample <- c(2017, year_sample)

future_vts_vtrap_data <- vts_trap_data[which(vts_trap_data$year %in% year_sample &(vts_trap_data$TRAP_TYPE=="V" |vts_trap_data$TRAP_TYPE=="v")),]
#future_vts_vtrap_data <- vts_trap_data[which(vts_trap_data$month<9 & vts_trap_data$year %in% year_sample &(vts_trap_data$TRAP_TYPE=="V" |vts_trap_data$TRAP_TYPE=="v")),]
future_vts_vtrap_data <- na.omit(future_vts_vtrap_data[,1:(ncol(future_vts_vtrap_data)-1)])
write.csv(future_vts_vtrap_data, "./data/vts/future_vts_vtrap_data.csv")

depth_grid_plot <- read.csv("./output/grid_depth_data.csv")
prediction_points <- SpatialPoints(cbind(depth_grid_plot$lon[which(depth_grid_plot$fathom<0)], depth_grid_plot$lat[which(depth_grid_plot$fathom<0)]), proj4string=CRS("+proj=longlat +datum=WGS84")) 
vts_points <- SpatialPoints(cbind(future_vts_vtrap_data$LONGITUDE_DECIMAL, future_vts_vtrap_data$LATITUDE_DECIMAL), proj4string=CRS("+proj=longlat +datum=WGS84"))

new_data <- future_vts_vtrap_data
for (i in 1:(num_year-1)){
  print(i)
  new_data$year[which(new_data$year==year_sample[i+1])]=2017+i
}
# for (i in 1:(num_year-1)){
#   print(i)
#   temp <- na.omit(vts_trap_data[which(vts_trap_data$year == year_sample[i] &(vts_trap_data$TRAP_TYPE=="V" |vts_trap_data$TRAP_TYPE=="v")),1:(ncol(vts_trap_data)-1)])
#   temp$year <- 2017+i
#   temp$quantity <- future_vts_vtrap_data$quantity #quantity is 2017 true quantity
#   new_data <-as.data.frame(rbind(new_data, temp))
# }
# save(new_data, file="./output/new_data.RData")

if(T){
  nearest_pred_points <- c()
  nearest_pred_points <- sapply(1:length(vts_points), function(x) which.min(gDistance(vts_points[x], prediction_points, byid=T)))
  # for (i in 1:length(vts_points)){
  #   print(i)
  #   nearest_pred_points[i] <- which.min(gDistance(vts_points[i], prediction_points, byid=T))
  #   gc()
  # }
  # nearest_pred_points_10yr <- rep(nearest_pred_points, times=num_year)
  nearest_pred_points_10yr <- nearest_pred_points
  save(nearest_pred_points_10yr, file="./data/vts/future_vts_nearest_prediction_point.RData")
}

load("./data/vts/future_vts_nearest_prediction_point.RData")
load(paste("./output/population_1000_future",num_year, "year_t",increasing_rate, ".RData", sep=""))
#load("./output/population_1000_future.RData")
simulated_vts_samples_prediction <- matrix(NA, nrow = nrow(new_data), ncol=sim_num)
for (i in 1:nrow(new_data)){
  print(i)
  time_id <- prediction_id$id[which(prediction_id$prediction_year==new_data[i,"year"] & prediction_id$prediction_month == new_data[i,"month"])]
  if (length(time_id)<1) simulated_vts_samples_prediction[i,]<- rep(NA, sim_num) else{
    temp_value <- population_1000[[time_id]][nearest_pred_points_10yr[i],]
    if (is.na(temp_value)) simulated_vts_samples_prediction[i,]<- rep(NA, sim_num)
    else simulated_vts_samples_prediction[i,]<- temp_value
  }
}
summary(simulated_vts_samples_prediction)

save(simulated_vts_samples_prediction, file=paste("./output/future_vts_simulated_samples_", increasing_rate, ".RData", sep=""))  



#### Predict catchability for 10 years ####
mean_temperature <- prediction_id
temp<- c()
for(i in 1:length(temperature_prediction_10year)){
  temp[i]<-mean(temperature_prediction_10year[[i]])
}
mean_temperature$temperature <- round(temp, digits = 2)

temperature_point <- c()
for(i in 1:nrow(new_data)){
  temperature_point[i] <- mean_temperature$temperature[which(mean_temperature$prediction_year==new_data$year[i] & mean_temperature$prediction_month==new_data$month[i])]
  
}

load("./output/catchability_mean_sd_1000.RData")
new_data$temperature <- temperature_point
temperature_point <- c()
for(i in 1:nrow(new_data)){
  temp <- catchability_mean$temperature[which(catchability_mean$assigneddepthstrata == new_data$assigneddepthstrata[i])]
  temperature_point[i] <- temp[which.min(abs(new_data$temperature[i]-temp))]
}
new_data$temperature <- temperature_point

temp_data <- new_data
merge_data <- merge(temp_data, catchability_mean, by=c("temperature","assigneddepthstrata"))
for (i in 1:nrow(new_data)){
  new_data$q_mean[i] <- merge_data$mean[which(merge_data$year==new_data$year[i] & merge_data$TRAP_ID==new_data$TRAP_ID[i])]
}

merge_data <- merge(temp_data, catchability_sd, by=c("temperature","assigneddepthstrata"))
for (i in 1:nrow(new_data)){
  new_data$q_sd[i] <- merge_data$sd[which(merge_data$year==new_data$year[i] & merge_data$TRAP_ID==new_data$TRAP_ID[i])]
}
#new_data$quantity <-simulated_vts_samples
#load("./output/simulated_vts_samples_1000.RData")
load(paste("./output/future_vts_simulated_samples_", increasing_rate, ".RData", sep=""))
if(T){
  future_simulated_vts_samples_q <- matrix(NA, nrow = nrow(new_data), ncol=sim_num)
  for(i in 1:nrow(simulated_vts_samples_prediction)){
    print(i)
    if(is.na(simulated_vts_samples_prediction[i,])) future_simulated_vts_samples_q[i,] <- rep(NA, sim_num)
    else{
      future_simulated_vts_samples_q[i,] <- simulated_vts_samples_prediction[i,]*rnorm(1, mean=new_data$q_mean[i], sd=new_data$q_sd[i])
      if(future_simulated_vts_samples_q[i,]<0) future_simulated_vts_samples_q[i,]<-simulated_vts_samples_prediction[i,]*new_data$q_mean[i]
    }
  }
  
  save(future_simulated_vts_samples_q, file="./output/future_vts_simulated_samples_q.RData")
}
#### calculate vts sim abundance q index ####
load("./output/future_vts_simulated_samples_q.RData") 
future_sim_substrat_mean_list_q <- list()
new_data_nona <- new_data[-which(is.na(future_simulated_vts_samples_q[,1])),]
future_simulated_vts_samples_q_nona <- future_simulated_vts_samples_q[-which(is.na(future_simulated_vts_samples_q[,1])),]

for (sim in 1:sim_num){
  lobster_per_year_sa_depth <- aggregate(future_simulated_vts_samples_q_nona[,sim], by=list(new_data_nona$year, new_data_nona$STATAREA, new_data_nona$assigneddepthstrata), sum)
  colnames(lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  
  lobster_per_year_sa_depth$Trap_Quantity <- aggregate(new_data_nona$TRAP_ID, by=list(new_data_nona$year, new_data_nona$STATAREA, new_data_nona$assigneddepthstrata), length)$x
  
  lobster_per_year_sa_depth$Site_Quantity <- aggregate(new_data_nona$SITE_ID, by=list(new_data_nona$year, new_data_nona$STATAREA, new_data_nona$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(lobster_per_year_sa_depth)
  
  lobster_per_year_sa_depth$Ave_Lob_Trap <- lobster_per_year_sa_depth$Lob_Quantity/lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  ## substrat_mean
  future_sim_substrat_mean <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(lobster_per_year_sa_depth$Year)
  colnames(future_sim_substrat_mean) <- c("511", "512", "513")
  rownames(future_sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    future_sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
    
    future_sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
    
    future_sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
    
  }
  
  future_sim_substrat_mean_list_q[[sim]]<-future_sim_substrat_mean
}

mean_future_sim_substra_mean_q <- matrix(NA, nrow=length(year_id), ncol=3)
sd_future_sim_substra_mean_q <- matrix(NA, nrow=length(year_id), ncol=3)
for(i in 1:length(year_id)){
  for(j in 1:3){
    temp <- c()
    for(sim in 1:sim_num){
      temp[sim] <- future_sim_substrat_mean_list_q[[sim]][i,j]
    }
    mean_future_sim_substra_mean_q[i,j]<-mean(temp)
    sd_future_sim_substra_mean_q[i,j]<-sd(temp)
  }
}
colnames(mean_future_sim_substra_mean_q) <- c("511", "512", "513")
rownames(mean_future_sim_substra_mean_q) <- year_id
colnames(sd_future_sim_substra_mean_q) <- c("511", "512", "513")
rownames(sd_future_sim_substra_mean_q) <- year_id

sim_future_substrat_mean_upper_q <- mean_future_sim_substra_mean_q+sd_future_sim_substra_mean_q
sim_future_substrat_mean_lower_q <- mean_future_sim_substra_mean_q-sd_future_sim_substra_mean_q

ylim=range(sim_future_substrat_mean_upper_q, sim_future_substrat_mean_lower_q)
jpeg(filename = "./plot/future_vts_pop_mean_1000_q_2017_2026.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id, mean_future_sim_substra_mean_q[1:length(year_id),"513"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_future_substrat_mean_upper_q[1:length(year_id),"513"], rev(sim_future_substrat_mean_lower_q[1:length(year_id),"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "513", bty="n",  cex=0.8)


plot(year_id, mean_future_sim_substra_mean_q[1:length(year_id),"512"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_future_substrat_mean_upper_q[1:length(year_id),"512"], rev(sim_future_substrat_mean_lower_q[1:length(year_id),"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "512", bty="n",  cex=0.8)


plot(year_id, mean_future_sim_substra_mean_q[1:length(year_id),"511"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_future_substrat_mean_upper_q[1:length(year_id),"511"], rev(sim_future_substrat_mean_lower_q[1:length(year_id),"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "511", bty="n",  cex=0.8)
dev.off()

#### calculate population index ####
#load("./output/population_1000_future.RData")
load(paste("./output/population_1000_future",num_year, "year_t",increasing_rate, ".RData", sep=""))
polygon_points <- over(prediction_points, sa511_513)
summary(polygon_points$Id)

population_511_month_index <- matrix(NA, nrow=length(population_1000), ncol=sim_num)
population_512_month_index <- matrix(NA, nrow=length(population_1000), ncol=sim_num)
population_513_month_index <- matrix(NA, nrow=length(population_1000), ncol=sim_num)

for (i in 1:length(population_1000)){
  population_511_month_index[i,] <- apply(population_1000[[i]][which(polygon_points$Id==511),], 2, sum, na.rm = T)
  population_512_month_index[i,] <- apply(population_1000[[i]][which(polygon_points$Id==512),],2, sum, na.rm = T)
  population_513_month_index[i,] <- apply(population_1000[[i]][which(polygon_points$Id==513),],2, sum, na.rm = T)
}


population_511_year_index_sim <- matrix(NA, nrow=length(year_id), ncol=sim_num)
population_512_year_index_sim <- matrix(NA, nrow=length(year_id), ncol=sim_num)
population_513_year_index_sim <- matrix(NA, nrow=length(year_id), ncol=sim_num)
for (sim in 1:sim_num){
  population_511_year_index_sim[,sim] <- rowMeans(matrix(population_511_month_index[,sim], ncol=3, byrow = T))
  population_512_year_index_sim[,sim] <- rowMeans(matrix(population_512_month_index[,sim], ncol=3, byrow = T))
  population_513_year_index_sim[,sim] <- rowMeans(matrix(population_513_month_index[,sim], ncol=3, byrow = T))
}

population_511_year_index <- apply(population_511_year_index_sim,1,mean)
population_512_year_index <- apply(population_512_year_index_sim,1,mean)
population_513_year_index <- apply(population_513_year_index_sim,1,mean)

population_index <- cbind(population_511_year_index, population_512_year_index, population_513_year_index)
colnames(population_index) <- c("511", "512", "513")
rownames(population_index) <- year_id

population_511_year_sd <- apply(population_511_year_index_sim,1,sd)
population_512_year_sd <- apply(population_512_year_index_sim,1,sd)
population_513_year_sd <- apply(population_513_year_index_sim,1,sd)

population_sd <- cbind(population_511_year_sd, population_512_year_sd, population_513_year_sd)
colnames(population_sd) <- c("511", "512", "513")
rownames(population_sd) <- year_id

population_upper <- population_index+population_sd
population_lower <- population_index-population_sd
ylim=range(population_upper[as.character(year_id),], population_lower[as.character(year_id),])

jpeg(filename = "./plot/future_pop_mean_1000_2017_2026.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
#plot(year_id, population_index[1:length(year_id),"513"], pch=16, ylim=ylim_513, type="o", col="deepskyblue3", lty=1, xlab="Year", ylab="Abundance Index")
plot(year_id, population_index[as.character(year_id),"513"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(population_upper[as.character(year_id),"513"], rev(population_lower[as.character(year_id),"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "513", bty="n",  cex=0.8)

#plot(year_id, population_index[1:length(year_id),"512"], pch=16, ylim=ylim_512, type="o", col="deepskyblue3", lty=1, xlab="Year", ylab="Abundance Index")
plot(year_id, population_index[as.character(year_id),"512"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(population_upper[as.character(year_id),"512"], rev(population_lower[as.character(year_id),"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "512", bty="n",  cex=0.8)

#plot(year_id, population_index[1:length(year_id),"511"], pch=16, ylim=ylim_511, type="o", col="deepskyblue3", lty=1, xlab="Year", ylab="Abundance Index")
plot(year_id, population_index[as.character(year_id),"511"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(population_upper[as.character(year_id),"511"], rev(population_lower[as.character(year_id),"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "511", bty="n",  cex=0.8)
dev.off()

#### calculate vts sim abundance index ####
#load("./output/future_vts_simulated_samples.RData")
load(paste("./output/future_vts_simulated_samples_", increasing_rate, ".RData", sep="")) #simulated_vts_samples

future_sim_substrat_mean_list <- list()
new_data_nona <- new_data[-which(is.na(future_simulated_vts_samples_q[,1])),]
future_simulated_vts_samples_nona <- simulated_vts_samples_prediction[-which(is.na(future_simulated_vts_samples_q[,1])),]

for (sim in 1:sim_num){
  lobster_per_year_sa_depth <- aggregate(future_simulated_vts_samples_nona[,sim], by=list(new_data_nona$year, new_data_nona$STATAREA, new_data_nona$assigneddepthstrata), sum)
  colnames(lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 
  
  lobster_per_year_sa_depth$Trap_Quantity <- aggregate(new_data_nona$TRAP_ID, by=list(new_data_nona$year, new_data_nona$STATAREA, new_data_nona$assigneddepthstrata), length)$x
  
  lobster_per_year_sa_depth$Site_Quantity <- aggregate(new_data_nona$SITE_ID, by=list(new_data_nona$year, new_data_nona$STATAREA, new_data_nona$assigneddepthstrata), function(x) length(unique(x)))$x
  summary(lobster_per_year_sa_depth)
  
  lobster_per_year_sa_depth$Ave_Lob_Trap <- lobster_per_year_sa_depth$Lob_Quantity/lobster_per_year_sa_depth$Trap_Quantity
  
  area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
  colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
  rownames(area_per_sa_depth) <- c("511", "512", "513")
  ## substrat_mean
  future_sim_substrat_mean <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
  year_id <- unique(lobster_per_year_sa_depth$Year)
  colnames(future_sim_substrat_mean) <- c("511", "512", "513")
  rownames(future_sim_substrat_mean) <- year_id
  for (i in 1:length(year_id)){
    future_sim_substrat_mean[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
    
    future_sim_substrat_mean[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
    
    future_sim_substrat_mean[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
    
  }
  
  future_sim_substrat_mean_list[[sim]]<-future_sim_substrat_mean
}

mean_future_sim_substra_mean <- matrix(NA, nrow=length(year_id), ncol=3)
sd_future_sim_substra_mean <- matrix(NA, nrow=length(year_id), ncol=3)
for(i in 1:length(year_id)){
  for(j in 1:3){
    temp <- c()
    for(sim in 1:sim_num){
      temp[sim] <- future_sim_substrat_mean_list[[sim]][i,j]
    }
    mean_future_sim_substra_mean[i,j]<-mean(temp)
    sd_future_sim_substra_mean[i,j]<-sd(temp)
  }
}
colnames(mean_future_sim_substra_mean) <- c("511", "512", "513")
rownames(mean_future_sim_substra_mean) <- year_id
colnames(sd_future_sim_substra_mean) <- c("511", "512", "513")
rownames(sd_future_sim_substra_mean) <- year_id

sim_future_substrat_mean_upper <- mean_future_sim_substra_mean+sd_future_sim_substra_mean
sim_future_substrat_mean_lower <- mean_future_sim_substra_mean-sd_future_sim_substra_mean

ylim=range(sim_future_substrat_mean_upper, sim_future_substrat_mean_lower)
jpeg(filename = "./plot/future_vts_pop_mean_1000_2017_2026.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id, mean_future_sim_substra_mean[1:length(year_id),"513"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_future_substrat_mean_upper[1:length(year_id),"513"], rev(sim_future_substrat_mean_lower[1:length(year_id),"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "513", bty="n",  cex=0.8)


plot(year_id, mean_future_sim_substra_mean[1:length(year_id),"512"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_future_substrat_mean_upper[1:length(year_id),"512"], rev(sim_future_substrat_mean_lower[1:length(year_id),"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "512", bty="n",  cex=0.8)


plot(year_id, mean_future_sim_substra_mean[1:length(year_id),"511"], pch=16, ylim=ylim,  col="black",  xlab="Year", ylab="Abundance Index", cex=0.5, type="o")
polygon(x=c(year_id, rev(year_id)), y=c(sim_future_substrat_mean_upper[1:length(year_id),"511"], rev(sim_future_substrat_mean_lower[1:length(year_id),"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.50), border = NA)
legend("topleft",  "511", bty="n",  cex=0.8)
dev.off()

#### scaled prediction (2017-2026) in one plot ####
pop_scale_prediction <- (population_index-min(population_index))/diff(range(population_index))
vts_sim_scale_prediction <- (mean_future_sim_substra_mean-min(mean_future_sim_substra_mean))/diff(range(mean_future_sim_substra_mean))
vts_q_scale_prediction <- (mean_future_sim_substra_mean_q-min(mean_future_sim_substra_mean_q))/diff(range(mean_future_sim_substra_mean_q))

ylim=range(pop_scale_prediction, vts_sim_scale_prediction, vts_q_scale_prediction)
jpeg(filename = "./plot/scaled_indices_one_plot_projection.jpeg", width=175, height=60, units = "mm", res = 600)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id, pop_scale_prediction[,"513"], ylim=ylim, xlab="Year", ylab="Abundance Index", pch=2, type="o", col="purple", lty=2)
lines(year_id, vts_sim_scale_prediction[,"513"], pch=3, type="o", col="deepskyblue", lty=3)
lines(year_id, vts_q_scale_prediction[,"513"], pch=4, type="o", col="coral", lty=4)
legend("topleft",  c("Simulated Population Index", "Simulated VTS available lobsters index", "Simulated VTS with q index"), bty="n",  pch=c(2, 3, 4), lty=c(2,3,4), col=c("purple", "deepskyblue", "coral"), title="513", cex=0.7)

plot(year_id, pop_scale_prediction[,"512"], ylim=ylim, xlab="Year", ylab="Abundance Index", pch=2, type="o", col="purple", lty=2)
lines(year_id, vts_sim_scale_prediction[,"512"], pch=3, type="o", col="deepskyblue", lty=3)
lines(year_id, vts_q_scale_prediction[,"512"], pch=4, type="o", col="coral", lty=4)
legend("topleft",  "512", bty="n",  cex=0.7)


plot(year_id, pop_scale_prediction[,"511"], ylim=ylim, xlab="Year", ylab="Abundance Index", pch=2, type="o", col="purple", lty=2)
lines(year_id, vts_sim_scale_prediction[,"511"], pch=3, type="o", col="deepskyblue", lty=3)
lines(year_id, vts_q_scale_prediction[,"511"], pch=4, type="o", col="coral", lty=4)
legend("topleft",  "511", bty="n",  cex=0.7)
dev.off()
