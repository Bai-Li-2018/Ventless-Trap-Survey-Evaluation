#### Data preparation of abundance index for sample from 3 traps and 1 trap ####
vts_lobster_data <- read.csv("./data/vts/VTS_biodata2006_2017.csv")
vts_trap_data <- read.csv("./data/vts/VTS_traps.csv")

start_date <- as.Date(vts_lobster_data$TRIP_START_DATE, "%m/%d/%Y")
vts_lobster_data$month <- as.numeric(format(start_date, format="%m"))
vts_lobster_data$year <- as.numeric(format(start_date, format="%Y"))

trip_date <- as.Date(vts_trap_data$TRIP_START_DATE, "%m/%d/%Y")
vts_trap_data$month <- as.numeric(format(trip_date, format="%m"))
vts_trap_data$year <- as.numeric(format(trip_date, format="%Y"))
## lobster number per trap ##
sublegal_lobsters <- vts_lobster_data[which(vts_lobster_data$SAMPLE_LENGTH < 83 & (vts_lobster_data$TRAP_TYPE=="V" | vts_lobster_data$TRAP_TYPE=="v")),]

summary(sublegal_lobsters)

lobster_per_trap <- aggregate(sublegal_lobsters$SAMPLE_LENGTH, by=list(sublegal_lobsters$TRAP_ID, sublegal_lobsters$SET_OVER_DAYS), length)
lobster_per_trap$x <- round(lobster_per_trap$x/lobster_per_trap$Group.2)*3
lobster_per_trap <- as.data.frame(cbind(lobster_per_trap$Group.1, lobster_per_trap$x))
colnames(lobster_per_trap) <- c("Group.1", "x")

for (i in 1:nrow(vts_trap_data)){
  print(i)
  if (vts_trap_data$TRAP_ID[i] %in% lobster_per_trap$Group.1) vts_trap_data$quantity[i] <- lobster_per_trap$x[which(lobster_per_trap$Group.1==vts_trap_data$TRAP_ID[i])]
  else vts_trap_data$quantity[i] <- NA
}

vts_vtrap_data <- vts_trap_data[which((vts_trap_data$TRAP_TYPE=="V" |vts_trap_data$TRAP_TYPE=="v")),]

vts_vtrap_data <- na.omit(vts_vtrap_data)
summary(vts_vtrap_data)

vts_vtrap_data_trap1 <- matrix(NA, ncol=ncol(vts_vtrap_data), nrow=1)
colnames(vts_vtrap_data_trap1) <- colnames(vts_vtrap_data)
vts_vtrap_data_trap2 <- matrix(NA, ncol=ncol(vts_vtrap_data), nrow=1)
colnames(vts_vtrap_data_trap2) <- colnames(vts_vtrap_data)
vts_vtrap_data_trap3 <- matrix(NA, ncol=ncol(vts_vtrap_data), nrow=1)
colnames(vts_vtrap_data_trap3) <- colnames(vts_vtrap_data)
trip_id <- unique(vts_vtrap_data$TRIP_ID)
for(i in 1:length(trip_id)){
  temp <- vts_vtrap_data[which(vts_vtrap_data$TRIP_ID==trip_id[i]),]
  site_id <- unique(temp$SITE_ID[which(temp$TRIP_ID==trip_id[i])])
  for (j in 1:length(site_id)){
    trap_id1 <- min(temp$TRAP_ID[temp$SITE_ID==site_id[j]]) # use the data from the first trap
    trap_id2 <- median(temp$TRAP_ID[temp$SITE_ID==site_id[j]]) # use the data from the second trap
    trap_id3 <- max(temp$TRAP_ID[temp$SITE_ID==site_id[j]]) # use the data from the third trap
    vts_vtrap_data_trap1<-rbind(vts_vtrap_data_trap1, vts_vtrap_data[which(vts_vtrap_data$TRAP_ID==trap_id1),])
    vts_vtrap_data_trap2<-rbind(vts_vtrap_data_trap2, vts_vtrap_data[which(vts_vtrap_data$TRAP_ID==trap_id2),])
    vts_vtrap_data_trap3<-rbind(vts_vtrap_data_trap3, vts_vtrap_data[which(vts_vtrap_data$TRAP_ID==trap_id3),])
  }
}
vts_vtrap_data_trap1 <- vts_vtrap_data_trap1[2:nrow(vts_vtrap_data_trap1),]
vts_vtrap_data_trap2 <- vts_vtrap_data_trap2[2:nrow(vts_vtrap_data_trap2),]
vts_vtrap_data_trap3 <- vts_vtrap_data_trap3[2:nrow(vts_vtrap_data_trap3),]

#### calculate abundance index by collecting information from the first trap ####
lobster_per_year_sa_depth <- aggregate(vts_vtrap_data_trap1$quantity, by=list(vts_vtrap_data_trap1$year, vts_vtrap_data_trap1$STATAREA, vts_vtrap_data_trap1$assigneddepthstrata), sum)
colnames(lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 

lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data_trap1$TRAP_ID, by=list(vts_vtrap_data_trap1$year, vts_vtrap_data_trap1$STATAREA, vts_vtrap_data_trap1$assigneddepthstrata), length)$x

lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data_trap1$SITE_ID, by=list(vts_vtrap_data_trap1$year, vts_vtrap_data_trap1$STATAREA, vts_vtrap_data_trap1$assigneddepthstrata), function(x) length(unique(x)))$x
summary(lobster_per_year_sa_depth)

lobster_per_year_sa_depth$Ave_Lob_Trap <- lobster_per_year_sa_depth$Lob_Quantity/lobster_per_year_sa_depth$Trap_Quantity

area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
rownames(area_per_sa_depth) <- c("511", "512", "513")
## substrat_mean
substrat_mean_trap1 <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
year_id <- unique(lobster_per_year_sa_depth$Year)
colnames(substrat_mean_trap1) <- c("511", "512", "513")
rownames(substrat_mean_trap1) <- year_id
for (i in 1:length(year_id)){
  substrat_mean_trap1[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
  substrat_mean_trap1[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
  substrat_mean_trap1[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
}

## substrat_variance
lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data_trap1$quantity, by=list(vts_vtrap_data_trap1$year, vts_vtrap_data_trap1$STATAREA, vts_vtrap_data_trap1$assigneddepthstrata, vts_vtrap_data_trap1$SITE_ID), sum)
colnames(lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 

lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data_trap1$TRAP_ID, by=list(vts_vtrap_data_trap1$year, vts_vtrap_data_trap1$STATAREA, vts_vtrap_data_trap1$assigneddepthstrata, vts_vtrap_data_trap1$SITE_ID), length)$x

lobster_per_year_sa_depth_site$Quantity_per_Trap <- lobster_per_year_sa_depth_site$Lob_Quantity/lobster_per_year_sa_depth_site$Trap_Quantity

lobster_per_year_sa_depth_site=merge(lobster_per_year_sa_depth_site, lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))

lobster_per_year_sa_depth_site$Sub <- (lobster_per_year_sa_depth_site$Ave_Lob_Trap - lobster_per_year_sa_depth_site$Quantity_per_Trap)^2

individual_value <- aggregate(lobster_per_year_sa_depth_site$Sub, by=list(lobster_per_year_sa_depth_site$Year, lobster_per_year_sa_depth_site$SA, lobster_per_year_sa_depth_site$Depth), sum)
colnames(individual_value) <- c("Year", "SA", "Depth", "x")
individual_value$individual_value <- individual_value$x/aggregate(lobster_per_year_sa_depth_site$Site, by=list(lobster_per_year_sa_depth_site$Year, lobster_per_year_sa_depth_site$SA, lobster_per_year_sa_depth_site$Depth), length)$x

substrat_var_trap1 <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
colnames(substrat_var_trap1) <- c("511", "512", "513")
rownames(substrat_var_trap1) <- year_id

for (i in 1:length(year_id)){
  substrat_var_trap1[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
  
  substrat_var_trap1[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
  
  substrat_var_trap1[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
}

jpeg(filename = "./plot/vts_lobster_substrat_mean_real_trap1.jpeg", width=150, height=50, units = "mm", res = 600)
par(mfrow=c(1,3))
ylim_min <- min(substrat_mean_trap1-substrat_var_trap1)
ylim_max <- max(substrat_mean_trap1+substrat_var_trap1)

plot(year_id, substrat_mean_trap1[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap1[,"513"]+substrat_var_trap1[,"513"], rev(substrat_mean_trap1[,"513"]-substrat_var_trap1[,"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "513", bty="n")

plot(year_id, substrat_mean_trap1[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap1[,"512"]+substrat_var_trap1[,"512"], rev(substrat_mean_trap1[,"512"]-substrat_var_trap1[,"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "512", bty="n")

plot(year_id, substrat_mean_trap1[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap1[,"511"]+substrat_var_trap1[,"511"], rev(substrat_mean_trap1[,"511"]-substrat_var_trap1[,"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "511", bty="n")

dev.off()

#### calculate abundance index by collecting information from the second trap ####
lobster_per_year_sa_depth <- aggregate(vts_vtrap_data_trap2$quantity, by=list(vts_vtrap_data_trap2$year, vts_vtrap_data_trap2$STATAREA, vts_vtrap_data_trap2$assigneddepthstrata), sum)
colnames(lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 

lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data_trap2$TRAP_ID, by=list(vts_vtrap_data_trap2$year, vts_vtrap_data_trap2$STATAREA, vts_vtrap_data_trap2$assigneddepthstrata), length)$x

lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data_trap2$SITE_ID, by=list(vts_vtrap_data_trap2$year, vts_vtrap_data_trap2$STATAREA, vts_vtrap_data_trap2$assigneddepthstrata), function(x) length(unique(x)))$x
summary(lobster_per_year_sa_depth)

lobster_per_year_sa_depth$Ave_Lob_Trap <- lobster_per_year_sa_depth$Lob_Quantity/lobster_per_year_sa_depth$Trap_Quantity

area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
rownames(area_per_sa_depth) <- c("511", "512", "513")
## substrat_mean
substrat_mean_trap2 <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
year_id <- unique(lobster_per_year_sa_depth$Year)
colnames(substrat_mean_trap2) <- c("511", "512", "513")
rownames(substrat_mean_trap2) <- year_id
for (i in 1:length(year_id)){
  substrat_mean_trap2[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
  substrat_mean_trap2[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
  substrat_mean_trap2[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
}

## substrat_variance
lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data_trap2$quantity, by=list(vts_vtrap_data_trap2$year, vts_vtrap_data_trap2$STATAREA, vts_vtrap_data_trap2$assigneddepthstrata, vts_vtrap_data_trap2$SITE_ID), sum)
colnames(lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 

lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data_trap2$TRAP_ID, by=list(vts_vtrap_data_trap2$year, vts_vtrap_data_trap2$STATAREA, vts_vtrap_data_trap2$assigneddepthstrata, vts_vtrap_data_trap2$SITE_ID), length)$x

lobster_per_year_sa_depth_site$Quantity_per_Trap <- lobster_per_year_sa_depth_site$Lob_Quantity/lobster_per_year_sa_depth_site$Trap_Quantity

lobster_per_year_sa_depth_site=merge(lobster_per_year_sa_depth_site, lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))

lobster_per_year_sa_depth_site$Sub <- (lobster_per_year_sa_depth_site$Ave_Lob_Trap - lobster_per_year_sa_depth_site$Quantity_per_Trap)^2

individual_value <- aggregate(lobster_per_year_sa_depth_site$Sub, by=list(lobster_per_year_sa_depth_site$Year, lobster_per_year_sa_depth_site$SA, lobster_per_year_sa_depth_site$Depth), sum)
colnames(individual_value) <- c("Year", "SA", "Depth", "x")
individual_value$individual_value <- individual_value$x/aggregate(lobster_per_year_sa_depth_site$Site, by=list(lobster_per_year_sa_depth_site$Year, lobster_per_year_sa_depth_site$SA, lobster_per_year_sa_depth_site$Depth), length)$x

substrat_var_trap2 <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
colnames(substrat_var_trap2) <- c("511", "512", "513")
rownames(substrat_var_trap2) <- year_id

for (i in 1:length(year_id)){
  substrat_var_trap2[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
  
  substrat_var_trap2[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
  
  substrat_var_trap2[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
}

jpeg(filename = "./plot/vts_lobster_substrat_mean_real_trap2.jpeg", width=150, height=50, units = "mm", res = 600)
par(mfrow=c(1,3))
ylim_min <- min(substrat_mean_trap2-substrat_var_trap2)
ylim_max <- max(substrat_mean_trap2+substrat_var_trap2)

plot(year_id, substrat_mean_trap2[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap2[,"513"]+substrat_var_trap2[,"513"], rev(substrat_mean_trap2[,"513"]-substrat_var_trap2[,"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "513", bty="n")

plot(year_id, substrat_mean_trap2[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap2[,"512"]+substrat_var_trap1[,"512"], rev(substrat_mean_trap2[,"512"]-substrat_var_trap2[,"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "512", bty="n")

plot(year_id, substrat_mean_trap2[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap2[,"511"]+substrat_var_trap2[,"511"], rev(substrat_mean_trap2[,"511"]-substrat_var_trap2[,"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "511", bty="n")

dev.off()

#### calculate abundance index by collecting information from the third trap ####
lobster_per_year_sa_depth <- aggregate(vts_vtrap_data_trap3$quantity, by=list(vts_vtrap_data_trap3$year, vts_vtrap_data_trap3$STATAREA, vts_vtrap_data_trap3$assigneddepthstrata), sum)
colnames(lobster_per_year_sa_depth) <- c("Year", "SA", "Depth", "Lob_Quantity") 

lobster_per_year_sa_depth$Trap_Quantity <- aggregate(vts_vtrap_data_trap3$TRAP_ID, by=list(vts_vtrap_data_trap3$year, vts_vtrap_data_trap3$STATAREA, vts_vtrap_data_trap3$assigneddepthstrata), length)$x

lobster_per_year_sa_depth$Site_Quantity <- aggregate(vts_vtrap_data_trap3$SITE_ID, by=list(vts_vtrap_data_trap3$year, vts_vtrap_data_trap3$STATAREA, vts_vtrap_data_trap3$assigneddepthstrata), function(x) length(unique(x)))$x
summary(lobster_per_year_sa_depth)

lobster_per_year_sa_depth$Ave_Lob_Trap <- lobster_per_year_sa_depth$Lob_Quantity/lobster_per_year_sa_depth$Trap_Quantity

area_per_sa_depth <- as.data.frame(matrix(c(122, 82, 92, 296, 566, 395, 420, 1381, 315, 338, 198, 851), nrow=3, byrow=T))
colnames(area_per_sa_depth) <- c("depth1", "depth2", "depth3", "total")
rownames(area_per_sa_depth) <- c("511", "512", "513")
## substrat_mean
substrat_mean_trap3 <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
year_id <- unique(lobster_per_year_sa_depth$Year)
colnames(substrat_mean_trap3) <- c("511", "512", "513")
rownames(substrat_mean_trap3) <- year_id
for (i in 1:length(year_id)){
  substrat_mean_trap3[i,1] <- area_per_sa_depth["511", "depth1"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth2"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["511", "depth3"]/area_per_sa_depth["511", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
  substrat_mean_trap3[i,2] <- area_per_sa_depth["512", "depth1"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth2"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["512", "depth3"]/area_per_sa_depth["512", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
  substrat_mean_trap3[i,3] <- area_per_sa_depth["513", "depth1"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth2"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40"& lobster_per_year_sa_depth$Year==year_id[i])] + area_per_sa_depth["513", "depth3"]/area_per_sa_depth["513", "total"]*lobster_per_year_sa_depth$Ave_Lob_Trap[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60"& lobster_per_year_sa_depth$Year==year_id[i])] 
  
}

## substrat_variance
lobster_per_year_sa_depth_site <- aggregate(vts_vtrap_data_trap3$quantity, by=list(vts_vtrap_data_trap3$year, vts_vtrap_data_trap3$STATAREA, vts_vtrap_data_trap3$assigneddepthstrata, vts_vtrap_data_trap3$SITE_ID), sum)
colnames(lobster_per_year_sa_depth_site) <- c("Year", "SA", "Depth", "Site", "Lob_Quantity") 

lobster_per_year_sa_depth_site$Trap_Quantity <- aggregate(vts_vtrap_data_trap3$TRAP_ID, by=list(vts_vtrap_data_trap3$year, vts_vtrap_data_trap3$STATAREA, vts_vtrap_data_trap3$assigneddepthstrata, vts_vtrap_data_trap3$SITE_ID), length)$x

lobster_per_year_sa_depth_site$Quantity_per_Trap <- lobster_per_year_sa_depth_site$Lob_Quantity/lobster_per_year_sa_depth_site$Trap_Quantity

lobster_per_year_sa_depth_site=merge(lobster_per_year_sa_depth_site, lobster_per_year_sa_depth[, c("Year", "SA", "Depth", "Ave_Lob_Trap")], by=c("Year", "SA", "Depth"))

lobster_per_year_sa_depth_site$Sub <- (lobster_per_year_sa_depth_site$Ave_Lob_Trap - lobster_per_year_sa_depth_site$Quantity_per_Trap)^2

individual_value <- aggregate(lobster_per_year_sa_depth_site$Sub, by=list(lobster_per_year_sa_depth_site$Year, lobster_per_year_sa_depth_site$SA, lobster_per_year_sa_depth_site$Depth), sum)
colnames(individual_value) <- c("Year", "SA", "Depth", "x")
individual_value$individual_value <- individual_value$x/aggregate(lobster_per_year_sa_depth_site$Site, by=list(lobster_per_year_sa_depth_site$Year, lobster_per_year_sa_depth_site$SA, lobster_per_year_sa_depth_site$Depth), length)$x

substrat_var_trap3 <- matrix(NA, nrow=length(unique(lobster_per_year_sa_depth$Year)), ncol=3)
colnames(substrat_var_trap3) <- c("511", "512", "513")
rownames(substrat_var_trap3) <- year_id

for (i in 1:length(year_id)){
  substrat_var_trap3[i,1] <- 1/area_per_sa_depth["511", "total"]^2*((area_per_sa_depth["511", "depth1"]*(area_per_sa_depth["511", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth2"]*(area_per_sa_depth["511", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["511", "depth3"]*(area_per_sa_depth["511", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="511" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
  
  substrat_var_trap3[i,2] <- 1/area_per_sa_depth["512", "total"]^2*((area_per_sa_depth["512", "depth1"]*(area_per_sa_depth["512", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth2"]*(area_per_sa_depth["512", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["512", "depth3"]*(area_per_sa_depth["512", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="512" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
  
  substrat_var_trap3[i,3] <- 1/area_per_sa_depth["513", "total"]^2*((area_per_sa_depth["513", "depth1"]*(area_per_sa_depth["513", "depth1"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="1 to 20" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth2"]*(area_per_sa_depth["513", "depth2"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="21 to 40" & lobster_per_year_sa_depth$Year==year_id[i])]) + (area_per_sa_depth["513", "depth3"]*(area_per_sa_depth["513", "depth3"]-lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])])*individual_value$individual_value[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]/lobster_per_year_sa_depth$Site_Quantity[which(lobster_per_year_sa_depth$SA=="513" & lobster_per_year_sa_depth$Depth=="41 to 60" & lobster_per_year_sa_depth$Year==year_id[i])]))
}

jpeg(filename = "./plot/vts_lobster_substrat_mean_real_trap3.jpeg", width=150, height=50, units = "mm", res = 600)
par(mfrow=c(1,3))
ylim_min <- min(substrat_mean_trap3-substrat_var_trap3)
ylim_max <- max(substrat_mean_trap3+substrat_var_trap3)

plot(year_id, substrat_mean_trap3[,"513"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap3[,"513"]+substrat_var_trap3[,"513"], rev(substrat_mean_trap3[,"513"]-substrat_var_trap3[,"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "513", bty="n")

plot(year_id, substrat_mean_trap3[,"512"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap3[,"512"]+substrat_var_trap3[,"512"], rev(substrat_mean_trap3[,"512"]-substrat_var_trap3[,"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "512", bty="n")

plot(year_id, substrat_mean_trap3[,"511"], xlab="Year", ylab="Abundance Index", lty=1, type="o", pch=16, col="deepskyblue3", lwd=0.5, cex=0.4, ylim=c(ylim_min, ylim_max))
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap3[,"511"]+substrat_var_trap3[,"511"], rev(substrat_mean_trap3[,"511"]-substrat_var_trap3[,"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
legend("topleft", "511", bty="n")

dev.off()

#### VTS index from three traps per site ####
vts_lobster_data <- read.csv("./data/vts/VTS_biodata2006_2017.csv")
vts_trap_data <- read.csv("./data/vts/VTS_traps.csv")
start_date <- as.Date(vts_lobster_data$TRIP_START_DATE, "%m/%d/%Y")
vts_lobster_data$month <- as.numeric(format(start_date, format="%m"))
vts_lobster_data$year <- as.numeric(format(start_date, format="%Y"))

trip_date <- as.Date(vts_trap_data$TRIP_START_DATE, "%m/%d/%Y")
vts_trap_data$month <- as.numeric(format(trip_date, format="%m"))
vts_trap_data$year <- as.numeric(format(trip_date, format="%Y"))
## lobster number per trap ##
sublegal_lobsters <- vts_lobster_data[which(vts_lobster_data$SAMPLE_LENGTH < 83 & (vts_lobster_data$TRAP_TYPE=="V" | vts_lobster_data$TRAP_TYPE=="v")),]

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

vts_vtrap_data <- vts_trap_data[which((vts_trap_data$TRAP_TYPE=="V" |vts_trap_data$TRAP_TYPE=="v")),]
vts_vtrap_data <- na.omit(vts_vtrap_data)

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

jpeg(filename = "./plot/3traps_trap1_index.jpeg", width=175, height=65, units = "mm", res = 600)

ylim=range(substrat_mean+substrat_var, substrat_mean-substrat_var, substrat_mean_trap1+substrat_var_trap1, substrat_mean_trap1-substrat_var_trap1)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id, substrat_mean[,"513"], pch=16, xlab="Year", ylab="Abundance Index", ylim=ylim, axes=F, type="o", lty=2, col="deepskyblue3")
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"513"]+substrat_var[,"513"], rev(substrat_mean[,"513"]-substrat_var[,"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
lines(year_id, substrat_mean_trap1[,"513"], pch=16, col="coral3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap1[,"513"]+substrat_var_trap1[,"513"], rev(substrat_mean_trap1[,"513"]-substrat_var_trap1[,"513"])), col=adjustcolor("coral3", alpha.f = 0.20), border = NA)
legend("topleft", "513", bty="n", cex=0.8)
axis(1, at=year_id, labels = year_id)
axis(2)
box()

plot(year_id, substrat_mean[,"512"], pch=16, col="deepskyblue3", xlab="Year", ylab="Abundance Index", ylim=ylim, axes=F, type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"512"]+substrat_var[,"512"], rev(substrat_mean[,"512"]-substrat_var[,"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
lines(year_id, substrat_mean_trap1[,"512"], pch=16, col="coral3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap1[,"512"]+substrat_var_trap1[,"512"], rev(substrat_mean_trap1[,"512"]-substrat_var_trap1[,"512"])), col=adjustcolor("coral3", alpha.f = 0.20), border = NA)
legend("topleft", "512", bty="n", cex=0.8)
axis(1, at=year_id, labels = year_id)
axis(2)
box()
legend("bottomleft", legend=c("Samples from 3 traps", "Samples from 1st trap"), lty=c(2,2), pch=c(16, 16), col=c("deepskyblue3", "coral3"), bty="n", cex=0.8)


plot(year_id, substrat_mean[,"511"], pch=16, xlab="Year", ylab="Abundance Index", ylim=ylim, axes=F, type="o", lty=2, col="deepskyblue3")
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"511"]+substrat_var[,"511"], rev(substrat_mean[,"511"]-substrat_var[,"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
lines(year_id, substrat_mean_trap1[,"511"], pch=16, col="coral3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap1[,"511"]+substrat_var_trap1[,"511"], rev(substrat_mean_trap1[,"511"]-substrat_var_trap1[,"511"])), col=adjustcolor("coral3", alpha.f = 0.20), border = NA)
legend("topleft", "511", bty="n", cex=0.8)
axis(1, at=year_id, labels = year_id)
axis(2)
box()
dev.off()

cor.test(substrat_mean[,"511"], substrat_mean_trap1[,"511"])
cor.test(substrat_mean[,"512"], substrat_mean_trap1[,"512"])
cor.test(substrat_mean[,"513"], substrat_mean_trap1[,"513"])

jpeg(filename = "./plot/3traps_trap2_index.jpeg", width=175, height=65, units = "mm", res = 600)

ylim=range(substrat_mean+substrat_var, substrat_mean-substrat_var, substrat_mean_trap2+substrat_var_trap2, substrat_mean_trap2-substrat_var_trap2)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id, substrat_mean[,"513"], pch=16, xlab="Year", ylab="Abundance Index", ylim=ylim, axes=F, type="o", lty=2, col="deepskyblue3")
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"513"]+substrat_var[,"513"], rev(substrat_mean[,"513"]-substrat_var[,"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
lines(year_id, substrat_mean_trap2[,"513"], pch=16, col="coral3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap2[,"513"]+substrat_var_trap2[,"513"], rev(substrat_mean_trap2[,"513"]-substrat_var_trap2[,"513"])), col=adjustcolor("coral3", alpha.f = 0.20), border = NA)
legend("topleft", "513", bty="n", cex=0.8)
axis(1, at=year_id, labels = year_id)
axis(2)
box()

plot(year_id, substrat_mean[,"512"], pch=16, col="deepskyblue3", xlab="Year", ylab="Abundance Index", ylim=ylim, axes=F, type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"512"]+substrat_var[,"512"], rev(substrat_mean[,"512"]-substrat_var[,"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
lines(year_id, substrat_mean_trap2[,"512"], pch=16, col="coral3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap2[,"512"]+substrat_var_trap2[,"512"], rev(substrat_mean_trap2[,"512"]-substrat_var_trap2[,"512"])), col=adjustcolor("coral3", alpha.f = 0.20), border = NA)
legend("topleft", "512", bty="n", cex=0.8)
axis(1, at=year_id, labels = year_id)
axis(2)
box()
legend("bottomleft", legend=c("Samples from 3 traps", "Samples from 2nd trap"), lty=c(2,2), pch=c(16, 16), col=c("deepskyblue3", "coral3"), bty="n", cex=0.8)


plot(year_id, substrat_mean[,"511"], pch=16, xlab="Year", ylab="Abundance Index", ylim=ylim, axes=F, type="o", lty=2, col="deepskyblue3")
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"511"]+substrat_var[,"511"], rev(substrat_mean[,"511"]-substrat_var[,"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
lines(year_id, substrat_mean_trap2[,"511"], pch=16, col="coral3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap2[,"511"]+substrat_var_trap2[,"511"], rev(substrat_mean_trap2[,"511"]-substrat_var_trap2[,"511"])), col=adjustcolor("coral3", alpha.f = 0.20), border = NA)
legend("topleft", "511", bty="n", cex=0.8)
axis(1, at=year_id, labels = year_id)
axis(2)
box()
dev.off()

jpeg(filename = "./plot/3traps_trap3_index.jpeg", width=175, height=65, units = "mm", res = 600)

ylim=range(substrat_mean+substrat_var, substrat_mean-substrat_var, substrat_mean_trap3+substrat_var_trap3, substrat_mean_trap3-substrat_var_trap3)

par(mfrow=c(1,3), mar=c(4,4,1,1))
plot(year_id, substrat_mean[,"513"], pch=16, xlab="Year", ylab="Abundance Index", ylim=ylim, axes=F, type="o", lty=2, col="deepskyblue3")
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"513"]+substrat_var[,"513"], rev(substrat_mean[,"513"]-substrat_var[,"513"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
lines(year_id, substrat_mean_trap3[,"513"], pch=16, col="coral3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap3[,"513"]+substrat_var_trap3[,"513"], rev(substrat_mean_trap3[,"513"]-substrat_var_trap3[,"513"])), col=adjustcolor("coral3", alpha.f = 0.20), border = NA)
legend("topleft", "513", bty="n", cex=0.8)
axis(1, at=year_id, labels = year_id)
axis(2)
box()

plot(year_id, substrat_mean[,"512"], pch=16, col="deepskyblue3", xlab="Year", ylab="Abundance Index", ylim=ylim, axes=F, type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"512"]+substrat_var[,"512"], rev(substrat_mean[,"512"]-substrat_var[,"512"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
lines(year_id, substrat_mean_trap3[,"512"], pch=16, col="coral3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap3[,"512"]+substrat_var_trap3[,"512"], rev(substrat_mean_trap3[,"512"]-substrat_var_trap3[,"512"])), col=adjustcolor("coral3", alpha.f = 0.20), border = NA)
legend("topleft", "512", bty="n", cex=0.8)
axis(1, at=year_id, labels = year_id)
axis(2)
box()
legend("bottomleft", legend=c("Samples from 3 traps", "Samples from 3rd trap"), lty=c(2,2), pch=c(16, 16), col=c("deepskyblue3", "coral3"), bty="n", cex=0.8)


plot(year_id, substrat_mean[,"511"], pch=16, xlab="Year", ylab="Abundance Index", ylim=ylim, axes=F, type="o", lty=2, col="deepskyblue3")
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean[,"511"]+substrat_var[,"511"], rev(substrat_mean[,"511"]-substrat_var[,"511"])), col=adjustcolor("deepskyblue3", alpha.f = 0.20), border = NA)
lines(year_id, substrat_mean_trap3[,"511"], pch=16, col="coral3", type="o", lty=2)
polygon(x=c(year_id, rev(year_id)), y=c(substrat_mean_trap3[,"511"]+substrat_var_trap3[,"511"], rev(substrat_mean_trap3[,"511"]-substrat_var_trap3[,"511"])), col=adjustcolor("coral3", alpha.f = 0.20), border = NA)
legend("topleft", "511", bty="n", cex=0.8)
axis(1, at=year_id, labels = year_id)
axis(2)
box()
dev.off()
#### Sex ratio ####
vts_lobster_data <- read.csv("./data/vts/VTS_biodata2006_2017.csv")
vts_trap_data <- read.csv("./data/vts/VTS_traps.csv")

start_date <- as.Date(vts_lobster_data$TRIP_START_DATE, "%m/%d/%Y")
vts_lobster_data$month <- as.numeric(format(start_date, format="%m"))
vts_lobster_data$year <- as.numeric(format(start_date, format="%Y"))

trip_date <- as.Date(vts_trap_data$TRIP_START_DATE, "%m/%d/%Y")
vts_trap_data$month <- as.numeric(format(trip_date, format="%m"))
vts_trap_data$year <- as.numeric(format(trip_date, format="%Y"))
## lobster number per trap ##
sublegal_lobsters <- vts_lobster_data[which(vts_lobster_data$SAMPLE_LENGTH < 83 & (vts_lobster_data$TRAP_TYPE=="V" | vts_lobster_data$TRAP_TYPE=="v")),]

summary(sublegal_lobsters)

lobster_per_trap <- aggregate(sublegal_lobsters$SAMPLE_LENGTH, by=list(sublegal_lobsters$TRAP_ID, sublegal_lobsters$SET_OVER_DAYS), length)
lobster_per_trap$x <- round(lobster_per_trap$x/lobster_per_trap$Group.2)*3
lobster_per_trap <- as.data.frame(cbind(lobster_per_trap$Group.1, lobster_per_trap$x))
colnames(lobster_per_trap) <- c("Group.1", "x")

for (i in 1:nrow(vts_trap_data)){
  print(i)
  if (vts_trap_data$TRAP_ID[i] %in% lobster_per_trap$Group.1) vts_trap_data$quantity[i] <- lobster_per_trap$x[which(lobster_per_trap$Group.1==vts_trap_data$TRAP_ID[i])]
  else vts_trap_data$quantity[i] <- NA
}

vts_vtrap_data <- vts_trap_data[which((vts_trap_data$TRAP_TYPE=="V" |vts_trap_data$TRAP_TYPE=="v")),]

vts_vtrap_data <- na.omit(vts_vtrap_data)
summary(vts_vtrap_data)

vts_vtrap_data_trap1 <- matrix(NA, ncol=ncol(vts_vtrap_data), nrow=1)
colnames(vts_vtrap_data_trap1) <- colnames(vts_vtrap_data)
vts_vtrap_data_trap2 <- matrix(NA, ncol=ncol(vts_vtrap_data), nrow=1)
colnames(vts_vtrap_data_trap2) <- colnames(vts_vtrap_data)
vts_vtrap_data_trap3 <- matrix(NA, ncol=ncol(vts_vtrap_data), nrow=1)
colnames(vts_vtrap_data_trap3) <- colnames(vts_vtrap_data)
trip_id <- unique(vts_vtrap_data$TRIP_ID)
for(i in 1:length(trip_id)){
  temp <- vts_vtrap_data[which(vts_vtrap_data$TRIP_ID==trip_id[i]),]
  site_id <- unique(temp$SITE_ID[which(temp$TRIP_ID==trip_id[i])])
  for (j in 1:length(site_id)){
    trap_id1 <- min(temp$TRAP_ID[temp$SITE_ID==site_id[j]]) # use the data from the first trap
    trap_id2 <- median(temp$TRAP_ID[temp$SITE_ID==site_id[j]]) # use the data from the second trap
    trap_id3 <- max(temp$TRAP_ID[temp$SITE_ID==site_id[j]]) # use the data from the third trap
    vts_vtrap_data_trap1<-rbind(vts_vtrap_data_trap1, vts_vtrap_data[which(vts_vtrap_data$TRAP_ID==trap_id1),])
    vts_vtrap_data_trap2<-rbind(vts_vtrap_data_trap2, vts_vtrap_data[which(vts_vtrap_data$TRAP_ID==trap_id2),])
    vts_vtrap_data_trap3<-rbind(vts_vtrap_data_trap3, vts_vtrap_data[which(vts_vtrap_data$TRAP_ID==trap_id3),])
  }
}
vts_vtrap_data_trap1 <- vts_vtrap_data_trap1[2:nrow(vts_vtrap_data_trap1),]
vts_vtrap_data_trap2 <- vts_vtrap_data_trap1[2:nrow(vts_vtrap_data_trap2),]
vts_vtrap_data_trap3 <- vts_vtrap_data_trap3[2:nrow(vts_vtrap_data_trap3),]

vts_lobster_data_trap1 <- sublegal_lobsters[which(sublegal_lobsters$TRAP_ID %in% vts_vtrap_data_trap1$TRAP_ID),]
vts_lobster_data_trap2 <- sublegal_lobsters[which(sublegal_lobsters$TRAP_ID %in% vts_vtrap_data_trap2$TRAP_ID),]
vts_lobster_data_trap3 <- sublegal_lobsters[which(sublegal_lobsters$TRAP_ID %in% vts_vtrap_data_trap3$TRAP_ID),]

female_num_3traps <- aggregate(sublegal_lobsters$SEX, by=list(sublegal_lobsters$year), function(x) length(which(x=="Female")))
male_num_3traps <- aggregate(sublegal_lobsters$SEX, by=list(sublegal_lobsters$year), function(x) length(which(x=="Male")))
female_ratio_3traps <- female_num_3traps$x/(female_num_3traps$x+male_num_3traps$x)*100
male_ratio_3traps <- (100-female_ratio_3traps)

female_num_trap1 <- aggregate(vts_lobster_data_trap1$SEX, by=list(vts_lobster_data_trap1$year), function(x) length(which(x=="Female")))
male_num_trap1 <- aggregate(vts_lobster_data_trap1$SEX, by=list(vts_lobster_data_trap1$year), function(x) length(which(x=="Male")))
female_ratio_trap1 <- female_num_trap1$x/(female_num_trap1$x+male_num_trap1$x)*100
male_ratio_trap1 <- (100-female_ratio_trap1)


female_num_trap2 <- aggregate(vts_lobster_data_trap2$SEX, by=list(vts_lobster_data_trap2$year), function(x) length(which(x=="Female")))
male_num_trap2 <- aggregate(vts_lobster_data_trap2$SEX, by=list(vts_lobster_data_trap2$year), function(x) length(which(x=="Male")))
female_ratio_trap2 <- female_num_trap2$x/(female_num_trap2$x+male_num_trap2$x)*100
male_ratio_trap2 <- (100-female_ratio_trap2)

female_num_trap3 <- aggregate(vts_lobster_data_trap3$SEX, by=list(vts_lobster_data_trap3$year), function(x) length(which(x=="Female")))
male_num_trap3 <- aggregate(vts_lobster_data_trap3$SEX, by=list(vts_lobster_data_trap3$year), function(x) length(which(x=="Male")))
female_ratio_trap3 <- female_num_trap3$x/(female_num_trap3$x+male_num_trap3$x)*100
male_ratio_trap3 <- (100-female_ratio_trap3)

year_id <- 2006:2017
jpeg(filename = "./plot/3traps_trap1_sex_ratio.jpeg", width=175, height=100, units = "mm", res = 600)
par(mfrow=c(1,2))
sex_ratio_3traps <- barplot(rbind(female_ratio_3traps, male_ratio_3traps), col=c("deepskyblue3", "coral3"), xlab="Year", ylab="Proportion (%)")
axis(1, at=sex_ratio_3traps, labels=year_id, padj=-1)
box()
legend("topleft", c("Female", "Male"), col=c("deepskyblue3", "coral3"), bg="white", pch=c(15,15), title="A)", cex=0.8)

sex_ratio_trap1 <- barplot(rbind(female_ratio_trap1, male_ratio_trap1), col=c("deepskyblue3", "coral3"), xlab="Year", ylab="Proportion (%)")
axis(1, at=sex_ratio_trap1, labels=year_id, padj=-1)
box()
legend("topleft", c("Female", "Male"), col=c("deepskyblue3", "coral3"), bg="white", pch=c(15,15), title="B)", cex=0.8)
dev.off()

jpeg(filename = "./plot/3traps_trap2_sex_ratio.jpeg", width=175, height=100, units = "mm", res = 600)
par(mfrow=c(1,2))
sex_ratio_3traps <- barplot(rbind(female_ratio_3traps, male_ratio_3traps), col=c("deepskyblue3", "coral3"), xlab="Year", ylab="Proportion (%)")
axis(1, at=sex_ratio_3traps, labels=year_id, padj=-1)
box()
legend("topleft", c("Female", "Male"), col=c("deepskyblue3", "coral3"), bg="white", pch=c(15,15), title="A)", cex=0.8)

sex_ratio_trap2 <- barplot(rbind(female_ratio_trap2, male_ratio_trap2), col=c("deepskyblue3", "coral3"), xlab="Year", ylab="Proportion (%)")
axis(1, at=sex_ratio_trap2, labels=year_id, padj=-1)
box()
legend("topleft", c("Female", "Male"), col=c("deepskyblue3", "coral3"), bg="white", pch=c(15,15), title="B)", cex=0.8)
dev.off()

jpeg(filename = "./plot/3traps_trap3_sex_ratio.jpeg", width=175, height=100, units = "mm", res = 600)
par(mfrow=c(1,2))
sex_ratio_3traps <- barplot(rbind(female_ratio_3traps, male_ratio_3traps), col=c("deepskyblue3", "coral3"), xlab="Year", ylab="Proportion (%)")
axis(1, at=sex_ratio_3traps, labels=year_id, padj=-1)
box()
legend("topleft", c("Female", "Male"), col=c("deepskyblue3", "coral3"), bg="white", pch=c(15,15), title="A)", cex=0.8)

sex_ratio_trap3 <- barplot(rbind(female_ratio_trap3, male_ratio_trap3), col=c("deepskyblue3", "coral3"), xlab="Year", ylab="Proportion (%)")
axis(1, at=sex_ratio_trap3, labels=year_id, padj=-1)
box()
legend("topleft", c("Female", "Male"), col=c("deepskyblue3", "coral3"), bg="white", pch=c(15,15), title="B)", cex=0.8)
dev.off()
#### Size distribution ####
jpeg(filename = "./plot/3traps_trap1_size_distribution.jpeg", width=175, height=175, units = "mm", res = 600)
plot_id_matrix <- matrix(c(1:length(year_id)), nrow=4, byrow=T)
plot_id_matrix <- cbind(c(13,13,13,13), plot_id_matrix, c(13,13,13,13))
plot_id_matrix <- rbind(c(13,13,13,13,13), plot_id_matrix, c(13,13,13,13,13))


layout(plot_id_matrix)
par(mar=c(0,0,0,0))
for(i in 1:length(year_id)){
  size_3traps_data <- hist(sublegal_lobsters$SAMPLE_LENGTH[which(sublegal_lobsters$year==year_id[i])], breaks=seq(0, 85, by=5), plot=F)
  size_trap1_data <- hist(vts_lobster_data_trap1$SAMPLE_LENGTH[which(vts_lobster_data_trap1$year==year_id[i])], breaks=seq(0, 85, by=5), plot=F)
  plot(size_3traps_data$mids, size_3traps_data$density, type="l", col="deepskyblue3", lty=1, axes=F, ylim=c(0,0.06))
  lines(size_trap1_data$mids, size_trap1_data$density, type="l", col="coral3", lty=2)
  legend("topleft", paste(year_id[i]), bty="n", cex=0.7)
  if (i %in% 1) legend("top", c("3 traps", "1st trap"), lty=c(1,2), col=c("deepskyblue3", "coral3"), bty="n")
  box()
  if (i %in% c(1,7)) axis(2)
  if (i %in% 2) axis(3)
  if (i %in% c(6,12)) axis(4)
  if (i %in% c(10, 12)) axis(1)
  
  if (i == 4) mtext("Density", side=2, line=2.2)
  if (i == 11) mtext("Carapace Length (mm)", side=1, line=2.2)
}

dev.off()

jpeg(filename = "./plot/3traps_trap2_size_distribution.jpeg", width=175, height=175, units = "mm", res = 600)
plot_id_matrix <- matrix(c(1:length(year_id)), nrow=4, byrow=T)
plot_id_matrix <- cbind(c(13,13,13,13), plot_id_matrix, c(13,13,13,13))
plot_id_matrix <- rbind(c(13,13,13,13,13), plot_id_matrix, c(13,13,13,13,13))


layout(plot_id_matrix)
par(mar=c(0,0,0,0))
for(i in 1:length(year_id)){
  size_3traps_data <- hist(sublegal_lobsters$SAMPLE_LENGTH[which(sublegal_lobsters$year==year_id[i])], breaks=seq(0, 85, by=5), plot=F)
  size_trap2_data <- hist(vts_lobster_data_trap2$SAMPLE_LENGTH[which(vts_lobster_data_trap2$year==year_id[i])], breaks=seq(0, 85, by=5), plot=F)
  plot(size_3traps_data$mids, size_3traps_data$density, type="l", col="deepskyblue3", lty=1, axes=F, ylim=c(0,0.06))
  lines(size_trap2_data$mids, size_trap2_data$density, type="l", col="coral3", lty=2)
  legend("topleft", paste(year_id[i]), bty="n", cex=0.7)
  if (i %in% 1) legend("top", c("3 traps", "2nd trap"), lty=c(1,2), col=c("deepskyblue3", "coral3"), bty="n")
  box()
  if (i %in% c(1,7)) axis(2)
  if (i %in% 2) axis(3)
  if (i %in% c(6,12)) axis(4)
  if (i %in% c(10, 12)) axis(1)
  
  if (i == 4) mtext("Density", side=2, line=2.2)
  if (i == 11) mtext("Carapace Length (mm)", side=1, line=2.2)
}

dev.off()

jpeg(filename = "./plot/3traps_trap3_size_distribution.jpeg", width=175, height=175, units = "mm", res = 600)
plot_id_matrix <- matrix(c(1:length(year_id)), nrow=4, byrow=T)
plot_id_matrix <- cbind(c(13,13,13,13), plot_id_matrix, c(13,13,13,13))
plot_id_matrix <- rbind(c(13,13,13,13,13), plot_id_matrix, c(13,13,13,13,13))


layout(plot_id_matrix)
par(mar=c(0,0,0,0))
for(i in 1:length(year_id)){
  size_3traps_data <- hist(sublegal_lobsters$SAMPLE_LENGTH[which(sublegal_lobsters$year==year_id[i])], breaks=seq(0, 85, by=5), plot=F)
  size_trap3_data <- hist(vts_lobster_data_trap3$SAMPLE_LENGTH[which(vts_lobster_data_trap3$year==year_id[i])], breaks=seq(0, 85, by=5), plot=F)
  plot(size_3traps_data$mids, size_3traps_data$density, type="l", col="deepskyblue3", lty=1, axes=F, ylim=c(0,0.06))
  lines(size_trap3_data$mids, size_trap3_data$density, type="l", col="coral3", lty=2)
  legend("topleft", paste(year_id[i]), bty="n", cex=0.7)
  if (i %in% 1) legend("top", c("3 traps", "3rd trap"), lty=c(1,2), col=c("deepskyblue3", "coral3"), bty="n")
  box()
  if (i %in% c(1,7)) axis(2)
  if (i %in% 2) axis(3)
  if (i %in% c(6,12)) axis(4)
  if (i %in% c(10, 12)) axis(1)
  
  if (i == 4) mtext("Density", side=2, line=2.2)
  if (i == 11) mtext("Carapace Length (mm)", side=1, line=2.2)
}

dev.off()