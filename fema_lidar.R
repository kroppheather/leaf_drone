library(lidR)
library(future)
library(dplyr)
library(terra)
library(sf)
library(leafR)
library(ggplot2)

hL <- readLAS("C:/Users/hkropp/Documents/NEON_lidar-point-cloud-line/NEON.D19.HEAL.DP1.30003.001.2018-08.basic.20230907T165247Z.RELEASE-2023/NEON_D19_HEAL_DP1_385000_7080000_classified_point_cloud_colorized.laz")

dirFigures <- "E:/Google Drive/research/proposal/MRI/figures"

l1 <- readLAS("C:/Users/hkropp/Documents/ArcGIS/Projects/Lidar/data/l2019/18TVN650670.las")

plot(l1)

l2 <- readLAS("C:/Users/hkropp/Documents/ArcGIS/Projects/Lidar/data/l2019/18TVN665670.las")
# remove outliers
plot(l2)
hist(l2@data$Z)
las <- filter_poi(l2, Z >= 188, Z <= 285)

plot(las, size=2, bg="white")
hist(las@data$Z)

# dtm
dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
plot(dtm_tin)
plot_dtm3d(dtm_tin, bg = "white")

# clip to forest area

nlas <- las - dtm_tin
plot(nlas, size = 4, bg = "white")

nclip <- clip_rectangle(nlas, 466500,4767200,467700,4767900)

chm_p2r_05 <- rasterize_canopy(nlas, 0.5, p2r(subcircle = 0.2), pkg = "terra")
plot(chm_p2r_05)

kernel <- matrix(1,3,3)
chm_p2r_05_smoothed <- terra::focal(chm_p2r_05, w = kernel, fun = median, na.rm = TRUE)

ttops_chm_p2r_05 <- locate_trees(chm_p2r_05, lmf(3))
ttops_chm_p2r_05_smoothed <- locate_trees(chm_p2r_05_smoothed, lmf(3))

algo <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
tlas <- segment_trees(nclip, algo) # segment point cloud
plot(tlas, bg = "white", size = 3, color = "treeID") # visualize trees

FI_plotsSF <- sf::st_read("C:/Users/hkropp/Documents/Lidar/FI_15m.shp")
FI_plotsp <- as(FI_plotsSF, "Spatial")
FI_plotsu <- vect(FI_plotsp)

FI_plots <- project(FI_plotsu, chm_p2r_05_smoothed)

Plots_crop <- crop(FI_plots, chm_p2r_05_smoothed)
plot(chm_p2r_05_smoothed)
plot(FI_plots, add=TRUE)

Plots <- sf::st_as_sf(Plots_crop)
plot(Plots["Plot"])

plot(chmRG01)
treeRG01 <- clip_roi(tlas, Plots %>% filter(Plot=="RG01"))
plot(treeRG01, bg = "white", size = 2, color = "treeID")

writeRaster(chmRG01, "C:/Users/hkropp/Documents/Lidar/RG01_CHM.tif")

plot(treeRG01, bg = "white", size = 2, color = "treeID")

library(treetop)
launchApp(launch.browser = TRUE)

hist(treeRG01$Z)

RG03 <- subset(FI_plots, FI_plots$Plot == "RG03")
chmRG03c <- crop(chm_p2r_05_smoothed, RG03)
chmRG03 <- mask(chmRG03c, RG03)
plot(chmRG03)
lasRG03 <- clip_roi(nclip, Plots %>% filter(Plot=="RG03"))
treeRG03 <- clip_roi(tlas, Plots %>% filter(Plot=="RG03"))
plot(lasRG03 , bg = "white", size = 3)
plot(treeRG03, bg = "white", size = 3, color="treeID")
hist(treeRG03$Z)

treeRG08 <- clip_roi(tlas, Plots %>% filter(Plot=="RG08"))
plot(treeRG08, bg = "white", size = 4, color="treeID")

hemM <- clip_rectangle(tlas, 467000,4767450,467100, 4767550)
plot(hemM, bg = "white", size = 4, color="treeID")
unique(treeRG03$treeID)

indvTree <- filter_poi(treeRG03, treeID ==2408) 

plot(indvTree, bg="white", size=5)
unique(indvTree$treeID)

writeLAS(lasRG03,"C:/Users/hkropp/Documents/Lidar/nlasRG03.laz")

# pull out lidar for plots
plotAll <- unique(Plots_crop$Plot)
plotSub <- list()
chmplotC <- list()
chmplot <- list()
# subset chm
for(i in 1:length(plotAll)){
  plotSub[[i]] <-  subset(Plots_crop, Plots_crop$Plot == plotAll[i])
  chmplotC[[i]] <- crop(chm_p2r_05_smoothed, plotSub[[i]])
  chmplot[[i]] <- mask(chmplotC[[i]], plotSub[[i]])
  
}

lasPlot <- list()
for(i in 1:length(plotAll)){
  lasPlot[[i]] <- clip_roi(nlas, Plots %>% filter(Plot==plotAll[i]))
  writeLAS(lasPlot[[i]],paste0("C:/Users/hkropp/Documents/Lidar/plots/",plotAll[i],".laz"))
}

plotAll




voxelsG03 <- lad.voxels("C:/Users/hkropp/Documents/Lidar/plots/RG03.laz")
ladG03 <- lad.profile(voxelsG03)
laiG03 <- lai.raster(voxelsG03)

voxelsPlot <- lad.voxels(paste0("C:/Users/hkropp/Documents/Lidar/plots/",plotAll[i],".laz"))
for(i in 1:length(plotAll)){
  voxelsPlot[[i]] <- lad.voxels(paste0("C:/Users/hkropp/Documents/Lidar/plots/",plotAll[i],".laz"))
}

ladPlot <- list()
laiPlot <- list()
for(i in 1:length(plotAll)){
  ladPlot[[i]] <- lad.profile(lad.voxels(paste0("C:/Users/hkropp/Documents/Lidar/plots/",plotAll[i],".laz")))
}
ladTest <- ladPlot[[1]]

plot(ladPlot[[1]])
plot(ladPlot[[4]])
str(ladPlot[[1]])

ladData <- list()
for(i in 1:length(plotAll)){
  ladPlot[[i]]$plot <- rep(plotAll[i], nrow(ladPlot[[i]]))
}

laiPlot <- list()
for(i in 1:length(plotAll)){
  laiPlot[[i]] <- lai.raster(voxelsPlot[[i]])
}

ladDF <- list()
for(i in 1:length(plotAll)){
  ladDF[[i]] <- data.frame(height = ladPlot[[i]]$height,
                           lad = ladPlot[[i]]$lad,
                           plotID = rep(plotAll[i], 
                                        length(ladPlot[[i]]$lad)))
}

ladAll <- do.call("rbind", ladDF)

plotAll

laiAll <- list()
for(i in 1:length(plotAll)){
  laiAll[[i]] <- data.frame(lai=values(laiPlot[[i]]),
                            plotID=rep(plotAll[i],length(values(laiPlot[[i]]))))
}

laiDF <- do.call("rbind", laiAll)

ggplot(laiDF, aes(as.factor(plotID),lai))+
  geom_boxplot()

canopyLAI <- read.csv("K:/Environmental_Studies/hkropp/Private/canopy/canopy_lai.csv")
canopyPlot <- canopyLAI %>%
  group_by(site_id) %>%
  summarize(LAI = mean(PAR_LAI))

SpeciesInfo <- read.csv("K:/Environmental_Studies/hkropp/Private/canopy/speciesID.csv")
forestInventory <- read.csv("K:/Environmental_Studies/hkropp/Private/canopy/HCEF forest inventory data.csv")
forestInventory$tree_area.cm2 <- (((forestInventory$DBH.cm / 2)^2) * pi/10000) 
FI <- forestInventory %>%
  filter(Dead == "N", DBH.cm >3 ) %>%
  group_by(Plot, Species) %>%
  summarise(totArea = sum(tree_area.cm2,na.rm=TRUE),
            ncount = n(),
            aveDBH = mean(DBH.cm,na.rm=TRUE))

FITot <-  forestInventory %>%
  filter(Dead == "N", DBH.cm >3 ) %>%
  group_by(Plot) %>%
  summarise(totArea = sum(tree_area.cm2,na.rm=TRUE),
            ncount = n(),
            aveDBH = mean(DBH.cm,na.rm=TRUE))

FIjoin <- left_join(FI,FITot, by="Plot")
FIjoin$PercBA <- (FIjoin$totArea.x/FIjoin$totArea.y)*100  


FItop <- FIjoin %>%
  filter(PercBA > 20)

PlotSpec <- FItop %>%
  select(Species,Plot, PercBA)

plotsI <- unique(PlotSpec$Plot)

PlotComp <- left_join(PlotSpec, SpeciesInfo, by=c("Species"="Code"))

NameSub <- character()
for(i in 1:nrow(PlotComp)){
  NameSub[i] <- paste0(substr(strsplit(PlotComp$Species.y[i], " ")[[1]][1],1,1), ". ",
                         strsplit(PlotComp$Species.y[i], " ")[[1]][2])
}

PlotComp$NameSub <- NameSub
PlotComp$Name <- ifelse(PlotComp$Genus =="Malus", "Malus sp.", PlotComp$NameSub ) 

PlotComp <- PlotComp %>%
  arrange(desc(PercBA))
PlotComp[PlotComp$Plot == "RG03",]

pasteSub <- character()
namecomp <- character()
namePerc <- character()

for(i in 1:length(plotsI)){

  pasteSub <- PlotComp$Name[PlotComp$Plot == plotsI[i]]
  percSub <-  paste0(pasteSub, "(",round(PlotComp$PercBA[PlotComp$Plot == plotsI[i]],0),")")
  namecomp[i] <- paste(pasteSub,  collapse = ", ")
  namePerc[i] <- paste(percSub,  collapse = ", ")
}
namecomp
namePerc

nameDF <- data.frame(Plot = plotsI, Names = namecomp, namePerc=namePerc)

canopyLAINames <- left_join(canopyLAI, nameDF, by=c("site_id"="Plot"))

# get nine colors
colP <- hcl.colors(9, palette="Dark3")
colorL <- c(colP[1],#RG01
            colP[2],#RG03
            colP[3],#RG06
            colP[2],#RG010
            colP[5],#RG14
            colP[6])#RG17
namesLAI <- unique(data.frame(Plot=canopyLAINames$site_id, namePerc=canopyLAINames$namePerc))
namesLAI <- namesLAI %>%
  arrange(Plot)
namesLAIPlot <- namesLAI$namePerc
namesLAI$namesCommon <- c("sugar maple", "apple-buckthorn",
                 "norway spruce plantation", "apple-buckthorn",
                  "buckthorn", "norway maple-sugar maple-buckthorn")

namesJoinF <- left_join(canopyLAINames, namesLAI, by=c("site_id"="Plot"))


a <- ggplot(namesJoinF, aes(x=site_id, y=PAR_LAI, fill=namesCommon))+
  geom_boxplot()+
  xlab("Forest inventory plot")+
  ylab(expression(paste("Leaf area index (m"^2,""[leaf], " m"^-2,""[ground],")")))+
  theme_classic()+labs(fill="Dominant species composition")+ 
  theme(text = element_text(size = 15),axis.text.x = element_blank())+
  scale_fill_manual(values=c(rgb(204,121,167,maxColorValue=255), #reddish purple
                             rgb(230,159,0,maxColorValue=255), #orange
                             rgb(240,228,66,maxColorValue=255), # yellow
                             rgb(0,114,178,maxColorValue=255), #blue
                             rgb(0,158,115,maxColorValue=255) #blue green
                             ))
ggsave("/LAI.png"  ,
       a, path=dirFigures, width=8, height=5, units="in", dpi=300)

#Lai lidar
laiLid <- laiDF %>%
  filter(plotID == "RG01" |
           plotID == "RG03" |
           plotID == "RG06" |
           plotID == "RG10" |
           plotID == "RG14" |
           plotID == "RG17" )

laiLidN <- left_join(laiLid, namesLAI, by=c("plotID"="Plot"))

laiLID <- laiLidN %>%
  filter(lai !=0)
ggplot(laiLID, aes(x=plotID, y=lai, fill=namesCommon))+
  geom_boxplot()+
  xlab("Forest inventory plot")+
  ylab(expression(paste("LiDAR-derived leaf area index (m"^2,""[leaf], " m"^-2,""[ground],")")))+
  theme_classic()+labs(fill="Dominant species composition")+ 
  theme(text = element_text(size = 15),axis.text.x = element_blank())+
  scale_fill_manual(values=c(rgb(204,121,167,maxColorValue=255), #reddish purple
                             rgb(230,159,0,maxColorValue=255), #orange
                             rgb(240,228,66,maxColorValue=255), # yellow
                             rgb(0,114,178,maxColorValue=255), #blue
                             rgb(0,158,115,maxColorValue=255) #blue green
  ))

# to do: # lidar profiles to match


#NDVI (from drone), see about matching plots versus not

dronedir <- "K:/Environmental_Studies/hkropp/Private/drone"

p0719 <- c(rast(paste0(dronedir,"/flight07_19/Flight2_7_19_transparent_reflectance_blue.tif")),
           rast(paste0(dronedir,"/flight07_19/Flight2_7_19_transparent_reflectance_green.tif")),
           rast(paste0(dronedir,"/flight07_19/Flight2_7_19_transparent_reflectance_red.tif")),
           rast(paste0(dronedir,"/flight07_19/Flight2_7_19_transparent_reflectance_nir.tif")))

ndvi0719 <- (p0719[[4]]- p0719[[3]])/(p0719[[4]]+ p0719[[3]])
plot(ndvi0719)
plot(FI_plotsSF["Plot"],add=TRUE)

plots0719 <- crop(FI_plots,ndvi0719)
zone0719 <- extract(ndvi0719,plots0719)
IDtable0719 <- data.frame(ID=seq(1,nrow(values(plots0719))),
                         Plot=values(plots0719)$Plot)
values0719 <- na.omit(left_join(zone0719,IDtable0719, by="ID"))
values0719$flight <- rep("phantom 07/19", nrow(values0719))
colnames(values0719)[2] <- "NDVI"

m0719 <- c(rast(paste0(dronedir,"/mica_07_19_23/mica2_07_19_23_transparent_reflectance_blue.tif")),
           rast(paste0(dronedir,"/mica_07_19_23/mica2_07_19_23_transparent_reflectance_green.tif")),
           rast(paste0(dronedir,"/mica_07_19_23/mica2_07_19_23_transparent_reflectance_red.tif")),
           rast(paste0(dronedir,"/mica_07_19_23/mica2_07_19_23_transparent_reflectance_nir.tif")))

mndvi0719 <- (m0719[[4]]- m0719[[3]])/(m0719[[4]]+ m0719[[3]])
plot(mndvi0719)
plot(FI_plotsSF["Plot"],add=TRUE)
plotsm0719 <- crop(FI_plots,mndvi0719)
zonem0719 <- extract(mndvi0719,plotsm0719)
IDtablem0719 <- data.frame(ID=seq(1,nrow(values(plotsm0719))),
                          Plot=values(plotsm0719)$Plot)
valuesm0719 <- na.omit(left_join(zonem0719,IDtablem0719, by="ID"))
valuesm0719 <- valuesm0719  %>%
  filter(Plot != "RG08")
valuesm0719$flight <- rep("mica 07/19", nrow(valuesm0719))
colnames(valuesm0719)[2] <- "NDVI"

m0725 <- c(rast(paste0(dronedir,"/mica_07_25/mica_07_25_transparent_reflectance_blue.tif")),
           rast(paste0(dronedir,"/mica_07_25/mica_07_25_transparent_reflectance_green.tif")),
           rast(paste0(dronedir,"/mica_07_25/mica_07_25_transparent_reflectance_red.tif")),
           rast(paste0(dronedir,"/mica_07_25/mica_07_25_transparent_reflectance_nir.tif")))

mndvi0725 <- (m0725[[4]]- m0725[[3]])/(m0725[[4]]+ m0725[[3]])
plot(mndvi0725)
plot(FI_plotsSF["Plot"],add=TRUE)
plotsm0725 <- crop(FI_plots,mndvi0725)
zonem0725 <- extract(mndvi0725,plotsm0725)
IDtablem0725 <- data.frame(ID=seq(1,nrow(values(plotsm0725))),
                           Plot=values(plotsm0725)$Plot)
valuesm0725 <- na.omit(left_join(zonem0725,IDtablem0725, by="ID"))
valuesm0725 <- valuesm0725  %>%
  filter(Plot == "RG08")
valuesm0725$flight <- rep("mica 07/25", nrow(valuesm0725))
colnames(valuesm0725)[2] <- "NDVI"


m0726 <- c(rast(paste0(dronedir,"/mica_07_26/micafix_07_26_transparent_reflectance_blue.tif")),
           rast(paste0(dronedir,"/mica_07_26/micafix_07_26_transparent_reflectance_green.tif")),
           rast(paste0(dronedir,"/mica_07_26/micafix_07_26_transparent_reflectance_red.tif")),
           rast(paste0(dronedir,"/mica_07_26/micafix_07_26_transparent_reflectance_nir.tif")))

mndvi0726 <- (m0726[[4]]- m0726[[3]])/(m0726[[4]]+ m0726[[3]])
plot(mndvi0726)
plot(FI_plotsSF["Plot"],add=TRUE)
plotsm0726 <- crop(FI_plots,mndvi0726)
zonem0726 <- extract(mndvi0726,plotsm0726)
IDtablem0726 <- data.frame(ID=seq(1,nrow(values(plotsm0726))),
                           Plot=values(plotsm0726)$Plot)
valuesm0726 <- na.omit(left_join(zonem0726,IDtablem0726, by="ID"))
valuesm0726$flight <- rep("mica 07/26", nrow(valuesm0726))
colnames(valuesm0726)[2] <- "NDVI"


ndviAll <- rbind(values0719,valuesm0719,valuesm0725,valuesm0726)
ndviAllNames <- left_join(ndviAll, nameDF, by=c("Plot"))
unique(ndviAllNames$Plot)
unique(canopyLAINames$site_id)
NDVIcommon <- data.frame(namePerc = unique(ndviAllNames$namePerc),
                         commonName = c("apple-buckthorn",
                                        "buckthorn",
                                        "norway maple-sugar maple-buckthorn",
                                        "apple-buckthorn",
                                        "apple-buckthorn",
                                        "sugar maple-hemlock",
                                        "ash spp.",
                                        "white spruce plantation",
                                       "Locust-Buckthorn" ))

NDVIAllNames2 <- left_join(ndviAllNames, NDVIcommon, by="namePerc")

ggplot(NDVIAllNames2, aes(x=Plot, y=NDVI, fill=commonName))+
  geom_boxplot()

ndviSub <- NDVIAllNames2 %>%
  filter(Plot == "RG10" | Plot == "RG03" | Plot == "RG17" | Plot == "RG08" | Plot == "RG14" | Plot == "RG09")


colorN <- c(colP[2],#RG03
            colP[7],#RG08
            colP[8],#RG09
            colP[4],#RG010
            colP[5],#RG14
            colP[6])#RG17
namesNDVI <- unique(data.frame(Plot=ndviSub$Plot, namePerc=ndviSub$namePerc))
namesNDVI <- namesNDVI %>%
  arrange(Plot)
namesNDVIPlot <- namesNDVI$namePerc

b <- ggplot(ndviSub, aes(x=Plot, y=NDVI, fill=commonName))+
  geom_boxplot(outlier.shape=NA)+
  xlab("Forest inventory plot")+
  ylab("sUAS NDVI (-)")+
  theme_classic()+
  labs(fill="Dominant species composition")+ylim(0.5,1)+
   theme(text = element_text(size = 15),axis.text.x = element_blank())+
  scale_fill_manual(values=c(rgb(204,121,167,maxColorValue=255), #reddish purple
                             rgb(230,159,0,maxColorValue=255), #orange
                             rgb(240,228,66,maxColorValue=255), # yellow
                             rgb(0,158,115,maxColorValue=255),
                            rgb(0,114,178,maxColorValue=255))) #blue


ggsave("/NDVI.png"  ,
       b, path=dirFigures, width=8, height=5, units="in", dpi=300)
  




plot(seq(1,8),seq(1,8),pch=19, col=colP)



# plots for lidar

plotLidar <- c("RG01", "RG03","RG06","RG10","RG14","RG17")

PlotName <- nameDF %>%
  filter(Plot %in% plotLidar)

namesPLi <- PlotName$namePerc

test <- terra::values(laiPlot[[1]])
plot(laiPlot[[1]])

LidarLAIDF <- list()
for(i in 1:length(plotLidar)){
  j = which(plotAll == plotLidar[i])
  LidarLAIDF[[i]] = data.frame(LAI = terra::values(laiPlot[[j]]),
                          Plot = rep(plotLidar[i], length(terra::values(laiPlot[[j]]))))
  
}


lidarLDF <- do.call("rbind", LidarLAIDF)
lidarLDF <- lidarLDF %>%
  filter(LAI > 0)


colorLi <- c(colP[1],#RG01
             colP[2],#RG03
             colP[3],#RG06
             colP[4],#RG010
             colP[5],#RG14
             colP[6])#RG17


  

ggplot(lidarLDF, aes(x=Plot, y=LAI, fill=Plot))+
  geom_boxplot()+
  xlab("Forest inventory plot")+
  ylab(expression(paste("Leaf area index (m"^2,""[leaf], " m"^-2,""[ground],")")))+
  scale_fill_manual(values=colorLi,
                    labels=namesPLi)+theme_classic()+labs(fill="Dominant species composition")+ theme(text = element_text(size = 15)) 



plot(c(0,7), c(0,35), type="n", axes=FALSE)
for(i in 1:length(plotLidar)){
  j = which(plotAll == plotLidar[i])
  polygon(c(i+ladPlot[[j]]$lad, i-rev(ladPlot[[j]]$lad)),c(ladPlot[[j]]$height,rev(ladPlot[[j]]$height)), col=colorLi[i])
}
axis(1, seq(1,6), labels=plotLidar)



# LAD plots



ladPlot <- inner_join(ladAll, namesLAI, by=c("plotID"="Plot"))


ggplot(ladPlot, aes(x=height, y=lad, color=namesCommon))+
  geom_line(size=2)+
  theme_classic()+
  scale_fill_manual(values=c(rgb(204,121,167,maxColorValue=255), #reddish purple
                             rgb(230,159,0,maxColorValue=255), #orange
                             rgb(240,228,66,maxColorValue=255), # yellow
                             rgb(0,158,115,maxColorValue=255),
                             rgb(0,114,178,maxColorValue=255))) #blue

ggplot(ladPlot, aes(x=height, ymax=lad, ymin=0, fill=namesCommon,
                    color=namesCommon))+
  geom_ribbon(alpha=0.25)+
  geom_line(data=ladPlot,aes(x=height, y=lad, color=namesCommon), size=1)+
  theme_classic()+
  scale_fill_manual(values=c(rgb(204,121,167,maxColorValue=255), #reddish purple
                             rgb(230,159,0,maxColorValue=255), #orange
                             rgb(240,228,66,maxColorValue=255), # yellow
                             rgb(0,158,115,maxColorValue=255),
                             rgb(0,114,178,maxColorValue=255)))+ #blue
  scale_color_manual(values=c(rgb(204,121,167,maxColorValue=255), #reddish purple
                             rgb(230,159,0,maxColorValue=255), #orange
                             rgb(240,228,66,maxColorValue=255), # yellow
                             rgb(0,158,115,maxColorValue=255),
                             rgb(0,114,178,maxColorValue=255)))+ 
  labs(x="Canopy height",y="Density",fill="Dominant species composition", color="Dominant species composition")+
  theme(text = element_text(size = 15))
