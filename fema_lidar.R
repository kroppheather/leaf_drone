library(lidR)
library(future)
library(dplyr)
library(terra)
library(sf)
library(leafR)

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

nclip <- clip_rectangle(nlas, 466500,4768250,467400,4767900)

chm_p2r_05 <- rasterize_canopy(nlas, 0.5, p2r(subcircle = 0.2), pkg = "terra")
plot(chm_p2r_05)

kernel <- matrix(1,3,3)
chm_p2r_05_smoothed <- terra::focal(chm_p2r_05, w = kernel, fun = median, na.rm = TRUE)

ttops_chm_p2r_05 <- locate_trees(chm_p2r_05, lmf(5))
ttops_chm_p2r_05_smoothed <- locate_trees(chm_p2r_05_smoothed, lmf(5))

algo <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
tlas <- segment_trees(nclip, algo) # segment point cloud
plot(tlas, bg = "white", size = 2, color = "treeID") # visualize trees

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


library(treetop)
launchApp(launch.browser = TRUE)

hist(treeRG01$Z)

RG03 <- subset(FI_plots, FI_plots$Plot == "RG03")
chmRG03c <- crop(chm_p2r_05_smoothed, RG03)
chmRG03 <- mask(chmRG03c, RG03)
plot(chmRG03)
lasRG03 <- clip_roi(nclip, Plots %>% filter(Plot=="RG03"))
treeRG03 <- clip_roi(tlas, Plots %>% filter(Plot=="RG03"))
plot(lasRG03 , bg = "white", size = 2)
plot(treeRG03, bg = "white", size = 3, color="treeID")
hist(treeRG03$Z)

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

voxelsG03 <- lad.voxels("C:/Users/hkropp/Documents/Lidar/plots/RG03.laz")
ladG03 <- lad.profile(voxelsG03)
laiG03 <- lai.raster(ladG03)

voxelsPlot <- lad.voxels(paste0("C:/Users/hkropp/Documents/Lidar/plots/",plotAll[i],".laz"))
for(i in 1:length(plotAll)){
  voxelsPlot[[i]] <- lad.voxels(paste0("C:/Users/hkropp/Documents/Lidar/plots/",plotAll[i],".laz"))
}

ladPlot <- list()
laiPlot <- list()
for(i in 1:length(plotAll)){
  ladPlot[[i]] <- lad.profile(lad.voxels(paste0("C:/Users/hkropp/Documents/Lidar/plots/",plotAll[i],".laz")))
}

laiPlot <- list()
for(i in 1:length(plotAll)){
  laiPlot[[i]] <- lai.raster(voxelsPlot[[i]])
}
plot(laiPlot[[15]])




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
  select(Species,Plot)

plotsI <- unique(PlotSpec$Plot)

PlotComp <- left_join(PlotSpec, SpeciesInfo, by=c("Species"="Code"))

NameSub <- character()
for(i in 1:nrow(PlotComp)){
  NameSub[i] <- paste0(substr(strsplit(PlotComp$Species.y[i], " ")[[1]][1],1,1), ". ",
                         strsplit(PlotComp$Species.y[i], " ")[[1]][2])
}

PlotComp$NameSub <- NameSub
PlotComp$Name <- ifelse(PlotComp$Genus =="Malus", "Malus sp.", PlotComp$NameSub )   

pasteSub <- character()
namecomp <- character()
for(i in 1:length(plotsI)){
  pasteSub <- PlotComp$Name[PlotComp$Plot == plotsI[i]]
  namecomp[i] <- paste(pasteSub,  collapse = ", ")
}
namecomp

nameDF <- data.frame(Plot = plotsI, Names = namecomp)

canopyLAINames <- left_join(canopyLAI, nameDF, by=c("site_id"="Plot"))

library(ggplot2)
ggplot(canopyLAINames, aes(x=site_id, y=PAR_LAI, fill=Names))+
  geom_boxplot()

# to do: # lidar profiles to match


#NDVI (from drone), see about matching plots versus not



