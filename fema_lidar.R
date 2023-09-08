library(lidR)
library(future)
library(dplyr)
library(terra)
library(sf)

l1 <- readLAS("C:/Users/hkropp/Documents/ArcGIS/Projects/Lidar/data/l2019/18TVN650670.las")

plot(l1)

l2 <- readLAS("C:/Users/hkropp/Documents/ArcGIS/Projects/Lidar/data/l2019/18TVN665670.las")

plot(l2)
hist(l2@data$Z)
las <- filter_poi(l2, Z >= 188, Z <= 285)

plot(las, size=2, bg="white")
hist(las@data$Z)


dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
plot(dtm_tin)
plot_dtm3d(dtm_tin, bg = "white")


nlas <- las - dtm_tin
plot(nlas, size = 4, bg = "white")

nclip <- clip_rectangle(nlas, 466700,4767250,467800,4767900)

chm_p2r_05 <- rasterize_canopy(nclip, 0.5, p2r(subcircle = 0.2), pkg = "terra")
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
plot(chm_p2r_05_smoothed)
plot(FI_plots)
plot(chm_p2r_05_smoothed, add=TRUE, legend=FALSE)

plot(chm_p2r_05_smoothed)
plot(FI_plots["Plot"], add=TRUE)

RG01 <- subset(FI_plots, FI_plots$Plot == "RG01")



chmRG01c <- crop(chm_p2r_05_smoothed, RG01)
chmRG01 <- mask(chmRG01c, RG01)

Plots <- sf::st_as_sf(FI_plots)
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
treeRG03 <- clip_roi(nclip, Plots %>% filter(Plot=="RG03"))
lidR::plot(treeRG03, bg = "white", size = 2)

hist(treeRG03$Z)
