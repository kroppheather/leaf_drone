install.packages(c("ggplot2","dplyr","lubridate"))
install.packages("lidR")
library(lidR)
library(future)

l1 <- readLAS("C:/Users/hkropp/Documents/Lidar/Forest_neon/NEON_lidar-point-cloud-line/NEON.D01.BART.DP1.30003.001.2019-08.basic.20230908T144943Z.RELEASE-2023/NEON_D01_BART_DP1_313000_4875000_classified_point_cloud_colorized.laz"
              , filter = "-drop_z_below 0")



range(l1@data$Z)
hist(l1@data$Z)
las <- filter_poi(l1, Z >= 350, Z <= 461)
hist(las@data$Z)

plot(l1, color="RGB", bg="white")
plot(las, color="RGB", bg="white")





x <-plot(las, color="RGB", bg="white")


dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "white")

nlas <- las - dtm_tin
plot(nlas, size = 4, bg = "white")
plot(nlas, color="RGB",size = 4, bg = "white")


las2 <- readLAS("C:/Users/hkropp/Documents/Lidar/Forest_neon/NEON_lidar-point-cloud-line/NEON.D01.BART.DP1.30003.001.2019-08.basic.20230908T144943Z.RELEASE-2023/NEON_D01_BART_DP1_313000_4875000_classified_point_cloud_colorized.laz", select = "xyzr", filter = "-drop_z_below 0")
las3 <-filter_poi(las2, Z >= 350, Z <= 461)

chm <- rasterize_canopy(nlas, 0.5, pitfree(subcircle = 0.2))

ttops <- locate_trees(nlas, lmf(ws=30))


plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)

x <- plot(nlas, size = 4, bg = "white")
add_treetops3d(x, ttops)

chm_p2r_05 <- rasterize_canopy(nlas, 0.5, p2r(subcircle = 0.2), pkg = "terra")
chm_p2r_1 <- rasterize_canopy(nlas, 1, p2r(subcircle = 0.2), pkg = "terra")


# Post-processing median filter
kernel <- matrix(1,3,3)
chm_p2r_05_smoothed <- terra::focal(chm_p2r_05, w = kernel, fun = median, na.rm = TRUE)
chm_p2r_1_smoothed <- terra::focal(chm_p2r_1, w = kernel, fun = median, na.rm = TRUE)

ttops_chm_p2r_05 <- locate_trees(chm_p2r_05, lmf(5))
ttops_chm_p2r_1 <- locate_trees(chm_p2r_1, lmf(5))
ttops_chm_p2r_05_smoothed <- locate_trees(chm_p2r_05_smoothed, lmf(5))
ttops_chm_p2r_1_smoothed <- locate_trees(chm_p2r_1_smoothed, lmf(5))

algo <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
tlas <- segment_trees(nlas, algo) # segment point cloud
plot(tlas, bg = "white", size = 2, color = "treeID") # visualize trees

ttops_chm_p2r_05_smoothed2 <- locate_trees(chm_p2r_05_smoothed, lmf(ws=30))

algo2 <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed2)
tlas2 <- segment_trees(nlas, algo2) # segment point cloud
plot(tlas2, bg = "white", size = 2, color = "treeID") # visualize trees


x <- plot(nlas, size = 4, bg = "white")
add_treetops3d(x, ttops)

tclip <- clip_rectangle(tlas2, 313400,4875000,313800,4875600)
plot(tclip, bg = "white", size = 2, color = "treeID") # visualize trees
unique(tclip$treeID)

tree <- filter_poi(tclip, treeID == 555)
plot(tree, bg = "white", size = 4) # visualize trees

tree <- filter_poi(tclip, treeID >= 506, treeID <= 600)
plot(tree, bg = "white", size = 2, color ="RGB") # visualize trees

nclip <- clip_rectangle(nlas, 313400,4875000,313800,4875600)

plot(nclip, size = 2, bg = "white", color="RGB")



### forest metrics
install.packages("treetop")


library(treetop)
launchApp(launch.browser = TRUE)
library(terra)
writeRaster(chm_p2r_05_smoothed, "C:/Users/hkropp/Documents/Lidar/Forest_neon/BART_DP1_313000_4875000_CHM.tif")

launchApp(launch.browser = TRUE)


library(gapfraction)