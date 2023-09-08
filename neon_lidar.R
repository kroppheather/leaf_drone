install.packages(c("ggplot2","dplyr","lubridate"))
install.packages("forestr")
library(forestr)
install.packages("lidR")
library(lidR)


l1 <- readLAS("C:/Users/hkropp/Documents/Lidar/Forest_neon/NEON_lidar-point-cloud-line/NEON.D01.BART.DP1.30003.001.2019-08.basic.20230908T144943Z.RELEASE-2023/NEON_D01_BART_DP1_313000_4875000_classified_point_cloud_colorized.laz"
              , filter = "-drop_z_below 0")



range(l1@data$Z)
hist(l1@data$Z)
las <- filter_poi(l1, Z >= 350, Z <= 461)
hist(las@data$Z)

plot(l1, color="RGB", bg="white")
plot(las, color="RGB", bg="white")


ttops <- locate_trees(las, lmf(ws=5))

x <-plot(las, color="RGB", bg="white")
add_treetops3d(x, ttops)


chm <- rasterize_canopy(las, 0.5, pitfree(subcircle = 0.2))

plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)

algo <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
las <- segment_trees(las, algo) # segment point cloud
plot(las, bg = "white", size = 4, color = "treeID") # visualize trees


# Point-to-raster 2 resolutions
chm_p2r_05 <- rasterize_canopy(las, 0.5, p2r(subcircle = 0.2), pkg = "terra")
chm_p2r_1 <- rasterize_canopy(las, 1, p2r(subcircle = 0.2), pkg = "terra")

# Pitfree with and without subcircle tweak
chm_pitfree_05_1 <- rasterize_canopy(las, 0.5, pitfree(), pkg = "terra")
chm_pitfree_05_2 <- rasterize_canopy(las, 0.5, pitfree(subcircle = 0.2), pkg = "terra")

# Post-processing median filter
kernel <- matrix(1,3,3)
chm_p2r_05_smoothed <- terra::focal(chm_p2r_05, w = kernel, fun = median, na.rm = TRUE)
chm_p2r_1_smoothed <- terra::focal(chm_p2r_1, w = kernel, fun = median, na.rm = TRUE)
