library(lidR)
library(future)
library(terra)
dirDat <- "/Users/hkropp/Documents/offline/Healy/NEON_lidar-point-cloud-line/NEON.D19.HEAL.DP1.30003.001.2018-08.basic.20240701T225544Z.RELEASE-2024/"
dirSave <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/Healy_ET/2024/LiDAR/2018"

l1 <- readLAS(paste0(dirDat,"NEON_D19_HEAL_DP1_388000_7081000_classified_point_cloud_colorized.laz"),
              filter = "-drop_z_below 0")
range(l1@data$Z)
hist(l1@data$Z)
plot(l1, color="RGB", bg="white")

las <- filter_poi(l1, Z >= 750, Z <= 1050)
plot(las, color="RGB", bg="white")
hist(las@data$Z)

dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "white")


nlas <- las - dtm_tin
plot(nlas, size = 4, bg = "white")

chm <- rasterize_canopy(nlas, 0.5, pitfree(subcircle = 0.2))
plot(chm)
writeRaster(chm, paste0(dirSave, "/NEON_D19_HEAL_DP1_388000_7081000.tif"))


l1 <- readLAS(paste0(dirDat,"NEON_D19_HEAL_DP1_388000_7086000_classified_point_cloud_colorized.laz"),
              filter = "-drop_z_below 0")
range(l1@data$Z)
hist(l1@data$Z)

las <- filter_poi(l1, Z >= 600, Z <= 700)
plot(las, color="RGB", bg="white")
hist(las@data$Z)

dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "white")


nlas <- las - dtm_tin
plot(nlas, size = 4, bg = "white")

chm <- rasterize_canopy(nlas, 0.5, pitfree(subcircle = 0.2))
plot(chm)
writeRaster(chm, paste0(dirSave, "/NEON_D19_HEAL_DP1_388000_7086000.tif"))


# just a strip
l1 <- readLAS(paste0(dirDat,"NEON_D19_HEAL_DP1_389000_7079000_classified_point_cloud_colorized.laz"),
              filter = "-drop_z_below 0")
range(l1@data$Z)
hist(l1@data$Z)
plot(l1, color="RGB", bg="white")


las <- filter_poi(l1, Z >= 800, Z <= 1300)
plot(las, color="RGB", bg="white")
hist(las@data$Z)

dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "white")


nlas <- las - dtm_tin
plot(nlas, size = 4, bg = "white")

chm <- rasterize_canopy(nlas, 0.5, pitfree(subcircle = 0.2))
plot(chm)
writeRaster(chm, paste0(dirSave, "/NEON_D19_HEAL_DP1_388000_7086000.tif"))


l1 <- readLAS(paste0(dirDat,"NEON_D19_HEAL_DP1_389000_7087000_classified_point_cloud_colorized.laz"),
              filter = "-drop_z_below 0")
range(l1@data$Z)
hist(l1@data$Z)


las <- filter_poi(l1, Z >= 600, Z <= 800)
plot(las, color="RGB", bg="white")
hist(las@data$Z)

dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "white")


nlas <- las - dtm_tin
plot(nlas, size = 4, bg = "white")

chm <- rasterize_canopy(nlas, 0.5, pitfree(subcircle = 0.2))
plot(chm)
writeRaster(chm, paste0(dirSave, "/NEON_D19_HEAL_DP1_389000_7087000.tif"))

l1 <- readLAS(paste0(dirDat,"NEON_D19_HEAL_DP1_389000_7091000_classified_point_cloud_colorized.laz"),
              filter = "-drop_z_below 0")
range(l1@data$Z)
hist(l1@data$Z)


las <- filter_poi(l1, Z >= 600, Z <= 800)
plot(las, color="RGB", bg="white")
hist(las@data$Z)

dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())


nlas <- las - dtm_tin

chm <- rasterize_canopy(nlas, 0.5, pitfree(subcircle = 0.2))
plot(chm)
writeRaster(chm, paste0(dirSave, "/NEON_D19_HEAL_DP1_389000_7091000.tif"))


l1 <- readLAS(paste0(dirDat,"NEON_D19_HEAL_DP1_390000_7081000_classified_point_cloud_colorized.laz"),
              filter = "-drop_z_below 0")
range(l1@data$Z)
hist(l1@data$Z)


las <- filter_poi(l1, Z >= 750, Z <= 1100)
plot(las, color="RGB", bg="white")
hist(las@data$Z)

dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())


nlas <- las - dtm_tin

chm <- rasterize_canopy(nlas, 0.5, pitfree(subcircle = 0.2))
plot(chm)
writeRaster(chm, paste0(dirSave, "/NEON_D19_HEAL_DP1_390000_7081000.tif"))


l1 <- readLAS(paste0(dirDat,"NEON_D19_HEAL_DP1_390000_7090000_classified_point_cloud_colorized.laz"),
              filter = "-drop_z_below 0")
range(l1@data$Z)
hist(l1@data$Z)


las <- filter_poi(l1, Z >= 550, Z <= 900)
plot(las, color="RGB", bg="white")
hist(las@data$Z)

dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())


nlas <- las - dtm_tin

chm <- rasterize_canopy(nlas, 0.5, pitfree(subcircle = 0.2))
plot(chm)
writeRaster(chm, paste0(dirSave, "/NEON_D19_HEAL_DP1_390000_7090000.tif"))




l1 <- readLAS(paste0(dirDat,"NEON_D19_HEAL_DP1_391000_7083000_classified_point_cloud_colorized.laz"),
              filter = "-drop_z_below 0")
range(l1@data$Z)
hist(l1@data$Z)

# filter out outliers
las <- filter_poi(l1, Z >= 700, Z <= 900)
plot(las, color="RGB", bg="white")
hist(las@data$Z)
# get digital terrain model
dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())

# height of surface
nlas <- las - dtm_tin
# canopy height raster
chm <- rasterize_canopy(nlas, 0.5, pitfree(subcircle = 0.2))
plot(chm)
writeRaster(chm, paste0(dirSave, "/NEON_D19_HEAL_DP1_391000_7083000.tif"))

l1 <- readLAS(paste0(dirDat,"NEON_D19_HEAL_DP1_393000_7081000_classified_point_cloud_colorized.laz"),
              filter = "-drop_z_below 0")
range(l1@data$Z)
hist(l1@data$Z)

# filter out outliers
las <- filter_poi(l1, Z >= 700, Z <= 1000)
plot(las, color="RGB", bg="white")
hist(las@data$Z)
# get digital terrain model
dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())

# height of surface
nlas <- las - dtm_tin
# canopy height raster
chm <- rasterize_canopy(nlas, 0.5, pitfree(subcircle = 0.2))
plot(chm)
writeRaster(chm, paste0(dirSave, "/NEON_D19_HEAL_DP1_393000_7081000.tif"))

####### old tree class -----

l2 <- readLAS("/Users/hkropp/Documents/offline/Healy/NEON_lidar-point-cloud-line/NEON.D19.HEAL.DP1.30003.001.2018-08.basic.20240701T225544Z.RELEASE-2024/NEON_D19_HEAL_DP1_391000_7083000_classified_point_cloud_colorized.laz"
              , filter = "-drop_z_below 0")
range(l2@data$Z)
hist(l2@data$Z)
plot(l2, color="RGB", bg="white")

las2 <- filter_poi(l2, Z >= 600, Z <= 800)
plot(las2, color="RGB", bg="white")
hist(las2@data$Z)

dtm_tin <- rasterize_terrain(las2, res = 1, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "white")
nlas2 <- las2 - dtm_tin
chm2 <- rasterize_canopy(nlas2, 0.5, pitfree(subcircle = 0.2))
plot(chm2)

ttops <- locate_trees(nlas2, lmf(ws=30))
plot(ttops)
x <- plot(nlas2, color="RGB", size = 4, bg = "white")
add_treetops3d(x, ttops)
chm_p2r_05 <- rasterize_canopy(nlas2, 0.5, p2r(subcircle = 0.2), pkg = "terra")
plot(chm_p2r_05)
kernel <- matrix(1,3,3)
chm_p2r_05_smoothed <- terra::focal(chm_p2r_05, w = kernel, fun = median, na.rm = TRUE)
ttops_chm_p2r_05 <- locate_trees(chm_p2r_05, lmf(5))
ttops_chm_p2r_05_smoothed <- locate_trees(chm_p2r_05_smoothed, lmf(5))

algo <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
tlas <- segment_trees(nlas2, algo) # segment point cloud
plot(tlas, bg = "white", size = 2, color = "treeID") 
