library(terra)


dir <- "K:/Environmental_Studies/hkropp/GIS/drone/campus/2023/mica_07_19/flight"


filesR <- list.files(paste0(dir), pattern="_3")


fileN <- character()
for(i in 1:length(filesR)){
  fileN[i] <- strsplit(filesR[i], "_")[[1]][[2]]
}

RGB <- rast(paste0(dir,"/IMG_",fileN[1],"_3.tif"))

Rcomp <- list()
Gcomp <- list()
Bcomp <- list()

for(i in 1:length(filesR)){
  Rcomp[[i]] <- rast(paste0(dir,"/IMG_",fileN[i],"_3.tif"))
  Gcomp[[i]] <-  rast(paste0(dir,"/IMG_",fileN[i],"_2.tif"))    
  Bcomp[[i]] <-  rast(paste0(dir,"/IMG_",fileN[i],"_1.tif"))
}

scaleR <- list()
for(i in 1:length(filesR)){
  scaleR[[i]] <- stretch(Rcomp[[i]], datatype='INT1U')
}

scaleB <- list()
for(i in 1:length(filesR)){
  scaleB[[i]] <- stretch(Bcomp[[i]], datatype='INT1U')
}

scaleG <- list()
for(i in 1:length(filesR)){
  scaleG[[i]] <- stretch(Gcomp[[i]], datatype='INT1U')
}

RGBi<- list()
for(i in 1:length(filesR)){
  RGBi[[i]] <- c(scaleR[[i]],
                 scaleG[[i]],
                 scaleB[[i]]) 
}
for(i in 1:length(filesR)){
writeRaster(RGBi[[i]], paste0("K:/Environmental_Studies/hkropp/Private/sfm/RGB_test/RGB_",i,".tif"))
}
plotRGB(RGBi[[400]], r=1, g=2, b=3)
plot( Rcomp[[400]], col=grey(1:100/100))

"K:/Environmental_Studies/hkropp/GIS/drone/campus/2023/mica_07_19/
