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
maxL <- numeric()
for(i in 1:length(filesR)){
min(values(Rcomp[[2]]))
  max(values(Bcomp[[4]]))
  
}
