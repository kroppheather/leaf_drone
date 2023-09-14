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
maxR <- numeric()
minR <- numeric()
maxB <- numeric()
minB <- numeric()
maxG <- numeric()
minG <- numeric()

for(i in 1:length(filesR)){
  minR[i] <- min(values(Rcomp[[i]]))
  maxR[i] <- max(values(Rcomp[[i]]))
  minB[i] <- min(values(Bcomp[[i]]))
  maxB[i] <- max(values(Bcomp[[i]]))
  minG[i] <- min(values(Gcomp[[i]]))
  maxG[i] <- max(values(Gcomp[[i]]))
  
}
exF <- function(x){
  min(values(x))
}
minR <- lapply(Rcomp,exF)
minRv <- do.call("rbind", minR)
minB <- lapply(Bcomp,exF)
minBv <- do.call("rbind", minB)
minG <- lapply(Gcomp,exF)
minGv <- do.call("rbind", minG)


maxF <- function(x){
  max(values(x))
}

maxR <- lapply(Rcomp,maxF)
maxRv <- do.call("rbind", maxR)
maxB <- lapply(Bcomp,maxF)
maxBv <- do.call("rbind", maxB)
maxG <- lapply(Gcomp,maxF)
maxGv <- do.call("rbind", maxG)


Rmax <- as.numeric(max(maxRv))
Gmax <- as.numeric(max(maxGv))
Bmax <- as.numeric(max(maxBv))

Rmin <- as.numeric(max(minRv))
Gmin <- as.numeric(max(minGv))
Bmin <- as.numeric(max(minBv))

scaleF <- function(x, min,max){
  (x-min)*/
}

for(i in 1:length(filesR)){
scaleR <- app(Rcomp[[i]])



