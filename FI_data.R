library(dplyr)


SpeciesInfo <- read.csv("K:/Environmental_Studies/hkropp/Private/canopy/speciesID.csv")
forestInventory <- read.csv("K:/Environmental_Studies/hkropp/Private/canopy/HCEF forest inventory data 24.csv")

AshPlots <- forestInventory %>%
  filter(Species == "FRAM" | Species == "FRPE")

AshPlots$plotID <- as.numeric(gsub("RG","", AshPlots$Plot))
AshPlotsF <- AshPlots %>%
  arrange(plotID)
