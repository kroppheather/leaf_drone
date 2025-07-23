library(dplyr)


SpeciesInfo <- read.csv("K:/Environmental_Studies/hkropp/Private/canopy/speciesID.csv")
forestInventory <- read.csv("K:/Environmental_Studies/hkropp/Private/canopy/HCEF forest inventory data 24.csv")

AshPlots <- forestInventory %>%
  filter(Species == "FRAM" | Species == "FRPE")

AshPlots$plotID <- as.numeric(gsub("RG","", AshPlots$Plot))
AshPlotsF <- AshPlots %>%
  arrange(plotID)

AshPlotsL <- AshPlots %>%
  filter(Dead == "N")

write.csv(AshPlotsF, "K:/Environmental_Studies/hkropp/Private/canopy/ash survey sheet.csv")

RG19 <- forestInventory %>% filter(Plot == "RG19")
