library(dplyr)


SpeciesInfo <- read.csv("K:/Environmental_Studies/hkropp/Private/canopy/speciesID.csv")
forestInventory <- read.csv("K:/Environmental_Studies/hkropp/Private/canopy/HCEF forest inventory data 7_25_25.csv")

AshPlots <- forestInventory %>%
  filter(Species == "FRAM" | Species == "FRPE")

AshPlots$plotID <- as.numeric(gsub("RG","", AshPlots$Plot))
AshPlotsF <- AshPlots %>%
  arrange(plotID)

AshPlotsL <- AshPlots %>%
  filter(Dead == "N")

write.csv(AshPlotsF, "K:/Environmental_Studies/hkropp/Private/canopy/ash survey sheet.csv")
maplePlots <- forestInventory %>%
  filter(Species == "ACSA" & Dead =="N"& DBH.cm >5 )

mapleCount <- maplePlots %>%
  group_by(Plot) %>%
  summarize(n_maple = n())


RG19 <- forestInventory %>% filter(Plot == "RG19")

forestInventory25 <- read.csv("K:/Environmental_Studies/hkropp/Private/canopy/HCEF forest inventory data 7_25.csv")
unique(forestInventory25$Dead)
inventory <- forestInventory25 %>%
  filter(Dead == "N")
write.csv(inventory,  "K:/Environmental_Studies/hkropp/Private/canopy/inv_25_for_qa.csv")
