#global.R

source("loadData.R")
source("loadItemDB.R")
source("loadShadowMissionsTest.R")
source("loadOptionSet.R")
source("parseItems.R")
source("parseMonsters.R")
source("parseEnchants.R")
source("compileUniques.R")
source("uniqueEnchants.R")
source("addShadowMonsters.R")
source("addShadowMissions.R")
source("loadTranslations.R")
source("createSMTables.R")


lootTable <- loadData("itemdroptype.json")
monsters <- loadShadowMissions("E:\\Studying\\R\\Mabinogi Loot Table\\shadowmonsters.xml")
missions <- loadShadowMissions("E:\\Studying\\R\\Mabinogi Loot Table\\shadowmissions.xml")
lootTable <- addShadowMonsters() #Table now includes monsters
missions <- addShadowMissions() #mission componenets
shadowMissions <- createSMTables() #Actual missions

#removes basic missions from loot table since missions are their own table
lootTable <- lootTable[!(lootTable$ID %in% unique(missions$ID)),]

#Removes foreign characters
lootTable$Name <- iconv(lootTable$Name, "latin1", "ASCII", sub="waddafug")
lootTable <- lootTable[!grepl("waddafug", lootTable$Name),]


combinedList <- c(unlist(lootTable[,6]), unlist(lootTable[,7]), unlist(lootTable[,8]), unlist(shadowMissions$Bonus), unlist(shadowMissions[,6:8]))

itemDBJapan <- loadTranslations("E:\\Studying\\R\\Mabinogi Loot Table\\itemdbconverted.txt")
itemDB <- loadItemDB("itemdb.xml", compileUniques("id:",combinedList))
optionSet <- loadOptionSet("optionset.xml", uniqueEnchants(combinedList))
itemTable <- itemDB$id
names(itemTable) <- itemDB$name