#global.R

source("loadData.R")
source("loadItemDB.R")
source("parseItems.R")
source("parseMonsters.R")
source("parseEnchants.R")
source("compileUniques.R")
source("loadOptionSet.R")
source("uniqueEnchants.R")
source("loadShadowMissionsTest.R")
source("addShadowMonsters.R")


lootTable <- loadData("itemdroptype.json")
monsters <- loadShadowMissions("E:\\Studying\\R\\Mabinogi Loot Table\\shadowmonsters.xml")
missions <- loadShadowMissions("E:\\Studying\\R\\Mabinogi Loot Table\\shadowmissions.xml")
lootTable <- addShadowMonsters()
missions <- addShadowMissions


itemDB <- loadItemDB("itemdb.xml", compileUniques("id:",lootTable[,6:8]))
optionSet <- loadOptionSet("optionset.xml", uniqueEnchants(lootTable))
itemTable <- itemDB$id
names(itemTable) <- itemDB$name