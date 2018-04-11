#global.R


source("loadFunctions.R")
source("parsingFunctions.R")
source("otherFunctions.R")

#This order ensures that each subsequent function has the pre-requisite data loaded
lootTable <- loadData("files\\itemdroptype.json")
monsters <- loadShadowMissions("files\\shadowmonsters.xml")
missions <- loadShadowMissions("files\\shadowmissions.xml")
lootTable <- addShadowMonsters() #Table now includes monsters
missions <- addShadowMissions() #mission componenets
shadowMissions <- createSMTables("files\\shadowMissionsCSV.csv") #Actual missions

#removes basic missions from loot table since missions are their own table
lootTable <- lootTable[!(lootTable$ID %in% unique(missions$ID)),]

#Removes all rows containing foreign characters
lootTable$Name <- iconv(lootTable$Name, "latin1", "ASCII", sub="waddafug")
lootTable <- lootTable[!grepl("waddafug", lootTable$Name),]

#creates a list of all types of IDs
combinedList <- c(unlist(lootTable[,6]), unlist(lootTable[,7]), unlist(lootTable[,8]), unlist(shadowMissions$Bonus), unlist(shadowMissions[,6:8]))

#loads translations, and then loads all pertinent items and enchants
itemDBJapan <- loadTranslations("files\\itemdbconverted.txt")
itemDB <- loadItemDB("files\\itemdb.xml", compileUniques("id:",combinedList))
optionSet <- loadOptionSet("files\\optionset.xml", uniqueEnchants(combinedList))

#Creates a vector of the itemDB for Shiny to use
itemTable <- itemDB$id
names(itemTable) <- itemDB$name