#Here are all the functions that load data from other files


#---------------------------------------------------------------------------------------------------------

#loads in all the monster data from the "itemdroptype.json" into a data frame
#ID, Name, Drop1Rate, Drop2Rate, Drop3Rate, Drop1, Drop2, Drop3

loadData <- function(filepath){
  
  #parses the JSOn information
  dropTable <- fromJSON(filepath, simplifyVector = TRUE)
  
  #initializes the data frame
  lootTable <- data.frame("ID"="ID", "Rebalanced"=0, "Name"="Name", "Drop1Rate"=0, "Drop2Rate"=0, "Drop3Rate"=0,
                          drop1 = NA, drop2 = NA, drop3 = NA, stringsAsFactors = FALSE)
  
  #creates temporary holder for row binds
  row <- vector(length = 9, mode="list")
  
  
  for(i in 1:length(dropTable$ItemDropTypeList)){
    
    #Grabs the ID, Rebalanced, Name, Drop1Rate, Drop2Rate, and Drop3Rate from the json file
    row[1:6] <- unlist(dropTable$ItemDropTypeList[[i]][c(1, 2, 10, 3, 8, 4)])

    #Grabs the first list of items for each category if they exist, applies NA otherwise. Removes parentheses and
    #the semicolon delimiter.
    ifelse(dropTable$ItemDropTypeList[[i]]$Drop1==""| dropTable$ItemDropTypeList[[i]]$Drop1==";",
           row[[7]] <- NA,
           row[[7]]<- list(strsplit( strsplit(gsub("\\(|\\)", "", dropTable$ItemDropTypeList[[i]][[5]]), split=";")[[1]], split=" ")))
    
    ifelse(dropTable$ItemDropTypeList[[i]]$Drop2==""| dropTable$ItemDropTypeList[[i]]$Drop2==";", 
           row[[8]] <- NA, 
           row[[8]] <- list(strsplit( strsplit(gsub("\\(|\\)", "", dropTable$ItemDropTypeList[[i]][[6]]), split=";")[[1]], split=" ")))
    
    ifelse(dropTable$ItemDropTypeList[[i]]$Drop3==""| dropTable$ItemDropTypeList[[i]]$Drop3==";", 
           row[[9]] <- NA, 
           row[[9]] <- list(strsplit( strsplit(gsub("\\(|\\)", "", dropTable$ItemDropTypeList[[i]][[7]]), split=";")[[1]], split=" ")))
    
    #Binds row
    lootTable <- rbind(lootTable, row )
    
  }
  
  #Erases first row, renames everything, and reindexes rows
  lootTable <- lootTable[-1,]
  colnames(lootTable) <-c("ID", "Rebalanced", "Name",  "Drop1Rate", "Drop2Rate", "Drop3Rate", "Drop1", "Drop2", "Drop3")
  rownames(lootTable) <- 1:nrow(lootTable)
  
  #Converts all drop rates from string to numeric
  lootTable$Drop1Rate <- as.numeric(lootTable$Drop1Rate)
  lootTable$Drop2Rate <- as.numeric(lootTable$Drop2Rate)
  lootTable$Drop3Rate <- as.numeric(lootTable$Drop3Rate)
  
  #Checks to see if monster has a rebalanced version, and if so, eliminates the "0" version from the table
  focus <- lootTable$Name[lootTable$Name %in% lootTable$Name[lootTable$Rebalance==0][
    (lootTable$ID[lootTable$Rebalance==0] %in% lootTable$ID[lootTable$Rebalanced==1])]]
  lootTable <- lootTable[!(lootTable$Name %in% focus & lootTable$Rebalanced==0),]
  lootTable <- lootTable[,-2]
  
  #Splits up names by finding the end of one word and the beginning of another by capitalization. Also removes underscores
  #and replaces with spaces
  lootTable$Name <- gsub("([a-z])([A-Z])", "\\1 \\2", lootTable$Name)
  lootTable$Name <- gsub("_", " ", lootTable$Name)
  
  return(lootTable)
}

#---------------------------------------------------------------------------------------------------------

#loads in relevant items from "itemdb.xml" into a dataframe
#ID, Name

loadItemDB<- function(filepath, unique){
  
  #parses the XML data
  itemDB <- xmlTreeParse(filepath)
  
  #initializes the data frame
  itemID <- data.frame("id", "name", stringsAsFactors=FALSE)
  
  for(i in 1:length(itemDB$doc$children$Items)){
    
    #For each item in the source file, it checks if it is in the compiled list of unique item IDs.
    #This implementation, while seemingly odd, is a simpler alternative to querying the source for each unique.
    #It also shouldn't matter which way the comparison goes.
    if(paste("id:",unlist(itemDB$doc$children$Items[[i]])["attributes.ID"], sep="") %in% unique){
      
      #stores item as ID with tag
      id <- paste("id:",unlist(itemDB$doc$children$Items[[i]])["attributes.ID"], sep="")
      
      #acquires location of translated name and then looks up translated name
      name <- unlist(itemDB$doc$children$Items[[i]])["attributes.Text_Name1"]
      name <- itemDBJapan[itemDBJapan$ID==regmatches(name, regexpr("\\d+.*[^]\\]", name)),2]
      
      #binds to table
      itemID <- rbind(itemID, c(id,name) )
      
    }
  }
  
  #Eliminates temporary first row, renames columns, and reindexes rows
  itemID <- itemID[-1,]
  colnames(itemID) <-c("id", "name")
  rownames(itemID) <- 1:nrow(itemID)
  
  return(itemID)
  
}

#---------------------------------------------------------------------------------------------------------

#loads in relevant enchants from "optionset.xml" into a dataframe
#ID, name

loadOptionSet<- function(filepath, unique){
  
  #parses XML data
  optionSet <- xmlTreeParse(filepath)
  
  #initializes data frame
  optionID <- data.frame("id", "name", stringsAsFactors=FALSE)
  
  
  for(i in 1:length(optionSet$doc$children$OptionSet[[2]])){
    
    #Same reasoning as similar loop in loadItemDB above
    if(unlist(optionSet$doc$children$OptionSet[[2]][[i]])["attributes.ID"] %in% unique){
      
      #Acquires ID
      id <- unlist(optionSet$doc$children$OptionSet[[2]][[i]])["attributes.ID"]
      
      #Acquires name
      name <- unlist(optionSet$doc$children$OptionSet[[2]][[i]])["attributes.Name"]
      
      #binds to data frame
      optionID <- rbind(optionID, c(id,name) )
    }
  }
  
  #eliminates temporary first row, renames columns, and reindexes rows.
  optionID <- optionID[-1,]
  colnames(optionID) <-c("id", "name")
  rownames(optionID) <- 1:nrow(optionID)
  
  return(optionID)
  
}


#---------------------------------------------------------------------------------------------------------

#loads all drop tables from the shadow mission files into a data frame
#ID, Diff, drop1rate, drop2rate, drop3rate, drop1, drop2, drop3

loadShadows <- function(filepath){
  
  #parses XML file
  root <- xmlRoot(xmlTreeParse(filepath, useInternalNodes = TRUE))
  
  #acquires all node sets of the first season child of type, or simply the node sets if the ID attribute is above 900
  #this acquires the most up to date drop lists, and all ID over 900 do not have a season child
  temp <- getNodeSet(root, "//type[season[1]] | //type[@id>900]")
  
  #initializes holder row
  row <- vector(mode="list")
  
  #initializes data frame
  shadowMissions <- data.frame(ID = 1, Diff = 0, drop1rate = 1, drop2rate=1, drop3rate=1,
                               drop1 = NA, drop2 = NA, drop3 = NA, stringsAsFactors = FALSE)
  
  for(i in 1:length(temp)){
    
    #gets ID, which is unchanging between difficulties
    row[1] <- getNodeSet(temp[[i]], "@id")
    
    #acquires drops for each difficulty
    for(j in 1:4){
      
      #acquires difficulty level
      row[2] <- j+1
      
      #acquires drop rates for item groups and items within those groups               
      for(k in 1:3){
        
        #Chooses between appropriate query for different types of nodesets
        #There are different formats for the <900 IDs from the >900 IDs
        if(row[1]<=900)
          drop <- getNodeSet(temp[[i]], sprintf("season[1]/level[%s]/item[%s]//@drop", j, k))
        else
          drop <- getNodeSet(temp[[i]], sprintf("level[%s]/item[%s]//@drop", j, k))
        
        #if there were no drops, then rate is 0 and the drop is NA
        if(is.null(drop)){
          row[2+k] <- 0
          row[5+k] <- NA}
        
        
        else{
          
          #acquires drop rates based on type of node set
          #sprintf is enables compatibility of this node query with this loop
          if(row[1]<=900)
            row[2+k] <- getNodeSet(temp[[i]], sprintf("season[1]/level[%s]/item[%s]//@rate", j, k))
          
          else
            row[2+k] <- getNodeSet(temp[[i]], sprintf("level[%s]/item[%s]//@rate", j, k))
          
          #Adds drops
          row[5+k] <- list(list(strsplit( strsplit(gsub("\\(|\\)", "",drop), split=";")[[1]], split=" ")))}
      }
      
      #appends row
      shadowMissions <- rbind(shadowMissions, row, stringsAsFactors=FALSE)
    }
  }
  
  
  
  #removes temporary first row, renames columns, and reindexes rows
  shadowMissions <- shadowMissions[-1,]
  colnames(shadowMissions) <-c("ID", "Diff",  "Drop1Rate", "Drop2Rate", "Drop3Rate", "Drop1", "Drop2", "Drop3")
  rownames(shadowMissions) <- 1:nrow(shadowMissions)
  
  #casts drop rates as numerics
  shadowMissions$Drop1Rate <- as.numeric(shadowMissions$Drop1Rate)
  shadowMissions$Drop2Rate <- as.numeric(shadowMissions$Drop2Rate)
  shadowMissions$Drop3Rate <- as.numeric(shadowMissions$Drop3Rate)
  
  
  
  return(shadowMissions)
}

#---------------------------------------------------------------------------------------------------------

#This function constructs the actual shadow mission tables for their corresponding missions
#Name, Bonus, Drop1Rate, Drop2Rate, Drop3Rate, Drop1, Drop2, Drop3

createSMTables <- function(filepath){
  
  sm <- read.csv(filepath)
  
  df <- data.frame("", list(0), 0, 0, 0, list(0), list(0), list(0), stringsAsFactors=FALSE)
  colnames(df) <- c("Name", "Bonus", "Drop1Rate", "Drop2Rate", "Drop3Rate", "Drop1", "Drop2", "Drop3")
  
  dict <- vector(mode="list", length=5)
  
  dict[[1]] <- " (Basic)"
  dict[[2]] <- " (Intermediate)"
  dict[[3]] <- " (Advanced)"
  dict[[4]] <- " (Hard)"
  dict[[5]] <- " (Elite)"
  
  for(i in 1:length(sm$Mission)){
    
    cosRates <- sapply(strsplit(toString(sm$CostumeRates[i]), ","), as.numeric)
    itemRates <- sapply(strsplit(toString(sm$ItemRates[i]), ","), as.numeric)
    stoneRates <- sapply(strsplit(toString(sm$StoneRates[i]), ","), as.numeric)
    enchantRates <- sapply(strsplit(toString(sm$EnchantRates[i]), ","), as.numeric)
    
    for(j in 1:5){
      df <- rbind(df, c("Name" = paste(sm$Mission[i], 
                                       dict[[j]]), "Bonus" = addBonuses(sm[i,], cosRates[j], itemRates[j], stoneRates[j], enchantRates[j]), 
                        missions[missions$ID==sm$Primary.Loot[i]&missions$Diff==j,3:8]))
    }
  }
  
  df <- df[-1,]
  
  return(df)
}

#---------------------------------------------------------------------------------------------------------

#This function goes along with the createSMTables function. Building it separately from the rest of the function
#provides some visual clarity to the code.

addBonuses <- function(row, cosRate, itemRate, stoneRate, enchantRate){
  
  bonus <- vector(mode="list", length=7)
  bonus[[1]] <- list(row$Boss.Pass, row$X)
  bonus[[2]] <- list(row$Elite.Pass, row$X)
  bonus[[3]] <- list(row$CostumedID, cosRate)
  bonus[[4]] <- list(row$EnhanceItem1, itemRate/2)
  bonus[[5]] <- list(row$EnhanceItem2, itemRate/2)
  bonus[[6]] <- list(row$StoneType, stoneRate)
  bonus[[7]] <- list(row$EnchantID, enchantRate)
  
  return(list(list(bonus)))
}

#---------------------------------------------------------------------------------------------------------

#loads translations from a localization file
#ID, Name

loadTranslations <- function(filepath){
  
  #reads translation and stores table
  local <- read.delim(filepath, header=FALSE, sep="~", stringsAsFactors = FALSE)
  
  #renames columns
  names(local) <- c("ID", "Name")
  
  #weird thing happens with the first element of the first column
  #this fixes it
  local[1,1] <- "0"
  
  return(local)
  
}