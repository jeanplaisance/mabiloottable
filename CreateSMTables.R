

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


createSMTables <- function(){
  sm <- read.csv("E:\\Studying\\R\\Mabinogi Loot Table\\shadowMissionsCSV.csv")
  
  
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