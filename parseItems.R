#This function will take an item ID or a monster name and return the other.

#Accepts an item name and returns a table of all monsters that drop it
parseItems <- function(input, mult){
  
  lootTable$Drop1Rate <- lootTable$Drop1Rate*mult
  lootTable$Drop1Rate[lootTable$Drop1Rate>1000000] <- 1000000
  lootTable$Drop2Rate <- lootTable$Drop2Rate*mult
  lootTable$Drop3Rate <- lootTable$Drop3Rate*mult
  
  #Initializes data frame
  dropRates <- data.frame("Name", "Rate", stringsAsFactors = FALSE)
  
  #Search for Hits
  for(i in 6:8){
    for(j in 1:length(lootTable[[i]])){
      
      ifelse(is.na(lootTable[[i]][[j]][[1]]), next, 1)
      
      if( input %in% unlist(lootTable[[i]][[j]])){
        temp <- lootTable[[i-3]][[j]] / (1000000 * length(lootTable[[i]][[j]]) / sum(lapply(lootTable[[i]][[j]],"[[", 1)==input) ) 
        if(i==7) temp <- temp*(1-(lootTable[[5]][[j]]/1000000))
        if(i==6) temp <- temp*(1-(lootTable[[5]][[j]]/1000000))*(1-(lootTable[[4]][[j]]/1000000))
        #Creates a table of lists containing the name and the corresponding drop rate.
        dropRates <- rbind(dropRates, c(lootTable$Name[[j]], temp))
        
      }
    }
  }
  
  colnames(dropRates) <- c("Monster", "Rate %")
  
  if(nrow(dropRates)>1){
    dropRates <- dropRates[-1,]
    dropRates[[2]] <- round(as.numeric(dropRates[[2]])*100,2)
    rownames(dropRates) <- 1:nrow(dropRates)
    dropRates <- dropRates[order(-dropRates[[2]]),]
  }
  
  
  
  
  return(dropRates)
}



