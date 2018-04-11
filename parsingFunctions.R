#This file contains all the parsing functions that will be called when data
#needs to be located

#---------------------------------------------------------------------------------------------------------

#takes an enchant ID and returns what drops it and in what form

parseEnchants <- function(input, mult){
  
  lootTable$Drop1Rate <- lootTable$Drop1Rate*mult
  lootTable$Drop1Rate[lootTable$Drop1Rate>1000000] <- 1000000
  lootTable$Drop2Rate <- lootTable$Drop2Rate*mult
  lootTable$Drop3Rate <- lootTable$Drop3Rate*mult
  
  input <- optionSet$id[match(input,optionSet$name)]
  
  dropRates <- data.frame("Name", "Item", "Rate", stringsAsFactors=FALSE)
  
  name<-c()
  item<-c()
  rate<-c()
  
  
  for(i in 1:length(lootTable$Name)){
    
    for(j in 6:8){
      if(input %in% gsub("prefix:","",gsub("suffix:","",unlist(lootTable[[i,j]])))){
        for(k in lootTable[[i,j]][mapply("length",lootTable[[i,j]])>1]){
          if(input %in% gsub("prefix:","",gsub("suffix:","",k))){
            name <- lootTable$Name[i]
            item <- itemDB$name[match(k[[1]],itemDB$id)]
            
            #for calculating bayesian probability
            rate <- lootTable[[i,j-3]]/(1000000*length(lootTable[[i,j]]))
            
            if(j==7) rate <- rate*(1-(lootTable[[i,5]]/1000000))
            if(j==6) rate <- rate*(1-(lootTable[[i,5]]/1000000))*(1-(lootTable[[i,4]]/1000000))
            
            temp <- c("","")
            for(l in 2:length(k)){
              if(grepl("suffix:", k[[l]])){
                temp[2] <- optionSet$name[match(gsub("suffix:","",k[[l]]), optionSet$id)]
              }
              else if(grepl("prefix:", k[[l]])){
                temp[1] <- optionSet$name[match(gsub("prefix:","",k[[l]]), optionSet$id)]
              }
            }
            
            item <- paste(temp[1],temp[2], item, collapse=" ")
            
            
            dropRates <- rbind(dropRates, c(name,item,rate))
          }
        }
      } 
    }
  }
  
  colnames(dropRates) <- c("Name", "Item", "Rate %")
  
  if(nrow(dropRates)>1){
    dropRates <- dropRates[-1,]
    dropRates[[3]] <- round(as.numeric(dropRates[[3]])*100,2)
    rownames(dropRates) <- 1:nrow(dropRates)
    dropRates <- dropRates[order(-dropRates[[3]]),]
  }
  
  return(dropRates)
  
}

#---------------------------------------------------------------------------------------------------------


#Accepts an item name and returns a table of all monsters and missions that drop it

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

#---------------------------------------------------------------------------------------------------------

#takes a monster name as input, returns their drops

parseMonsters <- function(input, mult){
  
  
  lootTable$Drop1Rate <- lootTable$Drop1Rate*mult
  lootTable$Drop1Rate[lootTable$Drop1Rate>1000000] <- 1000000
  lootTable$Drop2Rate <- lootTable$Drop2Rate*mult
  lootTable$Drop3Rate <- lootTable$Drop3Rate*mult
  
  vector <- c()
  
  index <- match(input,lootTable$Name)
  
  for(i in 6:8){
    for(j in lootTable[[index,i]]){
      
      #This section calculates outcome probability for each instance in the drops
      temp <- lootTable[[index,i-3]]/(1000000*length(lootTable[[index,i]]))
      if(i==7) temp <- temp*(1-(lootTable[[index,5]]/1000000))
      if(i==6) temp <- temp*(1-(lootTable[[index,5]]/1000000))*(1-(lootTable[[index,4]]/1000000))
      
      vector <- c(vector,temp)
      
      #This section is for naming
      temp <- c(itemDB$name[match(j[[1]],itemDB$id)])       
      if(length(j)>1){
        for(k in 2:length(j)){
          if(grepl("suffix:", j[[k]])){
            temp <- c(optionSet$name[match(gsub("suffix:","",j[[k]]), optionSet$id)], temp)
          }
          else if(grepl("prefix:", j[[k]])){
            temp <- c(optionSet$name[match(gsub("prefix:","",j[[k]]), optionSet$id)], temp)
          }
          else{
            temp <- c(temp, j[[k]])
          }
        }
      }
      names(vector)[length(vector)]<- paste(temp, collapse=" ")
    }
  }
  
  newVector <- c()
  j <- 1    
  for(i in unique(names(vector))){
    newVector[j] <- sum(vector[names(vector) %in% i])
    names(newVector)[j] <- i
    j <- j+1
  }
  
  #fixes vector for display in a table
  newVector <- round(newVector*100,2)
  temp <- data.frame(names(newVector), newVector)
  colnames(temp) <- c("Item", "Rate %")
  rownames(temp) <- 1:nrow(temp)
  temp <- temp[order(-temp[[2]]),]
  return(temp)
  
}




