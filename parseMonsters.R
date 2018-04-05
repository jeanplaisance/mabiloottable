
#takes a monster name as input, returns their loot table
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