
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



