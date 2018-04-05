#This file is designed to take the itemdroptype.json, which contains all the monster files, and to load them into a dataframe for
#further inspection.
#library("jsonlite", lib.loc="E:\\Program Files\\R\\R-3.3.1\\library")



loadData <- function(filepath){
  
  dropTable <- fromJSON(filepath, simplifyVector = TRUE)
  
  lootTable <- data.frame("ID"="ID", "Rebalanced"=0, "Name"="Name", "Drop1Rate"=0, "Drop2Rate"=0, "Drop3Rate"=0, stringsAsFactors = FALSE)
  
  drop1 <- list(0)
  drop2 <- list(0)
  drop3 <- list(0)
  
  for(i in 1:length(dropTable$ItemDropTypeList)){
    
    vector <- unlist(dropTable$ItemDropTypeList[[i]][c(1, 2, 10, 3, 8, 4)])
    
    k<-5
    
    
    ifelse(dropTable$ItemDropTypeList[[i]]$Drop1==""| dropTable$ItemDropTypeList[[i]]$Drop1==";",
           drop1[i+1] <- NA,
           drop1[i+1]<- list(strsplit( strsplit(gsub("\\(|\\)", "", dropTable$ItemDropTypeList[[i]][[5]]), split=";")[[1]], split=" ")))
  
    ifelse(dropTable$ItemDropTypeList[[i]]$Drop2==""| dropTable$ItemDropTypeList[[i]]$Drop2==";", 
           drop2[i+1] <- NA, 
           drop2[i+1] <- list(strsplit( strsplit(gsub("\\(|\\)", "", dropTable$ItemDropTypeList[[i]][[6]]), split=";")[[1]], split=" ")))
    
    ifelse(dropTable$ItemDropTypeList[[i]]$Drop3==""| dropTable$ItemDropTypeList[[i]]$Drop3==";", 
           drop3[i+1] <- NA, 
           drop3[i+1] <- list(strsplit( strsplit(gsub("\\(|\\)", "", dropTable$ItemDropTypeList[[i]][[7]]), split=";")[[1]], split=" ")))
    
    
    lootTable <- rbind(lootTable, vector )
    
  }
  
  lootTable$Drop1 <- drop1
  lootTable$Drop2 <- drop2
  lootTable$Drop3 <- drop3
  
  lootTable <- lootTable[-1,]
  colnames(lootTable) <-c("ID", "Rebalanced", "Name",  "Drop1Rate", "Drop2Rate", "Drop3Rate", "Drop1", "Drop2", "Drop3")
  rownames(lootTable) <- 1:nrow(lootTable)
  
  test <- lootTable$Drop1Rate
  lootTable$Drop1Rate <- as.numeric(lootTable$Drop1Rate)
  lootTable$Drop2Rate <- as.numeric(lootTable$Drop2Rate)
  lootTable$Drop3Rate <- as.numeric(lootTable$Drop3Rate)
  
  focus <- lootTable$Name[lootTable$Name %in% lootTable$Name[lootTable$Rebalance==0][
    (lootTable$ID[lootTable$Rebalance==0] %in% lootTable$ID[lootTable$Rebalanced==1])]]
  lootTable <- lootTable[!(lootTable$Name %in% focus & lootTable$Rebalanced==0),]
  lootTable <- lootTable[,-2]
  
  lootTable$Name <- gsub("([a-z])([A-Z])", "\\1 \\2", lootTable$Name)
  lootTable$Name <- gsub("_", " ", lootTable$Name)
  
  return(lootTable)
}