

loadShadowMissions <- function(filepath){
  
  root <- xmlRoot(xmlTreeParse(filepath, useInternalNodes = TRUE))
  
  temp <- getNodeSet(root, "//type[season[1]] | //type[@id>900]")
  row <- vector(mode="list")
  
  shadowMissions <- data.frame(ID = 1, Diff = 0, drop1rate = 1, drop2rate=1, drop3rate=1,
                               drop1 = NA, drop2=NA, drop3=NA, stringsAsFactors = FALSE)
  
  for(i in 1:length(temp)){
    
    #gets ID, which is unchanging between difficulties
    row[1] <- getNodeSet(temp[[i]], "@id")
    for(j in 1:4){
      
      #notes difficulty level
      row[2] <- j+1
      
      #acquires drop rates for item groups and items within those groups               
      for(k in 1:3){
        
        if(row[1]<=900)
          drop <- getNodeSet(temp[[i]], sprintf("season[1]/level[%s]/item[%s]//@drop", j, k))
        else
          drop <- getNodeSet(temp[[i]], sprintf("level[%s]/item[%s]//@drop", j, k))
        
        if(is.null(drop)){
          row[2+k] <- 0
          row[5+k] <- NA}
        
        else{
          if(row[1]<=900)
            row[2+k] <- getNodeSet(temp[[i]], sprintf("season[1]/level[%s]/item[%s]//@rate", j, k))
          else
            row[2+k] <- getNodeSet(temp[[i]], sprintf("level[%s]/item[%s]//@rate", j, k))
          
          row[5+k] <- ifelse(drop=="", NA,list(list(strsplit( strsplit(gsub("\\(|\\)", "",drop), split=";")[[1]], split=" "))))}
      }
      
      #appends row
      shadowMissions <- rbind(shadowMissions, row, stringsAsFactors=FALSE)
    }
  }
  
  #Cleans up this mess. Renames columns, rows, removes holder-row. Casts drop rates as numerics  
  colnames(shadowMissions) <- c("ID", "Diff", "Drop1Rate", "Drop2Rate", "Drop3Rate", "Drop1", "Drop2", "Drop3")
 
  shadowMissions <- shadowMissions[-1,]
  colnames(shadowMissions) <-c("ID", "Diff",  "Drop1Rate", "Drop2Rate", "Drop3Rate", "Drop1", "Drop2", "Drop3")
  rownames(shadowMissions) <- 1:nrow(shadowMissions)
  
  test <- shadowMissions$Drop1Rate
  shadowMissions$Drop1Rate <- as.numeric(shadowMissions$Drop1Rate)
  shadowMissions$Drop2Rate <- as.numeric(shadowMissions$Drop2Rate)
  shadowMissions$Drop3Rate <- as.numeric(shadowMissions$Drop3Rate)
  
  
  
  return(shadowMissions)
}