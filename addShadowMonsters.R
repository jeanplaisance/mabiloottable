
addShadowMonsters <- function(){
  
  for(i in 1:110){
    
    name <- lootTable[lootTable$ID==monsters[4*i,1],2]
    lootTable[lootTable$ID==monsters[4*i,1],2] <- paste(name, " (Basic)")
    monsters[4*i-3,2] <- paste(name," (Intermediate)")
    monsters[4*i-2,2] <- paste(name," (Advanced)")
    monsters[4*i-1,2] <- paste(name," (Hard)")
    monsters[4*i,2] <- paste(name," (Elite)")
    
    #minimal number of rbinds to add all four to lootTable
    lootTable <- rbind(
      lootTable,
      setNames(
        rbind(
          rbind(monsters[4*i-3,],monsters[4*i-2,]),
          rbind(monsters[4*i-1,],monsters[4*i,])
        ),
        names(lootTable)
      )
    )
    
  }
  
  return(lootTable)
  
}