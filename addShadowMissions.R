
addShadowMissions <- function(){
  
  basicMissions <- lootTable[lootTable$ID %in% unique(missions$ID),]
  basicMissions$Name = 1
  missions <- rbind(missions, setNames(basicMissions, names(missions)))
  
  return(missions)
  
}