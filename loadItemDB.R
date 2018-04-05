#library("XML", lib.loc="E:\\Program Files\\R\\R-3.3.1\\library")
#library("stringr", lib.loc="E:\\Program Files\\R\\R-3.3.1\\library")



#Accepts a file path and a list of itemIDs
#Loads in the itemDB, and then returns a dataframe of all items that appeared in the provided list
loadItemDB<- function(filepath, unique){
  itemDB <- xmlTreeParse(filepath)
  
  itemID <- data.frame("id", "name", stringsAsFactors=FALSE)
  
  for(i in 1:length(itemDB$doc$children$Items)){
    if(paste("id:",unlist(itemDB$doc$children$Items[[i]])["attributes.ID"], sep="") %in% unique){
      id <- paste("id:",unlist(itemDB$doc$children$Items[[i]])["attributes.ID"], sep="")
      name <- unlist(itemDB$doc$children$Items[[i]])["attributes.Text_Name1"]
      name <- itemDBJapan[itemDBJapan$ID==regmatches(name, regexpr("\\d+.*[^]\\]", name)),2]
      itemID <- rbind(itemID, c(id,name) )
    }
  }
  
  itemID <- itemID[-1,]
  colnames(itemID) <-c("id", "name")
  rownames(itemID) <- 1:nrow(itemID)
  
  return(itemID)
  
}