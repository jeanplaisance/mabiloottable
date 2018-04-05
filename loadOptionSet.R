#Accepts a file path and a list of optionSetIDs
#Loads in the optionSet, and then returns a dataframe of all items that appeared in the provided list

loadOptionSet<- function(filepath, unique){
  optionSet <- xmlTreeParse(filepath)
  
  optionID <- data.frame("id", "name", stringsAsFactors=FALSE)
  
  for(i in 1:length(optionSet$doc$children$OptionSet[[2]])){
    if(unlist(optionSet$doc$children$OptionSet[[2]][[i]])["attributes.ID"] %in% unique){
      id <- unlist(optionSet$doc$children$OptionSet[[2]][[i]])["attributes.ID"]
      name <- unlist(optionSet$doc$children$OptionSet[[2]][[i]])["attributes.Name"]
      optionID <- rbind(optionID, c(id,name) )
    }
  }
  
  optionID <- optionID[-1,]
  colnames(optionID) <-c("id", "name")
  rownames(optionID) <- 1:nrow(optionID)
  
  return(optionID)
  
}