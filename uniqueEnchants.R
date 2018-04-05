#compiles a list of unique enchants

uniqueEnchants <- function(compiledList){
  
  enchants <- c()
  enchants <- c(enchants,gsub("suffix:","",grep("suffix:",compiledList,value=TRUE)))
  enchants <- c(enchants,gsub("prefix:","",grep("prefix:",compiledList,value=TRUE)))

  
  return(unique(enchants))
}