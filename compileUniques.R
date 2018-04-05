#Compiles a list of uniques from a given subset of a data frame

compileUniques <- function(phrase, itemList){
  return( unique(grep(phrase, itemList, value=TRUE)) )
}