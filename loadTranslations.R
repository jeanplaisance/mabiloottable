
loadTranslations <- function(filepath){
  
  local <- read.delim(filepath, header=FALSE, sep="~", stringsAsFactors = FALSE)
  names(local) <- c("ID", "Name")
  local[1,1] <- "0"
  return(local)
  
}