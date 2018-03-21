#' Assign a code with an existing list of words (indices used)
#' 
#' @param words vector of strings (tokens)
#' @param wordCode vector of strings (types)
#' @return a vector of numeric identifiers of lexical items

# NOTE: I thought that this function could have been used 
# to subcategorize words under the same numerical identifier 

.packageName <- 'crqanlp'

assign_codes = function(words,wordCode) {
  series = c()
  for (word in words) {
    series = c(series, which(word==wordCode))
  }
  return(series)
}