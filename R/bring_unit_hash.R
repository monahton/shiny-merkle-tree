# function bring data from api
bring_unit_hash <- function() {
  require(httr)
  require(jsonlite)
  # url/endpoint of the published API
  # network name specified in the docker run command is 'API'
  url <- "API:8797/hash_unit"
  # execute GET request
  res <- GET(url)    
  # interpret the results
  element <- rawToChar(res$content)
  # transform from JSON
  element <- fromJSON(element)
  # return the result or the error message if the result is not good
  if(res$status_code == 200) {return(element)} else {return(res$status_code)}
}