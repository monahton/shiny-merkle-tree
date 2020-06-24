# function bring data from api
bring_group_hash <- function(a) {
  require(httr)
  require(jsonlite)
  #http://localhost:8797/p/1c2808f8/hash_group?grp_num=1
  #a <- 1
  # url/endpoint of the published API
  # network name specified in the docker run command is 'API'
  url <- paste0("API:8797/hash_group?grp_num=", a)
  # execute GET request
  res <- GET(url)    
  # interpret the results
  element <- rawToChar(res$content)
  # transform from JSON
  element <- fromJSON(element)
  # return the result or the error message if the result is not good
  if(res$status_code == 200) {return(element)} else {return(res$status_code)}
}