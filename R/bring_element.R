# function bring data from api
bring_element <- function(a, b) {
  require(httr)
  require(jsonlite)
  #a <- 1
  #b <- 2
  # url/endpoint of the published API
  # network name specified in the docker run command is 'API'
  url <- paste0("API:8797/config?grp_num=", a,"&part_num=", b)
  # execute GET request
  res <- GET(url)    
  # interpret the results
  element <- rawToChar(res$content)
  # transform from JSON
  element <- fromJSON(element)
  # return the result or the error message if the result is not good
  if(res$status_code == 200) {return(element)} else {return(res$status_code)}
}