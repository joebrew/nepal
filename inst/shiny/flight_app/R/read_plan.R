#' read_plan
#' 
#' Read a .plan file
#' @param plan_file The path to the file
#' @param return_name Whether to parse the filename to include in the output
#' @import jsonlite
#' @import tidyverse
#' @export

read_plan <- function(plan_file = 'Pyuthan to Dharmawati.plan',
                      return_name = TRUE){
  require(jsonlite) 
  require(tidyverse)
  # Read the file
  data <- jsonlite::fromJSON(plan_file)
  # Extract the waypoints
  waypoints <- data[[4]]
  true_points <- waypoints$items
  y <- lapply(true_points$params,
                function(x){x[5]}) %>% unlist
  x <- lapply(true_points$params,
                function(x){x[6]}) %>% unlist
  z <- lapply(true_points$params,
              function(x){x[7]}) %>% unlist
  height <- true_points$Altitude
  out <- 
    tibble(longitude = x,
           latitude = y,
           height,
           elevation = z)
  if(return_name){
    places <- unlist(strsplit(plan_file, ' to '))
    places <- gsub('.plan', '', places, fixed = TRUE)
    take_off <- places[1]
    landing <- places[2]
    out$take_off <- take_off
    out$landing <- landing
  }
  return(out)
}
