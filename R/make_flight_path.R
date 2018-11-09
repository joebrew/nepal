#' Make flight path plot
#' 
#' Create a flight path plot
#' @param x Longitude coordinates of start and end point
#' @param y Latitude coordinates of start and end point
#' @param country
#' @return plot
#' @import raster
#' @import dplyr
#' @import ggplot2
#' @import databrew
#' @import ggrepel
#' @export

make_flight_path <- function(x,y,
                             country = 'Nepal'){
  # Get distance
  dr <- data_frame(x,y) 
  # save(dr,
  #      file = 'dr.RData')
  coordinates(dr) <- ~x+y
  ic <- country
  if(is.null(ic)){
    return(NULL)
  }
  if(ic == 'Nepal'){
    proj4string(dr) <- proj4string(nepal::pyuthan_elevation_detailed)
  } else {
    proj4string(dr) <- proj4string(nepal::fiana_elevation_detailed)
    
  }
  n <- nrow(dr@coords)
  distances <- rep(0, n)
  for(i in 2:n){
    distances[i] <- geosphere::distGeo(dr[(i-1),],
                                       dr[i,])
  }
  distances <- cumsum(distances)
  
  # Get path
  expandify <- function(df){
    new_df <- data_frame(x = seq(df$x[1],
                                 df$x[2],
                                 length =  round(df$d[2]) - round(df$d[1])),
                         y = seq(df$y[1],
                                 df$y[2],
                                 length =  round(df$d[2]) - round(df$d[1])),
                         d = seq(df$d[1],
                                 df$d[2],
                                 length =  round(df$d[2]) - round(df$d[1])))
    return(new_df)
  }
  # Get how many coords
  df <- data_frame(x = x,
                   y = y,
                   d = distances)
  if(nrow(df) == 2){
    df <- expandify(df)
  } else {
    # Need to build for multi stop journey
    out_list <- list()
    for(i in 1:(nrow(df) - 1)){
      out_list[[i]] <- expandify(df[i:(i+1),])
    }
    df <- bind_rows(out_list)
  }
  # Get elevation
  df_data<- df
  coordinates(df) <- ~x+y
  
  
  if(ic == 'Nepal'){
    proj4string(df) <- proj4string(nepal::pyuthan_elevation_detailed)
    values <- raster::extract(nepal::pyuthan_elevation_detailed,
                              y = df)
    pops <- raster::extract(nepal::pyuthan_population,
                            y = df)
  } else {
    proj4string(df) <- proj4string(nepal::fiana_elevation_detailed)
    values <- raster::extract(nepal::fiana_elevation_detailed,
                              y = df)
    pops <- raster::extract(nepal::fiana_population,
                            y = df)
    
  }
  
  df <- df_data
  df$elevation <- values
  df$population <- pops
  return(df)
}
