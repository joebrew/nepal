#' Make strip map plot
#' 
#' Create a strip map plot
#' @param df a dataframe generated from make_flight_path
#' @param every the distance in meters the flight should be broken up into
#' @return plot
#' @import raster
#' @import dplyr
#' @import ggplot2
#' @import databrew
#' @import ggrepel
#' @import ggthemes
#' @import leaflet
#' @import mapview
#' @import rgeos
#' @import rgdal
#' @export

make_strip_map <- function(df, every = 1000){
  df$n <- 1:nrow(df)
  # Get number of kilometers
  max_d <- max(df$d)
  max_r <- nrow(df)
  kms <- ceiling((max_d)/every)
  
  # Get a buffer
  df <- data.frame(df)
  dfs <- df
  dfs <- data.frame(dfs)
  coordinates(dfs) <- ~x+y
  proj4string(dfs) <- proj4string(nep0)
  dfsl <- dfs
  # Make metric (UTM) system
  # http://spatialreference.org/ref/sr-org/8024/proj4/
  dfs <- spTransform(dfs, CRS( "+proj=tmerc +lat_0=0 +lon_0=84 +k=0.9999 +x_0=500000 +y_0=0 +a=6377276.345 +b=6356075.41314024 +units=m +no_defs" )) 
  samplers <- sort(unique(c(seq(0, floor(max_r), by = 10), max_r)))
  dfsb <- rgeos::gBuffer(spgeom = dfs[samplers,], byid = F, 
                         width = 500,
                         quadsegs = 'ROUND',
                         joinStyle = 'MITRE')
  # Convert back to lat lon
  dfsbl <- spTransform(dfsb, CRS("+init=epsg:4326"))
  
  make_leafy <- function(df,dfs, dfsbl){
    m <- 
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      # addProviderTiles(providers$OpenTopoMap) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      # addRasterImage(r, colors = pal, opacity = 0.6) %>%
      # addLegend(pal = pal, values = values(r),
      #           position = 'bottomleft',
      #           title = "Elevation") %>%
      # addPolylines(data = nepal::pyuthan,
      #              color = 'red',
      #              weight = 2,
      #              group = 'District borders') %>%
      addScaleBar(position = 'topright') %>%
      addPolylines(data = df,
                   lng = df$x,
                   lat = df$y,
                   weight = 1,
                   opacity = 1,
                   color = 'red') %>%
      addPolygons(data = dfsbl,
                  weight = 1,
                  opacity = 0.3,
                  fillOpacity = 0.03,
                  color = 'white')
    return(m)
  }
  # Make overall plot
  map_list <- list()
  map_list[[1]] <- make_leafy(df = df,
                              dfs = dfs,
                              dfsbl = dfsbl)
  # If more than 1 km, make each segment
  if(kms > 1){
    for(i in 1:kms){
      message('km ', i, ' of ', kms)
      # Get sub path
      sub_df <- df %>% filter(d <= 1000 * i,
                              d >= 1000 * (i-1))
      # Get sub buffer
      samplers <- sort(unique(c(which(df$d <= 1000 *i & df$d >= 1000 * (i-1)))))
      sub_dfsb <- rgeos::gBuffer(spgeom = dfs[samplers,], byid = F, 
                             width = 500,
                             quadsegs = 'ROUND',
                             joinStyle = 'MITRE')
      # Convert back to lat lon
      sub_dfsbl <- spTransform(sub_dfsb, CRS("+init=epsg:4326"))
      map_list[[i+1]] <-
        make_leafy(df = sub_df,
                   dfs = dfs,
                   dfsbl = sub_dfsbl)
    }
  }
  message('Returning a map list of length ', length(map_list))
  return(map_list)
}
