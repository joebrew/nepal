library(raster)

make_flight_path_plot <- function(df){
  df$n <- 1:nrow(df)
  extremes <- 
    df %>%
    filter(n == max(n) |
             n == min(n) |
             elevation == max(elevation) |
             elevation == min(elevation))
  extremes <- extremes %>%
    mutate(label = ifelse(n == min(n), 'Start location',
                          ifelse(n == max(n), 'End location',
                                 ifelse(elevation == max(elevation), 'High point',
                                        ifelse(elevation == min(elevation), 'Low point', NA)))))
  extremes <- extremes %>%
    dplyr::distinct(label,
                    .keep_all = TRUE)
  g <- ggplot(data = df,
         aes(x = x, y = y)) +
    geom_path() +
    theme_databrew() +
    labs(x = "Longitude", y = 'Latitude') +
    geom_point(data = extremes,
               aes(x = x,
                   y = y)) +
    geom_label_repel(data = extremes,
                     aes(x = x,
                         y = y,
                         label = label))
  return(g)
}

make_flight_path_elevation_plot <- function(df){
  df$n <- 1:nrow(df)
  
  extremes <- 
    df %>%
    filter(n == max(n) |
             n == min(n) |
             elevation == max(elevation) |
             elevation == min(elevation))
  extremes <- extremes %>%
    mutate(label = ifelse(n == min(n), 'Start location',
                          ifelse(n == max(n), 'End location',
                                 ifelse(elevation == max(elevation), 'High point',
                                        ifelse(elevation == min(elevation), 'Low point', NA)))))
  extremes <- extremes %>%
    dplyr::distinct(label,
                    .keep_all = TRUE)    
  
  # save(extremes,
  #      file = 'extremes.RData')
  
  ggplot(data = df,
         aes(x = d, y = elevation)) +
    geom_line(color = 'darkred') +
    # geom_area(fill = 'darkorange',
    #           alpha = 0.5) +
    theme_databrew() +
    labs(x = 'Distance (meters)',
         y = 'Elevation (meters)') +
    geom_vline(xintercept = extremes$d[extremes$label %in% c('High point', 'Low point')],
               alpha = 0.6,
               lty = 2) +
    geom_label_repel(data = extremes %>%
                 filter(label %in% c('High point', 'Low point')),
               aes(x = d,
                   y = elevation,
                   label = label),
               alpha = 0.6)
}


make_flight_path_population_plot <- function(df){
  df$n <- 1:nrow(df)
  
  extremes <- df %>%
    mutate(population = ifelse(is.na(population), 0, population)) %>%
    filter(n == max(n) |
             n == min(n) |
             population == max(population, na.rm = TRUE) |
             population == min(population, na.rm = TRUE))
  extremes <- extremes %>%
    mutate(label = ifelse(n == min(n), 'Start location',
                          ifelse(n == max(n, na.rm = TRUE), 'End location',
                                 ifelse(population == max(population, na.rm = TRUE), 'Most population dense point',
                                        ifelse(population == min(population, na.rm = TRUE), 'Least population dense point', NA)))))
  extremes <- extremes %>%
    dplyr::distinct(label,
                    .keep_all = TRUE)    
  
  
  ggplot(data = df,
         aes(x = d, y = population)) +
    geom_line(color = 'darkred') +
    # geom_area(fill = 'darkorange',
    #           alpha = 0.5) +
    theme_databrew() +
    labs(x = 'Distance (meters)',
         y = 'Population density') +
    geom_vline(xintercept = extremes$d[extremes$label %in% c('Most population dense point', 'Least population dense point')],
               alpha = 0.6,
               lty = 2) +
    geom_label_repel(data = extremes %>%
                 filter(label %in% c('Most population dense point', 'Least population dense point')) %>%
                 mutate(label = gsub('n d', 'n\nd', label)),
               aes(x = d,
                   y = population,
                   label = label),
              alpha = 0.7)
}


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
