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
    geom_line() +
    geom_area(fill = 'darkorange',
              alpha = 0.5) +
    theme_databrew() +
    labs(x = 'Distance (meters)',
         y = 'Elevation (meters)') +
    geom_vline(xintercept = extremes$d[extremes$label %in% c('High point', 'Low point')],
               alpha = 0.6,
               lty = 2) +
    geom_label(data = extremes %>%
                 filter(label %in% c('High point', 'Low point')),
               aes(x = d,
                   y = 0,
                   label = label))
}