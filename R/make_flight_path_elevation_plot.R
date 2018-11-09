#' Make flight path elevation plot
#' 
#' Create a flight path elevation plot
#' @param df a dataframe generated from make_flight_path
#' @return plot
#' @import raster
#' @import dplyr
#' @import ggplot2
#' @import databrew
#' @import ggrepel
#' @export


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
