#' Make flight path population plot
#' 
#' Create a flight path population plot
#' @param df a dataframe generated from make_flight_path
#' @return plot
#' @import raster
#' @import dplyr
#' @import ggplot2
#' @import databrew
#' @import ggrepel
#' @export

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