#' Make flight path plot
#' 
#' Create a flight path plot
#' @param df a dataframe generated from make_flight_path
#' @return plot
#' @import raster
#' @import dplyr
#' @import ggplot2
#' @import databrew
#' @import ggrepel
#' @export

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
