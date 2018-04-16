#' Drone chart
#' 
#' Create a drone chart
#' @return Web application served
#' @import googlesheets
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @import ggrepel
#' @import plotly
#' @export

drone_chart <- function(){
  sheet <- gs_url('https://docs.google.com/spreadsheets/d/1JI5xyqNN85WkZqrbJTmSEJ-jV49GsMJ5pyYztrGGjek/edit#gid=1554870069')
  df <- gs_read_csv(sheet)
  df <- df %>%
    mutate(Vehicle = paste0(Manufacturer, ' ',
                            ifelse(is.na(Model), '', Model)))
  cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, 'Set1'))(length(unique(df$Vehicle)))
  options(scipen = '999')
  g <- ggplot(data = df,
         aes(x = Price,
             y = `Range (km)`)) +
    geom_point(alpha = 0.8,
               aes(size = `Payload (kg)`,
                   color = Vehicle)) +
    lendable::theme_lendable() +
    geom_text_repel(aes(label = Vehicle),
                     size = 3,
                     alpha = 0.5) +
    theme(legend.position = 'bottom') +
    scale_color_manual(name = '',
                       values = cols) +
    # geom_smooth(method = "lm", se = FALSE,
    #             alpha = 0.5,
    #             color = 'grey') +
    scale_x_sqrt(breaks = c(10000, seq(0, 100000, 20000))) +
    scale_y_sqrt(breaks = c(20, 50, seq(0, 1000, 100))) +
    guides(size=guide_legend(ncol=1),
           color = guide_legend(ncol = 3))
  
  g
  # ggplotly(g)
}
