#' Riskr ggplot theme
#'
#' Theme based on \emph{Highcharts JS}.
#' @param base_size base font size
#' @param base_family base font family
#' @references
#'
#' \url{http://www.highcharts.com/demo/line-basic}
#'
#'
#' @examples
#' library("ggplot2")
#' qplot(hp, mpg, data = mtcars, geom = 'point') + theme_rskr() + ggtitle('Diamond Prices')
#' @export
theme_rskr <- function(base_size = 12, base_family = "") {
  
  library("ggplot2")
  library("grid")
  
  theme_gray() + 
    theme(
      rect = element_rect(fill = "white", linetype = 0,  colour = NA),
      text = element_text(size = base_size, family = base_family), 
      title = element_text(hjust = 0.5),
      axis.title.x = element_text(hjust = 0.5), 
      axis.title.y = element_text(hjust = 0.5),
      panel.grid.major.y = element_line(color = "gray"), 
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(), 
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(), 
      panel.background = element_blank(),
      legend.position = "bottom", 
      legend.key = element_rect(fill = "#FFFFFF00"),
      strip.text = element_text(color = "white"),
      strip.background = element_rect(fill = "#434348")
      )
  
}

#' Riskr color palette
#'
#' Palette based on \emph{Highcharts JS}.
#' @references
#' \url{https://github.com/highslide-software/highcharts.com/blob/1c33fd375da4f7cf63dba0c3dd74c3f6d0118541/js/parts/Options.js}
#'
#' @examples
#' library("ggplot2")
#' ggplot(mtcars) +
#'  geom_point(aes(hp, mpg, color = factor(gear)), size = 4) + 
#'  theme_rskr() + 
#'  scale_color_manual(values = palette_rskr())
#' 
#' @export
palette_rskr <- function(){
  values <-  c('#7cb5ec', '#434348', '#90ed7d', '#f7a35c', '#8085e9',
               '#f15c80', '#e4d354', '#2b908f', '#f45b5b', '#91e8e1') 
}
  


