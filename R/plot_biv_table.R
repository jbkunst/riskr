#' Plot Bivariate Analysis
#'
#' @param var A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @param count.labels A par
#' @param target.labels A par
#' @param coord.flip A par
#' @param add.legend A par
#' @param legend.color A par
#' @param rate.color A par
#' @param bar.width A par
#' @param rate.size A par
#' @param size.text A par
#' @param size.text2 A par
#' @param remove.axis.y A par
#' @return A ggplot2 object
#' @examples
#' data("credit")
#' 
#' var <- credit$sex
#' target <- credit$bad
#' 
#' plot_biv_table(var, target)
#' plot_biv_table(var, target) + theme_light()
#' plot_biv_table(var, target) + theme_gray()
#' @export
plot_biv_table <- function(var,
                           target,
                           count.labels = FALSE,
                           target.labels = FALSE,
                           arrange.plot.by = "variable",
                           coord.flip = FALSE,
                           add.legend = TRUE,
                           legend.color = "gray80",
                           rate.color = "darkblue",
                           bar.color = "gray80",
                           bar.width = .6,
                           rate.size = 1,
                           size.text = 4,
                           size.text2 = 10,
                           remove.axis.y = FALSE
){
  
  require("ggplot2")
  require("dplyr")
  require("scales")
  
  #### DATA ####  
  daux <- data_frame(var = addNA(var), target) %>%
    group_by(var) %>%
    summarise(count = n(), percent = count/nrow(.), target_rate = mean(target)) %>%
    mutate(id = seq(nrow(.)),
           count_format = prettyNum(count, big.mark = ".", decimal.mark = ", "),
           target_rate_format = percent(target_rate))
  
  if (arrange.plot.by == "variable") {
    daux <- daux %>% arrange(desc(count))
  } else if (arrange.plot.by == "target") {
    daux <- daux %>% arrange(desc(target_rate))
  }
  
  daux <- daux %>% mutate(var = factor(var))
  
  #### MAIN PLOT ###
  p <- ggplot(daux)
  
  if (add.legend) {
    
    cols <- c("percent_category" = bar.color, "target_rate" = rate.color)
    
    p <- p + 
      geom_bar(aes(var, percent, fill = "percent_category"),
               stat = "identity", width = bar.width, colour =  bar.color) +
      geom_line(aes(id, target_rate, colour = "target_rate"), size = rate.size) +
      geom_point(aes(id, target_rate), colour = rate.color) +
      scale_fill_manual("", values = cols) +
      scale_colour_manual(name = "", values = cols) 
  } else {
    p <- p + 
      geom_bar(aes(var, percent), stat = "identity", width = bar.width, fill = bar.color) +
      geom_line(aes(id, target_rate), colour = rate.color, size = rate.size) +
      geom_point(aes(id, target_rate), colour = rate.color) 
  }
  
  p <- p + scale_y_continuous(labels = percent)
  
  #### COORDFLIP ####
  if (coord.flip) p <- p + coord_flip()
  
  #### LABELS COUNT ###
  if (count.labels) {
    if (coord.flip) {
      p <- p + geom_text(aes(var, percent, label = count_format),
                         size = size.text, hjust = 1.2, colour = "white")
    } else {
      p <- p + geom_text(aes(var, percent, label = count_format),
                         size = size.text, vjust = 1.5, colour = "white")
    }
  }
  
  #### LABELS TARGET ####
  if (target.labels) {
    if (coord.flip) {
      p <- p + geom_text(aes(var, target_rate, label = target_rate_format),
                         size = size.text, hjust = -1, colour = "darkred")
    } else {
      p <- p + geom_text(aes(var, target_rate, label = target_rate_format),
                         size = size.text, vjust = -.5, colour = "darkred")
    }
  }
  
  p <- p + xlab(NULL) + ylab(NULL)
  
  p <- p + theme(rect = element_rect(fill = "#FFFFFF", linetype = 0, colour = NA),
                 title = element_text(hjust = 0.5), axis.title.x = element_text(hjust = 0.5), 
                 axis.title.y = element_text(hjust = 0.5), panel.grid.major.y = element_line(color = "gray"), 
                 panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), 
                 panel.grid.minor.x = element_blank(), panel.border = element_blank(), 
                 panel.background = element_blank(), legend.position = "bottom", 
                 legend.key = element_rect(fill = "#FFFFFF00"))
  
  p
}
