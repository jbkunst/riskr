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
#' @export
plot_biv_table <- function(var,
                           target,
                           count.labels = FALSE,
                           target.labels = FALSE,
                           coord.flip = FALSE,
                           add.legend = TRUE,
                           legend.color = "gray80",
                           rate.color = "darkred",
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
  daux <- data_frame(var, target) %>%
    group_by(var) %>%
    summarise(count = n(), percent = count/nrow(.), targetRate = mean(target)) %>%
    mutate(var = factor(var),
           id = seq(nrow(.)),
           count_format = prettyNum(count, big.mark = ".", decimal.mark = ", "),
           targetRate_format = percent(targetRate))
  
  #### MAIN PLOT ###
  p <- ggplot(daux)
  
  if (add.legend) {
    
    cols <- c("PercentCatergory" = bar.color, "targetRate" = rate.color)
    
    p <- p + 
      geom_bar(aes(var, percent, fill = "PercentCatergory"),
               stat = "identity", width = bar.width, colour =  bar.color) +
      geom_line(aes(id, targetRate, colour = "targetRate"), size = rate.size) +
      geom_point(aes(id, targetRate), colour = rate.color) +
      scale_fill_manual("", values = cols) +
      scale_colour_manual(name = "", values = cols) 
  } else {
    p <- p + 
      geom_bar(aes(var, percent), stat = "identity", width = bar.width, fill = bar.color) +
      geom_line(aes(id, targetRate), colour = rate.color, size = rate.size) +
      geom_point(aes(id, targetRate), colour = rate.color) 
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
      p <- p + geom_text(aes(var, targetRate, label = targetRate_format),
                         size = size.text, hjust = -1, colour = "darkred")
    } else {
      p <- p + geom_text(aes(var, targetRate, label = targetRate_format),
                         size = size.text, vjust = -.5, colour = "darkred")
    }
  }
  
  p <- p + xlab(NULL) + ylab(NULL)
  
  p
}
