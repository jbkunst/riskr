#' Plot AUC Curve
#'
#' @param score A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @return the plot
#' @examples
#' data(predictions)
#' 
#' score <- predictions$score
#' target <- predictions$target
#' 
#' plot_roc(score, target)
#' @export
plot_roc <- function(score, target){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  library("ROCR")
  library("ggplot2")
  library("scales")

  pred <- prediction(score, target)
  perf <- performance(pred, "tpr", "fpr")
  
  df <- data.frame(x = unlist(perf@"x.values") , y = unlist(perf@"y.values"))
  
  p <- ggplot(df, aes(x, y)) +
    geom_line(size = 1.2, colour = "darkred") +
    geom_path(data = data.frame(x = c(0, 1), y = c(0, 1)), colour = "gray", size = 0.7) +
    scale_x_continuous("False Positive Rate (1 - Specificity)",
                       label = percent_format(),
                       limits = c(0, 1)) +
    scale_y_continuous("True Positive Rate (Sensivity or Recall)",
                       label = percent_format(),
                       limits = c(0, 1))
  p
}

#' Plot Gain Curve
#'
#' @description Return a ggplot object. 
#' @param score A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @return the plot
#' @examples
#' data(predictions)
#' 
#' score <- predictions$score
#' target <- predictions$target
#' 
#' plot_gain(score, target)
#' @export
plot_gain <- function(score, target){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  library("ggplot2")
  
  df <- data.frame(percentiles = seq(0, 1, length = 100),
                   gain = gain(score, target , seq(0, 1, length = 100)))
  
  p <- ggplot(df, aes(percentiles, gain)) +
    geom_line(size = 1.2, colour = "darkred") +
    geom_line(aes(x = c(0, 1), y = c(0, 1)), colour = "gray", size = 0.7) +
    scale_x_continuous("Sample Percentiles",
                       label = percent_format(),
                       limits = c(0, 1)) +
    scale_y_continuous("Target Cumulative Percents",
                       label = percent_format(),
                       limits = c(0, 1))
  p
}

#' Plot to compare cumulatives distrbutions
#'
#' @description Return a ggplot object. 
#' @param score A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @return The plot
#' @examples
#' data(predictions)
#' 
#' score <- predictions$score
#' target <- predictions$target
#' 
#' plot_ks(score, target)
#' @export
plot_ks <- function(score, target){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  suppressPackageStartupMessages(library("dplyr"))
  library("ggplot2")
  library("scales")
  
  n.length <- 50
  
  ecd.0 <- ecdf(score[target == 0])
  ecd.1 <- ecdf(score[target == 1])
  
  cuts <- seq(min(score), max(score), length = n.length)
  
  df <- data_frame(score = rep(cuts, 2),
                   class = rep(c("Target 0", "Target 1"), each = 50)) %>% 
    mutate(ecdf = ifelse(class == "Target 0", ecd.0(score), ecd.1(score)))

  cut <- cuts[abs(ecd.1(cuts) - ecd.0(cuts)) == max(abs(ecd.1(cuts) - ecd.0(cuts)))]  
  
  p <- ggplot(df) +
    geom_line(aes(score, ecdf, colour = class), size = 1.2) + 
    scale_colour_manual(values = c("darkred", "darkblue")) + 
    scale_y_continuous("ecdf", label = percent_format(), limits = c(0, 1)) + 
    xlab("score") 
  
#   p <- p + geom_segment(aes(x = cut, xend = cut, y = ecd.1(cut), yend = ecd.0(cut)),
#                         colour = grey(.6), size = 1.0)
    
  p
}