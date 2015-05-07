#' Plot AUC Curve
#'
#' @param score A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @return The KS statistic
#' @examples
#' data(predictions)
#' 
#' score <- predictions$score
#' target <- predictions$target
#' 
#' plot_roc(score, target)
#' @export
plot_roc <- function(score, target){
  
  library("ROCR")
  library("ggplot2")
  library("scales")

  pred <- prediction(score, target)
  perf <- performance(pred, "tpr", "fpr")
  
  df <- data.frame(x = unlist(perf@"x.values") , y = unlist(perf@"y.values"))
  
  p <- ggplot(df, aes(x, y)) +
    geom_line(size = 1.2, colour = "darkred") +
    geom_path(data= data.frame(x = c(0, 1), y = c(0, 1)), colour = "gray", size = 0.7) +
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
#' @return The KS statistic
#' @examples
#' data(predictions)
#' 
#' score <- predictions$score
#' target <- predictions$target
#' 
#' plot_gain(score, target)
#' @export
plot_gain <- function(score, target){
  
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