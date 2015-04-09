plot_roc <- function(score, target){
  library(ROCR)
  library(ggplot2)

  pred <- prediction(score, target)
  perf <- performance(pred,"tpr","fpr")
  
  df <- data.frame(x = unlist(perf@"x.values") , y = unlist(perf@"y.values"))
  
  p <- ggplot(df, aes(x, y))  + geom_line(size = 1.2, colour = "darkred") +
    geom_path(data= data.frame(x = c(0,1), y = c(0,1)), colour = "gray", size = 0.7) +
    scale_x_continuous("False Positive Rate (1 - Specificity)", target = percent_format(), limits = c(0, 1)) +
    scale_y_continuous("True Positive Rate (Sensivity or Recall)", target = percent_format(), limits = c(0, 1))
  p
}

plot_gain <- function(score, target){
  library(ggplot2)
  
  df <- data.frame(percentiles = seq(0, 1, length = 100),
                   gain = gain(score, target , seq(0, 1, length = 100)))
  
  p <-  ggplot(df, aes(percentiles, gain)) +
    geom_line(size = 1.2, colour = "darkred") +
    geom_line(aes(x = c(0,1), y = c(0,1)), colour = "gray", size = 0.7) +
    scale_x_continuous("Sample Percentiles", target = percent_format(), limits = c(0, 1)) +
    scale_y_continuous("Cumulative Percents of Bads", target = percent_format(), limits = c(0, 1))
  p
}


plot_br_heatmap <- function(var.x, var.y, target){
  library(plyr)
  
  daux <- ddply(data.frame(var.x, var.y, target),
                .(var.x, var.y), summarise, BadRate = (1-mean(target)))
  
  p <- ggplot(daux, aes(var.x, var.y)) +
    geom_tile(aes(fill = BadRate)) +
    geom_text(aes(target = percent(BadRate))) +
    scale_fill_gradient2(low = "blue", high = "red") +
    xlab(NULL) + ylab(NULL)
  p
  
}