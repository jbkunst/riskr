x <- NULL
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
  
  p <- ggplot(df, aes_string("x", "y")) +
    geom_line() +
    geom_path(data = data.frame(x = c(0, 1), y = c(0, 1)), colour = "gray") +
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
  
  p <- ggplot(df, aes_string("percentiles", "gain")) +
    geom_line() +
    geom_path(aes(x = c(0, 1), y = c(0, 1)), colour = "gray") +
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
  
  ecd_0 <- ecdf(score[target == 0])
  ecd_1 <- ecdf(score[target == 1])
  
  cuts <- seq(min(score), max(score), length = n.length)
  
  df <- data_frame(score = rep(cuts, 2),
                   target = rep(c(0, 1), each = 50),
                   target_label = ifelse(target == 1, "target", "non taget"),
                   ecdf = ifelse(target == 0, ecd_0(score), ecd_1(score)))
  

  cut <- cuts[abs(ecd_1(cuts) - ecd_0(cuts)) == max(abs(ecd_1(cuts) - ecd_0(cuts)))]  
  
  p <- ggplot(df) +
    geom_line(aes_string("score", "ecdf", colour = "target_label")) + 
    scale_colour_manual(values = c("red", "darkblue")) + 
    scale_y_continuous("ecdf", label = percent_format(), limits = c(0, 1)) + 
    xlab("score") +
    theme(legend.position = "bottom")
  
  p
}

#' Plot to compare distrbutions
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
#' plot_dists(score, target)
#' @export
plot_dists <- function(score, target){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  suppressPackageStartupMessages(library("dplyr"))
  library("ggplot2")
  library("scales")
  
  df <- data_frame(score,
                   target,
                   target_label = ifelse(target == 1, "target", "non taget"))
  
  p <- ggplot(df) +
    geom_density(aes_string("score", fill = "target_label"), alpha = 0.5) + 
    scale_fill_manual(values = c("red", "blue")) + 
    theme(legend.position = "bottom")
  
  p
}

#' Plot Performance
#'
#' @description Return a ggplot object. 
#' @param score A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @return The plot
#' @examples
#' data(predictions)
#' 
#' score <- 1000 * predictions$score
#' target <- predictions$target
#' 
#' plot_perf(score, target)
#' @export
plot_perf <- function(score, target){
  
  library("ROCR")
  library("plyr")
  suppressPackageStartupMessages(library("dplyr"))
  library("ggplot2")
  library("scales")
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  # roc data ------
  pred <- prediction(score, target)
  perf <- performance(pred, "tpr", "fpr")
  df1 <- data_frame(x = unlist(perf@"x.values"),
                    y = unlist(perf@"y.values"),
                    plot = "roc curve")
  
  # gain data
  df2 <- data_frame(x = seq(0, 1, length = 100),
                    y = gain(score, target , seq(0, 1, length = 100)),
                    plot = "gain")
  
  # ks data
  n.length <- 50
  ecd_0 <- ecdf(score[target == 0])
  ecd_1 <- ecdf(score[target == 1])
  cuts <- seq(min(score), max(score), length = n.length)
  
  df3 <- data_frame(x = rep(cuts, 2),
                    target = rep(c(0, 1), each = 50),
                    target_label = ifelse(target == 1, "target", "non taget"),
                    y = ifelse(target == 0, ecd_0(x), ecd_1(x)),
                    plot = "cumulative")

  
  # dist data
  df4 <- data_frame(x = score,
                    target,
                    target_label = ifelse(target == 1, "target", "non taget"),
                    # y = ifelse(target == 0, ds_0(x), ds_1(x)),
                    plot = "distributions")
  
  df <- rbind.fill(df1, df2, df3, df4) %>% tbl_df()
  
  ggplot(df, aes_string("x", group = 1)) + 
    # roc
    geom_line(data = subset(df, plot == "roc curve"), aes_string(y = "y")) +
    geom_segment(data = subset(df, plot == "roc curve")[1,],
                 aes(x = 0, y = 0,xend = 1,yend = 1), alpha = 0.5) +
    # gain
    geom_line(data = subset(df, plot == "gain"), aes_string(y = "y")) +
    geom_segment(data = subset(df, plot == "gain")[1, ],
                 aes(x = 0,y = 0,xend = 1,yend = 1), alpha = 0.5) +
    # ecdf
    geom_line(data = subset(df, plot == "cumulative"),
              aes_string(y = "y", color = "target_label", group = "target_label")) +
    # densities
    geom_density(data = subset(df, plot == "distributions"),
                 aes_string(fill = "target_label", color = "target_label", group = "target_label"),
                 alpha = 0.5) +
    # style
    scale_color_manual(values = c("red", "blue")) +
    scale_fill_manual(values = c("red", "blue")) +
    facet_wrap(~plot, scales = "free") +
    theme(legend.position = "bottom") + 
    xlab(NULL) + ylab(NULL)
  
}