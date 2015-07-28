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
  
  suppressPackageStartupMessages(library("dplyr"))
  library("ggplot2")
  library("scales")
  
  ecdf_score <- ecdf(score)
  ecdf_non_target <- ecdf(score[target == 0])
  ecdf_target <- ecdf(score[target == 1])
  
  df <- data_frame(score = quantile(score, seq(100)/100)) %>%  arrange(score)
  
  df <- rbind(
    df %>% mutate(x = ecdf_score(score), y = ecdf_non_target(score), target_label = "non target"),
    df %>% mutate(x = 1 - ecdf_score(score), y = 1 - ecdf_target(score), target_label = "target")
  )
  
  ggplot(df) + 
    geom_line(aes_string("x", "y", color = "target_label")) + 
    geom_path(aes(x = c(0, 1), y = c(0, 1)), colour = "gray") +
    scale_color_manual(labels = c("Samples percentiles (- to +) / non target cumulative percents",
                                  "Samples percentiles (+ to -) / target cumulative percents"),
                       values = c("darkred", "darkblue")) +
    labs(colour = NULL) +
    scale_x_continuous("Sample Percentiles", label = percent_format(), limits = c(0, 1)) +
    scale_y_continuous("Cumulative Percent", label = percent_format(), limits = c(0, 1)) +
    theme(legend.position = "bottom", legend.direction = "vertical")
  
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
                   target_label = ifelse(target == 1, "target", "non target"),
                   ecdf = ifelse(target == 0, ecd_0(score), ecd_1(score)))
  

  cut <- cuts[abs(ecd_1(cuts) - ecd_0(cuts)) == max(abs(ecd_1(cuts) - ecd_0(cuts)))]  
  
  p <- ggplot(df) +
    geom_line(aes_string("score", "ecdf", colour = "target_label")) + 
    scale_colour_manual(values = c("darkred", "darkblue")) + 
    scale_y_continuous("ecdf", label = percent_format(), limits = c(0, 1)) + 
    labs(colour = "Legend: ") +
    xlab("Score") + ylab("ECDF") +
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
                   target_label = ifelse(target == 1, "target", "non target"))
  
  p <- ggplot(df) +
    geom_density(aes_string("score", fill = "target_label"), alpha = 0.5) + 
    scale_fill_manual(values = c("darkred", "darkblue")) +
    xlab("Score") + ylab("Densities") +
    labs(fill = "Legend: ") +
    theme(legend.position = "bottom")
  
  p
}


#' Plot Lift Chart
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
#' plot_lift(score, target)
#' @references http://www2.cs.uregina.ca/~dbd/cs831/notes/lift_chart/lift_chart.html http://www.saedsayad.com/model_evaluation_c.htm
#' @export
plot_lift <- function(score, target){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  library("ROCR")
  library("ggplot2")
  library("scales")
  
  pred <- prediction(score, target)
  perf <- performance(pred,"lift","rpp")
  
  df <- data.frame(x = unlist(perf@"x.values") , y = unlist(perf@"y.values"))
  
  p <- ggplot(df, aes_string("x", "y")) +
    geom_line() +
    geom_path(data = data.frame(x = c(0, 1), y = c(1, 1)), colour = "gray") +
    scale_x_continuous("Rate of positive predictions",
                       label = percent_format()) +
    scale_y_continuous("Lift Value", limits = c(.9, NA))
    
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
  
  ecdf_score <- ecdf(score)
  ecdf_non_target <- ecdf(score[target == 0])
  ecdf_target <- ecdf(score[target == 1])
  
  # roc data ------
  pred <- prediction(score, target)
  perf <- performance(pred, "tpr", "fpr")
  df1 <- data_frame(x = unlist(perf@"x.values"),
                    y = unlist(perf@"y.values"),
                    plot = "roc curve")
  
  # gain data
  df2 <- data_frame(score = quantile(score, seq(100)/100)) %>%  arrange(score)
  
  df2 <- rbind(
    df2 %>% mutate(x = ecdf_score(score), y = ecdf_non_target(score), target_label = "non target"),
    df2 %>% mutate(x = 1 - ecdf_score(score), y = 1 - ecdf_target(score), target_label = "target")
  )
  
  df2 <- df2 %>% mutate(plot = "gain")
  
  # ks data
  n.length <- 50
  cuts <- seq(min(score), max(score), length = n.length)
  
  df3 <- data_frame(x = rep(cuts, 2),
                    target = rep(c(0, 1), each = 50),
                    target_label = ifelse(target == 1, "target", "non target"),
                    y = ifelse(target == 0, ecdf_non_target(x), ecdf_target(x)),
                    plot = "cumulative")

  
  # dist data
  df4 <- data_frame(x = score,
                    target,
                    target_label = ifelse(target == 1, "target", "non target"),
                    # y = ifelse(target == 0, ds_0(x), ds_1(x)),
                    plot = "distributions")
  
  df <- rbind.fill(df1, df2, df3, df4) %>% tbl_df()
  
  ggplot(df, aes_string(group = 1)) + 
    # roc
    geom_line(data = subset(df, plot == "roc curve"),
              aes_string(x = "x", y = "y")) +
    geom_segment(data = subset(df, plot == "roc curve")[1,],
                 aes(x = 0, y = 0,xend = 1,yend = 1), alpha = 0.5) +
    # gain
    geom_line(data = subset(df, plot == "gain"),
              aes_string(x = "x", y = "y", color = "target_label", group = "target_label")) +
    geom_segment(data = subset(df, plot == "gain")[1, ],
                 aes(x = 0,y = 0,xend = 1,yend = 1), alpha = 0.5) +
    # ecdf
    geom_line(data = subset(df, plot == "cumulative"),
              aes_string(x = "x", y = "y", color = "target_label", group = "target_label")) +
    # densities
    geom_density(data = subset(df, plot == "distributions"),
                 aes_string(x = "x", fill = "target_label", color = "target_label", group = "target_label"),
                 alpha = 0.5) +
    # style
    scale_color_manual(values = c("darkred", "darkblue")) +
    scale_fill_manual(values = c("darkred", "darkblue")) +
    facet_wrap(~plot, scales = "free") +
    labs(color = "Legend: ", fill = "Legend: ") +
    theme(legend.position = "bottom") + 
    xlab(NULL) + ylab(NULL)
  
}