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

  pred <- ROCR::prediction(score, target)
  perf <- ROCR::performance(pred, "tpr", "fpr")
  
  df <- data.frame(x = unlist(perf@"x.values") , y = unlist(perf@"y.values"))
  
  p <- ggplot2::ggplot(df, ggplot2::aes_string("x", "y")) +
    ggplot2::geom_line() +
    ggplot2::geom_path(data = data.frame(x = c(0, 1), y = c(0, 1)), colour = "gray") +
    ggplot2::scale_x_continuous("False Positive Rate (1 - Specificity)",
                                label = scales::percent_format(),
                                limits = c(0, 1)) +
    ggplot2::scale_y_continuous("True Positive Rate (Sensivity or Recall)",
                                label = scales::percent_format(),
                                limits = c(0, 1))
  p
}

#' Plot Gain Curve
#'
#' @description Return a plot for gains 
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
  
  
  
  ecdf_score <- ecdf(score)
  ecdf_non_target <- ecdf(score[target == 0])
  ecdf_target <- ecdf(score[target == 1])
  
  df <- dplyr::data_frame(score = quantile(score, seq(100)/100)) %>%  dplyr::arrange(score)
  
  df <- rbind(
    df %>%
      dplyr::mutate(x = ecdf_score(score),
                    y = ecdf_non_target(score),
                    target_label = "non target"),
    df %>%
      dplyr::mutate(x = 1 - ecdf_score(score),
                    y = 1 - ecdf_target(score),
                    target_label = "target")
  )
  
  p <- ggplot2::ggplot(df) + 
    ggplot2::geom_line(ggplot2::aes_string("x", "y", color = "target_label")) + 
    ggplot2::geom_path(ggplot2::aes(x = c(0, 1), y = c(0, 1)), colour = "gray") +
    ggplot2::scale_color_manual(labels = c("Samples percentiles (- to +) / non target cumulative percents",
                                           "Samples percentiles (+ to -) / target cumulative percents"),
                                values = c("darkred", "darkblue")) +
    ggplot2::labs(colour = NULL) +
    ggplot2::scale_x_continuous("Sample Percentiles",
                                label = scales::percent_format(),
                                limits = c(0, 1)) +
    ggplot2::scale_y_continuous("Cumulative Percent",
                                label = scales::percent_format(),
                                limits = c(0, 1)) +
    ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")
  
  p
  
}

#' Plot to compare cumulatives distrbutions
#'
#' @description Return a plot with ecdfs
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
  
  suppressPackageStartupMessages(requireNamespace("dplyr", quietly = TRUE) )
  requireNamespace("ggplot2", quietly = TRUE) 
  requireNamespace("scales", quietly = TRUE) 
  
  n.length <- 50
  
  ecd_0 <- ecdf(score[target == 0])
  ecd_1 <- ecdf(score[target == 1])
  
  cuts <- seq(min(score), max(score), length = n.length)
  
  df <- dplyr::data_frame(score = rep(cuts, 2),
                   target = rep(c(0, 1), each = 50),
                   target_label = ifelse(target == 1, "target", "non target"),
                   ecdf = ifelse(target == 0, ecd_0(score), ecd_1(score)))
  

  cut <- cuts[abs(ecd_1(cuts) - ecd_0(cuts)) == max(abs(ecd_1(cuts) - ecd_0(cuts)))]  
  
  p <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes_string("score", "ecdf", colour = "target_label")) + 
    ggplot2::scale_colour_manual(values = c("darkred", "darkblue")) + 
    ggplot2::scale_y_continuous("ecdf", label = scales::percent_format(), limits = c(0, 1)) + 
    ggplot2::labs(colour = "Legend: ") +
    ggplot2::xlab("Score") +
    ggplot2::ylab("ECDF") +
    ggplot2::theme(legend.position = "bottom")
  
  p
}

#' Plot to compare distrbutions
#'
#' @description Return a plot with densities
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
  
  suppressPackageStartupMessages(requireNamespace("dplyr", quietly = TRUE) )
  requireNamespace("ggplot2", quietly = TRUE) 
  requireNamespace("scales", quietly = TRUE) 
  
  df <- dplyr::data_frame(score,
                   target,
                   target_label = ifelse(target == 1, "target", "non target"))
  
  p <- ggplot2::ggplot(df) +
    ggplot2::geom_density(ggplot2::aes_string("score", fill = "target_label"), alpha = 0.5) + 
    ggplot2::scale_fill_manual(values = c("darkred", "darkblue")) +
    ggplot2::xlab("Score") +
    ggplot2::ylab("Densities") +
    ggplot2::labs(fill = "Legend: ") +
    ggplot2::theme(legend.position = "bottom")
  
  p
}


#' Plot Lift Chart
#'
#' @description Return a plot with the lift chart
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
  
  pred <- ROCR::prediction(score, target)
  perf <- ROCR::performance(pred,"lift","rpp")
  
  df <- data.frame(x = unlist(perf@"x.values") , y = unlist(perf@"y.values"))
  
  p <- ggplot2::ggplot(df, ggplot2::aes_string("x", "y")) +
    ggplot2::geom_line() +
    ggplot2::geom_path(data = data.frame(x = c(0, 1), y = c(1, 1)), colour = "gray") +
    ggplot2::scale_x_continuous("Rate of positive predictions",
                                label = scales::percent_format()) +
    ggplot2::scale_y_continuous("Lift Value", limits = c(.9, NA))
    
  p
}

#' Plot Performance
#'
#' @description Return a plot with plot_roc, plot_dists, plot_ks, plot_gain
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

  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  ecdf_score <- ecdf(score)
  ecdf_non_target <- ecdf(score[target == 0])
  ecdf_target <- ecdf(score[target == 1])
  
  # roc data ------
  pred <- ROCR::prediction(score, target)
  perf <- ROCR::performance(pred, "tpr", "fpr")
  df1 <- dplyr::data_frame(x = unlist(perf@"x.values"),
                    y = unlist(perf@"y.values"),
                    plot = "roc curve")
  
  # gain data
  df2 <- dplyr::data_frame(score = quantile(score, seq(100)/100)) %>%  dplyr::arrange(score)
  
  df2 <- rbind(
    df2 %>% dplyr::mutate(x = ecdf_score(score), y = ecdf_non_target(score), target_label = "non target"),
    df2 %>% dplyr::mutate(x = 1 - ecdf_score(score), y = 1 - ecdf_target(score), target_label = "target")
  )
  
  df2 <- df2 %>% dplyr::mutate(plot = "gain")
  
  # ks data
  n.length <- 50
  cuts <- seq(min(score), max(score), length = n.length)
  
  df3 <- dplyr::data_frame(x = rep(cuts, 2),
                    target = rep(c(0, 1), each = 50),
                    target_label = ifelse(target == 1, "target", "non target"),
                    y = ifelse(target == 0, ecdf_non_target(x), ecdf_target(x)),
                    plot = "cumulative")

  
  # dist data
  df4 <- dplyr::data_frame(x = score,
                    target,
                    target_label = ifelse(target == 1, "target", "non target"),
                    # y = ifelse(target == 0, ds_0(x), ds_1(x)),
                    plot = "distributions")
  
  df <- plyr::rbind.fill(df1, df2, df3, df4) %>% dplyr::tbl_df()
  
  ggplot2::ggplot(df, ggplot2::aes_string(group = 1)) + 
    # roc
    ggplot2::geom_line(data = subset(df, plot == "roc curve"),
              ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_segment(data = subset(df, plot == "roc curve")[1,],
                 ggplot2::aes(x = 0, y = 0,xend = 1,yend = 1), alpha = 0.5) +
    # gain
    ggplot2::geom_line(data = subset(df, plot == "gain"),
              ggplot2::aes_string(x = "x", y = "y", color = "target_label", group = "target_label")) +
    ggplot2::geom_segment(data = subset(df, plot == "gain")[1, ],
                 ggplot2::aes(x = 0,y = 0,xend = 1,yend = 1), alpha = 0.5) +
    # ecdf
    ggplot2::geom_line(data = subset(df, plot == "cumulative"),
              ggplot2::aes_string(x = "x", y = "y", color = "target_label", group = "target_label")) +
    # densities
    ggplot2::geom_density(data = subset(df, plot == "distributions"),
                 ggplot2::aes_string(x = "x", fill = "target_label", color = "target_label", group = "target_label"),
                 alpha = 0.5) +
    # style
    ggplot2::scale_color_manual(values = c("darkred", "darkblue")) +
    ggplot2::scale_fill_manual(values = c("darkred", "darkblue")) +
    ggplot2::facet_wrap(~plot, scales = "free") +
    ggplot2::labs(color = "Legend: ", fill = "Legend: ") +
    ggplot2::theme(legend.position = "bottom") + 
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL)
  
}