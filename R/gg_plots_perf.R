x <- NULL
#' Plot AUC Curve
#'
#' @param target A numeric binary vector (0, 1)
#' @param ... A data frame of scores, or a multiples score vectors: score1, score2, etc
#' @return the plot
#' @examples
#'
#' set.seed(1313)
#' n <- 5000
#' score <- runif(n)
#' score_2 <- score + runif(n)
#' score_3 <- score + runif(n)
#' target <- rbinom(n, 1, prob = score)
#'
#' df <- data.frame(target, score, score_2, score_3)
#' 
#' gg_roc(target, score)
#' gg_roc(target, model = score, score_3)
#' 
#' library("dplyr")
#' 
#' gg_roc(target,  df %>% select(starts_with("score")))
#' 
#' @export
gg_roc <- function(target, ...){
  
  args <- list(...)
  
  if (length(args) == 1 && is.data.frame(args[[1]])) {
    
    scores_list <- as.list(...)
    
  } else {
    
    arglist <- match.call(expand.dots = FALSE)$...
    scores_list <- lapply(arglist, eval.parent, n = 2)
    score_names <- sapply(arglist, deparse)
    
    if (!is.null(names(score_names))) {
      score_names <- ifelse(names(score_names) == "", score_names, names(score_names))
    }
    
    names(scores_list) <- score_names
    
  }
  
  dfroc <- plyr::ldply(scores_list, function(score){
    pred <- ROCR::prediction(score, target)
    perf <- ROCR::performance(pred, "tpr", "fpr")
    df <- data.frame(x = unlist(perf@"x.values") , y = unlist(perf@"y.values"))
  })
  
  p <- ggplot(dfroc, aes_string("x", "y")) + 
    geom_path(data = data.frame(x = c(0, 1), y = c(0, 1)), colour = "gray") +
    scale_x_continuous("False Positive Rate (1 - Specificity)",
                                labels = scales::percent_format(),
                                limits = c(0, 1)) +
    scale_y_continuous("True Positive Rate (Sensivity or Recall)",
                                labels = scales::percent_format(),
                                limits = c(0, 1))
  
  if (length(scores_list) == 1) {
    
    p <- p + geom_line()
    
  } else {
    p <- p + geom_line(aes_string(group = ".id", color = ".id")) +
      labs(color = NULL) +
      theme(legend.position = "bottom")
  }

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
#' gg_gain(target, score)
#' @import ggplot2
#' @export
gg_gain <- function(target, score){
  
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
  
  p <- ggplot(df) + 
    geom_line(aes_string("x", "y", color = "target_label")) + 
    geom_path(data = data.frame(x = c(0,1), y = c(0,1)), aes_string("x", "y"), colour = "gray") +
    scale_color_manual(labels = c("Samples percentiles (- to +) / non target cumulative percents",
                                           "Samples percentiles (+ to -) / target cumulative percents"),
                                values = c("darkred", "darkblue")) +
    labs(colour = NULL) +
    scale_x_continuous("Sample Percentiles",
                       labels = scales::percent_format(), limits = c(0, 1)) +
    scale_y_continuous("Cumulative Percent",
                       labels = scales::percent_format(), limits = c(0, 1)) +
    theme(legend.position = "bottom", legend.direction = "vertical")
  
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
#' gg_cum(target, score)
#' @export
gg_cum <- function(target, score){
  
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
  
  p <- ggplot(df) +
    geom_line(aes_string("score", "ecdf", colour = "target_label")) + 
    scale_colour_manual(values = c("darkred", "darkblue")) + 
    scale_y_continuous("ecdf", labels = scales::percent_format(), limits = c(0, 1)) + 
    labs(colour = "Legend: ") +
    xlab("Score") +
    ylab("ECDF") +
    theme(legend.position = "bottom")
  
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
#' gg_dists(target, score)
#' @export
gg_dists <- function(target, score){
  
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
  
  p <- ggplot(df) +
    geom_density(aes_string("score", fill = "target_label"), alpha = 0.5) + 
    scale_fill_manual(values = c("darkred", "darkblue")) +
    xlab("Score") +
    ylab("Densities") +
    labs(fill = "Legend: ") +
    theme(legend.position = "bottom")
  
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
#' gg_lift(target, score)
#' @references http://www2.cs.uregina.ca/~dbd/cs831/notes/lift_chart/lift_chart.html http://www.saedsayad.com/model_evaluation_c.htm
#' @export
gg_lift <- function(target, score){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  pred <- ROCR::prediction(score, target)
  perf <- ROCR::performance(pred,"lift","rpp")
  
  df <- data.frame(x = unlist(perf@"x.values") , y = unlist(perf@"y.values"))
  
  p <- ggplot(df, aes_string("x", "y")) +
    geom_line() +
    geom_path(data = data.frame(x = c(0, 1), y = c(1, 1)), colour = "gray") +
    scale_x_continuous("Rate of positive predictions",
                                labels = scales::percent_format()) +
    scale_y_continuous("Lift Value", limits = c(.9, NA))
    
  p
}

#' Plot Performance
#'
#' @description Return a plot with gg_roc, gg_dists, gg_cum, gg_gain
#' @param score A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @return The plot
#' @examples
#' data(predictions)
#' 
#' score <- 1000 * predictions$score
#' target <- predictions$target
#' 
#' gg_perf(target, score)
#' @export
gg_perf <- function(target, score){

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
    xlab(NULL) +
    ylab(NULL)
  
}