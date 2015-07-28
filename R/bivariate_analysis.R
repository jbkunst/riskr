. <- variable <- count <- percent <- target_count <- target_rate <- target_percent <- non_target_count <- non_target_percent <- odds <- woe <- iv <- NULL
. <- variable <- var <- value <- total <- value_fmt <- value_fmt2 <- NULL
count_format <- target_rate_format <- NULL
#' Bivariate Table
#' @description This function calculate a bivariate table.
#' @param variable A variable 
#' @param target A numeric binary vector {0,1}
#' @return A data_frame object with the counts, percents and odds
#' @references http://documentation.statsoft.com/portals/0/formula%20guide/Weight%20of%20Evidence%20Formula%20Guide.pdf
#' @examples
#' data(credit)
#' 
#' variable <- credit$marital_status
#' target <- 1 - credit$bad
#'
#' bt(variable, target)
#' 
#' variable <- cut(credit$payment_day, breaks = c(-Inf, 10, 20, Inf))
#' 
#' bt(variable, target)
#'  
#' @export
bt <- function(variable, target){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(variable)
  )

  suppressPackageStartupMessages(library("plyr"))
  suppressPackageStartupMessages(library("dplyr"))
  
  tot_target <- sum(target)
  tot_non_target <- length(target) - tot_target
  
  df <- data_frame(class = as.character(addNA(variable)), target) %>% 
    group_by(class) %>% 
    dplyr::summarise(count = length(target),
                     percent = count/nrow(.),
                     target_count = sum(target),
                     target_rate = target_count/count,
                     target_percent = target_count/tot_target,
                     non_target_count = (count - target_count),
                     non_target_percent = (count - target_count)/tot_non_target,
                     odds = target_count/(count - target_count),
                     woe = log(target_percent/non_target_percent),
                     iv = (target_percent - non_target_percent) * woe) %>% 
    ungroup()

  if (is.factor(variable)) {
    lvls <- levels(variable)
    df <- df %>% mutate(class = factor(class, levels = lvls))
    df <- df[order(df$class),]
  }
  
  df
}

#' Plot Bivariate Analysis
#' @description This function calculate a bivariate table.
#' @param variable A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @param labels A par
#' @param order.by A par
#' @examples
#' data("credit")
#' 
#' variable <- credit$sex
#' target <- credit$bad
#' 
#' plot_ba(variable, target)
#' plot_ba(variable, target, order.by = "target")
#' @export
plot_ba <- function(variable, target, labels = TRUE, order.by = NULL){
  
  library("tidyr")
  library("ggplot2")
  library("dplyr")
  library("scales")
  
  df <- bt(variable, target)
  
  df2 <- df %>%
    select(class, count, target_count, non_target_count, target_rate, odds, woe) %>% 
    gather(var, value, -class)
  
  df3 <- df %>%
    summarise(count = sum(count),
              target_count = sum(target_count),
              non_target_count = sum(non_target_count)) %>% 
    gather(var, total)
  
  df2 <- left_join(df2 %>% mutate(var = as.character(var)),
                   df3 %>% mutate(var = as.character(var)),
                   by = "var")
  
  df2 <- df2 %>% 
    dplyr::mutate(value_fmt = "",
                  value_fmt = ifelse(var %in% c("count", "target_count", "non_target_count"),
                                     prettyNum(value, big.mark = ","),
                                     value_fmt),
                  value_fmt = ifelse(var %in% c("odds", "woe", "target_rate"),
                                     round(value, 2), value_fmt),
                  value_fmt2 = "",
                  value_fmt2 = ifelse(var %in% c("count", "target_count", "non_target_count"),
                                      percent(value/total),
                                      value_fmt2))
                  
  df2 <- df2 %>% 
    mutate(var = factor(var, c("count", "target_count", "non_target_count",
                               "target_rate", "odds", "woe")))
  
  p <- ggplot(df2, aes_string("class", "value", group = 1)) +
    geom_bar(data = subset(df2, var == "count"), stat = "identity", width = 0.5) +
    geom_bar(data = subset(df2, var == "target_count"), stat = "identity", width = 0.5) +
    geom_bar(data = subset(df2, var == "non_target_count"), stat = "identity", width = 0.5) +
    geom_line(data = subset(df2, var == "target_rate")) +
    geom_point(data = subset(df2, var == "target_rate")) +
    geom_line(data = subset(df2, var == "odds")) +
    geom_point(data = subset(df2, var == "odds")) +
    geom_line(data = subset(df2, var == "woe")) +
    geom_point(data = subset(df2, var == "woe")) +
    facet_wrap(~var, scales = "free_y") +
    xlab(NULL) + ylab(NULL) + 
    theme(legend.position = "bottom")
  
  if (labels) {
    p <- p +
      geom_text(aes(label = value_fmt), vjust = -0.5) +
      geom_text(aes(label = value_fmt2), vjust = 1.5)
  }
  
  p
  
}

#' Plot Bivariate Analysis (2) 
#' @description A minimal version for \emph{plot_ba}
#' @param variable A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @param labels A par
#' @param order.by A par
#' @return A ggplot2 object
#' @examples
#' data("credit")
#' 
#' variable <- as.character(credit$marital_status)
#' target <- credit$bad
#' 
#' plot_ba2(variable, target)
#' plot_ba2(variable, target, labels = FALSE)
#' plot_ba2(variable, target, order.by = "odds")
#' @export
plot_ba2 <- function(variable, target, labels = TRUE, order.by = NULL){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(variable)
  )
  
  library("ggplot2")
  library("dplyr")
  library("scales")
  
  daux <- bt(variable, target) %>% 
    mutate(id = seq(nrow(.)), 
          count_format = prettyNum(count, big.mark = ","),
           target_rate_format = percent(target_rate))

  p <- ggplot(daux) +
    geom_bar(aes(class, percent), stat = "identity", width = 0.5) +
    geom_line(aes(id, target_rate)) +
    geom_point(aes(id, target_rate)) +
    scale_y_continuous(labels = percent_format()) +
    xlab(NULL) + ylab(NULL)
  
  if (labels) {
    p <- p +
      geom_text(aes(class, percent, label = count_format), vjust = -0.5) +
      geom_text(aes(class, target_rate, label = target_rate_format), vjust = -0.5)
  }

  p
  
}