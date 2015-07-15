target_rate <- count_format <- target_rate_format <- target_count <- . <- NULL
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

  suppressPackageStartupMessages(library("dplyr"))
  
  tot_target <- sum(target)
  tot_non_target <- length(target) - tot_target
  
  df <- data_frame(variable = as.character(addNA(variable)), target) %>% 
    group_by(variable) %>% 
    summarise(count = n(),
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
    df <- df %>% mutate(variable = factor(variable, levels = lvls))
    df <- df[order(df$variable),]
  }
  
  df
}

#' Plot Bivariate Analysis
#' @description This function calculate a bivariate table.
#' @examples
#' data("credit")
#' 
#' variable <- credit$sex
#' target <- credit$bad
#' 
#' plot_ba(variable, target)
#' @export
plot_ba <- function(variable, target){
  
  library("tidyr")
  library("ggplot2")
  library("dplyr")
  library("scales")
  
  df <- bt(variable, target)
  
  df2 <- df %>%
    select(variable, count, target_count, non_target_count, target_rate, odds, woe) %>% 
    gather(var, value, -variable)
  
  df3 <- df %>%
    summarise(count = sum(count),
              target_count = sum(target_count),
              non_target_count = sum(non_target_count)) %>% 
    gather(var, total)
  
  df2 <- left_join(df2, df3, by = "var")
  
  df2 <- df2 %>% 
    mutate(value_fmt = "",
           value_fmt = ifelse(var %in% c("count", "target_count", "non_target_count"),
                              sprintf("%s (%s)",
                                      prettyNum(value, big.mark = ","),
                                      paste0(round(100*value/total, 2), "%")),
                              value_fmt),
           value_fmt = ifelse(var %in% c("target_rate"),
                              paste0(round(100*value, 2), "%") , value_fmt),
           value_fmt = ifelse(var %in% c("odds", "woe"),
                              round(value, 2) , value_fmt))
  
  df2 <- df2 %>% 
    mutate(var = factor(var, c("count", "target_count", "non_target_count",
                               "target_rate", "odds", "woe")))
  
  ggplot(df2, aes(variable, value, group = 1)) +
    geom_bar(data = subset(df2, var == "count"), stat = "identity", fill = "gray80") +
    geom_bar(data = subset(df2, var == "target_count"), stat = "identity", fill = "gray80") +
    geom_bar(data = subset(df2, var == "non_target_count"), stat = "identity", fill = "gray80") +
    geom_line(data = subset(df2, var == "target_rate")) +
    geom_point(data = subset(df2, var == "target_rate"), color = "darkblue", size = 2) +
    geom_line(data = subset(df2, var == "odds"), color = "darkblue") +
    geom_point(data = subset(df2, var == "odds"), color = "darkblue", size = 2) +
    geom_line(data = subset(df2, var == "woe"), color = "darkblue") +
    geom_point(data = subset(df2, var == "woe"), color = "darkblue", size = 2) +
    geom_text(aes(label = value_fmt), size = 4, vjust = -0.5) +
    facet_wrap(~var, scales = "free_y") +
    theme_minimal()
  
}

#' Bivariate Plot 
#'
#' @param variable A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @param count.labels A par
#' @param target.labels A par
#' @param coord.flip A par
#' @param add.legend A par
#' @param legend.color A par
#' @param target.color A par
#' @param bar.width A par
#' @param rate.size A par
#' @param size.text A par
#' @param size.text2 A par
#' @param remove.axis.y A par
#' @return A ggplot2 object
#' @examples
#' data("credit")
#' 
#' variable <- credit$sex
#' target <- credit$bad
#' 
#' plot_bt(variable, target)
#' plot_bt(variable, target) + theme_gray()
#' plot_bt(variable, target, coord.flip = TRUE)
#' plot_bt(variable, target, add.legend = FALSE)
#' plot_bt(variable, target, count.labels = TRUE, coord.flip = TRUE)
#' plot_bt(variable, target, count.labels = TRUE, target.labels = TRUE)
#' @export
plot_bt <- function(variable,
                    target,
                    count.labels = FALSE,
                    target.labels = FALSE,
                    arrange.plot.by = NULL, # options: variable, target
                    coord.flip = FALSE,
                    add.legend = TRUE,
                    legend.color = "gray80",
                    target.color = "darkblue",
                    bar.color = "gray80",
                    bar.width = .6,
                    rate.size = 1,
                    size.text = 4,
                    size.text2 = 10,
                    remove.axis.y = FALSE
){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(variable)
  )
  
  require("ggplot2")
  require("dplyr")
  require("scales")
  
  #### DATA ####
  daux <- bt(addNA(variable), target) %>% 
    mutate(id = seq(length(variable)),
           count_format = prettyNum(count, big.mark = ".", decimal.mark = ", "),
           target_rate_format = percent(target_rate))
  
  if (!is.null(arrange.plot.by)) {
    if (arrange.plot.by == "variable") {
      daux <- daux %>% arrange(desc(count))
    } else if (arrange.plot.by == "target") {
      daux <- daux %>% arrange(desc(target_rate))
    }
    
    daux <- daux %>% mutate(variable = factor(variable))
    
  } else {
    if (is.factor(variable)) {
      lvls <- levels(variable)
      daux <- daux %>% mutate(variable = factor(variable, levels = lvls))  
    } else {
      daux <- daux %>% mutate(variable = factor(variable))  
    }
  }
  
  #### MAIN PLOT ###
  p <- ggplot(daux)
  
  if (add.legend) {
    
    cols <- c("percent_category" = bar.color, "target_rate" = target.color)
    
    p <- p + 
      geom_bar(aes(variable, percent, fill = "percent_category"),
               stat = "identity", width = bar.width, colour = bar.color) +
      geom_line(aes(id, target_rate, colour = "target_rate"), size = rate.size) +
      geom_point(aes(id, target_rate), colour = target.color) +
      scale_fill_manual("", values = cols) +
      scale_colour_manual(name = "", values = cols)
    
  } else {
    p <- p + 
      geom_bar(aes(variable, percent), stat = "identity", width = bar.width, fill = bar.color) +
      geom_line(aes(id, target_rate), colour = target.color, size = rate.size) +
      geom_point(aes(id, target_rate), colour = target.color) 
    
  }
  
  p <- p + scale_y_continuous(labels = percent)
  
  #### COORDFLIP ####
  if (coord.flip) p <- p + coord_flip()
  
  #### LABELS COUNT ###
  if (count.labels) {
    if (coord.flip) {
      p <- p + geom_text(aes(variable, percent, label = count_format),
                         size = size.text, hjust = 1.2, colour = "white")
    } else {
      p <- p + geom_text(aes(variable, percent, label = count_format),
                         size = size.text, vjust = 1.5, colour = "white")
    }
  }
  
  #### LABELS TARGET ####
  if (target.labels) {
    if (coord.flip) {
      p <- p + geom_text(aes(variable, target_rate, label = target_rate_format),
                         size = size.text, hjust = -1, colour = target.color)
    } else {
      p <- p + geom_text(aes(variable, target_rate, label = target_rate_format),
                         size = size.text, vjust = -.5, colour = target.color)
    }
  }
  
  #### THEME #### 
  p <- p + xlab(NULL) + ylab(NULL)
  
  p <- p + theme(rect = element_rect(fill = "#FFFFFF", linetype = 0, colour = NA),
                 title = element_text(hjust = 0.5),
                 axis.title.x = element_text(hjust = 0.5), 
                 axis.title.y = element_text(hjust = 0.5),
                 panel.border = element_blank(), 
                 panel.background = element_blank(),
                 legend.key = element_rect(fill = "#FFFFFF00"))
  
  if (!coord.flip) {
    p <- p + theme(panel.grid.major.y = element_line(color = "gray"),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   legend.position = "bottom")
  } else {
    p <- p + theme(panel.grid.major.x = element_line(color = "gray"),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.major.y = element_blank(), 
                   panel.grid.minor.y = element_blank(),
                   legend.position = "right")
  }
  
  p
}

#' @export
plot_biv_table <- function(...){
  message("This function 'plot_biv_table' will be deprecated, use 'plot_bt' instead.")
  plot_bt(...)
}

#' @export
biv_table <- function(...){
  message("This function 'biv_table' will be deprecated, use 'bt' instead.")
  bt(...)
}