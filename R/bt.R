#' Bivariate Table
#' @description This function calculate a bivariate table.
#' @param variable A variable 
#' @param target A numeric binary vector {0,1}
#' @return A data_frame object with the counts, percents and odds
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

  library("dplyr")
  
  df <- data_frame(variable = as.character(addNA(variable)), target) %>% 
    group_by(variable) %>% 
    summarise(count = n(),
              percent = n()/nrow(.),
              target_count = sum(target),
              target_rate = target_count/count,
              target_percent = target_count,
              odds = target_count/(count - target_count)) %>% 
    ungroup() %>% 
    mutate(target_percent = target_count/sum(.$target_count))
  
  if (is.factor(variable)) {
    
    lvls <- levels(variable)
    df <- df %>% mutate(variable = factor(variable, levels = lvls))
    df <- df[order(df$variable),]
    
  }
  
  df
}

#' @export
biv_table <- function(...){
  message("This function 'biv_table' will be deprecated, use 'bt' instead.")
  bt(...)
}
