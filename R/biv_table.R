#' bivariate Analysis
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
#' biv_table(variable, target)
#' 
#' variable <- cut(credit$payment_day, breaks = c(-Inf, 10, 20, Inf))
#' 
#' biv_table(variable, target)
#'  
#' @export
biv_table <- function(variable, target){

  library("dplyr")
  
  df <- data_frame(class = variable, target) %>% 
    group_by(class) %>% 
    summarise(count = n(),
              percent = n()/nrow(.),
              count_target = sum(target),
              rate_target = count_target/count,
              percent_target = count_target,
              odds = count_target/(count - count_target)) %>% 
    ungroup() %>% 
    mutate(percent_target = count_target/sum(.$count_target))
  
  df
}