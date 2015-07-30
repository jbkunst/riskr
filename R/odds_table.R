#' Calculating Odd table
#' @description This function calculate a odds table.
#' @param score A numeric vector of predictions
#' @param target A numeric binary vector {0,1}
#' @param nclass A numeric input for determinate the number of classes if \code{breaks} is not given.
#' @param quantile A boolean to set if the cuts are made by quantile or counts. This parameter is udes if \code{breaks} is not given. 
#' @param breaks An optional numeric vector to set the intervals to use
#' @return A dplyr::data_frame object with the counts, percents and odds
#' @examples
#' data(predictions)
#' 
#' score <- round(predictions$score * 1000)
#' target <- predictions$target
#'
#' odds_table(score, target, nclass = 5)
#' odds_table(score, target, nclass = 5, quantile = FALSE)
#' odds_table(score, target, breaks = c(-Inf, 250, 750, Inf))
#'  
#' @export
odds_table <- function(score, target, nclass = 10, quantile = TRUE, breaks = NULL){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  if (missing(breaks) & quantile) {
    
    score_cat <- ggplot2::cut_number(score, n = nclass)
    
  } else if (missing(breaks) & !quantile) {
    
    score_cat <- ggplot2::cut_interval(score, n = nclass)
    
  } else {
    
    score_cat <- cut(score, breaks = breaks)
	
  }
  
  df <- bt(variable = score_cat, target = target)
  
  df
  
}
