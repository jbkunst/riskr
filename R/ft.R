#' Frecueny table from a vector
#'
#' @param x A vector
#' @return The frecuency table of \code{x}. Frecuencies and percents are returned in a
#' \code{data.frame} object (adding \code{tbl_df} class).
#' @examples
#' set.seed(1313)
#' x <- sample(LETTERS[1:10], size = 100, prob = 1:10/10, replace = TRUE)
#' ft(x)
#' @export
ft <- function(x, order.by.count = TRUE) {

  if (any(is.na(x)))  x <- addNA(x)
  
  freqt <- gb_sm(dplyr::data_frame(class = x), class)
  
  if (order.by.count) {
    freqt <- freqt %>% 
      dplyr::arrange(dplyr::desc(count))
  }
  
 
  return(freqt)
}