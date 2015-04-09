#' Create a frecueny table from a vector
#'
#' @param x A vector
#' @return The frecuency table of \code{x}. Frecuencies and percents are returned in a
#' \code{data.frame} object (adding \code{tbl_df} class).
#' @examples
#' set.seed(1313)
#' x <- sample(letters[1:10], size = 100, prob = 1:10/10, replace = TRUE)
#' ft(x)
#' @export
ft <- function (x) {
  
  library("dplyr")
  
  if (any(is.na(x))) {
    x <- addNA(x)
  }
  
  freqt <- data.frame(class = x) %>%
    tbl_df
    group_by(class) %>%
    summarise(freq = n(), percent = freq/nrow(.))
    
  return(freqt)
}
  