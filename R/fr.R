#' Create a frecueny table from a vector
#'
#' @param x A vector
#' @return The frecuency table of \code{x}. Frecuency and percents (adding \code{tbl_df} class)
#' @examples
#' setseed(1313)
#' x <- sample(letters[1:10], size = 100, prob = 1:10/10, replace = TRUE)
#' ft(x)
ft <- function(x) {
  require("dplyr")
  
  if (any(is.na(x))) {
    x <- addNA(x)
  }
  
  freqt <- data.frame(class = x) %>%
    tbl_df
    group_by(class) %>%
    summarise(freq = n(), percent = freq/nrow(.))
    
  return(freqt)
}
  