#' Group by and summarize
#' @description This is only a shortcut for \code{df} then \code{group_by(...)} and then \code{summarize(...)}
#' @param data A data frame
#' @param ... A list of variable (no characters) used in a \code{group_by}
#' @return A data_frame object with the counts and percets of each combination of \code{...}.
#' @examples
#' set.seed(1313)
#' x <- sample(LETTERS[1:10], size = 100, prob = 1:10/10, replace = TRUE)
#' df <- data.frame(class = x)
#' gb_sm(df, class)
#' 
#' library("dplyr")
#' df %>% gb_sm(class)
#' @export
gb_sm <- function(data, ...){
  
  library("dplyr")
  
  data %>%
    tbl_df %>%
    group_by(...) %>%
    dplyr::summarise(count = n(), percent = count/nrow(.)) %>%
    ungroup()

}