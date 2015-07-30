n <- NULL
#' Group by and summarize
#' @description This is only a shortcut for \code{df} then \code{group_by(...)} and then \code{summarize(...)}
#' @param data A data frame
#' @param ... A list of variable (no characters) used in a \code{group_by}
#' @return A dplyr::data_frame object with the counts and percets of each combination of \code{...}.
#' @examples
#' 
#' set.seed(1313)
#' x <- sample(LETTERS[1:4], size = 100, prob = 1:4/4, replace = TRUE)
#' data <- data.frame(class = x)
#' gb_sm(data, class)
#'
#' @export
gb_sm <- function(data, ...){

  data %>%
    dplyr::tbl_df() %>%
    dplyr::count(..., sort = TRUE) %>% 
    dplyr::rename(count = n) %>% 
    dplyr::mutate(percent = count/sum(count))

}