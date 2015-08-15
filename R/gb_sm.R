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
#' X <- sample(letters[1:2], size = 100, prob = 1:2/2, replace = TRUE)
#' data <- data.frame(class = x, class2 = X)
#' gb_sm(data, class)
#' gb_sm(data, class, class2)
#'
#' @export
gb_sm <- function(data, ...){

  data %>%
    dplyr::tbl_df() %>%
    dplyr::count(...) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(count = n) %>% 
    dplyr::mutate(percent = count/sum(count)) %>% 
    dplyr::arrange(dplyr::desc(count))

}