#' Selecting categorical variables from a data frame
#' 
#' @param df A data frame.
#' @param nuniques Limit to consider a numeric varialbe as categorical one
#' 
#' @examples 
#' 
#' data("credit")
#' 
#' credit %>% select_categorical()
#' 
#' @export
select_categorical <- function(df, nuniques = 10){
  
  selections <- purrr::map_lgl(df, function(x) {
    is.character(x) ||
      is.factor(x) ||
      is.logical(x) ||
      (length(unique(x)) <= nuniques) # this is when you have numeric variables with few uniques (dummies)
  })
  df[, selections]
  
}

#' Selecting numeric variables from a data frame
#' 
#' @param df A data frame.
#' 
#' @examples 
#' 
#' data("credit")
#' 
#' credit %>% select_numeric()
#' 
#' @export
select_numeric <- function(df){
  
  selections <- purrr::map_lgl(df, function(x) is.numeric(x))
  df[, selections]
  
}

#' #' Generate Tables for categorical variables
#' #' 
#' #' @param df A data frame.
#' #' 
#' #' @examples 
#' #' 
#' #' data("credit")
#' #' 
#' #' credit %>% select_categorical() %>% ez_summ_cat()
#' #' 
#' #' @export
#' ez_summ_cat <- function(df){
#'   
#'   key <- NULL
#'   
#'   grp_cols <- names(attr(df, "labels"))
#'   
#'   df %>%
#'     purrr::map_if(is.factor, as.character) %>% # avoid warning
#'     as_data_frame() %>% # this ungroup the df
#'     group_by_(.dots = lapply(grp_cols, as.symbol)) %>%  # http://stackoverflow.com/questions/21208801/
#'     do({ezsum = 
#'       tidyr::gather(., key, value) %>% # you can use tidyr::gather(., variable, category)
#'       ungroup() %>%
#'       count(key, value) %>% 
#'       mutate(p = n/sum(n))
#'     }) %>% 
#'     ungroup() %>%
#'     filter(!key %in% grp_cols) # not sure whiy appear as key a group col
#'   
#' }