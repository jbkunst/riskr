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

#' Generate Summary Tables for categorical variables
#'
#' @param df A data frame.
#'
#' @examples
#'
#' data("credit")
#'
#' credit %>% select_categorical() %>% ez_summ_cat()
#' 
#' @export
ez_summ_cat <- function(df){

  var <- NULL

  grp_cols <- names(attr(df, "labels"))

  df %>%
    purrr::map_if(is.factor, as.character) %>% # avoid warning
    dplyr::as_data_frame() %>% # this ungroup the df
    dplyr::group_by_(.dots = lapply(grp_cols, as.symbol)) %>%  # http://stackoverflow.com/questions/21208801/
    dplyr::do({ezsum =
      tidyr::gather(., var, value) %>% 
      dplyr::ungroup() %>%
      dplyr::count(var, value) %>%
      dplyr::mutate(p = n/sum(n))
    }) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!var %in% grp_cols) # not sure whiy appear as var a group col

}

#' Generate Summary Tables for numercial variables
#'
#' @param df A data frame.
#' @param na.rm A logical indicating whether missing values should be removed.
#' @param probs numeric vector of probabilities with values in [0,1].
#'
#' @examples
#'
#' data("credit")
#'
#' credit %>% select_numeric() %>% ez_summ_num()
#' 
#' @export
ez_summ_num <- function(df, na.rm = TRUE, probs = c(1:9/10)){
  
  r1 <- df %>%
    dplyr::do(
      tidyr::gather(., var, value) %>% 
      dplyr::group_by(var) %>%
        dplyr::summarize(
        n = length(value),
        na = sum(is.na(value)),
        na.percent = sum(is.na(value))/length(value),
        mean = mean(value, na.rm = na.rm),
        sd = sd(value, na.rm = na.rm),
        min = min(value, na.rm = na.rm),
        max = max(value, na.rm = na.rm))
    ) %>%
    dplyr::ungroup() 
  
  r2 <- df %>% 
    dplyr::do(
      tidyr::gather(., var, value) %>% 
        dplyr::group_by(var) %>% 
        dplyr::do(dplyr::data_frame(quantile = paste0("q", round(100*probs)),
                                    value = quantile(.$value, probs = probs)))
    ) %>%
    tidyr::spread(quantile, value)
  
  
  dplyr::left_join(r1, r2, by = "var")
  
}

    
    
    
    
    