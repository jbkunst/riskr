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
#' @param nuniques Limit to consider a numeric varialbe as categorical one
#' 
#' @examples 
#' 
#' data("credit")
#' 
#' credit %>% select_numeric()
#' 
#' @export
select_numeric <- function(df, nuniques = 10){
  
  selections <- purrr::map_lgl(df, function(x) is.numeric(x) | (length(unique(x)) > nuniques) )
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
                                    value = quantile(.$value, probs = probs, na.rm = na.rm)))
    ) %>%
    tidyr::spread(quantile, value)
  
  
  dplyr::left_join(r1, r2, by = "var")
  
}


#' Generate Summary Tables for numercial variables
#'
#' @param df A data frame.
#' @param target_name The name of response variable
#' @param nuniques Limit to consider a numeric varialbe as categorical one
#' 
#' @examples
#'
#' data("credit")
#'
#' credit %>% ez_summ_biv(target_name = "bad") 
#' 
#' 
#' @export
ez_summ_biv <- function(df, target_name = NULL, nuniques = 10){
  
  # library(dplyr)
  # data(credit)
  # df <- credit
  # target_name <- "bad"
  
  stopifnot(!is.null(target_name),
       setequal(df[[target_name]], c(0, 1)))
  
  target <- df[[target_name]]
  
  df <- df %>% dplyr::select_(paste0("-", target_name))
  
  res <- df %>% 
    purrr::map_df(function(var){
      
      if (length(unique(var)) > nuniques) {
        var <- superv_bin(var, target)$variable_new
      } 
      bt(var, target)
    }, .id = "variable")
  
  res
  
}

#' Generate Summary Tables for data frames
#'
#' @param df A data frame.
#' @param nuniques Limit to consider a numeric varialbe as categorical one
#' @param target_name Target name if bivariate summary is required
#' @param ... A list of additional arguments
#' 
#' @return A list
#'
#' @examples
#'
#' data("credit")
#' 
#' credit %>% ez_summ()
#' credit %>% ez_summ(target_name = "bad")
#'
#' @export
ez_summ <- function(df, nuniques = 10, target_name = NULL, ...){

  res <- NULL
  
  res$categorical <- df %>% 
    select_categorical(nuniques = nuniques) %>% 
    ez_summ_cat()
  
  res$numeric <- df %>% 
    select_numeric(nuniques = nuniques, ...) %>% 
    ez_summ_num()
  
  if (!is.null(target_name)) {
    
    res$predrank <- pred_ranking(df, target_name = target_name, nuniques = nuniques)
    
    res$bivariate <- df %>% ez_summ_biv(target_name = target_name, nuniques = nuniques) 
    
  }
  
  res
  
}
    
    