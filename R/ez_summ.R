#' Selecting categorical variables from a data frame
#' 
#' @param df A data frame.
#' 
#' @examples 
#' 
#' data("credit")
#' 
#' credit %>% select_categorical()
#' 
#' @export
select_categorical <- function(df){
  
  selections <- purrr::map_lgl(df, function(x) {
    is.character(x) ||
      is.factor(x) ||
      is.logical(x)
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
  
  selections <- purrr::map_lgl(df, function(x) is.numeric(x) )
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
ez_summ_num <- function(df, na.rm = TRUE, probs = c(0.01, 0.05, 1:9/10, 0.95,.99)){
  
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
        max = max(value, na.rm = na.rm),
        unvariability = unvariability(value, na.rm = na.rm))
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
#' 
#' @examples
#'
#' data("credit")
#'
#' credit %>% ez_summ_biv(target_name = "bad") 
#' 
#' 
#' @export
ez_summ_biv <- function(df, target_name = NULL, verbose = TRUE){
  
  # library(dplyr)
  # data(credit)
  # df <- credit
  # target_name <- "bad"
  
  stopifnot(!is.null(target_name), setequal(df[[target_name]], c(0, 1)))
  
  target <- df[[target_name]]
  
  df <- df %>% dplyr::select_(paste0("-", target_name))
  
  res <- purrr::map_df(names(df),function(pred_var_name){ # pred_var_name <- sample(names(df), size = 1)
    
    if (verbose) message("bivariate: ", pred_var_name)
    
    variable <- df[[pred_var_name]]
    
    varb <- superv_bin(variable, target)$variable_new
    
    cbind(variable = pred_var_name, bt(varb, target))
      
    })
  
  res
  
}

#' Generate Summary Tables for data frames
#'
#' @param df A data frame.
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
ez_summ <- function(df, target_name = NULL, verbose = TRUE, ...){

  res <- NULL
  
  res$categorical <- df %>% 
    select_categorical() %>% 
    ez_summ_cat()
  
  res$numeric <- df %>% 
    select_numeric() %>% 
    ez_summ_num()
  
  if (!is.null(target_name)) {
    
    res$predrank <- pred_ranking(df, target_name = target_name, verbose = verbose)
    
    res$bivariate <- df %>% ez_summ_biv(target_name = target_name, verbose = verbose) 
    
  }
  
  res$variability <- rbind(
    res$categorical %>% 
      dplyr::group_by(var) %>% 
      dplyr::summarise(maxp = max(p)),
    res$numeric %>% 
      dplyr::select(var,  maxp = unvariability)
    ) %>% 
    dplyr::arrange(desc(maxp))
  
  res
  
}


#' Calculate the unvariability
#' 
#' @param x A numeric vector
#' @param na.rm A logical indicating whether missing values should be removed.
#' 
#' @examples 
#' 
#' x <- rbinom(100, p = .008, 10)
#' unvariability(x)
#' 
#' x <- rnorm(500)
#' unvariability(x)
#' 
#' @export
unvariability <- function(x, na.rm = TRUE) {
  
  q <- quantile(x, setdiff(0:100/100, 0.5), na.rm = na.rm)
  q1 <- q[1:50]
  q2 <- q[100:51]
  daux <- dplyr::data_frame(p = 1:50 - 1, q1, q2) %>% 
    dplyr::filter(q1 == q2) 
  
  1 - 2 * ifelse(nrow(daux) == 0, 0.5, min(daux$p)/100)
  
}
    
    