#' Make a data frame looks a bit pretty
#' @description This function standarize a data frame converting factors to characters, setting to lowercase the column names.
#' @param df A data frame
#' @param to.lower.df.names df' names to lower?
#' @param factor.to.string Factor to string?
#' @param replace.num.na.with Value to replace NA when there are NAs in a numeric variables 
#' @param replace.chr.na.with Value to replace NA when there are NAs in a character variables 
#' @param trim.chr.vars Trim character vars=
#' @return The same df with \code{tbl_df} class, lower names.
#' @examples
#'
#' df <- data.frame(leTters = LETTERS[1:10], AA = rnorm(10), leTTers2 = letters[1:10])
#' 
#' df[c(1, 4),2] <- NA
#' df[c(2, 5),1] <- NA
#' 
#' str(df)
#' str(pretty_df(df))
#' pretty_df(df, replace.num.na.with = 0)
#' pretty_df(df, replace.chr.na.with = "no data")
#' @export
pretty_df <- function(df,
                      to.lower.df.names = TRUE,
                      factor.to.string = TRUE,
                      replace.num.na.with = NULL,
                      replace.chr.na.with = NULL,
                      trim.chr.vars = TRUE){
  
  assertthat::assert_that(is.data.frame(df))

  if (to.lower.df.names)
    names(df) <- tolower(names(df))  

  if (factor.to.string)
    df <- purrr::map_if(df, is.factor, as.character)

  if (!is.null(replace.num.na.with))
    df <- purrr::map_if(df, is.numeric, .repalce_na_with, replace = replace.num.na.with)
  
  if (!is.null(replace.chr.na.with))
    df <- purrr::map_if(df, is.character, .repalce_na_with, replace = replace.chr.na.with)
  
  if (trim.chr.vars)
    df <- purrr::map_if(df, is.character, stringr::str_trim)
  
  df <- dplyr::tbl_df(data.frame(df, stringsAsFactors = FALSE))
  
  df
  
}

.repalce_na_with <- function(x, replace = NULL) {
  ifelse(is.na(x), replace, x)
}
