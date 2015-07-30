#' Make a data frame looks a bit pretty
#' @description This function standarize a data frame converting factors to characters, setting to lowercase the column names.
#' @param df A data frame
#' @param to.lower.df.names df' names to lower?
#' @param factor.to.string Factor to string?
#' @param fill.num.na.with Value to replace NA when there are NAs in a numeric variables 
#' @param fill.chr.na.with Value to replace NA when there are NAs in a character variables 
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
#' @export
pretty_df <- function(df,
                      to.lower.df.names = TRUE,
                      factor.to.string = TRUE,
                      fill.num.na.with = 0,
                      fill.chr.na.with = "",
                      trim.chr.vars = TRUE){

  if (to.lower.df.names) {
    names(df) <- tolower(names(df))  
  }
  
  if (factor.to.string) {
    
    df[,plyr::laply(df, is.factor)] <- lapply(df[,plyr::laply(df, is.factor)] , as.character)
  
  } 
  
  if (!is.null(fill.num.na.with)) {
    df[,plyr::laply(df, is.numeric)] <- lapply(df[,plyr::laply(df, is.numeric)] , function(x) {
      ifelse(is.na(x), fill.num.na.with, x)
    })
  }
  
  if (!is.null(fill.chr.na.with)) {
    df[,plyr::laply(df, is.character)] <- lapply(df[,plyr::laply(df, is.character)] , function(x) {
      ifelse(is.na(x), fill.chr.na.with, x)
    })
  }
  
  if (trim.chr.vars) {
    df[,plyr::laply(df, is.character)] <- lapply(df[,plyr::laply(df, is.character)] , function(x) {
      gsub("^\\s+|\\s+$", "", x)
    })
  }
  
  df <- dplyr::tbl_df(df)
  
  df
  
}