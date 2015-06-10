#' Standarize a data frame
#' @description This function standarize a data frame converting factors to characters, setting to lowercase the column names.
#' @param df A data frame
#' @return The same df with \code{tbl_df} class, lower names.
#' @examples
#'
#' df <- data.frame(leTters = LETTERS[1:10], AA = rnorm(10), leTTers2 = letters[1:10])
#' 
#' df[c(1, 4),2] <- NA
#' df[c(2, 5),1] <- NA
#' 
#' str(df)
#' str(sd_df(df))
#' @export
sd_df <- function(df, to.lower.df.names = TRUE, factor.to.string = TRUE,
                  fill.num.na.with = 0, fill.chr.na.with = ""){
  
  require("plyr")
  
  df <- dplyr::tbl_df(df)
  
  if (to.lower.df.names) {
    names(df) <- tolower(names(df))  
  }
  
  if (factor.to.string) {
    
    df[,laply(df, is.factor)] <- lapply(df[,plyr::laply(df, is.factor)] , as.character)
  
  } 
  
  if (!is.null(fill.num.na.with)) {
    df[,laply(df, is.numeric)] <- lapply(df[,plyr::laply(df, is.numeric)] , function(x) {
      ifelse(is.na(x), fill.num.na.with, x)
    })
  }
  
  if (!is.null(fill.chr.na.with)) {
    df[,laply(df, is.character)] <- lapply(df[,plyr::laply(df, is.character)] , function(x) {
      ifelse(is.na(x), fill.chr.na.with, x)
    })
  }
  
  df <- dplyr::tbl_df(df)
  
  df
  
}