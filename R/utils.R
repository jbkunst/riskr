#' Standarize a data frame
#' @description This function standarize a data frame converting factors to characters, setting to lowercase the column names.
#' @param df A data frame
#' @return The same df with \code{tbl_df} class, lower names.
#' @examples
#' df <- data.frame(LETTERS, AA = rnorm(length(LETTERS)), leTTers2 = letters)
#' df[c(1, 4),2] <- NA
#' str(df)
#' str(sd_df(df))
#' @export
sd_df <- function(df, to.lower.df.names = TRUE, factor.to.string = TRUE, fill.num.na = TRUE, fill.chr.na = TRUE){
  
  df <- df %>% dplyr::tbl_df()
  
  if (to.lower.df.names) {
    names(df) <- tolower(names(df))  
  }
  
  if (factor.to.string) {
    
    df[,laply(df, is.factor)] <- lapply(df[,plyr::laply(df, is.factor)] , as.character)
  
  } 
  
  df
  
}