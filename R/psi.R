new_percent <- act_percent <- coefficient <- diff_percent <- woe <- NULL
#' PSI
#' @description Calculation of  Population Stability Index (PSI)
#' @param actual A vector of original distribution
#' @param new A vector with the new distribution
#' @return A list with psi index (value) a label and the table for with counts, percents, woe.
#' @examples
#' set.seed(1313)
#' 
#' actual <- sample(letters[1:3], size = 1000, prob = c(1,1,2), replace = TRUE)
#' new <- sample(letters[1:3], size = 3000, prob = c(1,2,2), replace = TRUE)
#' 
#' psi(actual, new)
#' @export
psi <- function(actual, new){
  
  stopifnot(
    setequal(actual, new)
  )
  
  psi_tbl <- psi_table(actual, new)
  
  value <- sum(psi_tbl$index)
  
  label <- cut(value, c(0, 0.1, 0.25, Inf),
               labels = c("Insignificant change", "Some minor change", "Major shift in population")) %>% 
    as.character()
  
  list(value = value, label = label, table = psi_tbl)
  
}

#' PSI Table
#' @description Table for calculate Population Stability Index (PSI)
#' @param actual A vector of original distribution
#' @param new A vector with the new distribution
#' @return A list with psi index (value) a label and the table for with counts, percents, woe.
#' @examples
#' set.seed(1313)
#' 
#' actual <- sample(letters[1:3], size = 1000, prob = c(1,1,2), replace = TRUE)
#' new <- sample(letters[1:3], size = 3000, prob = c(1,2,2), replace = TRUE)
#' 
#' psi_table(actual, new)
#' @export
psi_table <- function(actual, new){
  
  stopifnot(
    setequal(actual, new)
  )
   
  act_df <- ft(actual)
  
  new_df <- ft(new)
  
  names(act_df)[-1] <- paste("act", names(act_df)[-1], sep = "_")
  
  names(new_df)[-1] <- paste("new", names(new_df)[-1], sep = "_")
  
  psi_tbl <- dplyr::full_join(act_df, new_df, by = "class")
  
  psi_tbl <- psi_tbl %>% 
    dplyr::mutate(diff_percent = new_percent - act_percent,
           coefficient = new_percent / act_percent,
           woe = log(coefficient),
           index = diff_percent * woe)
  
  return(psi_tbl)
  
}