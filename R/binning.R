#' Supervised Binning
#' @description Automatic supervised binning 
#' @param target A numeric binary vector {0,1}
#' @param variable A variable 
#' @return A list of elements
#' @examples
#' 
#' data("credit")
#' 
#' bin_sup(variable = credit$age, credit$bad)
#' 
#' bin_sup(variable = credit$marital_status, credit$bad)
#' 
#' @export
bin_sup <- function(variable, target){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(variable)
    )
  
  df <- dplyr::data_frame(target, variable)

  tree <- partykit::ctree(factor(target) ~ variable, data = df)
  
  df$node <- predict(tree, type = "node")
  
  n

  if (is.numeric(variable)) {
    
    type <- "numeric"
    
    df2 <- df %>%
      dplyr::group_by(node) %>%
      dplyr::summarise(max = max(variable))
    
    dict <- c(-Inf, df2$max, Inf)
    
    nbins <- length(dict)
    
    df$new_variable <- cut(df$variable, dict, labels = FALSE)
    df$new_variable <- cut(df$variable, dict, labels = FALSE)
    
  } else {
    
    type <- "categorical"
    
    df2 <- df %>%
      dplyr::select(variable, node) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(variable_bin =  seq(nrow(.)),
                    variable_bin = paste0("group_", variable_bin))
    
    dict <- df2
    
    df <- dplyr::left_join(df,)
    
    df$variable_bin <- cut(df$variable, dict)
    
  }
  
  bt(df$node, df$target)
  
  list(data = dplyr::tbl_df(df), tree = tree, type = type, dict = dict)

}

