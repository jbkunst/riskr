#' Supervised Binning
#' @description Automatic supervised binning 
#' @param target A numeric binary vector {0,1}
#' @param variable A variable 
#' @return A list of elements
#' @examples
#' 
#' data("credit")
#' 
#' target <- credit$bad
#' variable <- credit$age
#' 
#' sup_bin(credit$bad, credit$age)
#' 
#' @export
sup_bin <- function(target, variable){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(variable)
    )
  
  if (!is.numeric(variable)) {
    variable <- factor(variable)
  }
  

  df <- data.frame(target = factor(target), variable)

  tree <- partykit::ctree(target ~ variable, data = df)
  
  df$node <- predict(tree, type = "node")
  
  ls <- list()
  
  attr(ls, "class") <-  "supervised_binning"
  
  ls["tree"] <- tree
  
  
  if (is.numeric(variable)){
    
    ls["type"] <- "numeric"
    
    df <-df %>%
      dplyr::group_by(node) %>%
      dplyr::summarise(max = max(variable))
    
    cuts <- c(-Inf, df$max, Inf)
    
  } else {
    ls["type"] <- "categorical"
    
  }
  
  ls
  
}

