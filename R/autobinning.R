#' Autobinning
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
#' autobinning(credit$bad, credit$age)
#' 
#' @export
autobinning <- function(target, variable){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(variable)
    )
  
  if (!is.numeric(variable)) {
    variable <- factor(variable)
  }
  

  df <- data.frame(target = factor(target), variable)

#   partykit::ctree(target ~ variable, data = df)
#   rpart::rpart(target ~ variable, data = df, control = rpart::rpart.control(cp = 0.00004))
  
  
  df
 
}

