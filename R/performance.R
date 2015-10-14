#' Calculate Kolmogorov-Smirnov statistic
#'
#' @param score A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @return The KS statistic
#' @examples
#' data(predictions)
#' 
#' score <- predictions$score
#' target <- predictions$target
#' 
#' ks(target, score)
#' 
#' @import ROCR
#' 
#' @export
ks <- function(target, score){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  pred <- prediction(score, target)
  perf <- performance(pred, "tpr", "fpr")
  
  ks <- max(abs(attr(perf, "y.values")[[1]] - attr(perf, "x.values")[[1]]))
  
  return(as.numeric(ks))
}

#' Calculate Kolmogorov-Smirnov statistic
#'
#' @param score1 A numeric vector containing scores or probabilities
#' @param score2 A numeric vector containing scores or probabilities
#' @return The KS statistic
#' @examples
#' data(predictions)
#' 
#' score <- predictions$score
#' target <- predictions$target
#' 
#' score1 <- score[target == 1]
#' score1 <- score[target != 1]
#' 
#' ks2(target, score)
#' @export
ks2 <- function(score1, score2){
  
  value <- as.numeric(suppressWarnings(ks.test(score1, score2)[["statistic"]]))
  
  return(value)
}


#' Calculate Area Under ROC Curve
#'
#' @param score A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @return The AUROC Curve
#' @examples
#' data(predictions)
#' 
#' score <- predictions$score
#' target <- predictions$target
#' 
#' aucroc(target, score)
#' @export
aucroc <- function(target, score){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  pred <- prediction(score, target)
  perf <- performance(pred, "auc")
  
  aucroc <- attr(perf, "y.values")[[1]]
  
  return(aucroc)
}

#' Calculate Gini Coefficient
#'
#' @param score A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @return The Gini Coefficient
#' @examples
#' data(predictions)
#' 
#' score <- predictions$score
#' target <- predictions$target
#' 
#' gini(target, score)
#' @references https://www.kaggle.com/wiki/RCodeForGini
#' @export
gini <- function(target, score){
  
  # gini <- 2*as.numeric(aucroc(target, score)) - 1
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  as.numeric(2 * aucroc(target, score) - 1)
}

#' Calculate Gains
#'
#' @param score A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @param percents Values to calculate the gain
#' @return The Gini Coefficient
#' @examples
#' data(predictions)
#' 
#' score <- predictions$score
#' target <- predictions$target
#' 
#' gain(target, score)
#' 
#' @export
gain <- function(target, score, percents = c(0.10, 0.20, 0.30, 0.40, 0.50)){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  g <- ecdf(score[target == 0])(quantile(score, percents))
  
  g
}

#' Calculate Divergence
#' @description Calculate the divergence between empirical distributions
#' @param score A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @return The Gini Coefficient
#' @examples
#' data(predictions)
#' 
#' score <- predictions$score
#' target <- predictions$target
#' 
#' divergence(target, score)
#' @export
divergence <- function(target, score) {
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  score_t <- score[target == 1]
  score_nt <- score[target == 0]
  
  (mean(score_t) - mean(score_nt)) ^ 2 / (var(score_t) + var(score_nt))*2
  
}


#' Summary of Performance
#'
#' @param target A numeric binary vector (0, 1)
#' @param ... A data frame of scores, or a multiples score vectors: score1, score2, etc
#' @return A 1 row data frame with the summary of the performance.
#' @examples
#' data(predictions)
#' 
#' score <- predictions$score
#' target <- predictions$target
#' 
#' perf(target, score)
#' perf(target, score, score2 = score + runif(length(score)))
#' @export
perf <- function(target, ...){

  args <- list(...)
  
  if (length(args) == 1 && is.data.frame(args[[1]])) {
    
    scores_list <- as.list(...)
    
  } else {
    
    arglist <- match.call(expand.dots = FALSE)$...
    scores_list <- lapply(arglist, eval.parent, n = 2)
    score_names <- sapply(arglist, deparse)
    
    if (!is.null(names(score_names))) {
      score_names <- ifelse(names(score_names) == "", score_names, names(score_names))
    }
    
    names(scores_list) <- score_names
    
  }
  
  df <- plyr::ldply(scores_list, function(score){
    
    aux <- c(ks = ks(target, score),
             aucroc = aucroc(target, score),
             gini = gini(target, score),
             divergence = divergence(target, score))
    
    data.frame(t(aux))
    
  })
  
  if (length(scores_list) == 1) {
    df <- df[,-1]
  } else {
    names(df)[1] <- "score"  
  } 

  df

}