#' Calculate Kolmogorov-Smirnov statistic
#'
#' @param score A numeric vector containing scores or probabilities
#' @param target A numeric binary vector (0, 1)
#' @return The KS statistic
#' @examples
#' 
#' ks(score, target)
#' @export
ks <- function(score, target){
  library(ROCR)
  pred <- prediction(score, target)
  perf <- performance(pred,"tpr","fpr")
  ks <- max(abs(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]]))
  return(ks)
}

ks2 <- function(score1, score2){
  value <- as.numeric(suppressWarnings(ks.test(score1, score2)[["statistic"]]))
  return(value)
}

aucroc <- function(score, target){
  library(ROCR)
  pred <- prediction(score,target)
  perf <- performance(pred,"tpr","fpr")
  aucroc <- attr(performance(pred,"auc"),"y.values")[[1]]
  return(aucroc)
}

gini <- function(score, target){
  gini <- 2*as.numeric(aucroc(score, target)) - 1
  return(gini)
}

divergence <- function(score, target){
  s.good <- score[target == 1]
  s.bad <- score[target == 0]
  divergence <- (mean(s.good) - mean(s.bad))^2/(var(s.good) + var(s.bad))*2
  return(divergence)
}

gain <- function(score, target, percents = c(0.10, 0.20, 0.30, 0.40, 0.50)){
  library(scales)
  g <- ecdf(score[target==0])(quantile(score,percents))
  names(g) <- percent(percents)
  g
}

score_indicators <- function(score, target){ 
  
  res <- c(Size = length(score),
           Goods = length(score[target == 1]),
           Bads = length(score[target == 0]),
           BadRate = 1 - mean(target),
           KS = ks(score,target),
           AUCROC = aucroc(score,target),
           Gini = gini(score,target),
           Divergence = divergence(score,target),
           Gain = gain(score,target))
  
  res <- data.frame(t(res))
  
  names(res) <- gsub("\\.", "", names(res))
  
  return(res)
}


