odds_table <- function(score, target, breaks = NULL, nclass = 10, quantile = TRUE){
  
  library("ggplot2")
  
  if (missing(breaks) & quantile) {
    
    score_cat <- cut_number(score, n = nclass)
    
  } else if (missing(breaks) & !quantile) {
    
    score_cat <- cut_interval(score, n = nclass)
    
  } else {
    
    score_cat <- cut(score, breaks = breaks)
  }
  
  t <- table(score_cat, target)
  
  nclass <- dim(t)[1]
  
  N <- sum(t)
  
  ot <- data.frame(Class          = row.names(t),
                   Freq           = (t[,1]+t[,2]),
                   FreqRel        = (t[,1]+t[,2])/N,
                   FreqRelAcum    = cumsum((t[,1]+t[,2])/N),
                   FreqRelDesAcum = c(1,((sum(t[,1]+t[,2])-cumsum(t[,1]+t[,2]))/N)[1:(nclass-1)]),
                   FreqBad        = t[,1],
                   FreqRelBad     = t[,1]/sum(t[,1]),
                   FreqRelBadAcum = cumsum(t[,1]/sum(t[,1])),
                   FreqRelBadDesAcum  = c(1,((sum(t[,1])-cumsum(t[,1]))/sum(t[,1]))[1:(nclass-1)]),
                   BadRate        = t[,1]/(t[,1]+t[,2]),
                   BadRateAcum    = cumsum(t[,1])/cumsum((t[,1]+t[,2])),
                   BadRateDesacum = c((cumsum(t[,1])/cumsum((t[,1]+t[,2])))[nclass],((sum(t[,1])-cumsum(t[,1]))/(sum(t[,1]+t[,2])-cumsum(t[,1]+t[,2])))[1:(nclass-1)]),
                   Odds           =  t[,2]/ t[,1], row.names = NULL)
  
  return(ot)
}
