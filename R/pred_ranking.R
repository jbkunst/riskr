#' Ranking for predictives variables 
#' @description Calculate the predictive ranking from a data frame.
#' @param df A data frame
#' @param response The name of response variable
#' @return A data frame with statistics like aucroc and ks from de univariate models `response ~ target`
#' @export
pred_ranking <- function(df, response = .(desercion_1)){
  
  require("ROCR")
  require("dplyr")
  
  response_var <- df[[names(response)]]
  
  df2 <- df[,-which(names(df) == names(response))]
  
  df2 <- df2[,laply(df2, function(v){ if (length(unique(na.omit(v))) == 1) { FALSE } else {TRUE} })]
  
  res <- ldply(names(df2), function(namevar){
    
    message(namevar)
    
    pred_var <- df[[namevar]]
    daux <- data.frame(response_var = response_var, pred_var = pred_var)
    daux_naomit <- na.omit(daux)
    
    model <- glm(response_var ~ pred_var, data = daux_naomit, family = binomial(link = logit))
    
    pred <- prediction(model$fitted.values, daux_naomit$response)
    perf <- performance(pred, "tpr","fpr")
    
    auc <- attr(performance(pred,"auc"),"y.values")[[1]]
    ks <- max(abs(attr(perf,'y.values')[[1]] - attr(perf,'x.values')[[1]]))
    
    daux_resp <- data.frame(variable = namevar,
                            aucroc = auc,
                            ks = ks,
                            na.prop = 1 - nrow(daux_naomit)/nrow(daux))
    
    return(daux_resp) 
    
  }, .progress = "text")
  
  res <- res %>%
    tbl_df() %>% 
    arrange(desc(aucroc))
  
  res
}