#' Ranking for predictives variables 
#' @description Calculate the predictive ranking from a data frame.
#' @param df A data frame
#' @param response The name of response variable
#' @return A data frame with statistics like aucroc and ks from de univariate models `response ~ target`
#' @examples
#' data("credit")
#' df <- credit
#' target_name <- "bad"
#' @export
pred_ranking <- function(df, target_name = "target", verbose = FALSE){
  
  require("ROCR")
  require("plyr")
  require("dplyr")
  
  target <- df[[target_name]]
  
  df2 <- df %>% subset(select = setdiff(names(df), target_name))
  
  df2 <- df2[,laply(df2, function(v){ if (length(unique(na.omit(v))) == 1) { FALSE } else {TRUE} })]
  
  res <- ldply(names(df2), function(namevar){
    # namevar <- sample(names(df2), size = 1)
    if(verbose) message(namevar)
    
    pred_var <- df[[namevar]]
    daux <- data.frame(target = target, pred_var = pred_var)
    daux_naomit <- na.omit(daux)
    
    model <- glm(target ~ pred_var, data = daux_naomit, family = binomial(link = logit))
    
    score <- model$fitted.values
    
    resp <- data_frame(variable = namevar,
                       ks = ks(score, target),
                       aucroc = aucroc(score, target))
    resp <- cbind(resp, perf(score, target))
    
    resp
    
  }, .progress = "text")
  
  res <- res %>%
    tbl_df() %>% 
    arrange(desc(aucroc))
  
  res
}