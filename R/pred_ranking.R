#' Ranking for predictives variables 
#' @description Calculate the predictive ranking from a data frame.
#' @param df A data frame
#' @param target_name The name of response variable
#' @param verbose verbose
#' @return A data frame with statistics like aucroc and ks from de univariate models `response ~ target`
#' @examples
#' data("credit")
#' df <- credit
#' target_name <- "bad"
#' 
#' pred_ranking(df, target_name)
#' 
#' @export
pred_ranking <- function(df, target_name = "target", verbose = FALSE){
    
  target <- df[[target_name]]
  
  df2 <- df %>% subset(select = setdiff(names(df), target_name))
  
  df2 <- df2[,plyr::laply(df2, function(v){ if (length(unique(na.omit(v))) == 1) { FALSE } else {TRUE} })]
  
  res <- plyr::ldply(names(df2), function(namevar){
    # namevar <- sample(names(df2), size = 1)
    if (verbose) message(namevar)
    
    pred_var <- df[[namevar]]
    daux <- data.frame(target = target, pred_var = pred_var)
    daux_naomit <- na.omit(daux)
    
    model <- glm(target ~ pred_var, data = daux_naomit, family = binomial(link = logit))
    
    score <- model$fitted.values
    
    resp <- dplyr::data_frame(variable = namevar,
                       ks = ks(target, score),
                       aucroc = aucroc(target, score))
  
  }, .progress = if (verbose) "text" else "none")
  
  res <- res %>%
    dplyr::tbl_df() %>% 
    dplyr::arrange(dplyr::desc(aucroc))
  
  res
}