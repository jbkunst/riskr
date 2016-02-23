#' Ranking for predictives variables 
#' @description Calculate the predictive ranking from a data frame.
#' @param df A data frame
#' @param target_name The name of response variable
#' @param nuniques Limit to consider a numeric varialbe as categorical one
#' 
#' @return A data frame with statistics like aucroc and ks from de univariate models `response ~ target`
#' 
#' @examples
#' data("credit")
#' df <- credit
#' target_name <- "bad"
#' 
#' pred_ranking(df, target_name)
#' 
#' @export
pred_ranking <- function(df, target_name = NULL, nuniques = 10){
  
  
  stopifnot(!is.null(target_name),
            target_name %in% names(df),
            setequal(df[[target_name]], c(0, 1)))
  
  target <- df[[target_name]]
  
  df <- df %>% dplyr::select_(paste0("-", target_name))

  res <- df %>%
    purrr::map_df(function(pred_var){
      
      if (length(unique(pred_var)) == 1)
        return(dplyr::data_frame(ks = NA))
      
      # Prepare data
      daux <- data.frame(target = target, pred_var = pred_var)
      daux_naomit <- na.omit(daux)
      
      # Logistic models
      model <- glm(target ~ pred_var, data = daux_naomit, family = binomial(link = logit))
      score <- model$fitted.values
      
      if (length(unique(pred_var)) > nuniques) {
        pred_var <- superv_bin(pred_var, target)$variable_new
      } 
      bvtb <- bt(pred_var, target)
      
      iv <- sum(bvtb$iv)
      
      dplyr::data_frame(ks = ks(daux_naomit$target, score),
                        aucroc = aucroc(daux_naomit$target, score),
                        na = nrow(daux) - nrow(daux_naomit),
                        iv = iv)
    }, .id = "variable")
  
  res <- res %>%
    mutate(iv_label = cut(iv, include.lowest = TRUE, 
                          breaks = c(0, 0.02, 0.1, 0.3, 0.5, Inf),
                          labels = c("unpredictive", "weak", "medium", "strong", "suspicious"))) %>% 
    arrange(-iv)

  res
}