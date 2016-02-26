#' Ranking for predictives variables 
#' @description Calculate the predictive ranking from a data frame.
#' @param df A data frame
#' @param target_name The name of response variable
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
pred_ranking <- function(df, target_name = "bm", verbose = TRUE){
  
  stopifnot(!is.null(target_name),
            target_name %in% names(df),
            setequal(df[[target_name]], c(0, 1)))
  
  target <- df[[target_name]]
  
  df <- df %>% dplyr::select_(paste0("-", target_name))
  
  res <- purrr::map_df(names(df),function(pred_var_name){
    
    if (verbose) message("pred_ranking: ", pred_var_name)
    
    pred_var <- df[[pred_var_name]]
    
    if (length(unique(pred_var)) == 1)
      return(dplyr::data_frame(variable = pred_var_name,
                               iv_label = "constant variable"))
    
    # Prepare data
    daux <- data.frame(target = target, pred_var = pred_var)
    daux_naomit <- na.omit(daux)
    
    if (length(unique(daux_naomit$target)) == 1)
      return(dplyr::data_frame(variable = pred_var_name,
                               iv_label = "constant label"))
    
    # Logistic models
    model <- glm(target ~ pred_var, data = daux_naomit, family = binomial(link = logit))
    score <- model$fitted.values
    pred_var <- superv_bin(pred_var, target)$variable_new
    
    bvtb <- bt(pred_var, target)
    
    dplyr::data_frame(variable = pred_var_name,
                      ks = ks(daux_naomit$target, score),
                      aucroc = aucroc(daux_naomit$target, score),
                      na = nrow(daux) - nrow(daux_naomit),
                      iv = sum(bvtb$iv))
    
  })
  
  res <- res %>%
    dplyr::mutate(iv_label2 = cut(iv, include.lowest = TRUE,
                                  breaks = c(0, 0.02, 0.1, 0.3, 0.5, Inf),
                                  labels = c("unpredictive", "weak", "medium", "strong", "suspicious")),
                  iv_label = ifelse(is.na(ks), iv_label, as.character(iv_label2))) %>% 
    dplyr::select(-iv_label2) %>% 
    dplyr::arrange(-iv)
  
  res
}