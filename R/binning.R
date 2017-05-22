node <- NULL
#' Supervised Binning
#' @description Automatic supervised binning 
#' @param target A numeric binary vector {0,1}
#' @param variable A variable 
#' @param min.p Minimal proportion in a group (a ctree_control argument).
#' @param min.cri Minimal critetion (a ctree_control argument).
#' @param max.depth Maximun depth in the tree
#' 
#' @return A list of elements
#' 
#' @examples
#' 
#' data("credit")
#' 
#' variable <-  credit$age
#' target <- credit$bad
#'
#' superv_bin(variable, target)
#' 
#' variable <- credit$marital_status
#' 
#' superv_bin(variable, target)
#' 
#' @export
superv_bin <- function(variable, target, min.p = 0.05, min.cri = 0.95, max.depth = 5){
  
  #### arguments validation ####
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(variable),
    dplyr::between(min.p, 0 + 1e-9, 1 - 1e-9)
    )
  
  ### ctree controls ###
  mb <- ceiling(round(min.p * length(target)))
  control <- partykit::ctree_control(minbucket = mb,
                                     mincriterion = min.cri,
                                     maxdepth = max.depth)
  
  ### tree ####
  if (!is.numeric(variable)) variable <- factor(variable)
  
  dftree <- dplyr::data_frame(target, variable)
  
  if (any(is.na(variable))) {
    
    message("warning: removing ", sum(is.na(variable)), " NA' values to construct tree")
    
    dftreena <- dftree %>% filter(!is.na(variable))
    
  }
    
  tree <- partykit::ctree(factor(target) ~ variable, data = dftree, control = control)
  
  # plot(tree, gp = grid::gpar(fontsize = 10)) 
  
  dftree$node <- predict(tree, newdata = dftree, type = "node")

  nbins <- partykit::width(tree)
  
  if (is.numeric(variable)) {
    
    type <- "numeric"
    
    dftree2 <- dftree %>%
      dplyr::group_by(node) %>%
      dplyr::summarise(max = max(variable, na.rm = TRUE))
    
    cuts <- c(-Inf, head(dftree2$max, -1), Inf)
    
    dftree$variable_new <- cut(dftree$variable, cuts)

  } else {
    
    type <- "categorical"
    
    dftree2 <- dftree %>%
      dplyr::group_by(node) %>% 
      dplyr::summarise(target_rate = mean(target)) %>% 
      dplyr::arrange(target_rate) %>% 
      dplyr::mutate(variable_new = seq(nbins)) %>% 
      dplyr::select(-target_rate)
    
    dftree <- dplyr::left_join(dftree, dftree2, by =  c("node"))
    
    max_width <- max(nchar(dftree$variable_new))
    
    dftree$variable_new <- stringr::str_pad(dftree$variable_new, width = max_width, side = "left", pad = "0")
    dftree$variable_new <- paste("group", dftree$variable_new, sep  = "_")
    dftree$variable_new <- ifelse(is.na(dftree$variable), NA, dftree$variable_new)
    cuts <- NA
  }
  
  
  
  dftreebt <- riskr::bt(dftree$variable_new, dftree$target) %>%
    dplyr::select(class, woe) %>%
    dplyr::rename(variable_new = class, variable_new_woe = woe)
  
  dftree <- dplyr::left_join(dftree, dftreebt, by =  c("variable_new")) 

  list(data = dplyr::tbl_df(dftree), tree = tree, type = type,
       variable_new = dftree$variable_new, variable_new_woe = dftree$variable_new_woe,
       cuts = cuts)

}