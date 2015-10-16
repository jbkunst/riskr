node <- NULL
#' Supervised Binning
#' @description Automatic supervised binning 
#' @param target A numeric binary vector {0,1}
#' @param variable A variable 
#' @param min.p Minimal proportion in a group
#' @return A list of elements
#' @examples
#' 
#' data("credit")
#' 
#' variable <-  credit$age
#' target <- credit$bad
#'
#' bin_sup(variable, target)
#' 
#' variable <- credit$marital_status
#' 
#' bin_sup(variable, target)
#' 
#' @export
bin_sup <- function(variable, target, min.p = 0.05){
  
  #### arguments validation ####
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(variable),
    dplyr::between(min.p, 0 + 1e-9, 1 - 1e-9)
    )
  
  ### ctree controls ###
  mb <- ceiling(round(min.p * length(target)))
  control <- partykit::ctree_control(minbucket = mb)
  
  ### tree ####
  if (!is.numeric(variable)) variable <- factor(variable)
  
  df <- dplyr::data_frame(target, variable)
  
  tree <- partykit::ctree(factor(target) ~ variable, data = df, control = control)
  
  # plot(tree, gp = grid::gpar(fontsize = 10)) 
  
  df$node <- predict(tree, type = "node")

  nbins <- partykit::width(tree)
  
  if (is.numeric(variable)) {
    
    type <- "numeric"

    df2 <- df %>%
      dplyr::group_by(node) %>%
      dplyr::summarise(max = max(variable))
    
    cuts <- c(-Inf, df2$max, Inf)
    
    df$variable_new <- cut(df$variable, cuts)

  } else {
    
    type <- "categorical"
    
    df2 <- df %>%
      dplyr::group_by(node) %>% 
      dplyr::summarise(target_rate = mean(target)) %>% 
      dplyr::arrange(target_rate) %>% 
      dplyr::mutate(variable_new = seq(nbins)) %>% 
      dplyr::select(-target_rate)
    
    df <- dplyr::left_join(df, df2, by =  c("node"))
    
    max_width <- max(nchar(df$variable_new))
    
    df$variable_new <- stringr::str_pad(df$variable_new, width = max_width, side = "left", pad = "0")
    df$variable_new <- paste("group", df$variable_new, sep  = "_")
    
  }
  
  dfbt <- riskr::bt(df$variable_new, df$target) %>%
    dplyr::select(class, woe) %>%
    dplyr::rename(variable_new = class, variable_new_woe = woe)
  
  df <- dplyr::left_join(df, dfbt, by =  c("variable_new")) 

  
  list(data = dplyr::tbl_df(df), tree = tree, type = type,
       variable_new = df$variable_new, variable_new_woe = df$variable_new_woe)

}

.bin_create_dict <- function(bin_sup_ls){
  str(bin_sup_ls)
  bin_sup_ls
}


#' Binning Class
#'
#' Binning class.
#'
#' @import R6
#' @export
Binning <- R6::R6Class(
  "Binning",
  private = list(
    tree = NULL
  ),
  public = list(
    initialize = function() {
    },
    init_ct = function(){
    },
    ### chessjs api
    ascii = function(){
    },
    clear = function(){
    },
    #### generic methods
    summary = function(){
      message("summary")
    },
    plot    = function(){
      message("plot")
    },
    print   = function(){
      message("print")
    }))



#' Generic Summary method for Binning class
#' @param object A binning object frome the Binning class
#' @param ... Other parameters
#' @export
summary.Binning <- function(object, ...) {
  object$summary()
}

#' Generic Plot method for Binning class
#' @param x A binning object frome the Binning class
#' @param y A parameter
#' @param ... Other parameters
#' @export
plot.Binning <- function(x, y=NULL, ...) {
  x$plot(...)
}

#' Generic Print method for Binning class
#' @param x A binning object frome the Binning class
#' @param ... Other parameters
#' @export
print.Binning <- function(x, ...) {
  x$print()
}