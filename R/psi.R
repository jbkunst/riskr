psi_table <- function(values_ori, values_new, prefix_new = "new"){
  library(dplyr)
  
  t_ori <- freqtable(values_ori, add.total = FALSE)[,c(1, 2, 3)]
  t_new <- freqtable(values_new, add.total = FALSE)[,c(1, 2, 3)]
  
  names(t_new)[-1] <- paste(prefix_new, names(t_new)[-1], sep = "_")
  
  psi_tbl <- join(t_ori, t_new, by = "Class")
  
  psi_tbl$dif_dec <- psi_tbl[,5]-psi_tbl[,3]
  psi_tbl$coef <- psi_tbl[,5]/psi_tbl[,3]
  psi_tbl$w_ev <- log(psi_tbl[, 7])
  psi_tbl$idx <- psi_tbl[,6]*psi_tbl[,8]

  return(psi_tbl)

}

psi <- function(values_ori, values_new){
  t_psi <- psi_table(values_ori, values_new)
  return(sum(t_psi[, 9]))
}


# values_ori <- sample(letters[1:3], size = 1000, prob = c(1,1,2), replace = TRUE)
# 
# values_new <- sample(letters[1:3], size = 3000, prob = c(1,2,2), replace = TRUE)
# psi(values_ori, values_new)
# 
# values_new <- sample(letters[1:3], size = 3000, prob = c(1,2,2), replace = TRUE)
# psi(values_ori, values_new)
# 
# 
# values_new <- sample(letters[1:3], size = 3000, prob = c(1,2,1), replace = TRUE)
# psi(values_ori, values_new)
