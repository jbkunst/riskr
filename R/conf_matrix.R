#' Create confusion matrix
#'
#' @param pred_class A vector numeric binary (0, 1) of predictions
#' @param target A numeric binary vector (0, 1)
#' @return A list
#' @examples
#' conf_matrix()
#' @export
#' @references http://www2.cs.uregina.ca/~dbd/cs831/notes/confusion_matrix/confusion_matrix.html
conf_matrix <- function(pred_class, target) {
  
  t <- table(true = target, prediction = pred_class)
  #                     Prediction
  #                 NegPred   PosPred
  # real NegOutcome
  # real PosOutcome
  AC <- sum(diag(t))/sum(t) #Accuracy (AC) is the he proportion of the total number of score that were correct.
  TP <- t[2,2]/sum(t[2,1:2])   #Recall or true positive rate (TP) is the proportion of positive cases that were correctly identified. (BB)
  FP <- t[1,2]/sum(t[1,1:2])   #False positive rate (FP) is the proportion of negatives cases that were incorrectly classified as positive
  TN <- t[1,1]/sum(t[1,1:2])   #True negative rate (TN) is defined as the proportion of negatives cases that were classified correctly (MM)
  FN <- t[2,1]/sum(t[2,1:2])   #False negative rate (FN) is the proportion of positives cases that were incorrectly classified as negative
  P <- t[2,2]/sum(t[1:2,2])    #Precision (P) is the proportion of the predicted positive cases that were correct
  
  t2 <-  as.data.frame.matrix(t, row.names = NULL)
  names(t2) <- paste("pred", colnames(t))
  t2 <- cbind(class = paste("true", rownames(t)),  t2)
  
  t3 <- data.frame(term = c("Accuracy",  "Recall | True Positive rate (GG)",  "False Positive rate",
                            "True Negative rate (BB)", "False Negative rate", "Precision"),
                   term.short = c("AC", "Recall", "FP", "TN", "FN", "P"),
                   value = c(AC, TP, FP, TN, FN, P), stringsAsFactors=FALSE)
  
  response <- list(confusion.matrix = t2,
                   indicators = t3,
                   indicators.t = setNames(data.frame(t(t3$value)), t3$term.short))
  
  return(response)
}