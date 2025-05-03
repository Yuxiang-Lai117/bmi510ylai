#' Compute Confusion Metrics
#'
#' Computes accuracy, sensitivity (recall), and specificity from binary truth and prediction vectors.
#'
#' @param truth A logical or binary numeric vector of ground truth labels.
#' @param pred A logical or binary numeric vector of predicted labels.
#'
#' @return A list with elements: `accuracy`, `sensitivity`, and `specificity`.
#' @export
#'
#' @examples
#' truth <- c(TRUE, FALSE, TRUE, FALSE)
#' pred <- c(TRUE, TRUE, FALSE, FALSE)
#' confusion_metrics(truth, pred)
confusion_metrics <- function(truth, pred) {
  if (length(truth) != length(pred)) {
    stop("truth and pred must be the same length.")
  }

  truth <- as.logical(truth)
  pred <- as.logical(pred)

  TP <- sum(truth & pred, na.rm = TRUE)
  TN <- sum(!truth & !pred, na.rm = TRUE)
  FP <- sum(!truth & pred, na.rm = TRUE)
  FN <- sum(truth & !pred, na.rm = TRUE)

  acc <- (TP + TN) / (TP + TN + FP + FN)
  sens <- if ((TP + FN) == 0) NA else TP / (TP + FN)
  spec <- if ((TN + FP) == 0) NA else TN / (TN + FP)

  return(list(
    accuracy = acc,
    sensitivity = sens,
    specificity = spec
  ))
}