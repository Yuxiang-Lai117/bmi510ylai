#' Fair Accuracy by Group
#'
#' Calculates classification accuracy within each group.
#'
#' @param truth A logical or binary numeric vector of ground truth labels.
#' @param pred A logical or binary numeric vector of predicted labels.
#' @param group A factor or character vector indicating group membership.
#'
#' @return A named numeric vector with accuracy per group.
#' @export
#'
#' @examples
#' truth <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
#' pred <-  c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)
#' group <- c("A", "A", "B", "B", "B", "A")
#' fair_accuracy(truth, pred, group)
fair_accuracy <- function(truth, pred, group) {
  if (length(truth) != length(pred) || length(truth) != length(group)) {
    stop("truth, pred, and group must be of the same length.")
  }

  group <- as.factor(group)
  truth <- as.logical(truth)
  pred <- as.logical(pred)

  acc_by_group <- tapply(seq_along(truth), group, function(idx) {
    mean(truth[idx] == pred[idx], na.rm = TRUE)
  })

  return(acc_by_group)
}