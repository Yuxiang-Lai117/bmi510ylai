#' Fit Bernoulli Distribution
#'
#' Estimates the success probability of a Bernoulli distribution using maximum likelihood estimation (MLE).
#'
#' @param x A logical or binary numeric vector (0/1).
#'
#' @return A list with one element `p`, the estimated probability of success.
#' @export
#'
#' @examples
#' fit_bernoulli(c(TRUE, FALSE, TRUE, TRUE))
#' fit_bernoulli(c(1, 0, 1, 1))
fit_bernoulli <- function(x) {
  if (!is.logical(x) && !all(x %in% c(0, 1), na.rm = TRUE)) {
    stop("x must be a logical vector or binary numeric (0/1).")
  }

  x <- as.numeric(x)
  p_hat <- mean(x, na.rm = TRUE)

  return(list(p = p_hat))
}