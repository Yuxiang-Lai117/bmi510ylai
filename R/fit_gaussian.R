#' Fit Gaussian Distribution
#'
#' Computes MLE estimates of the mean and variance for a numeric vector.
#'
#' @param x A numeric vector.
#'
#' @return A list with elements `mean` and `variance`, the MLE estimates.
#' @export
#'
#' @examples
#' fit_gaussian(c(1, 2, 3, 4, 5))
fit_gaussian <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be a numeric vector.")
  }

  n <- sum(!is.na(x))
  mu <- mean(x, na.rm = TRUE)
  sigma2 <- sum((x - mu)^2, na.rm = TRUE) / n  # MLE: divide by n, not n-1

  return(list(mean = mu, variance = sigma2))
}