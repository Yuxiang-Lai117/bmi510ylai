#' Truncate Values Below Zero
#'
#' Replaces any numeric values below zero with zero.
#'
#' @param x A numeric value or vector.
#'
#' @return A numeric vector with values < 0 replaced by 0.
#' @export
#'
#' @examples
#' gt_zero(c(-1, 0, 2, -3, 5))
gt_zero <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  x[x < 0] <- 0
  return(x)
}