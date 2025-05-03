#' Add-4 Odds Ratio
#'
#' Computes the add-4 odds ratio from two logical vectors.
#' This smooths 2x2 counts by adding 1 to each cell to avoid zero division.
#'
#' @param exposure A logical vector indicating exposure (TRUE/FALSE).
#' @param outcome A logical vector indicating outcome (TRUE/FALSE).
#'
#' @return A numeric value: the add-4 odds ratio.
#' @export
#'
#' @examples
#' exposure <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
#' outcome  <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
#' add4_or(exposure, outcome)
add4_or <- function(exposure, outcome) {
  if (!is.logical(exposure) || !is.logical(outcome)) {
    stop("Both exposure and outcome must be logical vectors.")
  }
  if (length(exposure) != length(outcome)) {
    stop("Exposure and outcome must be of equal length.")
  }

  a <- sum(exposure & outcome, na.rm = TRUE)
  b <- sum(exposure & !outcome, na.rm = TRUE)
  c <- sum(!exposure & outcome, na.rm = TRUE)
  d <- sum(!exposure & !outcome, na.rm = TRUE)

  or <- ((a + 1) / (b + 1)) / ((c + 1) / (d + 1))
  return(or)
}