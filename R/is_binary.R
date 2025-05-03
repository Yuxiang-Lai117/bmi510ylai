#' Check If Vector Is Binary
#'
#' Determines whether a vector is binary (has only two unique values).
#' Optionally enforces strict check: must be exactly 0/1 or TRUE/FALSE.
#'
#' @param x A vector of any type.
#' @param strict Logical. If TRUE, checks for exact values (0/1 or TRUE/FALSE). Default is FALSE.
#'
#' @return Logical TRUE if binary, FALSE otherwise.
#' @export
#'
#' @examples
#' is_binary(c(0, 1, 0, 1))
#' is_binary(c("yes", "no"))
#' is_binary(c(0, 1, 2))
#' is_binary(c(TRUE, FALSE, TRUE), strict = TRUE)
is_binary <- function(x, strict = FALSE) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) != 2) {
    return(FALSE)
  }

  if (strict) {
    valid <- all(ux %in% c(0, 1)) || all(ux %in% c(TRUE, FALSE))
    return(valid)
  }

  return(TRUE)
}