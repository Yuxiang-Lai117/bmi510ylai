#' Return Majority (Most Frequent) Label
#'
#' Returns the most frequent value (mode) in a vector. Optionally removes NA values.
#'
#' @param x A vector of any type.
#' @param na.rm Logical. Whether to remove NA values before computation. Default is TRUE.
#'
#' @return The most frequent value in the vector, or NA if no valid entries.
#' @export
#'
#' @examples
#' majority_label(c("cat", "dog", "cat", "bird"))
#' majority_label(c(1, 2, 2, 3, NA))
#' majority_label(c(NA, NA), na.rm = TRUE)
majority_label <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    return(NA)
  }

  tab <- table(x)
  mode_value <- names(tab)[which.max(tab)]
  return(type.convert(mode_value, as.is = TRUE))  # preserve original type
}