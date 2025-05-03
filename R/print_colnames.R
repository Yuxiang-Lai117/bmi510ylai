#' Print Column Names One per Line
#'
#' Prints the column names of a data frame or tibble, one per line.
#'
#' @param df A data frame or tibble.
#'
#' @return Invisibly returns a character vector of column names.
#' @export
#'
#' @examples
#' df <- data.frame(a = 1, b = 2, c = 3)
#' print_colnames(df)
print_colnames <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame or tibble.")
  }

  cols <- colnames(df)
  for (col in cols) {
    cat(col, "\n")
  }

  invisible(cols)
}