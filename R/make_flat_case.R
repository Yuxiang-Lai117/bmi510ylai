#' Convert Variable Names to Flat Lowercase
#'
#' Converts variable names to lowercase and removes common separators (underscores, dashes, spaces).
#'
#' @param var_names A character vector of variable names.
#'
#' @return A character vector of cleaned variable names.
#' @export
#'
#' @examples
#' make_flat_case(c("First_Name", "last-name", "Phone Number"))
make_flat_case <- function(var_names) {
  if (!is.character(var_names)) {
    stop("var_names must be a character vector.")
  }

  cleaned <- tolower(var_names)
  cleaned <- gsub("[-_ ]", "", cleaned)  # remove separators
  return(cleaned)
}