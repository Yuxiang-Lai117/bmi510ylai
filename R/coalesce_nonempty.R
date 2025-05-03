#' Coalesce First Non-empty String
#'
#' Returns the first element in a character vector that is neither NA nor an empty string ("").
#'
#' @param x A character vector.
#'
#' @return A character string that is the first non-empty, non-NA element. Returns NA if none found.
#' @export
#'
#' @examples
#' coalesce_nonempty(c(NA, "", "apple", "banana"))
#' coalesce_nonempty(c(NA, ""))
coalesce_nonempty <- function(x) {
  if (!is.character(x)) {
    stop("x must be a character vector.")
  }

  valid <- x[!is.na(x) & nzchar(x)]
  if (length(valid) > 0) {
    return(valid[1])
  } else {
    return(NA_character_)
  }
}