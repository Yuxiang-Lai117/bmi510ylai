#' Adjust and Filter P-values
#'
#' Adjusts p-values using a specified multiple comparisons correction method,
#' then filters to retain only those below the given significance threshold (alpha).
#'
#' @param p_values A numeric vector of raw p-values.
#' @param method Character. Adjustment method (e.g., "holm", "bonferroni", "fdr"). Default is "holm".
#' @param alpha Numeric. Significance threshold after adjustment. Default is 0.05.
#'
#' @return A numeric vector of adjusted p-values. Values not passing the threshold are set to NA.
#' @export
#'
#' @examples
#' raw_p <- c(0.01, 0.03, 0.2, 0.5)
#' adjust_and_filter_p(raw_p, method = "holm", alpha = 0.05)
adjust_and_filter_p <- function(p_values, method = "holm", alpha = 0.05) {
  if (!is.numeric(p_values)) {
    stop("p_values must be a numeric vector.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha > 1) {
    stop("alpha must be a number between 0 and 1.")
  }

  adjusted <- p.adjust(p_values, method = method)
  filtered <- ifelse(adjusted <= alpha, adjusted, NA_real_)
  return(filtered)
}