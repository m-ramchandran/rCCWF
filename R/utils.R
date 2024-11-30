#' Normalize a vector
#'
#' @param x Numeric vector to normalize
#' @returns Returns a numeric value representing the Euclidean (L2) norm of the input vector.
#'        This is calculated as the square root of the sum of squared elements.
#' @examples
#' norm_vec(c(3, 4)) # Should return 5
#' @export
norm_vec <- function(x) {
  sqrt(sum(x^2))
}

#' Absolute normalization of a vector
#'
#' @param vec Numeric vector to normalize
#' @param max.norm Logical, whether to use maximum normalization (default: FALSE)
#' @returns Returns a numeric vector of the same length as the input, where each element is:
#'        1. Multiplied by its original sign
#'        2. Normalized by dividing by the sum of absolute differences from either:
#'           - The minimum value (if max.norm = FALSE)
#'           - The maximum value (if max.norm = TRUE)
#' @examples
#' absnorm(c(-2, 1, 3))
#' absnorm(c(-2, 1, 3), max.norm = TRUE)
#' @export
absnorm <- function(vec, max.norm = FALSE) {
  sgn <- sign(vec)
  vec <- abs(vec)
  if (max.norm) {
    mvec <- max(vec)
  } else {
    mvec <- min(vec)
  }
  vec <- abs(vec - mvec)
  sgn * (vec / sum(vec))
}
