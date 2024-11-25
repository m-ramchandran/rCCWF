#' Normalize a vector
#'
#' @param x Numeric vector to normalize
#' @return Normalized vector with Euclidean (L2) norm
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
#' @return Normalized vector where each element is multiplied by its original sign
#'         and divided by the sum of absolute differences from min/max
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
