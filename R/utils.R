#' Normalize a vector
#'
#' @param x Numeric vector to normalize
#' @return Normalized vector
#' @export
norm_vec <- function(x) {
  sqrt(sum(x^2))
}

#' Absolute normalization of a vector
#'
#' @param vec Vector to normalize
#' @param max.norm Logical, whether to use maximum normalization
#' @return Normalized vector
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
