#' @title Random pseudo-numbers generator from the OwnFunction distribution
#'
#' @description
#' Generates pseudo-random numbers from a continuous distribution using the inverse transform method.
#'
#' @param n Integer. Length of the pseudo-random vector to be generated
#'
#' @details
#' This function uses the inverse of the cumulative distribution function (CDF):
#' \deqn{F^{-1}(u) = (9u - 1)^{1/3}}
#' where \eqn{u \sim U(0,1)}.
#'
#' To avoid numerical problems with negative values, the cube root is computed as:
#' \deqn{\text{sign}(x)\,|x|^{1/3}}
#'
#' @return
#' A numeric vector of length \code{n} with random values following the defined distribution.
#'
#' @examples
#' # Generate 1000 random values
#' rown(1000)
#'
#' @export
rown <- function(n){
  u <- runif(n)
  sign(9*u - 1) * abs(9*u - 1)^(1/3)
}
