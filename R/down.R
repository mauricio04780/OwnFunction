#' @title Probability Density Function of the OwnFunction Distribution
#'
#' @description
#' Evaluates the probability density function (PDF) of the OwnFunction
#' continuous distribution at given values of \code{x}.
#'
#' @param x A numeric value or vector where the density function is evaluated.
#'
#' @return A numeric vector with the corresponding density values.
#'
#' @details
#' The OwnFunction distribution is defined on the interval (-1, 2) with
#' probability density function:
#'
#' \deqn{
#' f(x) =
#' \begin{cases}
#' \frac{x^2}{3}, & -1 < x < 2 \\
#' 0, & \text{otherwise}
#' \end{cases}
#' }
#'
#' This function is vectorized and returns 0 for values outside the support.
#'
#' @examples
#' down(1)
#' down(c(-0.5, 0.5, 1.5))
#'
#' @export
down <- function(x){
  ifelse(x > -1 & x < 2, x^2/3, 0)
}
