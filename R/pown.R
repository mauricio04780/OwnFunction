#' @title Cumulative Distribution Function of the OwnFunction Distribution
#'
#' @description
#' Evaluates the cumulative distribution function (CDF) of the OwnFunction
#' continuous distribution at given values of \code{x}.
#'
#' @param x A numeric value or vector where the CDF is evaluated.
#'
#' @return A numeric vector with the corresponding cumulative probabilities.
#'
#' @details
#' The OwnFunction distribution is defined on the interval (-1, 2). Its
#' cumulative distribution function is:
#'
#' \deqn{
#' F(x) =
#' \begin{cases}
#' 0, & x \leq -1 \\
#' \frac{x^3 + 1}{9}, & -1 < x < 2 \\
#' 1, & x \geq 2
#' \end{cases}
#' }
#'
#' This function is vectorized and returns values in the interval [0, 1].
#'
#' @examples
#' pown(0)
#' pown(c(-1, 0, 1, 2))
#'
#' @export
pown <- function(x){
  ifelse(x <= -1, 0,
         ifelse(x < 2, (x^3 + 1)/9, 1))
}
