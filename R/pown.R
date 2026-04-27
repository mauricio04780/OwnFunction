#' Cumulative distribution function of the OwnFunction distribution
#'
#' @param x A numeric value or vector where the CDF is evaluated
#'
#' @return Returns the value of the cumulative distribution function at x
#'
#' @details
#' This function computes the cumulative distribution function (CDF) associated
#' with the custom continuous distribution defined in the package. The function is:
#' \deqn{
#' F(x) = \begin{cases}
#' 0 & x \leq -1 \\
#' \frac{x^3 + 1}{9} & -1 < x < 2 \\
#' 1 & x \geq 2
#' \end{cases}
#' }
#'
#' @examples
#' pmine(0)
#' pmine(c(-2, -1, 0, 1, 2, 3))
#'
#' @export
pmine <- function(x){
  ifelse(x <= -1, 0,
         ifelse(x < 2, (x^3 + 1)/9, 1))
}
