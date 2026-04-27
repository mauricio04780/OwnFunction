#' Function associated with the OwnFunction distribution
#'
#' @param x A numeric value or vector where the function is evaluated
#'
#' @return Returns the value of the function at x
#'
#' @details
#' This function evaluates a piecewise-defined function on the interval (-1, 2).
#' It is given by:
#' \deqn{
#' f(x) = \begin{cases}
#' \frac{x^2}{3} & -1 < x < 2 \\
#' 0 & \text{otherwise}
#' \end{cases}
#' }
#'
#' @examples
#' down(1)
#' down(c(-0.5, 0.5, 1.5))
#' @export
down <- function(x){
  ifelse(x > -1 & x < 2, x^2/3, 0)
}
