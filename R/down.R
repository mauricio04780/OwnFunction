#' Probability density function of the OwnFunction distribution
#'
#' @param x A numeric value or vector where the PDF is evaluated
#'
#' @return Returns the value of the probability density function at x
#'
#' @details
#' This function computes the probability density function (PDF) associated
#' with the custom continuous distribution defined in the package. The function is:
#' \deqn{
#' f(x) = \begin{cases}
#' \frac{x^2}{3} & -1 < x < 2 \\
#' 0 & \text{otherwise}
#' \end{cases}
#' }
#'
#' @examples
#' down(0)
#' down(c(-2, -0.5, 0, 0.5, 1.5, 2.5))
#' @export
down <- function(x){
  ifelse(x > -1 & x < 2, x^2/3, 0)
}
