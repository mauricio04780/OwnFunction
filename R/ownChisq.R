#' @title Chi-square Goodness-of-Fit Test for a sample of the OwnFunction Distribution
#'
#' @description
#' Performs a chi-square goodness-of-fit test comparing observed frequencies
#' from a sample with expected probabilities derived from the distribution:
#'
#' \deqn{
#' f(x) =
#' \begin{cases}
#' \frac{x^2}{3}, & -1 < x < 2 \\
#' 0, & \text{otherwise}
#' \end{cases}
#' }
#'
#' The interval [-1, 2] is divided into bins of width 0.25.
#'
#' @param sample Numeric vector of observations.
#'
#' @return A named numeric vector with:
#' \itemize{
#'   \item statistic : Chi-square statistic
#'   \item df        : Degrees of freedom
#'   \item p.val     : p-value
#' }
#'
#' @details
#' This function contrast the hypotheses \eqn{H_0: \pi = \hat{\pi}} Vs. \eqn{H_1: \pi \neq \hat{\pi}}.
#' Expected probabilities are computed as differences of the CDF over each interval.
#' Observed frequencies in the sample are obtained using the \code{fdth} package.
#'
#' @examples
#' sample <- rown(100)
#' ownChisq(sample)
#'
#' @importFrom fdth fdt
#'
#' @export
ownChisq <- function(sample) {
  x <- seq(-1, 2, by = 0.25)
  fx <- diff(pown(x))

  t <- fdt(sample, start = -1, end = 2, h = 0.25)
  fa <- t$table$f

  res <- chisq.test(x = fa, p = fx)

  c("statistic" = as.numeric(res$statistic),
    "df"        = as.numeric(res$parameter),
    "p.val"     = res$p.value)
}
