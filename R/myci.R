#' Confidence Interval Function
#'
#' @param x A numeric vector representing the sample data.
#' @param alpha The significance level for the confidence interval (default is 0.05 for a 95% CI).
#' @return A named vector with the lower and upper bounds of the confidence interval.
#' @export
myci <- function(x, alpha = 0.05) {
  n <- length(x)
  mean_x <- mean(x)
  stderr <- sd(x) / sqrt(n)
  t_crit <- qt(1 - alpha / 2, df = n - 1)
  error_margin <- t_crit * stderr
  c(lower = mean_x - error_margin, upper = mean_x + error_margin)
}
