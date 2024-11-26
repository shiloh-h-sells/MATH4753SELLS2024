#' Plot Normal Distribution Curve, Shade Area, and Display Probability
#'
#' This function plots the normal distribution curve for given mean and standard deviation,
#' shades the area under the curve from the left up to a specified value, and calculates the
#' cumulative probability up to that value.
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @param a The value up to which the area under the curve is shaded and the probability is calculated.
#' @return A list containing the mean (`mu`), standard deviation (`sigma`), and the cumulative probability (`prob`).
#' @export
#' @examples
#' result <- myncurve(mu = 10, sigma = 5, a = 6)
#' print(result)
myncurve = function(mu, sigma, a) {
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma))
  x <- seq(mu - 3 * sigma, a, length = 1000)
  y <- dnorm(x, mean = mu, sd = sigma)
  polygon(c(mu - 3 * sigma, x, a), c(0, y, 0), col = "Red")
  prob <- pnorm(a, mean = mu, sd = sigma)
  text(x = a, y = max(y) * 0.5, labels = paste("Area =", round(prob, 4)), pos = 4, col = "blue")
  list(mu = mu, sigma = sigma, prob = prob)
}


