#' Maximum Likelihood Estimation Function
#'
#' This function computes the maximum likelihood estimate for a given likelihood function.
#'
#' @param lfun Likelihood function to be evaluated
#' @param x Vector of data points
#' @param param Vector of parameter values
#' @param ... Additional arguments for plot customization
#' @return List containing the index, parameter value, log-likelihood value, and slope values at max
#' @export
mymaxlik <- function(lfun, x, param, ...) {
  np <- length(param)
  z <- outer(x, param, lfun)
  y <- apply(z, 2, sum)
  plot(param, y, col = "blue", type = "l", lwd = 2, ...)
  i <- max(which(y == max(y)))
  abline(v = param[i], lwd = 2, col = "red")
  points(param[i], y[i], pch = 19, cex = 1.5, col = "black")
  axis(3, param[i], round(param[i], 2))
  ifelse(i - 3 >= 1 & i + 2 <= np, slope <- (y[(i - 2):(i + 2)] - y[(i - 3):(i + 1)]) / (param[(i - 2):(i + 2)] - param[(i - 3):(i + 1)]), slope <- "NA")
  return(list(i = i, parami = param[i], yi = y[i], slope = slope))
}
