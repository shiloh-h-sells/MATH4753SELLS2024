#' Bootstrapped P-Value for Single Sample T-Test
#'
#' Performs a bootstrapped hypothesis test for a single sample t-test, providing the p-value, confidence interval, and test statistic.
#'
#' @param x A numeric vector representing the sample data.
#' @param mu0 The null hypothesis mean value (default is 0).
#' @param conf.level Confidence level for the interval (default is 0.95).
#' @param iter Number of bootstrap iterations (default is 3000).
#' @param test The type of test ("two", "upper", or "lower").
#' @return A list containing the p-value, test statistic, confidence interval, and additional details about the test.
#' @examples
#' set.seed(55)
#' x1 <- rnorm(30, mean = 25, sd = 5)
#' bootpval(x = x1, mu0 = 23, test = "two")
#' @export
bootpval <- function(x, mu0 = 0, conf.level = 0.95, iter = 3000, test = "two") {
  n <- length(x)
  y <- x - mean(x) + mu0  # Center the data at the null hypothesis mean

  rs.mat <- replicate(iter, sample(y, n, replace = TRUE))  # Bootstrap resampling

  tstat <- function(z) sqrt(n) * (mean(z) - mu0) / sd(z)  # Calculate t-statistic
  tcalc <- tstat(x)  # T-statistic for the original sample

  ytstat <- apply(rs.mat, 2, tstat)  # T-statistic for bootstrapped samples

  alpha <- 1 - conf.level
  ci <- quantile(apply(rs.mat, 2, mean), c(alpha / 2, 1 - alpha / 2))  # Confidence interval

  # Calculate p-value based on test type
  pvalue <- switch(test,
                   "two" = mean(abs(ytstat) >= abs(tcalc)),
                   "upper" = mean(ytstat >= tcalc),
                   "lower" = mean(ytstat <= tcalc),
                   stop("Invalid test type"))

  # Plot histogram of bootstrapped t-statistics
  h <- hist(ytstat, plot = FALSE)
  mid <- h$mids
  if (test == "two") {
    ncoll <- length(mid[mid <= -abs(tcalc)])
    ncolr <- length(mid[mid >= abs(tcalc)])
    col <- c(rep("Green", ncoll), rep("Gray", length(mid) - ncoll - ncolr), rep("Green", ncolr))
  } else if (test == "upper") {
    ncolr <- length(mid[mid >= abs(tcalc)])
    col <- c(rep("Gray", length(mid) - ncolr), rep("Green", ncolr))
  } else if (test == "lower") {
    ncoll <- length(mid[mid <= -abs(tcalc)])
    col <- c(rep("Green", ncoll), rep("Gray", length(mid) - ncoll))
  }
  hist(ytstat, col = col, freq = FALSE, las = 1, main = "", xlab = expression(T[stat]))
  abline(v = c(ci[1], ci[2]), col = "red", lwd = 2)  # Add confidence interval lines

  title(substitute(paste("P-value = ", round(pvalue, 4))))

  return(list(pvalue = pvalue, tcalc = tcalc, n = n, x = x, test = test, ci = ci))
}
