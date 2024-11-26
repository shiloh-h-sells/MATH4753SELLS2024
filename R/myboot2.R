myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, ...) {
  # Sample size
  n <- length(x)

  # Resample with replacement
  y <- sample(x, n * iter, replace = TRUE)

  # Create a matrix of resampled values
  rs.mat <- matrix(y, nr = n, nc = iter, byrow = TRUE)

  # Apply the function to each resample
  xstat <- apply(rs.mat, 2, fun)

  # Calculate the confidence interval
  ci <- quantile(xstat, c(alpha / 2, 1 - alpha / 2))

  # Plot the histogram
  para <- hist(xstat, freq = FALSE, las = 1, main = "Histogram of Bootstrap Sample Statistics", ...)

  # Point estimate
  mat <- matrix(x, nr = length(x), nc = 1, byrow = TRUE)
  pte <- apply(mat, 2, fun)

  # Add vertical line and segments to the plot
  abline(v = pte, lwd = 3, col = "Black")
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = 3)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = 3)
  text(pte, max(para$density) / 2, round(pte, 2), cex = 3)

  # Return the results as a list
  return(list(ci = ci, fun = fun, x = x, xstat = xstat))
}

