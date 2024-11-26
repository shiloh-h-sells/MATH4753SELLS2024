#' Central Limit Theorem Simulation for Uniform Distribution
#'
#' This function simulates the Central Limit Theorem (CLT) by sampling from a
#' uniform distribution and calculating the sum of the samples. It then creates
#' a histogram of the sums.
#'
#' @param n Sample size for each iteration.
#' @param iter Number of iterations.
#' @return A histogram of the sums of samples from the uniform distribution.
#' @export
myclt <- function(n, iter) {
  y <- runif(n * iter, 0, 5) # Sample from a uniform distribution
  data <- matrix(y, nr = n, nc = iter, byrow = TRUE) # Create matrix of samples
  sm <- apply(data, 2, sum) # Sum the values in each column
  hist(sm) # Plot histogram of sums
  return(sm)
}

