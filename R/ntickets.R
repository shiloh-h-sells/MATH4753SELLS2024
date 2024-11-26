#' Calculate the Number of Tickets to Sell
#'
#' This function calculates the number of tickets to sell for a flight given the number of seats, the probability of a "show", and the probability that the flight will be overbooked.
#'
#' @param N Integer. Number of seats in the flight.
#' @param gamma Numeric. Probability that the flight will be overbooked.
#' @param p Numeric. Probability of a "show".
#' @return A named list containing:
#' \describe{
#'   \item{nd}{Number of tickets calculated using the discrete distribution.}
#'   \item{nc}{Number of tickets calculated using the normal approximation.}
#'   \item{N}{Number of seats in the flight.}
#'   \item{p}{Probability of a "show".}
#'   \item{gamma}{Probability that the flight will be overbooked.}
#' }
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
#' @export
ntickets <- function(N, gamma, p) {
  # Discrete case: find nd by iterating
  candidate_n <- N
  while (TRUE) {
    cumulative_prob <- sum(stats::dbinom(0:N, candidate_n, p))
    if (1 - cumulative_prob <= gamma) {
      nd <- candidate_n
      break
    } else {
      candidate_n <- candidate_n + 1
    }
  }

  # Continuous case: find nc using normal approximation
  expected_mean <- N * p
  expected_sd <- sqrt(N * p * (1 - p))
  nc <- stats::qnorm(1 - gamma, mean = expected_mean, sd = expected_sd)

  # Plotting and objective calculation for discrete case
  possible_ns <- seq(N, N + 20)
  objective_values <- sapply(possible_ns, function(n) {
    cumulative_prob <- sum(stats::dbinom(0:N, floor(n), p))
    1 - cumulative_prob
  })
  optimal_n_discrete <- possible_ns[which.min(abs(objective_values - gamma))]

  plot(possible_ns, objective_values, type = "b",
       main = sprintf("Objective vs n for optimal ticket sales\n(%d) gamma=%.2f N=%d Discrete", optimal_n_discrete, gamma, N),
       ylab = "Objective (1 - P(X <= N))", xlab = "n")
  graphics::abline(h = gamma, col = "red", lwd = 2)
  graphics::abline(v = optimal_n_discrete, col = "red", lwd = 2)

  # Plotting and objective calculation for continuous case
  objective_values_continuous <- sapply(possible_ns, function(n) {
    mean_val <- n * p
    stddev_val <- sqrt(n * p * (1 - p))
    prob_val <- stats::pnorm(N, mean = mean_val, sd = stddev_val)
    1 - prob_val
  })
  optimal_n_continuous <- possible_ns[which.min(abs(objective_values_continuous - gamma))]

  plot(possible_ns, objective_values_continuous, type = "l",
       main = sprintf("Objective vs n for optimal ticket sales\n(%d) gamma=%.2f N=%d Continuous", optimal_n_continuous, gamma, N),
       ylab = "Objective", xlab = "n")
  graphics::abline(h = gamma, col = "blue", lwd = 2)
  graphics::abline(v = optimal_n_continuous, col = "blue", lwd = 2)

  # Return the results
  list(nd = optimal_n_discrete, nc = optimal_n_continuous, N = N, gamma = gamma, p = p)
}

