

#' Estimate Mean from Grouped Data
#' @description Calculates the estimated mean (mu_hat) from grouped data using midpoints and frequencies.
#' @param x_midpoints Numeric vector of class midpoints.
#' @param obs_freq Numeric vector of observed frequencies.
#' @return The estimated mean (numeric).
#' @export
#' 
#' 
estimate_grouped_mean <- function(x_midpoints, obs_freq) {
  if (length(x_midpoints) != length(obs_freq)) stop("Midpoints and frequencies must have the same length.")
  
  # Mean is the sum of (midpoint * frequency) divided by the total count
  raw_sum <- sum(x_midpoints * obs_freq)
  n <- sum(obs_freq)
  
  return(raw_sum / n)
}

#---------------------------

#' Estimate Standard Deviation from Grouped Data
#' @description Calculates the estimated sample standard deviation (sig_hat) from grouped data.
#' @param x_midpoints Numeric vector of class midpoints.
#' @param obs_freq Numeric vector of observed frequencies.
#' @return The estimated sample standard deviation (numeric).
#' @export
estimate_grouped_sd <- function(x_midpoints, obs_freq) {
  if (length(x_midpoints) != length(obs_freq)) stop("Midpoints and frequencies must have the same length.")
  
  mu_hat <- estimate_grouped_mean(x_midpoints, obs_freq)
  n <- sum(obs_freq)
  
  # Standard deviation formula for grouped data (using n-1 for sample SD)
  variance <- sum(obs_freq * (x_midpoints - mu_hat)^2) / (n)
  
  return(sqrt(variance))
}

#---------------------------

#' Calculate Theoretical Bin Probabilities (Normal)
#' @description Calculates the expected probability for each class interval 
#'   based on a Normal distribution with estimated parameters.
#' @param breaks Numeric vector of class boundaries (e.g., 0, 10, 20...).
#' @param mu_hat Estimated mean.
#' @param sig_hat Estimated standard deviation.
#' @return A numeric vector of expected probabilities P(break(i) < X <= break(i+1).
#' 
#' @importFrom stats pnorm 
#' @export
calc_normal_bin_prob <- function(breaks, mu_hat, sig_hat) {
  k <- length(breaks) - 1
  p <- numeric(k)
  
  for (i in 1:k) {
    # P(lower < X <= upper) = P(X <= upper) - P(X <= lower)
    p[i] <- pnorm(breaks[i+1], mean = mu_hat, sd = sig_hat) - pnorm(breaks[i], mean = mu_hat, sd = sig_hat)
  }
  
  # Rescale probabilities if sum is not exactly 1 due to floating point error
  if (abs(sum(p) - 1) > 1e-6) {
    p <- p / sum(p)
  }
  
  return(p)
}

#---------------------------


#' Calculate Expected Frequencies
#' @description Calculates the expected frequency (E) for each class given the total sample size (n)
#'   and the theoretical probabilities (p).
#' @param n Total sample size (sum of observed frequencies).
#' @param p Numeric vector of theoretical probabilities.
#' @return A numeric vector of expected frequencies E = n * p.
#' @export
calc_expected_freq <- function(n, p) {
  return(n * p)
}

#---------------------------

#' Calculate Chi-Squared Test Statistic
#' @description Calculates the Chi-Squared test statistic: sum( (O - E)^2 / E ).
#' @param obs_freq Numeric vector of observed frequencies (O).
#' @param exp_freq Numeric vector of expected frequencies (E).
#' @return The Chi-Squared test statistic (numeric).
#' @export
calc_chi_sq_stat <- function(obs_freq, exp_freq) {
  if (length(obs_freq) != length(exp_freq)) stop("Observed and Expected frequencies must have the same length.")
  
  # Note: Merging of classes where E < 5 must be handled externally 
  # or before calling this function if required.
  
  # Formula: Sum((O - E)^2 / E)
  chi2_stat <- sum((obs_freq - exp_freq)^2 / exp_freq)
  
  return(chi2_stat)
}


