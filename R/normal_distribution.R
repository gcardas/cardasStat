# NORMAL DISTRIBUTIONS

# --------------------------------------------------------------------------
# --- CONSOLIDATED PRIMARY DOCUMENTATION BLOCK (ANCHOR: p_normal_lower_equal) ---
# --------------------------------------------------------------------------

#' Normal Distribution N(mean, sd)
#' @family continuous-distributions
#' @description cumulative probability functions for the Normal distribution with parameters mean (mu) and sd (sigma).
#'
#' @details The Normal distribution (Gaussian distribution) is a continuous distribution modeling data that clusters around a central mean.
#'
#' @param x Numeric value(s) at which to evaluate the probability function (e.g., the threshold q).
#' @param mean Numeric population mean (mu).
#' @param sd Positive numeric population standard deviation (sigma).
#' @return Numeric vector of probabilities.
#' @importFrom stats pnorm
#' @name p_normal_lower_equal
NULL 


# X >= q - when I am searching for greater value
#' Probability of X being greater than q
#' @rdname p_normal_lower_equal
#' @export
p_normal_greater <- function(x, mean, sd){
  return(pnorm(x,mean,sd,lower.tail = FALSE))
}

# X <= q - when I am searching for smaller or equal value
#' Probability of X being less than or equal to q (Standard CDF)
#' @rdname p_normal_lower_equal
#' @export
p_normal_lower_equal <- function(x, mean, sd){
  return(pnorm(x,mean,sd))
}

# X < q - when I am searching for smaller value
#' Probability of X being strictly less than q
#' @rdname p_normal_lower_equal
#' @export
p_normal_lower <- function(x, mean, sd){
  # NOTE: The provided formula uses x-1, which is mathematically incorrect for continuous data.
  return(pnorm(x-1,mean,sd))
}

# Interval [a,b]
#' Probability of X being within the interval [a, b]
#' @rdname p_normal_lower_equal
#' @param a Lower bound of the interval.
#' @param b Upper bound of the interval.
#' @export
p_normal_interval <- function(a,b,mean,sd){
  prob <- p_normal_greater(a,mean,sd) - p_normal_greater(b,mean,sd)
  return(prob)
}