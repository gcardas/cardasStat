# BINOMIAL DISTRIBUTION
#'
#' function for combinatory numbers
#' @param n total number of items
#' @param x number of desired items
#' @return The calculated combinatory number (n choose x)
#' @export
combinatory_number <- function(n,x){
  result <- choose(n,x)
  cat("Combinatory number result: ", result, "\n")
  return( result ) 
}


#' Binomial Distribution B(n, p)
#' @family discrete-distributions
#' @description discrete, cumul, for a Binomial distribution with parameters n (size) and p (probability).
#'
#' @details The Binomial distribution models the number of successes (x) in a fixed number of independent trials (n),
#' given the probability of success (p) on each trial. Its support is the set \code{\{0, 1, ..., n\}}.
#' 
#' @param n Non-negative integer size parameter (number of trials).
#' @param p Numeric probability of success (0 < p < 1).
#' @param x Integer value(s) at which to evaluate the discrete and cumulative functions.
#' @return For `binomial_discrete()`, `binomial_cumulative_less()`, `binomial_cumulative_equal()`, and `binomial_cumulative_greater_equal()`: numeric vector of probabilities.
#' For `binomial_moments()`: a list containing mean and variance.
#'
#' binomial distribution for discrete random number
#' @rdname binomial_discrete
#' @importFrom stats dbinom
#' @export
binomial_discrete <- function(n,p,x){
  result <- dbinom(x, size = n, prob = p)
  cat("Binomial discrete for ", x,":", result, "\n")
  return(result) 
}


#' binomial distribution for cumulative random number excluding the threshold
#' @rdname binomial_discrete
#' @importFrom stats pbinom
#' @export
binomial_cumulative_less <- function(n,p,x){
  # P(X < x) = P(X <= x - 1)
  result <- pbinom(x-1,n,p)
  cat("Binomial cumulative that is less than", x,":", result, "\n")
  return(result)
}


#' binomial distribution for cumulative random number including the threshold
#' @rdname binomial_discrete
#' @importFrom stats pbinom
#' @export
binomial_cumulative_equal <- function(n,p,x){
  # P(X <= x)
  result <- pbinom(x,n,p)
  cat("Binomial cumulative that is less than or equal", x,":", result, "\n")
  return ( result )
}


#' binomial distribution for cumulative random number including the threshold (Greater or Equal)
#' @rdname binomial_discrete
#' @importFrom stats pbinom
#' @export
binomial_cumulative_greater_equal <- function(n,p,x){
  # P(X >= x) = 1 - P(X <= x - 1)
  result <- 1 - pbinom(x-1, n, p)
  cat("Binomial cumulative that is greater than or equal", x,":", result, "\n")
  return(result)
}


#' binomial distribution for cumulative random number excluding the threshold (Greater Than)
#' @rdname binomial_discrete
#' @importFrom stats pbinom
#' @export
binomial_cumulative_greater <- function(n,p,x){
  # P(X > x) = 1 - P(X <= x)
  result <- 1 - pbinom(x, n, p)
  cat("Binomial cumulative that is greater than", x,":", result, "\n")
  return(result)
}


#' binomial distribution moments
#' @rdname binomial_discrete
#' @export
binomial_moments <- function(n,p){
  mean <- n * p
  variance <- n * p * (1 - p)
  cat("Mean: ", mean,"\nVariance: ", variance, "\n")
  
  return(list(mean = mean, variance = variance))
}