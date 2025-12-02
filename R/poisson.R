
#' Poisson Distribution P(λ)
#' @family discrete-distributions
#' @description discrete, cumul, and random for a Poisson distribution with rate parameter λ > 0.
#'
#' @details The Poisson distribution models the number of events occurring in a fixed time interval
#' given an average rate λ. Its support is the set \code{\{0, 1, 2, ...\}}.
#'
#' @param k Integer value(s) at which to evaluate the discrete.
#' @param q Numeric thresholds at which to evaluate the cumul.
#' @param lambda Positive numeric rate parameter (λ > 0).
#' @param n Non-negative integer sample size for random.
#' @return For `poisson_discrete()` and `poisson_cumul()`: numeric vector of probabilities.
#' For `poisson_random()`: integer vector of simulated values.
#'
#' @examples
#' poisson_discrete(0:5, 3)
#' poisson_cumul(0:5, 3)
#' poisson_random(10, 3)
#'
#' @importFrom stats dpois
#' @export
poisson_discrete <- function(k, lambda) {
if (!is.numeric(lambda) || any(lambda <= 0)) stop("'lambda' must be positive.")
  
k <- as.numeric(k)
if (any(k < 0 | k %% 1 != 0)) stop("'k' must be non-negative integers.")

dpois(k, lambda)

}


#' @importFrom stats ppois 
#' @rdname poisson_discrete
#' @export
poisson_cumul <- function(q, lambda) {
  if (!is.numeric(lambda) || any(lambda <= 0)) stop("'lambda' must be positive.")
  
  q <- as.numeric(q)
  ppois(q, lambda)
}


#' @importFrom stats rpois 
#' @rdname poisson_discrete
#' @export
poisson_random <- function(n, lambda) {
  if (!is.numeric(lambda) || any(lambda <= 0)) stop("'lambda' must be positive.")
  if (!is.numeric(n) || length(n) != 1 || n < 0 || n %% 1 != 0) stop("'n' must be a non-negative integer.")
  
  rpois(n, lambda)
}