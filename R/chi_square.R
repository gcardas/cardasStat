#' Chi-square dist greater
#' @description compute the probability of sample variance is greater than threshold.
#' @param n number of sample.
#' @param sd standard deviation of samples.
#' @param threshold number of units.
#' @return A `probability` that sample variance is greater than threshold
#' @examples
#' chi_square_greater(25,1200^2,2500)
#' 
#' @importFrom stats pchisq
#' @export
chi_square_greater <- function(n, sd, threshold){
  
  # 1. Calculate Degrees of Freedom
  df <- n - 1
  
  # 2. Calculate the critical Chi-Square value
  chi_sq_crit <- (df * threshold) / sd
  
  # 3. Calculate the probability: P(Chi-Square > chi_sq_crit)
  # Using pchisq with lower.tail = FALSE gives the upper-tail probability
  probability <- pchisq(chi_sq_crit, df = df, lower.tail = FALSE)
  
  cat("Degrees of Freedom (df):", df, "\n")
  cat("Critical Chi-Square Value:", chi_sq_crit, "\n")
  cat("Required Probability sample variance is greater than 2500:", probability, "\n")
  
  return(probability)
}

#' Chi-square dist lower
#' @description compute the probability of sample variance is lower than threshold.
#' @importFrom stats pchisq
#' @param n number of sample.
#' @param sd standard deviation of samples.
#' @param threshold number of units.
#' @export
chi_square_lower <- function(n,sd, threshold){
  
  # 1. Calculate Degrees of Freedom
  df <- n - 1
  
  # 2. Calculate the critical Chi-Square value
  chi_sq_crit <- (df * threshold) / sd
  
  # 3. Calculate the probability: P(Chi-Square > chi_sq_crit)
  # Using pchisq with lower.tail = TRUE gives the down-tail probability
  probability <- pchisq(chi_sq_crit, df = df, lower.tail = TRUE)
  
  cat("Degrees of Freedom (df):", df, "\n")
  cat("Critical Chi-Square Value:", chi_sq_crit, "\n")
  cat("Required Probability P(s^2 > 2500):", probability, "\n")
  
  return(probability)
}