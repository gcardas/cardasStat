#' Confidence_score binomial
#' @description compute the confidence scores.
#' @param n number of sample.
#' @param p probability of success
#' @param z value of alpha
#' @examples
#' confidence_binomial(500,0.42,0.0025)
#' 
#' @importFrom stats qnorm
#' @export
confidence_binomial <- function(n, p, z){
  nA <- n
  pA <- p
  
  e <- qnorm(1-z) * sqrt(pA * (1-pA) / nA)
  c(pA - e, pA + e)
}

#' Confidence_score Z
#' @param z value of alpha
#' @return A `score`.
#' @importFrom stats qnorm
#' @examples
#' confidence_score_z(0.0025)
#' @export
confidence_score_z <-function(z){
  score <- qnorm(1-z)
  cat("T score:", score, "\n")
  return(score)
}

#' Confidence_score T
#' @param z value of alpha
#' @param n number of sample.
#' @return A `score`.
#' @importFrom stats qt
#' @examples
#' confidence_score_z(0.0025)
#' @export
confidence_score_t <-function(z,n){
  
  score <- qt(1-z,n)
  cat("T score:", score, "\n")
  return(score)
}