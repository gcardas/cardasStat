#' T student distribution
#' @description compute the probability of sample not being more deviated than difference.
#' @param n number of sample.
#' @param sd standard deviation of samples.
#' @param difference number of units.
#' @return A `probability` that the sample mean doesn't differ more than difference unit.
#' @examples
#' t_student(16,10.85,8)
#' 
#' @importFrom stats pt
#' @export
t_student <- function(n,sd,difference){
  df <- n - 1
  
  SE <- sd / sqrt(n)
  
  t_stat <- difference / SE
  
  probability <- pt(t_stat, df = df) - pt(-t_stat, df = df)
  
  cat("Standard Error (SE):", SE, "\n")
  cat("T-statistic:", t_stat, "\n")
  cat("Required Probability:", probability, "\n")
  
  return(probability)
}



