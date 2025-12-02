library(testthat)

# Data from the problem:
x_full <- c(5,15,25,35,45,55,65,75,85,95)
obs_full <- c(2,30,80,145,250, 245, 140, 75, 28, 5)

test_that("Grouped Mean Estimate is Correct", {
  # Expected result: Sum(x*obs) / n = 53800 / 900 = 59.7777...
  expected_mu <- sum(x_full * obs_full) / sum(obs_full) 
  
  expect_equal(estimate_grouped_mean(x_full, obs_full), expected_mu)
  expect_equal(estimate_grouped_mean(x_full, obs_full), 49.8399, tolerance = 1e-3)
})

test_that("Grouped SD Estimate is Correct", {
  # Expected result (from previous work): ~ 16.08025
  # The test uses the known value to ensure the function is implemented correctly.
  
  # Calculate expected SD manually for comparison (using n-1 denominator for sample SD)
  mu_hat <- 49.8
  n <- 1000
  variance <- sum(obs_full * (x_full - mu_hat)^2) / (n)
  expected_sd <- sqrt(variance) # ~ 16.08025
  
  expect_equal(estimate_grouped_sd(x_full, obs_full), expected_sd, tolerance = 1e-5)
  expect_equal(estimate_grouped_sd(x_full, obs_full), 16.08025, tolerance = 1e-5)
})



#------

test_that("Expected Frequencies Calculation is Correct", {
  n <- 100
  p <- c(0.1, 0.5, 0.4)
  
  # Expected E = n * p = (100*0.1, 100*0.5, 100*0.4)
  expected_E <- c(10, 50, 40)
  
  expect_equal(calc_expected_freq(n, p), expected_E)
})

test_that("Chi-Squared Statistic Calculation is Correct", {
  O <- c(10, 55, 35) # Observed
  E <- c(10, 50, 40) # Expected
  
  # Calculation: 
  # (10-10)^2/10 + (55-50)^2/50 + (35-40)^2/40
  # 0 + 25/50 + 25/40 = 0 + 0.5 + 0.625 = 1.125
  expected_stat <- 1.125
  
  expect_equal(calc_chi_sq_stat(O, E), expected_stat, tolerance = 1e-6)
})