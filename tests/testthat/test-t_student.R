# Define common parameters for testing
tolerance <- 1e-6 # Standard tolerance for floating-point comparisons

# --- Test 1: Validate Example Case ---
test_that("t_student returns the correct probability for the documented example (n=16, sd=10.85, diff=8)", {
  n <- 16
  sd <- 10.85
  difference <- 8
  
  # Manual calculation of expected value:
  df <- n - 1
  SE <- sd / sqrt(n)
  t_stat <- difference / SE
  
  # Calculate expected probability using the native R function pt()
  expected_probability <- pt(t_stat, df = df) - pt(-t_stat, df = df)
  
  # Check if the custom function output matches the native function output
  expect_equal(t_student(n, sd, difference), expected_probability, tolerance = tolerance)
})


# --- Test 2: Validate a Known CI Case (e.g., 90% CI for a specific t-statistic) ---
test_that("t_student correctly calculates probability symmetric around 0 (e.g., 90% CI)", {
  # Define parameters for a simple case
  n_test <- 7 # df = 6
  sd_test <- 1
  
  # Find the t-statistic that corresponds to a known probability (e.g., 90% probability)
  # qnorm(0.95) for the t-distribution gives the 90% critical value (t_0.05, df=6)
  df_test <- n_test - 1
  t_critical <- qt(0.95, df = df_test) # t_0.05,6 is approx 1.943
  
  # The difference value needed to generate this t_critical:
  # difference = t_critical * SE = t_critical * (1 / sqrt(7))
  SE_test <- sd_test / sqrt(n_test)
  difference_test <- t_critical * SE_test
  
  # The expected probability is exactly 0.90 (90%)
  expected_probability_90 <- 0.90
  
  # Check if the custom function output matches the expected value
  expect_equal(t_student(n_test, sd_test, difference_test), expected_probability_90, tolerance = tolerance)
})


# --- Test 3: Validate Behavior for Edge Case (difference = 0) ---
test_that("t_student returns probability 0 when difference is 0", {
  n <- 5
  sd <- 1
  difference <- 0
  
  # If difference=0, t_stat=0, P(-0 < T < 0) = 0
  expect_equal(t_student(n, sd, difference), 0, tolerance = tolerance)
})