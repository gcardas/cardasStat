test_that("combinatory_number works correctly", {
  # Test C(5, 2) which equals 10
  expect_equal(combinatory_number(5, 2), 10)
  
  # Test C(7, 0) which equals 1
  expect_equal(combinatory_number(7, 0), 1)
  
  # Test C(4, 4) which equals 1
  expect_equal(combinatory_number(4, 4), 1)
})


test_that("binomial_discrete returns correct PMF value", {
  n <- 3
  p <- 0.5
  x <- 1
  
  # Expected result: 0.375
  expected_p <- 0.375 
  
  # Use tolerance for floating-point comparisons
  expect_equal(binomial_discrete(n, p, x), expected_p, tolerance = 1e-6)
  
  # Test P(X=0)
  expect_equal(binomial_discrete(3, 0.5, 0), 0.125, tolerance = 1e-6)
})


test_that("binomial_discrete returns correct PMF value", {
  n <- 3
  p <- 0.5
  x <- 1
  
  # Expected result: 0.375
  expected_p <- 0.375 
  
  # Use tolerance for floating-point comparisons
  expect_equal(binomial_discrete(n, p, x), expected_p, tolerance = 1e-6)
  
  # Test P(X=0)
  expect_equal(binomial_discrete(3, 0.5, 0), 0.125, tolerance = 1e-6)
})


test_that("binomial_cumulative_equal returns correct CDF value (P(X <= x))", {
  n <- 2
  p <- 0.5
  x <- 1
  
  # Expected result: 0.75
  expected_cumulative_p <- 0.75
  
  expect_equal(binomial_cumulative_equal(n, p, x), expected_cumulative_p, tolerance = 1e-6)
})

test_that("binomial_cumulative_less returns correct value (P(X < x))", {
  n <- 2
  p <- 0.5
  x <- 1
  
  # Expected result: P(X <= 0) = 0.25
  expected_cumulative_p <- 0.25
  
  expect_equal(binomial_cumulative_less(n, p, x), expected_cumulative_p, tolerance = 1e-6)
})


test_that("binomial_cumulative_greater_equal returns correct value (P(X >= x))", {
  n <- 2
  p <- 0.5
  x <- 1
  
  # Expected result: 0.75
  expected_cumulative_p <- 0.75
  
  expect_equal(binomial_cumulative_greater_equal(n, p, x), expected_cumulative_p, tolerance = 1e-6)
})

test_that("binomial_moments returns correct mean and variance", {
  n <- 10
  p <- 0.4
  
  result <- binomial_moments(n, p)
  
  # Test the mean
  expect_equal(result$mean, 4.0)
  
  # Test the variance
  expect_equal(result$variance, 2.4)
  
  # Test edge case (p=1)
  result_edge <- binomial_moments(5, 1)
  expect_equal(result_edge$mean, 5)
  expect_equal(result_edge$variance, 0)
})