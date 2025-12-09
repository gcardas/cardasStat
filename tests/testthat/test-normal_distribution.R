# Define common parameters for testing
mean_std <- 0
sd_std <- 1
mean_ex <- 10
sd_ex <- 2
tolerance <- 1e-6

# --- Test 1: p_normal_greater(x, mean, sd) ---
test_that("p_normal_greater returns correct upper-tail probability (P(X > x))", {
  # Standard Normal: P(X > 0) = 0.5
  expect_equal(p_normal_greater(0, mean_std, sd_std), 0.5, tolerance = tolerance)
  
  # Standard Normal: P(X > 1.96) = 0.025
  expect_equal(p_normal_greater(1.96, mean_std, sd_std), 0.0249979, tolerance = tolerance)
  
  # Example Non-Standard: P(X > 12) in N(10, 2)
  # Expected: stats::pnorm(12, 10, 2, lower.tail = FALSE)
  expect_equal(p_normal_greater(12, mean_ex, sd_ex), stats::pnorm(12, 10, 2, lower.tail = FALSE), tolerance = tolerance)
})

# ---------------------------------------------------

# --- Test 2: p_normal_lower_equal(x, mean, sd) ---
test_that("p_normal_lower_equal returns correct cumulative probability (P(X <= x))", {
  # Standard Normal: P(X <= 0) = 0.5
  expect_equal(p_normal_lower_equal(0, mean_std, sd_std), 0.5, tolerance = tolerance)
  
  # Standard Normal: P(X <= 1.96) = 0.975
  expect_equal(p_normal_lower_equal(1.96, mean_std, sd_std), 0.9750021, tolerance = tolerance)
  
  # Example Non-Standard: P(X <= 8) in N(10, 2)
  expect_equal(p_normal_lower_equal(8, mean_ex, sd_ex), stats::pnorm(8, 10, 2), tolerance = tolerance)
})

# ---------------------------------------------------

# --- Test 3: p_normal_lower(x, mean, sd) ---
# NOTE: This test validates the function's internal logic (using x-1), 
# which is mathematically incorrect for continuous distributions but is required by the function's definition.
test_that("p_normal_lower uses the required (x-1) calculation for P(X < x)", {
  # Standard Normal: x=1. Function calculates P(X <= 0). Expected: 0.5
  expect_equal(p_normal_lower(1, mean_std, sd_std), 0.5, tolerance = tolerance)
  
  # Example Non-Standard: x=11. Function calculates P(X <= 10) in N(10, 2). Expected: 0.5
  expect_equal(p_normal_lower(11, mean_ex, sd_ex), 0.5, tolerance = tolerance)
  
  # General comparison: P(X < x) should equal P(X <= x-1)
  x_val <- 1.5 
  expected_p <- stats::pnorm(x_val - 1, mean_std, sd_std) # P(X <= 0.5)
  expect_equal(p_normal_lower(x_val, mean_std, sd_std), expected_p, tolerance = tolerance)
})

# ---------------------------------------------------

# --- Test 4: p_normal_interval(a, b, mean, sd) ---
# Calculates P(a < X <= b) using P(X > a) - P(X > b)
test_that("p_normal_interval returns correct interval probability P(a <= X <= b)", {
  # Standard Normal: P(-1.96 <= X <= 1.96). Expected: 0.975 - 0.025 = 0.95
  a <- -1.96
  b <- 1.96
  expected_interval_p <- 0.9500042
  expect_equal(p_normal_interval(a, b, mean_std, sd_std), expected_interval_p, tolerance = 1e-6)
  
  # Example Non-Standard: P(8 <= X <= 12) in N(10, 2). (Symmetric around the mean)
  # Expected: P(X <= 12) - P(X <= 8)
  p_le_12 <- stats::pnorm(12, 10, 2)
  p_le_8 <- stats::pnorm(8, 10, 2)
  expected_p <- p_le_12 - p_le_8
  
  expect_equal(p_normal_interval(8, 12, mean_ex, sd_ex), expected_p, tolerance = tolerance)
  
  # Test non-symmetric interval: P(9 <= X <= 13)
  p_ge_9 <- stats::pnorm(9, 10, 2, lower.tail = FALSE)
  p_ge_13 <- stats::pnorm(13, 10, 2, lower.tail = FALSE)
  
  expect_equal(p_normal_interval(9, 13, mean_ex, sd_ex), p_ge_9 - p_ge_13, tolerance = tolerance)
})