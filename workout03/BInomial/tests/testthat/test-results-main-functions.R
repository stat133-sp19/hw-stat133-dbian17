context("Check main functions")

test_that("check bin_choose works properly", {
  
  expect_error(bin_choose(5, 6), 
               "k cannot be greater than n")
  expect_equal(bin_choose(5, 2), 
               10)
  expect_equal(bin_choose(10, 1:3), 
               c(10, 45, 120))
})


test_that("check bin_probability works properly", {
  
  expect_error(bin_probability(9, 8, .2))
  expect_equal(bin_probability(2, 5, .5), 
               0.3125)
  expect_equal(bin_probability(0:2, 5, .5), 
               c(.03125, 0.15625, 0.31250))
})


test_that("check bin_distribution works properly", {
  
  expect_error(bin_distribution(5, 2))
  expect_equal(bin_distribution(6, .6)$success,
               0:6)
  expect_equal(bin_distribution(6, .6)$probability[5], 
               .31104)
  
})


test_that("check bin_cumulative works properly", {
  
  expect_error(bin_cumulative(5, 2))
  expect_equal(bin_cumulative(6, .6)$success,
               0:6)
  expect_equal(head(bin_cumulative(6, .6)$cumulative[5]),
               .76672)
})
  