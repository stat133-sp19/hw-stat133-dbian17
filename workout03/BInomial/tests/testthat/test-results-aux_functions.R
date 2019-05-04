context("Check aux functions")

test_that("aux_mean is correct", {

  expect_equal(aux_mean(8, .5),
               4)
  expect_equal(aux_mean(10, .5),
               5)
  expect_equal(aux_mean(40, .8),
                        32)
})


test_that("aux_variance is correct", {

  expect_equal(aux_variance(3, 1),
               0)
  expect_equal(aux_variance(2, .5),
               0.5)
  expect_equal(aux_variance(200, .75),
               37.5)
})


test_that("aux_mode is correct", {

  expect_equal(aux_mode(1, .5),
               c(1, 0))
  expect_equal(aux_mode(2, .75),
               2)
  expect_equal(aux_mode(1000, .25),
               250)
})


test_that("aux_skewness is correct", {

  expect_equal(aux_skewness(1000, .5),
               0)
  expect_equal(aux_skewness(1000, 0),
               Inf)
  expect_equal(aux_skewness(1000, 1),
               -Inf)
})


test_that("aux_kurtosis is correct", {

  expect_equal(aux_kurtosis(10, .5),
               -0.2)
  expect_equal(aux_kurtosis(10, 1),
               Inf)
  expect_equal(aux_kurtosis(1000, .5),
               -0.002)
})
