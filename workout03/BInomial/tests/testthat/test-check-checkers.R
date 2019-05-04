context("Check checking function arguments")

test_that("check_prob stops with incorrect args",{
  
  expect_true(check_prob(.7))
  expect_error(check_prob(6))
  expect_error(check_prob(-.023),
               "Invalid value for prob")
})


test_that("check_trials stops with incorrect args", {
  
  expect_true(check_trials(120))
  expect_error(check_trials(-2))
  expect_error(check_trials(10.5),
               "Invalid value for trials")
})


test_that("check_success stops with incorrect args", {
  
  expect_true(check_success(2, 6))
  expect_error(check_success(-3, 5), 
               "Invalid success value")
  expect_error(check_success(3, 5.2), 
               "Invalid success value")
  expect_error(check_success(9, 7),
               "Invalid success value")
})