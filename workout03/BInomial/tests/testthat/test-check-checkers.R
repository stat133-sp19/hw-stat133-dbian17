context("Check checking function arguments")

test_that("check_prob fails with invalid args",{
  
  expect_true(check_prob(.5))
  expect_error(check_prob(2))
  expect_error(check_prob(-.5),
               "Invalid value for prob")
})


test_that("check_trials fails with invalid args", {
  
  expect_true(check_trials(100))
  expect_error(check_trials(-2))
  expect_error(check_trials(10.5),
               "Invalid value for trials")
})


test_that("check_success fails with invalid args", {
  
  expect_true(check_success(3, 5))
  expect_error(check_success(-1, 5), 
               "Invalid success value")
  expect_error(check_success(6, 5),
               "Invalid success value")
})