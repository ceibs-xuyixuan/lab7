context("Ridge Regression")
test_that("Ridge Regression",{
  expect_that(ridgereg(Sepal.Length ~ Species, iris)$coef(), equals(c(5.0056720, 0.9302056, 1.5821199) ))
})
