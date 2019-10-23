context("Estimated coefficients of Ridge regression")

library(casl)
library(glmnet)

test_that("Your ridge() function works with iris dataset.", {

  data(iris)
  X <- iris[,2:5]
  X$Species <- as.numeric(X$Species)
  y <- iris[,1]

  betahat <- casl::casl_lm_ridge(as.matrix(X), y, lambda = 27)

  fit_ridge <- ridge(as.matrix(X), y, 27)

  expect_equivalent(fit_ridge, betahat,
                    tolerance = 1e-4)
})

test_that("Your ridge() function works with colinear dataset.", {

  data(iris)
  X <- iris[,2:5]
  X$Species <- as.numeric(X$Species)
  y <- iris[,1]
  alpha <- 0.001
  X[,1] <- X[,1] * alpha + X[,2] * (1 - alpha)

  betahat <- casl::casl_lm_ridge(as.matrix(X), y, lambda = 94)

  fit_ridge <- ridge(as.matrix(X), y, 94)

  expect_equivalent(fit_ridge, betahat,
                    tolerance = 1e-4)
})


test_that("Your opt_ridge() function works with iris dataset.", {

  data(iris)
  X <- iris[,2:5]
  X$Species <- as.numeric(X$Species)
  y <- iris[,1]
  lambda <- opt_ridge(X, y, 1:500)

  cv_fit <- glmnet::cv.glmnet(as.matrix(X), y, alpha = 0, lambda = 1:100)
  opt_lambda <- cv_fit$lambda.min

  expect_equivalent(lambda, opt_lambda,
                    tolerance = 1e-4)
})
