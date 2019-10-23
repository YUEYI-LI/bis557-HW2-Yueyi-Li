#' Fit a linear model using ridge regularization
#'
#' \code{ridge} returns the coefficients of the linear models fiited with different values of penalized parameter \eqn{\lambda}.
#'
#' @param X A data frame containing the variables in the model. The levels of categorical variables should be
#'   represented as numeric.
#' @param y A vector of the quantitative response variable.
#' @param lambda_vals A vector of possible values of the tuning paramenter \eqn{\lambda} in ridge regression.
#' @return \code{ridge} returns a matrix of regression coefficients with ncol(X) columns
#'   and length(lambda_vals) rows.
#' @examples
#' data(iris)
#' X <- iris[,2:5]
#' X$Species <- as.numeric(X$Species)
#' y <- as.numeric(iris[,1])
#' fit <- ridge(X, y, 0:500)
#' @keywords ridge regression
ridge <- function(X, y, lambda_vals) {
  svd_obj <- svd(X)
  U <- svd_obj$u
  V <- svd_obj$v
  d <- svd_obj$d
  ridge_beta <- matrix(NA_real_, nrow = length(lambda_vals), ncol = ncol(X))
  for (i in 1 : length(lambda_vals)) {
    D <- diag(d / (d^2 + lambda_vals[i]))
    ridge_beta[i,] <- V %*% D %*% t(U) %*% y
  }
  ridge_beta
}

#' Find the optimized value of parameter lambda in ridge regression
#'
#' \code{opt_ridge} returns the optimized ridge parameter \eqn{\lambda} within a given set of possible values of \eqn{\lambda}.
#'   Cross validation is used and optimization is achieved by the least value of mean squared error.
#' @param X A data frame containing the variables in the model. The levels of categorical variables should be
#'   represented as numeric.
#' @param y A vector of the quantitative response variable.
#' @param lambda_vals A vector of possible values of the tuning paramenter \eqn{\lambda} in ridge regression.
#' @return \code{opt_ridge} returns the optimized value of parameter \eqn{\lambda}.
#'   And a plot of mean squared errors versus \eqn{\lambda} is produced.
#' @examples
#' data(iris)
#' X <- iris[,2:5]
#' X$Species <- as.numeric(X$Species)
#' y <- iris[,1]
#' lambda <- opt_ridge(X, y, 1:500)
#' @keywords ridge regression
opt_ridge <- function (X, y, lambda_vals){
  set.seed(123)
  # shuffle X and y
  ind <- sample(nrow(X), nrow(X), replace = F)
  X <- X[ind,]
  y <- y[ind]
  error <- rep(0, length(lambda_vals))
  folds <- as.numeric(cut(1:nrow(X), 5))
  for (j in 1: length(lambda_vals)) {
    for (i in 1:5){
      X_train <- X[folds != i, ]
      y_train <- y[folds != i]
      X_test <- X[folds == i, ]
      y_test <- y[folds == i]
      svd_obj <- svd(X_train)
      U <- svd_obj$u
      V <- svd_obj$v
      d <- svd_obj$d
      D <- diag(d / (d^2 + lambda_vals[j]))
      ridge_beta <- V %*% D %*% t(U) %*% y_train
      pred <- as.matrix(X_test) %*% ridge_beta
      error[j] <- error[j] + sum(folds==i)/nrow(X) * mean((pred - y_test)^2)
    }
  }
  plot(lambda_vals,error)
  lambda_vals[which.min(error)]
}
