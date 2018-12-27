
#' Prediction from fitted CLR model(s)
#'
#' Takes a fitted \code{clr} object produced by \code{clr()} and produces
#' predictions given a new set of functions or the original values used for
#' the model fit.
#'
#' @param object A fitted \code{clr} object produced by \code{clr()}.
#' @param newX An object of class \code{clrdata} or a matrix with one function a
#' row. If this is not provided then predictions corresponding to the original
#' data are returned. If \code{newX} is provided then it should contain the
#' same type of functions as the original ones (same dimension, same clusters
#' eventually, ...).
#' @param newclust A new list of indices to obtain (approximately) homogeneous
#' dependence structure inside each cluster of functions.
#' @param newXmean To complete when done
#' @param simplify If TRUE, one matrix of predicted functions is returned
#' instead of a list of matrices (one matrix by cluster). In the final matrix,
#' rows are sorted by increasing row numbers.
#' @param ... Further arguments are ignored.
#'
#' @return predicted functions
#'
#' @export
#'
#' @examples
#' library(clr)
#' data(gb_load)
#'
#' clr_load <- clrdata(x = gb_load$ENGLAND_WALES_DEMAND,
#'                     order_by = gb_load$TIMESTAMP,
#'                     support_grid = 1:48)
#'
#' # data cleaning: replace zeros with NA
#' clr_load[rowSums((clr_load == 0) * 1) > 0, ] <- NA
#'
#' Y <- clr_load[2:nrow(clr_load), ]
#' X <- clr_load[1:(nrow(clr_load) - 1), ]
#'
#' begin_pred <- which(substr(rownames(Y), 1, 4) == '2016')[1]
#' Y_train <- Y[1:(begin_pred - 1), ]
#' X_train <- X[1:(begin_pred - 1), ]
#' Y_test <- Y[begin_pred:nrow(Y), ]
#' X_test <- X[begin_pred:nrow(X), ]
#'
#'
#' ## Example without any cluster
#' model <- clr(Y = Y_train, X = X_train)
#'
#' pred_on_train <- predict(model)
#' head(pred_on_train[[1]])
#'
#' pred_on_test <- predict(model, newX = X_test)
#' head(pred_on_test[[1]])
#'
#'
#' ## Example with clusters
#' model <- clr(Y = Y_train, X = X_train, clust = clust_train)
#'
#' pred_on_train <- predict(model)
#' str(pred_on_train)
#' head(pred_on_train[[1]])
#'
#' pred_on_test <- predict(model, newX = X_test, newclust = clust_test,
#'                         simplify = TRUE)
#' str(pred_on_test)
#' head(pred_on_test)
#'
#' # With dates as row names
#' rownames(pred_on_test) <- rownames(Y_test)[as.numeric(rownames(pred_on_test))]



predict.clr <- function(object, newX = NULL, newclust = NULL,
                        newXmean = NULL, simplify = FALSE, ...) {

  nclust <- length(object)

  if (!is.null(newX)) {
    if (ncol(newX) != length(object[[1]]$X_mean)) {
      stop(paste0('Functions in newX should have the same dimension as the ',
                  'original ones'))
    }
    X <- newX
    if (is.null(newclust)) {
      if (nclust != 1) {
        stop('Need clusters in newclust for newX')
      } else {
        newclust <- list(1:nrow(newX))
      }
    } else {
      if (length(newclust) != nclust) {
        stop(paste0('The number of clusters in newclust should be the same as',
                    ' in the clr object'))
      }
    }
  }

  predictions <- vector('list', nclust)

  for (i in 1:nclust) {

    if (is.null(newX)) {

      ortho_Y <- object[[i]]$ortho_Y
      d_hat <- object[[i]]$d_hat
      b_hat <- object[[i]]$b_hat
      if (ortho_Y) {
        INV_DELTA <- object[[i]]$INV_DELTA
      } else {
        phi <- object[[i]]$phi
      }
      Y_mean <- object[[i]]$Y_mean
      Y_nu <- length(Y_mean)
      eta <- object[[i]]$eta

      xi_hat <- eta[, 1:d_hat] * matrix(b_hat,
                                        nrow = nrow(eta),
                                        ncol = d_hat,
                                        byrow = TRUE)

      if (ortho_Y) {
        Y_hat <- xi_hat[, 1:d_hat, drop = FALSE] %*%
          INV_DELTA[1:d_hat, , drop = FALSE] +
          matrix(Y_mean,
                 nrow = nrow(xi_hat),
                 ncol = Y_nu,
                 byrow = TRUE)
      } else {
        Y_hat <- xi_hat[, 1:d_hat, drop = FALSE] %*%
          t(phi[, 1:d_hat, drop = FALSE]) +
          matrix(Y_mean,
                 nrow = nrow(xi_hat),
                 ncol = Y_nu,
                 byrow = TRUE)
      }

      row.names(Y_hat) <- object[[i]]$idx
      colnames(Y_hat) <- names(object[[i]]$Y_mean)
      predictions[[i]] <- Y_hat

      # End if no newX

    } else {

      if (is.null(newclust)) {
        idx <- 1:nrow(X)
      } else {
        idx <- newclust[[i]]
      }

      X_clust <- X[idx, ]

      X_mean <- object[[i]]$X_mean
      X_sd <- object[[i]]$X_sd
      Y_mean <- object[[i]]$Y_mean
      GAMMA <- object[[i]]$GAMMA
      ortho_Y <- object[[i]]$ortho_Y
      if (ortho_Y) {
        INV_DELTA <- object[[i]]$INV_DELTA
      } else {
        phi <- object[[i]]$phi
      }
      b_hat <- object[[i]]$b_hat
      d_hat <- object[[i]]$d_hat
      X_nu <- length(X_mean)
      Y_nu <- length(Y_mean)

      X_rs <- (X_clust - matrix(X_mean,
                                nrow = nrow(X_clust),
                                ncol = X_nu,
                                byrow = TRUE)) / matrix(X_sd,
                                                        nrow = nrow(X_clust),
                                                        ncol = X_nu,
                                                        byrow = TRUE)

      eta <- X_rs %*% GAMMA
      xi_hat <- eta[, 1:d_hat] * matrix(b_hat,
                                        nrow = nrow(eta),
                                        ncol = d_hat,
                                        byrow = TRUE)

      if (ortho_Y) {
        Y_hat <- xi_hat[, 1:d_hat, drop = FALSE] %*%
          INV_DELTA[1:d_hat, , drop = FALSE] +
          matrix(Y_mean,
                 nrow = nrow(xi_hat),
                 ncol = Y_nu,
                 byrow = TRUE)
      } else {
        Y_hat <- xi_hat[, 1:d_hat, drop = FALSE] %*%
          t(phi[, 1:d_hat, drop = FALSE]) +
          matrix(Y_mean,
                 nrow = nrow(xi_hat),
                 ncol = Y_nu,
                 byrow = TRUE)
      }

      row.names(Y_hat) <- idx
      colnames(Y_hat) <- names(object[[i]]$Y_mean)
      predictions[[i]] <- Y_hat
    }

  }

  if (simplify) {
    predictions <- do.call(rbind, predictions)
    predictions <- predictions[order(as.numeric(row.names(predictions))), ]
  }
  return(predictions)
}


# work on newMean: moyenne glissante, prev, online ...

