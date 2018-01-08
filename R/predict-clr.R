
#' Model predictions with Curve Linear Regression
#'
#' @param object
#' @param ...
#' @param newX
#' @param clust
#' @param newXmean
#'
#' @return
#' @export
#'
#' @examples
#' library(clr)
#'
#' Sys.setenv(TZ = 'Europe/London')
#' # PB between High Sierra and R
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
#'
#' model <- clr(Y = Y_train, X = X_train)
#'
#' pred_on_train <- predict.clr(model)
#' head(pred_on_train[[1]])
#'
#' X_test <- X[begin_pred:nrow(X), ]
#' pred_on_test <- predict.clr(model, newX = X_test)
#' head(pred_on_test[[1]])


predict.clr <- function(object, newX = NULL, clust = NULL,
                        newXmean = NULL, ...) {

  nclust <- length(object)

  if (!is.null(newX)) {
    X <- newX
    nclust <- length(object)
    if (is.null(clust)) {
      if (nclust != 1) {
        stop ('Need clusters in clust for newX')
      } else {
        clust_desc <- data.frame(n = nrow(X))
        clust_id <- list(1:nrow(X))
      }
    } else {
      clust_desc <- clust$clust_desc
      clust_id <- clust$clust_id
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

      row.names(Y_hat) <- NULL
      predictions[[i]] <- Y_hat

    } else {

      idx <- clust_id[[i]]
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
      X_nu <- ncol(X_clust)
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

      row.names(Y_hat) <- NULL
      predictions[[i]] <- Y_hat
      # comment gérer les dates ? via clust ?

    }

  }

  return(predictions)

}


# how to manage newMean: moyenne glissante, prev, online ...

