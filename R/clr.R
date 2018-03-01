
#' Curve Linear Regression via dimension reduction
#'
#' Fits a curve linear regression (CLR) model to data, using dimension
#' reduction based on singular value decomposition.
#'
#' @seealso \code{\link{clr-package}}, \code{\link{clrdata}} and
#' \code{\link{predict.clr}}.
#'
#' @param Y An object of class \code{clrdata} or \code{matrix}, of the response
#' curves (one curve a row).
#' @param X An object of class \code{clrdata} or \code{matrix}, of the regressor
#' curves (one curve a row).
#' @param clust If needed, a list of row indices for each cluster, to obtain
#' (approximately) homogeneous dependence structure inside each cluster.
#' @param qx_estimation A list containing both values for 'method' (among
#' 'ratio', 'ratioM', 'pctvar', 'fixed') and for 'param' (depending on the
#' selected method), in order to choose how to estimate the dimension of X (in
#' the sense that its Karhunen-Lo\`{e}ve decomposition has qx terms only.
#' @param ortho_Y If TRUE then Y is orthogonalized.
#' @param qy_estimation Same as for qx_estimation, if ortho_Y is set to TRUE.
#' @param d_estimation A list containing both values for 'method' (among
#' 'ratio', 'pctvar', 'cor') and for 'param' (depending on the
#' selected method), in order to choose how to estimate the correlation
#' dimension.
#'
#' @return An object of class \code{clr}, which can be used to compute
#' predictions.
#' This \code{clr} object is a list of lists: one list by cluster of data, each
#' list including:
#' \item{residuals}{The matrix of the residuals of d_hat simple linear
#' regressions.}
#' \item{b_hat}{The vector of the estimated coefficient of the d_hat simple
#' straight line regressions.}
#' \item{eta}{The matrix of the projections of X.}
#' \item{xi}{The matrix of the projections of Y.}
#' \item{qx_hat}{The estimated dimension of X.}
#' \item{qy_hat}{The estimated dimension of Y.}
#' \item{d_hat}{The estimated correlation dimension.}
#' \item{X_mean}{The mean of the regressor curves.}
#' \item{X_sd}{The standard deviation of the regressor curves.}
#' \item{Y_mean}{The mean of the response curves.}
#' \item{ortho_Y}{The value which was selected for ortho_Y.}
#' \item{GAMMA}{The standardized transformation for X.}
#' \item{INV_DELTA}{The standardized transformation for Y to predict if ortho_Y
#' was set to TRUE.}
#' \item{phi}{The eigenvectors for Y to predict if ortho_Y was set to FALSE.}
#' \item{idx}{The indices of the rows selected from X and Y for the current
#' cluster.}
#'
#' @importFrom stats cov lm sd var
#'
#' @export
#'
#' @examples
#' library(clr)
#' data(gb_load)
#' data(clust_train)
#'
#' clr_load <- clrdata(x = gb_load$ENGLAND_WALES_DEMAND,
#'                     order_by = gb_load$TIMESTAMP,
#'                     support_grid = 1:48)
#'
#' ## data cleaning: replace zeros with NA
#' clr_load[rowSums((clr_load == 0) * 1) > 0, ] <- NA
#' matplot(t(clr_load), ylab = 'Daily loads', type = 'l')
#'
#' Y <- clr_load[2:nrow(clr_load), ]
#' X <- clr_load[1:(nrow(clr_load) - 1), ]
#'
#' begin_pred <- which(substr(rownames(Y), 1, 4) == '2016')[1]
#' Y_train <- Y[1:(begin_pred - 1), ]
#' X_train <- X[1:(begin_pred - 1), ]
#'
#' ## Example without any cluster
#' model <- clr(Y = Y_train, X = X_train)
#'
#' ## Example with clusters
#' model <- clr(Y = Y_train, X = X_train, clust = clust_train)


clr <- function(Y, X, clust = NULL,
                qx_estimation = list(method = 'pctvar',
                                     param = 0.999),
                ortho_Y = TRUE,
                qy_estimation = list(method = 'pctvar',
                                     param = 0.999),
                d_estimation = list(method = 'cor',
                                    param = 0.5)) {

  # Conditions on data trains
  if (!is.matrix(Y) | !is.matrix(X)) {
    stop('Y and X should be matrices')
  }

  if (nrow(Y) != nrow(X)) {
    stop('Y and X should have the same number of rows!')
  }

  # option clust
  if (is.null(clust)) {
    clust <- list(1:nrow(Y))
  }

  nclust <- length(clust)

  Y_nu <- ncol(Y)
  X_nu <- ncol(X)

  # Conditions on qx_estimation
  if (!(qx_estimation$method %in% c('ratio', 'ratioM', 'pctvar', 'fixed'))) {
    stop('qx_estimation$method has to be one of ratio, ratioM, pctvar or fixed')
  }

  if (qx_estimation$method == 'pctvar' &
      (qx_estimation$param > 1 | qx_estimation$param <= 0)) {
    stop('qx_estimation$param value has to be between 0 and 1')
  }

  if (qx_estimation$method == 'ratioM' &
      qx_estimation$param <= 0) {
    stop('qx_estimation$param value has to be positive')
  }

  if (qx_estimation$method == 'fixed' &
      !is.integer(qx_estimation$param)) {
    stop('qx_estimation$param value has to be an integer')
  }

  if (qx_estimation$method == 'fixed' &
      (qx_estimation$param <= 0 | qx_estimation$param > X_nu)) {
    stop(paste('qx_estimation$param value has to be positive and lower than',
               'the dimension of X'))
  }


  # Conditions on qy_estimation
  if (!(qy_estimation$method %in% c('ratio', 'ratioM', 'pctvar', 'fixed'))) {
  stop('qy_estimation$method has to be one of ratio, ratioM, pctvar or fixed')
  }

  if (qy_estimation$method == 'pctvar' &
      (qy_estimation$param > 1 | qy_estimation$param <= 0)) {
    stop('qy_estimation$param value has to be between 0 and 1')
  }

  if (qy_estimation$method == 'ratioM' &
      qy_estimation$param <= 0) {
    stop('qy_estimation$param value has to be positive')
  }

  if (qy_estimation$method == 'fixed' &
      !is.integer(qy_estimation$param)) {
    stop('qy_estimation$param value has to be an integer')
  }

  if (qy_estimation$method == 'fixed' &
      (qy_estimation$param <= 0 | qy_estimation$param > Y_nu)) {
    stop(paste('qy_estimation$param value has to be positive and lower than',
               'the dimension of Y'))
  }

  # Conditions on d_estimation
  if (!(d_estimation$method %in% c('ratio', 'pctvar', 'cor'))) {
    stop('d_estimation$method has to be one of cor, ratio or pctvar')
  }

  if (d_estimation$method %in% c('pctvar', 'cor') &
      (d_estimation$param > 1 | d_estimation$param <= 0)) {
    stop('param value has to be between 0 and 1 ')
  }

  if (d_estimation$method == 'cor' & ortho_Y == FALSE) {
    stop('cor method should be used only with ortho_Y set to TRUE')
  }


  object <- vector('list', nclust)

  for (i in 1:nclust) {

    # PART I: SVD
    idx <- clust[[i]]
    Y_clust <- Y[idx, ]
    X_clust <- X[idx, ]

    # message d'alerte sur le nombre de données dispo ? y compris NA

    Y_mean <- colMeans(Y_clust, na.rm = TRUE)
    Y_dm <- Y_clust - matrix(Y_mean,
                             nrow = nrow(Y_clust),
                             ncol = Y_nu,
                             byrow = TRUE)  # de-meaned Y

    X_mean <- colMeans(X_clust, na.rm = TRUE)
    X_sd <- apply(X_clust, 2, sd, na.rm = TRUE)
    # re-scaled X
    X_rs <- (X_clust - matrix(X_mean,
                              nrow = nrow(X_clust),
                              ncol = X_nu,
                              byrow = TRUE)) / matrix(X_sd,
                                                      nrow = nrow(X_clust),
                                                      ncol = X_nu,
                                                      byrow = TRUE)

    # Orthogonalize X: extract low-dim structure of X_rs
    t1 <- svd(var(X_rs, na.rm = TRUE))
    omega <- t1$d
    qx_hat <- switch(qx_estimation$method,
                     ratio = which.max(omega[1:(ncol(X_rs) - 1)] /
                                         omega[2:ncol(X_rs)]),
                     ratioM = max(which(omega[1:(ncol(X_rs) - 1)] /
                                          omega[2:ncol(X_rs)] >
                                          qx_estimation$param)),
                     pctvar = {
                       k <- 1
                       while ((sum(omega[1:k]) / sum(omega)) <
                              qx_estimation$param) {
                         k <- k + 1
                       }
                       k
                     },
                     fixed = qx_estimation$param)
    # first qx_hat eigenfunctions
    gamma <- t1$u[, 1:qx_hat, drop = FALSE]
    # Standardized transformation for X
    if (qx_hat > 1) {
      GAMMA <- gamma %*% diag(omega[1:qx_hat] ^ (-0.5))
    } else {
      GAMMA <- gamma * omega[1:qx_hat] ^ (-0.5)
    }
    # XX is new standardized X
    XX  <- X_rs %*% GAMMA
    # var(XX, na.rm = TRUE) = Id



    # Orthogonalize Y: extract low-dim structure of Y_dm
    if (ortho_Y) {
      t2 <- svd(var(Y_dm, na.rm = TRUE))
      tau <- t2$d
      qy_hat <- switch(qy_estimation$method,
                       ratio = which.max(tau[1:(Y_nu - 1)] / tau[2:Y_nu]),
                       ratioM = max(which(tau[1:(Y_nu - 1)] / tau[2:Y_nu] >
                                            qy_estimation$param)),
                       pctvar = {
                         k <- 1
                         while (sum(tau[1:k]) / sum(tau) <
                                qy_estimation$param) {
                           k <- k + 1
                         }
                         k
                       },
                       fixed = qy_estimation$param)
      # first qy_hat  eigenfunctions
      delta <- t2$u[, 1:qy_hat, drop = FALSE]
      # Standardized transformation for Y
      if (qy_hat > 1) {
        DELTA <- delta %*% diag(tau[1:qy_hat] ^ (-0.5))
      } else {
        DELTA <- delta * tau[1:qy_hat] ^ (-0.5)
      }
      # yy is new standardized Y
      YY  <- Y_dm %*% DELTA
    } else {
      YY <- Y_dm
    }


    # SVD for cov(yy, xx)
    TT <- svd(cov(YY, XX, use = 'complete.obs'))
    lambda <- TT$d
    l <- length(lambda)

    d_hat <- l

    if (d_hat > 1) {
      d_hat <- switch(d_estimation$method,
                      max = l,
                      cor = max(which(lambda > d_estimation$param)),
                      ratio = which.max(lambda[1:(l - 1)] / lambda[2:l]),
                      pctvar = {
                        k <- 1
                        while ((sum(lambda[1:k]) / sum(lambda)) <
                               d_estimation$param) {
                          k <- k + 1
                        }
                        k
                      })
    }

    phi <- TT$u
    psi <- TT$v

    xi <- YY %*% phi
    eta <- XX %*% psi

    # Transformation matrices for Y and X, to be used for prediction
    if (ortho_Y) {
      DELTA <- DELTA %*% phi
      if (qy_hat > 1) {
        INV_DELTA <- t(phi) %*% t(delta %*% solve(diag(tau[1:qy_hat] ^ (-0.5))))
      } else {
        INV_DELTA <- t(phi) %*% t(delta %*% solve(tau[1:qy_hat] ^ (-0.5)))
      }
    }

    GAMMA <- GAMMA %*% psi


    # PART II: 1-1 LINEAR REGRESSION \xi_j on \eta_j
    lm_rows <- sum(!is.na(rowSums(cbind(xi, eta))))

    b_hat <- vector(length = d_hat)
    res <- matrix(nrow = lm_rows, ncol = d_hat)
    for (j in 1:d_hat) {
      t1 <- lm(xi[, j] ~ eta[, j] - 1)
      b_hat[j] <- t1$coefficients          # Estimated slope of j-th regression
      res[, j] <- t1$residuals             # Residuals of j-th regression
    }

    object[[i]]$residuals <- res
    object[[i]]$b_hat <- b_hat

    object[[i]]$eta <- eta
    object[[i]]$xi <- xi

    object[[i]]$qx_hat <- ncol(XX)
    object[[i]]$qy_hat <- ncol(YY)
    object[[i]]$d_hat <- d_hat

    object[[i]]$X_mean <- X_mean
    object[[i]]$X_sd <- X_sd
    object[[i]]$Y_mean <- Y_mean

    object[[i]]$ortho_Y <- ortho_Y
    object[[i]]$GAMMA <- GAMMA
    if (ortho_Y) {
      object[[i]]$INV_DELTA <- INV_DELTA
    } else {
      object[[i]]$phi <- phi
    }

    object[[i]]$idx <- idx

  }

  class(object) <- 'clr'
  return(object)
}


