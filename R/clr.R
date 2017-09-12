
#' Fit a CLR via dimension reduction
#'
#' blabla
#'
#' @param clust
#' @param qx_estimation
#' @param scale_x
#' @param ortho_y
#' @param kj
#' @param qy_estimation
#' @param d_estimation
#' @param pct
#' @param y
#' @param x
#'
#' @return
#'
#' @examples
#' library(clr)
#'
#' data(gb_load)
#'
#' clr_load <- clrdata(x = gb_load$ENGLAND_WALES_DEMAND,
#'                     order_by = gb_load$TIMESTAMP,
#'                     support_grid = 1:48)
#'
#' clr_weather <- clrdata(x = gb_load$TEMPERATURE,
#'                        order_by = gb_load$TIMESTAMP,
#'                        support_grid = 1:48)
#' @export clr


clr <- function(y, x, clust,
                qx_estimation = 'pctvar',
                scale_x = TRUE,
                ortho_y = TRUE,
                kj = NULL,
                qy_estimation = 'pctvar',
                d_estimation = 'cor',
                pct = 0.999) {

  if (missing(clust)) {
    clust_desc <- data.frame(n = nrow(y_data))
    clust_id <- list(1:nrow(y_data))
    names(clust_id[[1]]) <- rownames(y_data)
  } else {
    clust_desc <- clust[['clust_desc']]
    clust_id <- clust[['clust_id']]
  }

  y_nu <- ncol(y_data)
  x_nu <- ncol(x_data)

  clr_model <- vector('list', nrow(clust_desc))

  for (i in 1:nrow(clust_desc)) {

    # PART I: SVD

    idx <- clust_id[[i]]
    y <- y_data[idx, ]
    x <- x_data[idx, ]

    id_s <- which(substr(rownames(y), 1, 4) == '2016')[1]
    n <- nrow(y[1:(id_s - 1), ])

    y_mean <- colMeans(y[1:(id_s - 1), ], na.rm = TRUE)
    y_dm <- y[1:(id_s - 1), ] - matrix(y_mean,
                                       nrow = n,
                                       ncol = ncol(y),
                                       byrow = TRUE)  # de-meaned y

    x_mean <- colMeans(x[1:(id_s - 1), ], na.rm = TRUE)
    x_sd <- apply(x[1:(id_s - 1), ], 2, sd, na.rm = TRUE)
    # re-scaled x
    x_rs <- (x[1:(id_s - 1), ] - matrix(x_mean,
                                        nrow = n,
                                        ncol = ncol(x),
                                        byrow = TRUE)) / matrix(x_sd,
                                                                nrow = n,
                                                                ncol = ncol(x),
                                                                byrow = TRUE)
    # Orthogonalize x: extract low-dim structure of x_rs
    if (ortho_x) {

      t1 <- svd(var(x_rs, na.rm = TRUE))
      omega <- t1$d
      qx_hat <- switch(qx_estimation,
                       ratio = which.max(omega[1:(ncol(x_rs) - 1)] / omega[2:ncol(x_rs)]),
                       ratioM = max(which(omega[1:(ncol(x_rs) - 1)] / omega[2:ncol(x_rs)] > 2)),
                       pctvar = {
                         k <- 1
                         while ((sum(omega[1:k]) / sum(omega)) < pct) {
                           k <- k + 1
                         }
                         k
                       },
                       as.numeric(qx_estimation))
      # first qx_hat  eigenfunctions
      gamma <- t1$u[, 1:qx_hat, drop = FALSE]
      # Standardized transformation for X
      if (qx_hat > 1) {
        GAMMA <- gamma %*% diag(omega[1:qx_hat] ^ (-0.5))
      } else {
        GAMMA <- gamma * omega[1:qx_hat] ^ (-0.5)
      }
      # xx is new standardized X
      xx  <- x_rs %*% GAMMA
      # var(xx, na.rm = TRUE) = Id
    } else {
      xx <- x_rs
    }


    # Orthogonalize y: extract low-dim structure of y_dm
    if (ortho_y) {

      t2 <- svd(var(y_dm, na.rm = TRUE))
      tau <- t2$d
      qy_hat <- switch(qy_estimation,
                       ratio = which.max(tau[1:(ncol(y_dm) - 1)] / tau[2:ncol(y_dm)]),
                       ratioM = max(which(tau[1:(ncol(y_dm) - 1)] / tau[2:ncol(y_dm)] > 2)),
                       pctvar = {
                         k <- 1
                         while (sum(tau[1:k]) / sum(tau) < pct) {
                           k <- k + 1
                         }
                         k
                       },
                       as.numeric(qy_estimation))
      # first qy_hat  eigenfunctions
      delta <- t2$u[, 1:qy_hat, drop = FALSE]
      # Standardized transformation for Y
      if (qy_hat > 1) {
        DELTA <- delta %*% diag(tau[1:qy_hat] ^ (-0.5))
      } else {
        DELTA <- delta * tau[1:qy_hat] ^ (-0.5)
      }
      # yy is new standardized Y
      yy  <- y_dm %*% DELTA

    } else {
      yy <- y_dm
    }


    # SVD for cov(yy, xx)
    TT <- svd(cov(yy, xx, use = 'complete.obs'))
    lambda <- TT$d
    l <- length(lambda)

    d_hat <- l

    if (d_hat > 1) {
      d_hat <- switch(d_estimation,
                      max = l,
                      cor = max(which(lambda > 0.5)),
                      ratio = which.max(lambda[1:(l - 1)] / lambda[2:l]),
                      pctvar = {
                        k <- 1
                        while ((sum(lambda[1:k]) / sum(lambda)) < pct) {
                          k <- k + 1
                        }
                        k
                      })
    }

    phi <- TT$u
    psi <- TT$v

    xi <- yy %*% phi
    eta <- xx %*% psi

    # Transformation matrices for Y and X, to be used for prediction
    if (ortho_y) {
      DELTA <- DELTA %*% phi
      if (qy_hat > 1) {
        INV_DELTA <- t(phi) %*% t(delta %*% solve(diag(tau[1:qy_hat] ^ (-0.5))))
      } else {
        INV_DELTA <- t(phi) %*% t(delta %*% solve(tau[1:qy_hat] ^ (-0.5)))
      }
    }

    if (ortho_x) {
      GAMMA <- GAMMA %*% psi
    }



    # PART II: 1-1 LINEAR REGRESSION \xi_j on \eta_j

    lm_row <- sum(!is.na(rowSums(cbind(xi, eta))))

    if (ortho_x) {

      b_hat <- vector(length = l)
      sigma_hat <- vector(length = l)
      res <- matrix(nrow = lm_row, ncol = l)
      for (j in 1:l) {
        t1 <- lm(xi[, j] ~ eta[, j] - 1)
        b_hat[j] <- t1$coefficients          # Estimated slope of j-th regression
        sigma_hat[j] <- (summary(t1)$sigma)  # STD of residuals for j-th regression
        res[, j] <- t1$residuals             # Residuals of j-th regression
      }
      clr_model[[i]][['res']] <- res
      clr_model[[i]][['qy']] <- l

    } else {

      if (kj > l) stop('kj has to be lower than min(qx, qy)')
      b_hat <- matrix(nrow = l, ncol = kj)
      sigma_hat <- matrix(nrow = l, ncol = kj)
      res <- matrix(nrow = lm_row, ncol = l)
      for (j in 1:l) {
        t1 <- lm(xi[, j] ~ eta[, 1:kj] - 1)
        b_hat[j, ] <- t1$coefficients          # Estimated slope of j-th regression
        sigma_hat[j, ] <- summary(t1)$sigma  # STD of residuals for j-th regression
        res[, j] <- t1$residuals             # Residuals of j-th regression
      }
      clr_model[[i]][['res']] <- res
      clr_model[[i]][['qy']] <- ncol(xi)
    }


    clr_model[[i]][['d_hat']] <- d_hat
    clr_model[[i]][['sigma_hat']] <- sigma_hat

    if (ortho_x) {
      xi_hat <- eta * matrix(b_hat,
                             nrow = nrow(eta), ncol = ncol(eta), byrow = TRUE)
    } else {
      xi_hat <- eta[, 1:kj, drop = FALSE] %*% t(b_hat)
    }

    clr_model[[i]][['xi_hat']] <- xi_hat
    clr_model[[i]][['y_dm']] <- y_dm
    clr_model[[i]][['qx_hat']] <- ncol(xx)
    clr_model[[i]][['qy_hat']] <- ncol(yy)

    # PART III: CALCULATE PREDICTION

    # trend pb: y_mean (in-sample), much higher than y_s_mean (out-sample)

    # y_s (y to predict)
    y_s <- y[id_s:nrow(y), , drop = FALSE]
    x_s <- x[id_s:nrow(x), , drop = FALSE]

    y_mean_s <- y_mean
    x_mean_s <- x_mean

    if (corr_mean) {
      # with out-sample mean
      y_mean_s <- colMeans(y_s, na.rm = TRUE)
      x_mean_s <- colMeans(x_s, na.rm = TRUE)
    }

    y_s_dm <- y_s - matrix(y_mean_s,
                           nrow = nrow(y_s),
                           ncol = ncol(y_s),
                           byrow = TRUE)
    x_s_rs <- (x_s - matrix(x_mean_s,
                            nrow = nrow(x_s),
                            ncol = ncol(x_s),
                            byrow = TRUE)) / matrix(x_sd,
                                                    nrow = nrow(x_s),
                                                    ncol = ncol(x_s),
                                                    byrow = TRUE)

    if (ortho_x) {

      eta_s <- x_s_rs %*% GAMMA
      xi_s_hat <- eta_s * matrix(b_hat,
                                 nrow = nrow(eta_s),
                                 ncol = ncol(eta_s),
                                 byrow = TRUE)
      if (ortho_y) {
        y_s_hat <- (xi_s_hat[, 1:d_hat, drop = FALSE] %*% INV_DELTA[1:d_hat, , drop = FALSE]) +
          matrix(y_mean_s,
                 nrow = nrow(y_s),
                 ncol = ncol(y_s),
                 byrow = TRUE)
        xi_s <- y_s_dm %*% DELTA
      } else {
        y_s_hat <- xi_s_hat[, 1:d_hat, drop = FALSE] %*% t(phi[, 1:d_hat, drop = FALSE]) +
          matrix(y_mean_s,
                 nrow = nrow(y_s),
                 ncol = ncol(y_s),
                 byrow = TRUE)
        xi_s <- y_s_dm %*% phi
      }

    } else {
      eta_s <- x_s_rs %*% psi
      xi_s_hat <- eta_s[, 1:kj, drop = FALSE] %*% t(b_hat)
      y_s_hat <- xi_s_hat[, 1:d_hat, drop = FALSE] %*% t(phi[, 1:d_hat, drop = FALSE]) +
        matrix(y_mean_s,
               nrow = nrow(y_s),
               ncol = ncol(y_s),
               byrow = TRUE)
      xi_s <- y_s_dm %*% phi
    }

    rownames(y_s_hat) <- rownames(y_s)
    clr_model[[i]][['pointwise_pred']] <- y_s_hat

    clr_model[[i]][['epsilon']] <- xi_s - xi_s_hat

    if (ortho_y) {
      clr_model[[i]][['INV_DELTA']] <- INV_DELTA
    } else {
      clr_model[[i]][['phi']] <- phi
    }

    clr_model[[i]][['xi_s_hat']] <- xi_s_hat
    clr_model[[i]][['y_s_dm']] <- y_s_dm

  }



}


# gestion mean: moyenne glissante, prev, online ...
# possibilité de proposer des fonctions pour construire l'indexation cluster
#
#
#
