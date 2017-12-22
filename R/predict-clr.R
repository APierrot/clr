
#' Model predictions with Curve Linear Regression
#'
#' @param object
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'


predict.clr <- function(object, ...) {

}



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


# gestion mean: moyenne glissante, prev, online ...
# possibilité de proposer des fonctions pour construire l'indexation cluster
#
#

