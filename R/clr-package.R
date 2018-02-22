#' Curve Linear Regression
#'
#' \code{clr} provides functions for curve linear regression via dimension
#' reduction.
#'
#' The package implements a new methodology for linear regression with both
#' curve response and curve regressors, which is described in Cho et al. (2013)
#' and Cho et al. (2015).
#' The CLR model performs a data-driven dimension reduction, based on a
#' singular value decomposition in a Hilbert Space, as well as a data
#' transformation so that the relationship between the transformed data is
#' linear and can be captured by simple regression models.
#'
#' @name clr-package
#' @aliases clr-package
#' @docType package
#' @author Amandine Pierrot <amandine.m.pierrot@@gmail.com>
#'
#' with contributions and help from Qiwei Yao, Haeran Cho, Yannig Goude and
#' Tony Aldon.
#' @references These provide details for the underlying \code{clr} methods.
#'
#' Cho, H., Y. Goude, X. Brossat, and Q. Yao (2013) Modelling and Forecasting
#' Daily Electricity Load Curves: A Hybrid Approach. Journal of the American
#' Statistical Association 108: 7-21.
#'
#' Cho, H., Y. Goude, X. Brossat, and Q. Yao (2015) Modelling and Forecasting
#' Daily Electricity Load via Curve Linear Regression. In \emph{Modeling and
#' Stochastic Learning for Forecasting in High Dimension}, edited by Anestis
#' Antoniadis and Xavier Brossat, 35-54, Springer.
#' @keywords package
NULL
