
#' Create an object of \code{clrdata}
#'
#' \code{clrdata} is used to create a \code{clrdata} object from raw data
#' inputs.
#'
#' @param x A vector containing the time series values
#' @param order_by A corresponding vector of unique time-dates - must be of
#' class 'POSIXct'
#' @param support_grid A vector corresponding to the support grid of functional
#' data
#'
#' @return An object of class \code{clrdata} with one function a row. As it
#' inherits the \code{matrix} class, all \code{matrix} methods remain valid.
#' If time-dates are missing in x, corresponding NA functions are added by
#' \code{clrdata} so that time sequence is preserved between successive rows.
#' @import magrittr dplyr
#' @importFrom lubridate is.POSIXct date
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
#' head(clr_load)
#' dim(clr_load)
#' summary(clr_load)
#'
#' matplot(t(clr_load), ylab = 'Daily loads', type = 'l')
#' lines(colMeans(clr_load, na.rm = TRUE),
#'       col = 'black', lwd = 2)
#'
#'
#' clr_weather <- clrdata(x = gb_load$TEMPERATURE,
#'                        order_by = gb_load$TIMESTAMP,
#'                        support_grid = 1:48)
#' summary(clr_weather)
#' plot(1:48,
#'      colMeans(clr_weather, na.rm = TRUE),
#'      xlab = 'Instant', ylab = 'Mean of temperatures',
#'      type = 'l', col = 'cornflowerblue')


clrdata <- function(x, order_by, support_grid) {

  if (!lubridate::is.POSIXct(order_by)) {
    stop("order_by must be of class 'POSIXct'")
  }
  if (length(x) != length(order_by)) {
    stop('Bad dimensions: x and order_by must have same length')
  }

  df <- data.frame(order_by, x)
  nu <- length(support_grid)
  n <- nrow(df)

  n_instants <- NULL

  df <- df %>%
    dplyr::mutate(date = lubridate::date(order_by)) %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(n_instants = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_instants == nu)

  if (nrow(df) == 0) {
    stop(paste('Bad argument: the time step in order_by does not match',
               'the length of support_grid'))
  }

  clrmat <- matrix(df[['x']],
                   nrow = nrow(df) / nu,
                   ncol = nu,
                   byrow = TRUE,
                   dimnames = list(as.character(unique(df[['date']])),
                                   support_grid))

  ref_date <- as.character(seq(from = df[['date']][1],
                               to = df[['date']][nrow(df)],
                               by = 'day'))

  missing_curves <- setdiff(ref_date, rownames(clrmat))
  clrmat <- rbind(clrmat,
                  matrix(NA, nrow = length(missing_curves), ncol = nu,
                         dimnames = list(missing_curves, support_grid)))
  clrmat <- clrmat[order(rownames(clrmat)), ]
  class(clrmat) <- c('clrdata', 'matrix')

  if (n > nrow(df)) {
    message('Rows with less or more than ', nu,
            ' values have been replaced with NA')
  }

  return(clrmat)

}


