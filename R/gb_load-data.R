#' Electricity load from Great Britain
#'
#' A dataset containing half-hourly electricity load from Great Britain from
#' 2011 to 2016, together with observed temperatures. Temperatures are computed
#' from weather stations all over the country. It is a weighted averaged
#' temperature depending on population geographical distribution.
#'
#' @docType data
#' @author Amandine Pierrot <amandine.m.pierrot@@gmail.com>
#' @format A data frame with 105216 rows and 7 variables: \describe{
#'   \item{SETTLEMENT_DATE}{date, the time zone being Europe/London}
#'   \item{SETTLEMENT_PERIOD}{time of the day}
#'   \item{TIMESTAMP}{date-time, the time zone being Europe/London}
#'   \item{ENGLAND_WALES_DEMAND}{British electric load, measured in MW, on
#'   average over the half hour}
#'   \item{TEMPERATURE}{observed temperature in Celsius}
#'   \item{MV}{percentage of missing values when averaging over weather stations,
#'   depending on the weight of the station}
#'   \item{DAY_TYPE}{type of the day of the week, from 1 for Sunday to 7 for
#'   Saturday, 8 being banking holidays}}
#' @source
#' \href{http://www2.nationalgrid.com/UK/Industry-information/Electricity-transmission-operational-data/Data-Explorer/}{National Grid}\cr
#' \href{https://gis.ncdc.noaa.gov/maps/ncei/cdo/alltimes}{National Centers for Environmental Information}
'gb_load'
