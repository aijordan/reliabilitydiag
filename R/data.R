#' Precipitation forecasts and observations at Niamey, Niger in July to September
#' 2016
#'
#'
#' @format A data frame with 92 rows and 6 variables:
#' \describe{
#'   \item{\code{date}}{a date from \code{"2016-07-01"} to \code{"2016-09-30"} in \code{Date} format.}
#'   \item{\code{Logistic}}{prediction based on logistic regression, as a probability.}
#'   \item{\code{EMOS}}{prediction based on EMOS method, as a probability.}
#'   \item{\code{ENS}}{prediction based on ECMWF raw ensemble, as a probability.}
#'   \item{\code{EPC}}{prediction based on EPC method, as a probability.}
#'   \item{\code{obs}}{observation, indicator variable where \code{1} represents the
#'   occurrence of precipitation.}
#' }
#'
#' @description
#' A data set containing 24-hour ahead daily probability of precipitation
#' forecasts of four forecasting methods and corresponding observations of
#' precipitation occurrence.
#'
#' For a detailed description of the four prediction methods, see Vogel et al (2021).
#'
#' @source
#' Vogel P, Knippertz P, Gneiting T, Fink AH, Klar M, Schlueter A (2021). "Statistical forecasts for the occurrence of precipitation outperform global models over northern tropical Africa." Geophysical Research Letters, 48, e2020GL091022. \url{https://doi.org/10.1029/2020GL091022}.
#'
#' This data set contains modified historic products
#' from the European Center for Medium-Range Weather Forecasts
#' (ECMWF, \url{https://www.ecmwf.int/}), specifically:
#' ensemble forecasts of precipitation that have been summarized to a
#' probability of precipitation (column \code{ENS}), and
#' historical observations for the occurence of precipitation (column \code{obs}).
#' The ECMWF licenses the use of expired real-time data products under the
#' Creative Commons Attribution 4.0 International
#' (CC BY 4.0, \url{https://creativecommons.org/licenses/by/4.0/}).
"precip_Niamey_2016"
