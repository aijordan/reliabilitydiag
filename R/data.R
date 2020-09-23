#' Precipitation forecasts and observations at Niamey, Niger in July to September
#' 2016
#'
#' A dataset containing 24-hour ahead daily probability of precipitation
#' forecasts of four forecasting methods and corresponding observations of
#' precipitation occurrence.
#'
#' @format A data frame with 92 rows and 6 variables:
#' \describe{
#'   \item{date}{date}
#'   \item{Logistic}{prediction based on logistic regression, as a probability}
#'   \item{EMOS}{prediction based on EMOS method, as a probability}
#'   \item{ENS}{prediction based on ECMWF raw ensemble, as a probability}
#'   \item{EPC}{prediction based on EPC method, as a probability}
#'   \item{obs}{observation, indicator variable where \code{1} represents the
#'   occurrence of precipitation}
#' }
#' @source
#' This package contains data from the European Center for Medium-Range Weather
#' Forecasts (ECMWF, \url{https://www.ecmwf.int/}).
#'
#' The data set 'precip_Niamey_2016' contains
#' modified historic ECMWF products, specifically:
#' ensemble forecasts of precipitation that have been summarized to a
#' probability of precipitation (column 'ENS'), and
#' historical observations for the occurence of precipitation (column 'obs').
#' These data components are governed by the
#' Creative Commons Attribution 4.0 International
#' (CC BY 4.0, \url{https://creativecommons.org/licenses/by/4.0/}).
"precip_Niamey_2016"
