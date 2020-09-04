#' Summarizing reliability diagrams
#'
#' A summary of a reliability diagram in terms of quantitative measures of
#' predictive performance, miscalibration, discrimination,
#' and uncertainty.
#'
#' Predictive performance is measured by the mean score of the original
#' forecast values, denoted by \eqn{S}.
#'
#' Uncertainty, denoted by \eqn{UNC}, is the mean score of a constant
#' prediction at the value of the average observation.
#' It is the highest possible mean score of a calibrated prediction method.
#'
#' Discrimination, denoted by \eqn{DSC}, is \eqn{UNC} minus the mean score
#' of the PAV-recalibrated forecast values.
#' A small value indicates a low information content (low signal) in the
#' original forecast values.
#'
#' Miscalibration, denoted by \eqn{MCB}, is \eqn{S} minus the mean score
#' of the PAV-recalibrated forecast values.
#' A high value indicates that predictive performance of the prediction method
#' can be improved by recalibration.
#'
#' These measures are related by the following equation,
#' \deqn{S = MCB - DSC + UNC.}
#' Score decompositions of this type have been studied extensively, but the
#' optimality of the PAV solution ensures that \eqn{MCB} is nonnegative.
#' This is a unique property achieved by choosing PAV-recalibration.
#'
#' @param object an object inheriting from the class \code{'reliabilitydiag'}.
#' @param ... further arguments to be passed to or from methods.
#' @param score currently only "brier" or a vectorized scoring function,
#' that is, \code{function(observation, prediction)}.
#'
#' @return
#' A \code{'summary.reliability'} object, which is also a
#' tibble (see \code{\link{tibble::tibble()}}) with columns:
#' \tabular{ll}{
#'    \code{forecast} \tab the name of the prediction method.\cr
#'    \code{mean_score} \tab the empirical average \code{score}.\cr
#'    \code{miscalibration} \tab a measure of miscalibration
#'      (\emph{how reliable is the prediction method?}),
#'       smaller is better.\cr
#'    \code{discrimination} \tab a measure of discrimination
#'      (\emph{how variable are the recalibrated predictions?}),
#'      larger is better.\cr
#'    \code{uncertainty} \tab the mean score of a constant prediction at the
#'      value of the average observation.
#'  }
#'
#' @export
summary.reliabilitydiag <- function(object, ..., score = "brier") {
  r <- object
  score <- rlang::enquo(score)
  sr <- decomposition(r, score = rlang::eval_tidy(score))
  attr(sr, "score")$name <- if (is.character(rlang::eval_tidy(score))) {
    rlang::as_name(score)
  } else {
    rlang::as_label(score)
  }
  attr(sr, "score")$fn <- rlang::eval_tidy(score) %>%
    (function(x) if (is.character(x)) "scoringFunctions::x" else x)
  class(sr) <- c("summary.reliabilitydiag", class(sr))
  sr
}


decomposition <- function(r, score = "brier") {
  stopifnot(is.reliabilitydiag(r))
  if (is.character(score) && identical(length(score), 1L)) {
    score <- get(score)
  }
  lapply(r, function(l) {
    tibble::tibble(
      mean_score = with(l$cases, mean(score(y, x))),
      uncertainty = with(l$cases, mean(score(y, mean(y)))),
      Sc = with(l, dplyr::left_join(cases, bins, by = "bin_id")) %>%
        with(., mean(score(y, CEP_pav))),
      discrimination = .data$uncertainty - .data$Sc,
      miscalibration = .data$mean_score - .data$Sc
    )
  }) %>%
    dplyr::bind_rows(.id = "forecast") %>%
    dplyr::select(.data$forecast, .data$mean_score, .data$miscalibration,
                  .data$discrimination, .data$uncertainty)
}


brier <- function(y, x) {
  (x - y)^2
}
