#' Summary of reliability diagram objects
#'
#' @param object an object inheriting from the class \code{'reliabilitydiag'}.
#' @param ... further arguments to be passed to or from methods.
#' @param score currently only "brier"
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
