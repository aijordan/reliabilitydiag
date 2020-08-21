#' Miscalibration Test
#'
#' @param x an \R object inheriting from \code{'reliabilitydiag'} or a numeric
#' vector of probability predictions taking values in [0, 1].
#' @inheritParams as.reliabilitydiag
#'
#' @return
#' returns a \code{'tibble'} with entries
#' \tabular{ll}{
#'    \code{forecast} \tab the name of the prediction method. \cr
#'    \code{miscalibration} \tab the miscalibration statistic
#'    (see \code{\link{summary.reliabilitydiag}}). \cr
#'    \code{pvalue} \tab the pvalue.
#'  }
#'
#' @name miscalibration_test
NULL

#' @rdname miscalibration_test
#'
#' @export
miscalibration_test <- function(x, ...) {
  UseMethod("miscalibration_test")
}

#' @rdname miscalibration_test
#'
#' @export
miscalibration_test.reliabilitydiag <- function(x, ...) {
  statistic <- summary(x) %>%
    dplyr::select(.data$forecast, .data$miscalibration)
  class(statistic) <- class(statistic)[-1L]
  null_distribution <- lapply(x, function(X) {
    mcb_resampling(X$cases$x, 100)
  })
  statistic %>%
    dplyr::mutate(pvalue = purrr::map2_dbl(
      .x = null_distribution,
      .y = .data$miscalibration,
      .f = function(.x, .y) mean(.x >= .y)
    ))
}

#' @rdname miscalibration_test
#'
#' @export
miscalibration_test.numeric <- function(x, y, ...) {
  reliabilitydiag(x, y = y, region.level = NA) %>%
    miscalibration_test.reliabilitydiag(...)
}

mcb_resampling <- function(x, n) {
  replicate(n, {
    x <- sample(x, size = length(x), replace = TRUE)
    y <- stats::rbinom(length(x), size = 1, prob = x)
    reliabilitydiag(x, y = y, region.level = NA) %>%
      summary.reliabilitydiag() %>%
      dplyr::pull(.data$miscalibration)
  })
}
