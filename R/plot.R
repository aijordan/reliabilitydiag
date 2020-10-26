#' Plotting reliability diagram objects
#'
#' @param x an object inheriting from the class \code{'reliabilitydiag'}.
#' @param object an object inheriting from the class \code{'reliabilitydiag'}.
#' @param colour a colour to be used in the CEP layers.
#' @param params_histogram a list of arguments for
#' \code{ggplot2::geom_histogram}; ignored if \code{length(x) > 1}.
#' @param params_ribbon a list of arguments for \code{ggplot2::geom_ribbon};
#' ignored if \code{length(x) > 1}.
#' @param params_diagonal a list of arguments for \code{ggplot2::geom_line}.
#' @param params_CEPline a list of arguments for \code{ggplot2::geom_line}.
#' @param params_CEPsegment a list of arguments for
#' \code{ggplot2::geom_segment}; only used for \code{"continuous"} predictions
#' and ignored if \code{length(x) > 1}.
#' @param params_CEPpoint a list of arguments for \code{ggplot2::geom_point};
#' only used for \code{"discrete"} predictions and ignored if
#' \code{length(x) > 1}.
#' @param ... further arguments to be passed to or from methods.
#'
#' @name plot.reliabilitydiag
NULL

#' @rdname plot.reliabilitydiag
#'
#' @export
plot.reliabilitydiag <- function(x, ...) {
  autoplot(x, ...)
}

#' @rdname plot.reliabilitydiag
#' @importFrom ggplot2 autoplot
#'
#' @export
autoplot.reliabilitydiag <- function(
  object, ...,
  colour = "red",
  params_histogram = list(yscale = 0.2, colour = "black", fill = NA),
  params_ribbon = list(fill = "blue", alpha = 0.15),
  params_diagonal = list(size = 0.3),
  params_CEPline = list(size = 0.2, colour = colour),
  params_CEPsegment = list(size = 2, colour = colour),
  params_CEPpoint = list(size = 2, colour = colour, shape = 16)) {

  p.reldiag <- ggplot2::ggplot() +
    ggplot2::xlab("Forecast value") +
    ggplot2::ylab("CEP") +
    ggplot2::theme_bw()

  r <- object

  if (identical(length(r), 1L) && !isTRUE(is.na(params_histogram))) {
    ### Add a Histogram
    # possible 'breaks' specifications in ggplot2::geom_histogram
    breaks_specs <- c("breaks", "binwidth", "bins", "center", "boundary")
    if (!any(breaks_specs %in% names(params_histogram))) {
      # see utils.R
      params_histogram$breaks <- choose_breaks(
        x = r[[1L]]$cases$x,
        xtype = r[[1L]]$xinfo$type
      )
    }
    if (i <- "yscale" %in% names(params_histogram)) {
      yscale <- params_histogram[[which(i)]]
      params_histogram <- params_histogram[-which(i)]
    } else {
      yscale <- 0.2
    }
    p.reldiag <- p.reldiag +
      do.call(
        what = ggplot2::geom_histogram,
        args = c(
          list(data = r[[1L]]$cases),
          list(mapping = ggplot2::aes(
            x = .data$x, y = yscale * ggplot2::after_stat(.data$ncount))),
          params_histogram
        ))
  }

  if (identical(length(r), 1L) && !isTRUE(is.na(params_ribbon))) {
    ### Add ribbon for consistency/confidence regions
    p.reldiag <- p.reldiag +
      do.call(
        what = ggplot2::geom_ribbon,
        args = c(
          list(data = r[[1L]]$regions),
          list(mapping = ggplot2::aes(
            x = .data$x, ymin = .data$lower, ymax = .data$upper)),
          params_ribbon
        ))
  }

  if (!isTRUE(is.na(params_diagonal))) {
    ### Add the diagonal line
    p.reldiag <- p.reldiag +
      do.call(
        what = ggplot2::geom_segment,
        args = c(
          list(data = data.frame(1)),
          list(mapping = ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1)),
          params_diagonal
        ))
  }

  if (!isTRUE(is.na(params_CEPline))) {
    ### Plot the estimated CEP
    if (identical(length(r), 1L)) {
      p.reldiag <- p.reldiag +
        do.call(
          what = ggplot2::geom_line,
          args = c(
            list(data = tidyr::pivot_longer(
              r[[1L]]$bins,
              cols = dplyr::all_of(c("x_min", "x_max")),
              values_to = "x")),
            list(mapping = ggplot2::aes(x = .data$x, y = .data$CEP_pav)),
            params_CEPline
          ))
    } else if (length(r) > 1L) {
      if (missing(params_CEPline)) params_CEPline <- params_CEPline["size"]
      p.reldiag <- p.reldiag +
        do.call(
          what = ggplot2::geom_line,
          args = c(
            list(data = lapply(r, function(l) {
              tidyr::pivot_longer(
                l$bins,
                cols = dplyr::all_of(c("x_min", "x_max")),
                values_to = "x")
            }) %>%
              dplyr::bind_rows(.id = "Forecast")),
            list(mapping = ggplot2::aes(
              x = .data$x, y = .data$CEP_pav, col = .data$Forecast)),
            params_CEPline
          )
        )
    }
  }

  if (identical(length(r), 1L)) {
    if (identical(r[[1L]]$xinfo$type, "continuous") &&
        !isTRUE(is.na(params_CEPsegment))) {
      ### Add thick line for continuous forecasts
      p.reldiag <- p.reldiag +
        do.call(
          what = ggplot2::geom_segment,
          args = c(
            list(data = r[[1L]]$bins),
            list(mapping = ggplot2::aes(
              x = .data$x_min, xend = .data$x_max,
              y = .data$CEP_pav, yend = .data$CEP_pav)),
            params_CEPsegment
          ))
    } else if (identical(r[[1L]]$xinfo$type, "discrete") &&
               !isTRUE(is.na(params_CEPpoint))) {
      ### Add points for discrete forecasts
      p.reldiag <- p.reldiag +
        do.call(
          what = ggplot2::geom_point,
          args = c(
            list(data = r[[1L]]$cases %>%
              dplyr::distinct(.data$x, .data$bin_id) %>%
              dplyr::left_join(r[[1L]]$bins, by = "bin_id")),
            list(mapping = ggplot2::aes(x = .data$x, y = .data$CEP_pav)),
            params_CEPpoint
          ))
    }
  }
  plot(p.reldiag)
  invisible(p.reldiag)
}
