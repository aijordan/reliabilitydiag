#' Plotting reliability diagram objects
#'
#' @param x an object inheriting from the class \code{'reliabilitydiag'}.
#' @param object an object inheriting from the class \code{'reliabilitydiag'}.
#' @param type one of \code{"miscalibration"}, \code{"discrimination"};
#' determines which
#' layers are added by default, including default parameter values.
#' @param colour a colour to be used to draw focus;
#' used for the CEP layers when \code{type} is \code{"miscalibration"},
#' and for the horizontal segment layer and CEP margin histogram
#' when \code{type} is \code{"discrimination"}.
#' @param params_histogram a list of arguments for
#' \code{ggplot2::geom_histogram}.
#' @param params_ggMarginal a list of arguments for \code{ggExtra::ggMarginal}.
#' @param params_ribbon a list of arguments for \code{ggplot2::geom_ribbon}.
#' @param params_diagonal a list of arguments for \code{ggplot2::geom_line}.
#' @param params_vsegment a list of arguments for \code{ggplot2::geom_segment}.
#' @param params_hsegment a list of arguments for \code{ggplot2::geom_segment}.
#' @param params_CEPline a list of arguments for \code{ggplot2::geom_line}.
#' @param params_CEPsegment a list of arguments for
#' \code{ggplot2::geom_segment}.
#' @param params_CEPpoint a list of arguments for \code{ggplot2::geom_point}.
#' @param ... further arguments to be passed to or from methods.
#'
#' @name plot.reliabilitydiag
NULL

#' @rdname plot.reliabilitydiag
#'
#' @export
plot.reliabilitydiag <- function(x, ...) {
  p <- autoplot(x, ...)
  print(p)
}

#' @rdname plot.reliabilitydiag
#' @importFrom ggplot2 autoplot
#' @importFrom ggExtra ggMarginal
#'
#' @export
autoplot.reliabilitydiag <- function(
  object, ...,
  type = c("miscalibration", "discrimination"),
  colour = "red",
  params_histogram = NULL,
  params_ggMarginal = NULL,
  params_ribbon = NULL,
  params_diagonal = NULL,
  params_vsegment = NULL,
  params_hsegment = NULL,
  params_CEPline = NULL,
  params_CEPsegment = NULL,
  params_CEPpoint = NULL
) {
  r <- object
  type <- match.arg(type)
  if (identical(type, "discrimination") && length(r) > 1L) {
    stop("'discrimination' diagrams cannot display multiple forecast methods.")
  }

  # loading default values
  if (is.null(params_histogram)) {
    params_histogram <- switch(
      type,
      miscalibration = if (identical(length(r), 1L)) {
        list(yscale = 0.2, colour = "black", fill = NA)
      } else {
        NA
      },
      discrimination = NA)
  }
  if (is.null(params_ggMarginal)) {
    params_ggMarginal <- switch(
      type,
      miscalibration = NA,
      discrimination = list(
        type = "histogram",
        xparams = list(bins = 100, fill = "grey"),
        yparams = list(bins = 100, fill = colour)))
  }
  if (is.null(params_ribbon)) {
    params_ribbon <- switch(
      type,
      miscalibration = if (identical(length(r), 1L)) {
        list(fill = "blue", alpha = 0.15)
      } else {
        NA
      },
      discrimination = NA)
  }
  if (is.null(params_diagonal)) {
    params_diagonal <- switch(
      type,
      miscalibration = list(size = 0.3, colour = "black"),
      discrimination = list(size = 0.3, colour = "lightgrey"))
  }
  if (is.null(params_vsegment)) {
    params_vsegment <- switch(
      type,
      miscalibration = NA,
      discrimination = list(size = 1.5, colour = "grey"))
  }
  if (is.null(params_hsegment)) {
    params_hsegment <- switch(
      type,
      miscalibration = NA,
      discrimination = list(size = 1.5, colour = colour))
  }
  if (is.null(params_CEPline)) {
    params_CEPline <- switch(
      type,
      miscalibration = if (identical(length(r), 1L)) {
        list(size = 0.2, colour = colour)
      } else {
        list(size = 0.2)
      },
      discrimination = list(size = 0.2, colour = "black"))
  }
  if (is.null(params_CEPsegment)) {
    params_CEPsegment <- switch(
      type,
      miscalibration = if (identical(length(r), 1L) &&
                           identical(r[[1L]]$xinfo$type, "continuous")) {
        list(size = 2, colour = colour)
      } else {
        NA
      },
      discrimination = NA)
  }
  if (is.null(params_CEPpoint)) {
    params_CEPpoint <- switch(
      type,
      miscalibration = if (!isTRUE(is.na(params_ggMarginal))) {
        # ggExtra::ggMarginal requires a scatter plot
        list(colour = colour)
      } else if (identical(length(r), 1L) &&
                 identical(r[[1L]]$xinfo$type, "discrete")) {
        list(size = 2, colour = colour)
      } else {
        NA
      },
      discrimination = list(colour = "black"))
  }

  # initialize plot object
  p.reldiag <- ggplot2::ggplot() +
    ggplot2::xlab("Forecast value") +
    ggplot2::ylab("CEP") +
    ggplot2::theme_bw() +
    ggplot2::theme(aspect.ratio = 1)

  # add layers
  if (!isTRUE(is.na(params_histogram))) {
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
  if (!isTRUE(is.na(params_ribbon))) {
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
  if (!isTRUE(is.na(params_vsegment))) {
    p.reldiag <- p.reldiag +
      do.call(
        what = ggplot2::geom_segment,
        args = c(
          list(data = r[[1L]]$cases),
          list(mapping = ggplot2::aes(
            x = mean(.data$x),
            xend = mean(.data$x),
            y = min(.data$CEP_pav),
            yend = max(.data$CEP_pav)
          )),
          params_vsegment))
  }
  if (!isTRUE(is.na(params_hsegment))) {
    p.reldiag <- p.reldiag +
      do.call(
        what = ggplot2::geom_segment,
        args = c(
          list(data = r[[1L]]$cases),
          list(mapping = ggplot2::aes(
            x = min(.data$x),
            xend = max(.data$x),
            y = mean(.data$y),
            yend = mean(.data$y)
          )),
          params_hsegment))
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
            params_CEPline))
    } else if (length(r) > 1L) {
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
            params_CEPline))
    }
  }
  if (!isTRUE(is.na(params_CEPsegment))) {
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
  }
  if (!isTRUE(is.na(params_CEPpoint))) {
    p.reldiag <- p.reldiag +
      do.call(
        what = ggplot2::geom_point,
        args = c(
          list(data = if (identical(type, "miscalibration") &&
                          isTRUE(is.na(params_ggMarginal))) {
            r[[1L]]$cases %>%
              dplyr::distinct(.data$x, .data$bin_id) %>%
              dplyr::left_join(r[[1L]]$bins, by = "bin_id")
          } else {
            r[[1L]]$cases
          }),
          list(mapping = ggplot2::aes(x = .data$x, y = .data$CEP_pav)),
          params_CEPpoint))
  }

  # return plot object
  if (!isTRUE(is.na(params_ggMarginal))) {
    do.call(
      what = ggExtra::ggMarginal,
      args = c(
        list(p = p.reldiag),
        params_ggMarginal
      ))
  } else {
    p.reldiag
  }
}
