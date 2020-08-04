#' Coerce to a reliability diagram
#'
#' generic function with
#' a \code{default} method.
#'
#' In the default version, the user specifies all relevant information
#' (forecasts, realizations, information on the type of forecast) manually.
#'
#' @param x an \R object with probability predictions taking values in [0, 1]; usually
#' a numeric vector or a list/data.frame containing numeric vectors.
#' @param y a numeric vector of binary response values in \{0, 1\} to be predicted.
#' @param r an object inheriting from the class \code{'reliabilitydiag'};
#' alternative to \code{y}.
#' @param xnames a character vector of prediction names.
#' @param tol accuracy when comparing \code{y} in \code{'reliabilitydiag'}
#' objects.
#' @param xtype a string specifying whether the prediction values should be
#' treated as \code{"continuous"} or \code{"discrete"}.
#' @param xvalues a numeric vector of possible prediction values;
#' values in \code{x} are rounded to the nearest value in \code{xvalues} and
#' \code{xtype} is set to \code{"discrete"}.
#' @param region.level a value in (0, 1) specifying the level at which
#' consistency or confidence regions are calculated.
#' @param region.method a string specifying whether \code{"resampling"},
#' \code{"continuous_asymptotics"}, or \code{"discrete_asymptotics"} are used
#' to calculate consistency/confidence regions.
#' @param region.position a string specifying whether consistency regions
#' around the \code{"diagonal"} or confidence regions around the
#' \code{"estimate"} are calculated.
#' @param n.boot the number of bootstrap samples when
#' \code{region.method == "resampling"}.
#' @param ... further arguments to be passed to or from methods.
#'
#' @details
#' some details
#'
#' @return
#'  \code{as.reliabilitydiag} returns a \code{'reliabilitydiag'} object.
#'
#'  \code{is.reliabilitydiag} returns \code{TRUE} if its argument is a
#'  reliability diagram, that is, has \code{"reliabilitydiag"} among its classes,
#'  and \code{FALSE} otherwise.
#'
#' @name as.reliabilitydiag
NULL

#' @rdname as.reliabilitydiag
#'
#' @export
as.reliabilitydiag <- function(x, ...) {
  UseMethod("as.reliabilitydiag")
}


#' @rdname as.reliabilitydiag
#'
#' @export
is.reliabilitydiag <- function(x) {
  inherits(x, "reliabilitydiag")
}


#' @rdname as.reliabilitydiag
#'
#' @export
as.reliabilitydiag.reliabilitydiag <- function(x, y = NULL, r = NULL,
  tol = sqrt(.Machine$double.eps), ...) {

  if (!is.null(y) && !is.null(r)) {
    stop("specify 'y' or 'r', but not both")
  }
  if (is.null(r)) r <- reliabilitydiag0(y)
  stopifnot(is.reliabilitydiag(r))

  ref <- attributes_without_names(r)
  stopifnot(isTRUE(all.equal(attributes(x)$y, ref$y, tolerance = tol)))
  x
}

#' @rdname as.reliabilitydiag
#'
#' @export
as.reliabilitydiag.default <- function(x, y = NULL, r = NULL,
  xtype = NULL, xvalues = NULL, xnames = NULL,
  region.level = 0.9, region.method = NULL, region.position = "diagonal",
  n.boot = 100, ...) {

  if (!missing(y) && !missing(r)) {
    stop("specify 'y' or 'r', but not both")
  }
  if (is.null(r)) r <- reliabilitydiag0(y)
  stopifnot(is.reliabilitydiag(r))

  x <- as.data.frame(x, optional = TRUE, fix.empty.names = FALSE)

  as.reliabilitydiag(
    x, r = r,
    xtype = xtype, xvalues = xvalues, xnames = xnames,
    region.level = region.level, region.method = region.method, region.position = region.position,
    n.boot = n.boot, ...)
}

#' @rdname as.reliabilitydiag
#'
#' @export
as.reliabilitydiag.data.frame <- function(x, y = NULL, r = NULL,
  xtype = NULL, xvalues = NULL, xnames = NULL,
  region.level = 0.9, region.method = NULL, region.position = "diagonal",
  n.boot = 100, ...) {

  if (!is.null(y) && !is.null(r)) {
    stop("specify 'y' or 'r', but not both")
  }
  if (is.null(r)) r <- reliabilitydiag0(y)
  stopifnot(is.reliabilitydiag(r))
  if (is.null(y)) y <- attr(r, "y")
  attribs <- attributes_without_names(r)

  stopifnot(all(sapply(x, is.numeric)))
  stopifnot(identical(nrow(x), length(y)))

  if (!is.null(xnames)) names(x) <- xnames
  if (is.null(names(x))) names(x) <- paste0("X", seq_along(x))
  if (any(empty <- !nzchar(names(x)))) {
    names(x)[empty] <- paste0("X", seq_along(x))[empty]
  }
  if (any(duplicated(names(x)))) {
    prefix <- paste0("X", seq_along(names(x)), "_")
    prefix[empty] <- ""
    names(x) <- paste0(prefix, names(x))
  }

  if (identical(length(r), 0L)) r <- list(r)
  if (!is.list(xtype)) xtype <- list(xtype)
  if (!is.list(xvalues)) xvalues <- list(xvalues)
  if (!is.list(xnames)) xnames <- list(xnames)
  if (!is.list(region.level)) region.level <- list(region.level)
  if (!is.list(region.method)) region.method <- list(region.method)
  if (!is.list(region.position)) region.position <- list(region.position)
  if (!is.list(n.boot)) n.boot <- list(n.boot)

  r <- purrr::pmap(
    list(x = x, r = r,
      xtype = xtype, xvalues = xvalues,
      region.level = region.level, region.method = region.method,
      region.position = region.position, n.boot = n.boot, ...),
    as.reliabilitydiag
  ) %>%
    lapply(`names<-`, value = NULL) %>%
    unlist(recursive = FALSE)

  attributes(r) <- c(attributes(r), attribs)
  r
}

#' @rdname as.reliabilitydiag
#'
#' @export
as.reliabilitydiag.numeric <- function(x, y = NULL, r = NULL,
  xtype = NULL, xvalues = NULL, xnames = NULL,
  region.level = 0.9, region.method = NULL, region.position = "diagonal",
  n.boot = 100, ...) {

  if (!is.null(y) && !is.null(r)) {
    stop("specify 'y' or 'r', but not both")
  }
  if (is.null(r)) r <- reliabilitydiag0(y)
  stopifnot(is.reliabilitydiag(r))
  if (is.null(y)) y <- attributes(r)$y

  stopifnot(identical(length(x), length(y)))
  stopifnot(isTRUE(all(x >= 0 | x <= 1)))

  if (is.null(xtype) && is.null(xvalues)) {
    xtype <- detect_xtype(x)
  } else if (!is.null(xvalues)) {
    stopifnot(is.numeric(xvalues))
    if (!is.null(xtype)) stopifnot(identical(xtype, "discrete"))
    xtype <- "discrete"
    x <- snap(x, xvalues)
  } else {
    stopifnot(isTRUE(xtype %in% c("continuous", "discrete")))
  }

  stopifnot(is.na(region.level) || isTRUE(region.level > 0 & region.level < 1))
  stopifnot(isTRUE(region.position %in% c("diagonal", "estimate")))
  stopifnot(isTRUE(n.boot > 0))

  if (is.null(region.method)) {
    region.method <- detect_regionmethod(x, region.position)
  } else {
    stopifnot(isTRUE(
      region.method %in% c("continuous_asymptotics", "discrete_asymptotics",
                         "resampling", "restricted_resampling")
    ))
  }

  ###
  pav_result <- stats::isoreg(x, y)
  df_pav <- with(
    pav_result,
    tibble::tibble(
      x = if (isOrd) x else x[ord],
      y = if (isOrd) y else y[ord],
      CEP_pav = yf,
      bin_id = iKnots[!duplicated(yf[iKnots], fromLast = TRUE)] %>%
        (function(k) seq_along(k) %>% rep.int(times = diff(c(0, k))))
    )
  )
  df_bins <- with(
    pav_result,
    tibble::tibble(
      knots = iKnots[!duplicated(yf[iKnots], fromLast = TRUE)],
      bin_id = seq_along(.data$knots),
      n = diff(c(0, .data$knots)),
      x_min = df_pav$x[c(0, utils::head(.data$knots, -1)) + 1],
      x_max = df_pav$x[.data$knots],
      CEP_pav = df_pav$CEP_pav[.data$knots]
    )
  ) %>%
    dplyr::select(-.data$knots)

  regions <- if (is.na(region.level)) {
    tibble::tibble(
      x = NA, lower = NA, upper = NA,
      n = NA, level = NA, method = NA, position = NA
    )
  } else {
    region_method <- get(region.method)
    region_method(df_pav, df_bins, region.level, region.position, n.boot)
  }

  # Outputs
  x <- list(
    cases = dplyr::select(df_pav, .data$x, .data$y, .data$bin_id),
    bins = df_bins,
    regions = regions,
    xinfo = list(type = xtype, values = xvalues)
  ) %>%
    list()
  names(x) <- if (is.null(xnames)) "X" else xnames
  attributes(x) <- c(attributes(x), attributes_without_names(r))
  x
}
