#' Coerce to a reliability diagram
#'
#' Coerce numeric vectors, data frames, or anything else that can be coerced
#' by \code{as.data.frame()} to a data frame of prediction values, into
#' an object inheriting from the \code{'reliabilitydiag'} class.
#'
#' @param x an \R object with probability predictions taking values in [0, 1];
#' usually a numeric vector or a list/data.frame containing numeric vectors.
#' @param y a numeric vector of binary response values in \{0, 1\} to be
#' predicted.
#' @param r an object inheriting from the class \code{'reliabilitydiag'};
#' alternative to \code{y}.
#' @param tol accuracy when comparing \code{y} in \code{'reliabilitydiag'}
#' objects.
#' @param xtype a string specifying whether the prediction values should be
#' treated as \code{"continuous"} or \code{"discrete"}.
#' @param xvalues a numeric vector of possible prediction values;
#' values in \code{x} are rounded to the nearest value in \code{xvalues} and
#' \code{xtype} is set to \code{"discrete"}.
#' @param .name_repair This argument is passed on as \code{repair} to
#' \code{\link[vctrs:vec_as_names]{vctrs::vec_as_names()}}. See there for more details.
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
#' @return
#'  \code{as.reliabilitydiag} returns a \code{'reliabilitydiag'} object.
#'
#'  \code{is.reliabilitydiag} returns \code{TRUE} if its argument is a
#'  reliability diagram, that is, has \code{"reliabilitydiag"} among its classes,
#'  and \code{FALSE} otherwise.
#'
#' @seealso \code{\link{reliabilitydiag}}
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
as.reliabilitydiag.reliabilitydiag <- function(x,
                                               y = NULL,
                                               r = NULL,
                                               tol = sqrt(.Machine$double.eps),
                                               ...) {
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
as.reliabilitydiag.default <- function(x,
                                       y = NULL,
                                       r = NULL,
                                       xtype = NULL,
                                       xvalues = NULL,
                                       .name_repair = "unique",
                                       region.level = 0.9,
                                       region.method = NULL,
                                       region.position = "diagonal",
                                       n.boot = 100,
                                       ...) {

  if (!missing(y) && !missing(r)) {
    stop("specify 'y' or 'r', but not both")
  }
  if (is.null(r)) r <- reliabilitydiag0(y)
  stopifnot(is.reliabilitydiag(r))

  x <- as.data.frame(x, optional = TRUE, fix.empty.names = FALSE) %>%
    tibble::as_tibble(.name_repair = .name_repair)

  as.reliabilitydiag.data.frame(
    x,
    r = r,
    xtype = xtype,
    xvalues = xvalues,
    .name_repair = .name_repair,
    region.level = region.level,
    region.method = region.method,
    region.position = region.position,
    n.boot = n.boot,
    ...
  )
}


#' @rdname as.reliabilitydiag
#'
#' @export
as.reliabilitydiag.data.frame <- function(x,
                                          y = NULL,
                                          r = NULL,
                                          xtype = NULL,
                                          xvalues = NULL,
                                          .name_repair = "unique",
                                          region.level = 0.9,
                                          region.method = NULL,
                                          region.position = "diagonal",
                                          n.boot = 100,
                                          ...) {

  if (!is.null(y) && !is.null(r)) {
    stop("specify 'y' or 'r', but not both")
  }
  if (is.null(r)) r <- reliabilitydiag0(y)
  stopifnot(is.reliabilitydiag(r))
  if (is.null(y)) y <- attr(r, "y")
  attribs <- attributes_without_names(r)

  stopifnot(all(sapply(x, is.numeric)))
  stopifnot(identical(nrow(x), length(y)))

  x <- tibble::as_tibble(x, .name_repair = .name_repair)

  #if (identical(length(r), 0L)) r <- list(r)
  if (!is.list(xtype)) xtype <- list(xtype)
  if (!is.list(xvalues)) xvalues <- list(xvalues)
  #if (!is.list(region.level)) region.level <- list(region.level)
  #if (!is.list(region.method)) region.method <- list(region.method)
  #if (!is.list(region.position)) region.position <- list(region.position)
  #if (!is.list(n.boot)) n.boot <- list(n.boot)

  l_args <- list(
    x = x,
    xtype = xtype,
    xvalues = xvalues
  )

  r <- purrr::pmap(
    .l = l_args,
    .f = reldiag_numeric,
    r = r,
    region.level = region.level,
    region.method = region.method,
    region.position = region.position,
    n.boot = n.boot
  ) %>%
    lapply(`names<-`, value = NULL) %>%
    unlist(recursive = FALSE)

  attributes(r) <- c(attributes(r), attribs)
  r
}


# returns an unnamed reliabilitydiag
reldiag_numeric <- function(x,
                            r = NULL,
                            xtype = NULL,
                            xvalues = NULL,
                            region.level = 0.9,
                            region.method = NULL,
                            region.position = "diagonal",
                            n.boot = 100,
                            ...) {

  stopifnot(is.reliabilitydiag(r))
  y <- attributes(r)$y

  stopifnot(identical(length(x), length(y)))
  stopifnot(isTRUE(all(x >= 0 & x <= 1)))

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

  do_region <- !anyNA(list(
    region.level, region.method, region.position), recursive = TRUE)

  if (do_region) {
    stopifnot(isTRUE(region.level > 0 & region.level < 1))
    stopifnot(isTRUE(region.position %in% c("diagonal", "estimate")))
    stopifnot(isTRUE(n.boot > 0))
    if (is.null(region.method)) {
      region.method <- detect_regionmethod(x, region.position)
    }
    stopifnot(isTRUE(
      region.method %in% c(
        "continuous_asymptotics",
        "discrete_asymptotics",
        "resampling",
        "restricted_resampling"
      )
    ))
  }

  ###
  pav_result <- stats::isoreg(x, y)
  red_iKnots <- with(
    pav_result,
    which(!duplicated(yf[iKnots], fromLast = TRUE)) %>% iKnots[.]
  )
  df_pav <- with(
    pav_result,
    tibble::tibble(
      case_id = if (isOrd) seq_len(length(y)) else ord,
      x = if (isOrd) x else x[ord],
      y = if (isOrd) y else y[ord],
      bin_id = rep.int(seq_along(red_iKnots), times = diff(c(0, red_iKnots))),
      CEP_pav = yf
    )
  )
  df_bins <- tibble::tibble(
    bin_id = seq_along(red_iKnots),
    n = diff(c(0, red_iKnots)),
    x_min = df_pav$x[c(0, utils::head(red_iKnots,-1)) + 1],
    x_max = df_pav$x[red_iKnots],
    CEP_pav = df_pav$CEP_pav[red_iKnots]
  )

  regions <- if (!do_region) {
    tibble::tibble(
      x = numeric(0),
      lower = numeric(0),
      upper = numeric(0),
      n = integer(0),
      method = character(0),
      level = numeric(0),
      position = character(0)
    )
  } else {
    region_method <- get(region.method)
    region_method(df_pav, df_bins, region.level, region.position, n.boot)
  }

  # Outputs
  x <- list(
    cases = df_pav,
    bins = df_bins,
    regions = regions,
    xinfo = list(type = xtype, values = xvalues)
  ) %>%
    list()
  names(x) <- ""
  attributes(x) <- c(attributes(x), attributes_without_names(r))
  x
}
