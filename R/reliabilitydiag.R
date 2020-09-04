#' Reliability diagram object
#'
#' Documentation of the \code{'reliabilitydiag'} object, and its constructors.
#'
#' \code{reliabilitydiag} constructs and returns an object inheriting from the
#' class \code{'reliabilitydiag'}. It calls the
#' concatenation function described in \code{\link{c.reliabilitydiag}}, which
#' in turn calls the
#' coercion methods described in \code{\link{as.reliabilitydiag}}.
#'
#' \code{reliabilitydiag0} constructs an empty \code{'reliabilitydiag'} object
#' from the response values.
#'
#' @param ... objects to be coerced to \code{'reliabilitydiag'} and concatenated
#' @inheritParams as.reliabilitydiag
#'
#' @return
#'  \code{reliabilitydiag} returns a \code{'reliabilitydiag'} object,
#'  which is a named list-type vector class with attribute
#'  \tabular{ll}{
#'    \code{y} \tab a numeric vector of response values to be predicted.
#'  }
#'  Each entry of a \code{'reliabilitydiag'} object is a list
#'  with the following components:
#'  \tabular{ll}{
#'    \code{cases} \tab a \code{'tibble'}.\cr
#'    \code{bins} \tab a \code{'tibble'}.\cr
#'    \code{regions} \tab a \code{'tibble'}.\cr
#'    \code{xinfo} \tab a \code{'list'}.
#'  }
#'
#'  \code{reliabilitydiag0} returns an empty \code{'reliabilitydiag'} object
#'  with attribute \code{y}.
#'
#' @seealso
#'  \code{\link{c.reliabilitydiag}},
#'  \code{\link{[.reliabilitydiag}},
#'  \code{\link{plot.reliabilitydiag}}.
#'
#' @examples
#' set.seed(42)
#'
#' X <- runif(100)
#' Y <- rbinom(100, 1, X)
#' r <- reliabilitydiag(X, y = Y)
#' r
#'
#' r0 <- reliabilitydiag0(Y)
#' identical(r, reliabilitydiag(X, r = r0))
#'
#' X2 <- runif(100)
#' r1 <- reliabilitydiag(X2, r = r)
#' r2 <- reliabilitydiag(X2, r = r0)
#' identical(r1, r2)
#'
#' @name reliabilitydiag
NULL

#' @rdname reliabilitydiag
#'
#' @export
reliabilitydiag <- function(
  ..., y = NULL, r = NULL, tol = sqrt(.Machine$double.eps),
  xtype = NULL, xvalues = NULL, xnames = NULL,
  region.level = 0.9, region.method = NULL, region.position = "diagonal",
  n.boot = 100) {

  if (!is.null(y) && !is.null(r)) {
    stop("specify 'y' or 'r', but not both")
  }
  if (is.null(r)) r <- reliabilitydiag0(y)
  stopifnot(is.reliabilitydiag(r))

  c(r[NULL], ..., tol = tol,
    xtype = xtype, xvalues = xvalues, xnames = xnames,
    region.level = region.level, region.method = region.method,
    region.position = region.position, n.boot = n.boot)
}

#' @rdname reliabilitydiag
#'
#' @export
reliabilitydiag0 <- function(y) {
  stopifnot(length(y) > 0L)
  stopifnot(is.numeric(y))
  stopifnot(isTRUE(all(y == 0 | y == 1)))

  r <- structure(list(), names = character(0))
  attr(r, "y") <- y
  class(r) <- "reliabilitydiag"
  r
}
