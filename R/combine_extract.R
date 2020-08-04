#' Combining reliability diagram objects
#'
#' Combine two or more \code{'reliabilitydiag'} objects that are based on the
#' same observations. Other objects are coerced by \code{\link{as.reliabilitydiag}}
#' before combination.
#'
#'
#' @param ... objects to be concatenated.
#' @inheritParams as.reliabilitydiag
#'
#' @return an object inheriting from the class \code{'reliabilitydiag'}.
#'
#' @seealso \code{\link{as.reliabilitydiag}}, \code{\link{[.reliabilitydiag}}.
#'
#' @examples
#' X <- runif(100)
#' Y <- rbinom(100, 1, X)
#' r0 <- reliabilitydiag0(Y)
#' c(r0, X, X2 = runif(100))
#' c(r0, reliabilitydiag(X, y = Y))
#'
#' @export
c.reliabilitydiag <- function(
  ..., tol = sqrt(.Machine$double.eps),
  xtype = NULL, xvalues = NULL, xnames = NULL,
  region.level = 0.9, region.method = NULL, region.position = "diagonal",
  n.boot = 100) {

  input <- list(...)
  attribs <- attributes_without_names(input[[1L]])

  if (!is.list(xtype)) xtype <- list(xtype)
  if (!is.list(xvalues)) xvalues <- list(xvalues)
  if (!is.list(xnames)) xnames <- list(xnames)
  if (!is.list(region.level)) region.level <- list(region.level)
  if (!is.list(region.method)) region.method <- list(region.method)
  if (!is.list(region.position)) region.position <- list(region.position)
  if (!is.list(n.boot)) n.boot <- list(n.boot)

  r <- list(
    x = input, xtype = xtype, xvalues = xvalues, xnames = xnames,
    region.level = region.level, region.method = region.method,
    region.position = region.position, n.boot = n.boot
  ) %>%
    purrr::pmap(as.reliabilitydiag, r = input[[1L]], tol = tol)
  r_lens <- lengths(r, use.names = FALSE)
  r <- unlist(r, recursive = FALSE)
  r_lens <- r_lens[r_lens > 0L]
  if (any(duplicated(names(r)))) {
    names(r) <- paste0("I", rep.int(seq_along(r_lens), r_lens), "_", names(r))
  }

  attributes(r) <- c(attributes(r), attribs)
  r
}


#' Subsetting reliability diagram objects
#'
#' @param x an object inheriting from the class \code{'reliabilitydiag'}.
#' @param i index specifying which elements to extract.
#'
#' @return an object inheriting from the class \code{'reliabilitydiag'}.
#'
#' @seealso \code{\link{c.reliabilitydiag}}.
#'
#' @examples
#' X1 <- runif(100)
#' X2 <- runif(100)
#' Y <- rbinom(100, 1, X1)
#' r <- reliabilitydiag(X1, X2, y = Y)
#' length(r)
#' r[1]
#'
#' @export
`[.reliabilitydiag` <- function(x, i) {
  attribs <- attributes_without_names(x)
  class(x) <- NULL
  x <- x[i]
  attributes(x) <- c(attributes(x), attribs)
  x
}
