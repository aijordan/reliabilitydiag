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
#' set.seed(42)
#'
#' X <- runif(100)
#' Y <- rbinom(100, 1, X)
#' r0 <- reliabilitydiag0(Y)
#' c(r0, X, X2 = runif(100))
#' c(r0, reliabilitydiag(X, y = Y))
#'
#' @importFrom vctrs vec_as_names
#'
#' @export
c.reliabilitydiag <- function(
  ..., tol = sqrt(.Machine$double.eps),
  xtype = NULL, xvalues = NULL,
  region.level = 0.9, region.method = NULL, region.position = "diagonal",
  n.boot = 100) {

  input <- list(...)
  proto <- input[[1L]]
  attribs <- attributes_without_names(proto)

  if (!is.list(xtype)) xtype <- list(xtype)
  if (!is.list(xvalues)) xvalues <- list(xvalues)

  r <- list(
    x = input,
    xtype = xtype,
    xvalues = xvalues
  ) %>%
    purrr::pmap(
      .f = as.reliabilitydiag,
      r = proto,
      .name_repair = "minimal",
      tol = tol,
      region.level = region.level,
      region.method = region.method,
      region.position = region.position,
      n.boot = n.boot)
  r <- unlist(r, recursive = FALSE)
  names(r) <- vctrs::vec_as_names(names(r), repair = "unique")
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
#' set.seed(42)
#'
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
