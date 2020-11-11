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
#' data("precip_Niamey_2016", package = "reliabilitydiag")
#'
#' X <- precip_Niamey_2016[c("EMOS", "ENS")]
#' Y <- precip_Niamey_2016$obs
#' r0 <- reliabilitydiag0(Y)
#' r1 <- c(r0, X, EPC = precip_Niamey_2016$EPC)
#' r1
#' c(r1, reliabilitydiag(Logistic = precip_Niamey_2016$Logistic, y = Y))
#'
#' @importFrom vctrs vec_as_names
#'
#' @export
c.reliabilitydiag <- function(...,
                              tol = sqrt(.Machine$double.eps),
                              xtype = NULL,
                              xvalues = NULL,
                              region.level = 0.9,
                              region.method = NULL,
                              region.position = "diagonal",
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
#' data("precip_Niamey_2016", package = "reliabilitydiag")
#'
#' r <- reliabilitydiag(
#'   precip_Niamey_2016[c("Logistic", "EMOS", "ENS", "EPC")],
#'   y = precip_Niamey_2016$obs
#' )
#' length(r)
#' r[1]
#' r["EMOS"]
#'
#' @export
`[.reliabilitydiag` <- function(x, i) {
  attribs <- attributes_without_names(x)
  class(x) <- NULL
  x <- x[i]
  attributes(x) <- c(attributes(x), attribs)
  x
}
