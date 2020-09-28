#' Printing reliability diagram objects
#'
#' @param x an object inheriting from the class \code{'reliabilitydiag'}.
#' @param ... further arguments to be passed to or from methods.
#'
#' @name print.reliabilitydiag
NULL


#' @rdname print.reliabilitydiag
#' @importFrom ggplot2 autoplot
#'
#' @export
print.reliabilitydiag <- function(x, ...) {
  print(autoplot(x, ...))
  print(summary(x, ...))
  invisible(x)
}

#' @rdname print.reliabilitydiag
#'
#' @export
print.summary.reliabilitydiag <- function(x, ...) {
  if (is.character(x)) {
    cat(x)
    return(invisible(x))
  }
  xx <- x
  class(xx) <- class(x)[-1]
  cat(sprintf("'%s' score decomposition (see also ?summary.reliabilitydiag)\n",
              attr(x, "score")$name))
  print(xx)
  invisible(x)
}
