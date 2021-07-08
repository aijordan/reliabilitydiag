#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
NULL

globalVariables(".")

attributes_without_names <- function(r) {
  attributes(r)[names(attributes(r)) != "names"]
}

region_at <- function(regions, t) { # unused at the moment
  with(
    regions,
    tibble::tibble(
      lower = approx(c(0, x, 1), c(0, lower, 1), xout = t)$y,
      upper = approx(c(0, x, 1), c(0, upper, 1), xout = t)$y,
      x = t
    ) %>%
      dplyr::select(x, lower, upper)
  )
}

bound_correction <- function(bound, x, CEP_est, position) {
  switch(
    position,
    diagonal = bound,
    estimate = ifelse(CEP_est %in% c(0, 1) & !(x %in% range(x)), NA, bound) %>%
      stats::approx(x, y = ., xout = x) %>%
      .$y
  )
}

choose_breaks <- function(x, xtype) {
  switch(
    xtype,
    continuous = breaks_fd(x),
    discrete = breaks_discrete(x)
  )
}

breaks_fd <- function(x) {
  # Use the "Freedman–Diaconis" binning rule
  iqr <- stats::IQR(x)
  if (iqr < sqrt(.Machine$double.eps)) {
    # in particular, this covers the case where >75% of forecast values are 0
    # e.g., probability of precipitation forecasts
    # maybe that shouldn't be detected as "continuous" in the first place
    binwidth <- 1 / 400
  } else {
    binwidth <- 2 * iqr / length(x)^(1 / 3)
  }
  xrange <- range(x)
  round(diff(xrange) / binwidth) %>%
    max(5) %>%
    (function(n) seq(xrange[1L], xrange[2L], length.out = n + 1))
}

width_fd <- function(x) {
  min_binwidth <- 1 / 400 # no more than 400 bins on [0, 1]
  iqr <- stats::IQR(x)
  if (iqr < sqrt(.Machine$double.eps)) {
    # in particular, this covers the case where >75% of forecast values are 0
    # e.g., probability of precipitation forecasts
    return(min_binwidth)
  }
  # Use the "Freedman–Diaconis" binning rule
  binwidth <- 2 * iqr / length(x)^(1 / 3)
  binwidth <- min(binwidth, diff(range(x)) / 5) # at least 5 bins on support
  binwidth <- max(binwidth, min_binwidth)
  1 / round(1 / binwidth) # improve alignment with the bounds of [0, 1]
}

breaks_discrete <- function(x) {
  x_unique <- sort(unique(x))
  eps <- min(diff(x_unique) / 8, 0.02)
  rep(x_unique, each = 2) + c(-eps, eps)
}

detect_xtype <- function(x) {
  x_unique <- sort(unique(x))
  if (identical(length(x_unique), 1L)) {
    return("discrete")
  }
  if (isTRUE(min(diff(x_unique)) >= 0.01)) {
    return("discrete")
  }
  "continuous"
}

detect_regionmethod <- function(x, region.position) {
  if (region.position == "estimate") {
    return("resampling")
  }

  n <- length(x)
  n_unique <- length(unique(x))
  if (isTRUE(n <= max(1000L, min(5000L, 50L * n_unique)))) {
    return("resampling")
  }
  sprintf(
    "%s_asymptotics",
    ifelse(isTRUE(n >= 8L * n_unique^2), "discrete", "continuous")
  )
}

qchern <- approxfun(
  x = c(0.50, 0.51, 0.52, 0.53, 0.54, 0.55, 0.56, 0.57, 0.58, 0.59,
        0.60, 0.61, 0.62, 0.63, 0.64, 0.65, 0.66, 0.67, 0.68, 0.69,
        0.70, 0.71, 0.72, 0.73, 0.74, 0.75, 0.76, 0.77, 0.78, 0.79,
        0.80, 0.81, 0.82, 0.83, 0.84, 0.85, 0.86, 0.87, 0.88, 0.89,
        0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99,
        0.991, 0.992, 0.993, 0.994, 0.995, 0.996, 0.997, 0.998, 0.999, 0.9999),
  y = c(0.000000, 0.013187, 0.026383, 0.039595, 0.052830,
        0.066096, 0.079402, 0.092757, 0.106168, 0.119645,
        0.133196, 0.146831, 0.160560, 0.174393, 0.188342,
        0.202418, 0.216633, 0.230999, 0.245530, 0.260242,
        0.275151, 0.290274, 0.305629, 0.321238, 0.337123,
        0.353308, 0.369821, 0.386694, 0.403959, 0.421656,
        0.439828, 0.458525, 0.477804, 0.497731, 0.518383,
        0.539855, 0.562252, 0.585706, 0.610378, 0.636468,
        0.664235, 0.694004, 0.726216, 0.761477, 0.800658,
        0.845081, 0.896904, 0.960057, 1.043030, 1.171530,
        1.189813, 1.209897, 1.232241, 1.257496, 1.286659,
        1.321370, 1.364637, 1.423026, 1.516664, 1.784955)
)


snap <- function(x, xvalues) {
  stopifnot(is.numeric(xvalues))
  stopifnot(isTRUE(all(xvalues >= 0 & xvalues <= 1)))
  stopifnot(!is.unsorted(xvalues))
  stopifnot(!anyDuplicated(xvalues))
  xvalues[findInterval(x, xvalues[-1] - 0.5 * diff(xvalues)) + 1L]
}
