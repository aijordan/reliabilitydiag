continuous_asymptotics <- function(df_pav, df_bins, region.level, region.position, ...) {

  bde_at_x <- rlang::expr(
    bde::bde(
      df_pav$x,
      estimator = "betakernel",
      lower.limit = 0,
      upper.limit = 1
    ) %>%
      bde::density(.data$x)
  )

  CEP_prime <- switch(
    region.position,
    diagonal = rlang::expr(1),
    estimate = rlang::expr(1)
    # at some point we need to improve this:
    # a constant slope of 1 for the TRUE CEP function is unreasonable
    # the following would be a naive approximation
    # {
    #   hlp = dplyr::group_by(df.pava, bin_id) %>%
    #     dplyr::mutate(avg = mean(x)) %>%
    #     dplyr::group_by(x) %>%
    #     dplyr::summarise(avg = unique(avg)) %>%
    #     dplyr::pull(avg)
    #   predict(stats::loess(CEP_pav ~ hlp)) %>%
    #     (function(t) c(
    #       diff(head(t, 2)) / diff(head(hlp, 2)),
    #       diff(t, 2) / diff(hlp, 2),
    #       diff(tail(t, 2)) / diff(tail(hlp, 2))
    #     ))
    # }
  )
  # Here, we estimate  the F_i [in sigma^2(x_0) = (1-F_i)*F_i] by "FC",
  # as we "assume (basically as under the H0)" that CEP(x)=x
  x0 <- switch(region.position, diagonal = "x", estimate = "CEP_pav") %>%
    rlang::sym()
  data.frame(x = seq(0, 1, by = 0.01)) %>%
    dplyr::mutate(
      CEP_pav = switch(
        region.position,
        diagonal = .data$x,
        estimate = tidyr::pivot_longer(
            df_bins,
            cols = c(.data$x_min, .data$x_max),
            values_to = "t"
          ) %>%
          dplyr::distinct(.data$t, .data$CEP_pav) %>%
          with(., stats::approx(t, y = CEP_pav, xout = x)$y)),
      n = NA,
      level = 0.9,
      method = "continuous_asymptotics",
      position = region.position,
      tmp = ({{ x0 }} * (1 - {{ x0 }}) * !!CEP_prime / !!bde_at_x) %>%
        magrittr::multiply_by(4 / nrow(df_pav)) %>%
        magrittr::raise_to_power(1 / 3) %>%
        magrittr::multiply_by(qchern(0.5 + 0.5 * region.level)),
      lower = pmax(0, {{ x0 }} - .data$tmp) %>%
        bound_correction(.data$x, .data$CEP_pav, region.position),
      upper = pmin(1, {{ x0 }} + .data$tmp) %>%
        bound_correction(.data$x, .data$CEP_pav, region.position)
    ) %>%
    dplyr::select(.data$x, .data$lower, .data$upper,
                  .data$n, .data$method, .data$level, .data$position)
}

discrete_asymptotics <- function(df.pava, df_bins, region.level, region.position, ...) {
  x0 <- switch(region.position, diagonal = "x", estimate = "CEP_pav") %>%
    rlang::sym()
  df.pava %>%
    dplyr::group_by(.data$x) %>%
    dplyr::summarise(
      CEP_pav = unique(.data$CEP_pav),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      level = region.level,
      method = "discrete_asymptotics",
      position = region.position,
      tmp = sqrt({{ x0 }} * (1 - {{ x0 }}) / .data$n) %>%
        stats::qnorm(0.5 + 0.5 * region.level, sd = .),
      lower = pmax(0, {{ x0 }} - .data$tmp) %>%
        bound_correction(.data$x, .data$CEP_pav, region.position),
      upper = pmin(1, {{ x0 }} + .data$tmp) %>%
        bound_correction(.data$x, .data$CEP_pav, region.position)
    ) %>%
    dplyr::select(.data$x, .data$lower, .data$upper,
                  .data$n, .data$method, .data$level, .data$position)
}

resampling <- function(df_pav, df_bins, region.level, region.position, n.boot, ...)  {
  regions <- df_pav %>%
    dplyr::group_by(.data$x) %>%
    dplyr::summarise(
      CEP_pav = unique(.data$CEP_pav),
      n = dplyr::n(),
      .groups = "drop"
    )
  n.pav <- nrow(df_pav)
  n.regions <- nrow(regions)
  x0 <- switch(region.position,
               diagonal = df_pav$x,
               estimate = df_pav$CEP_pav)
  isofit <- if (requireNamespace("monotone", quietly = TRUE)) {
    monotone::monotone
  } else {
    function(y) {stats::isoreg(y)$yf}
  }
  bounds <-
    replicate(n.boot, simplify = FALSE, {
      s <- sample(n.pav, replace = TRUE)
      x <- df_pav$x[s]
      y <- stats::rbinom(n.pav, 1L, x0[s])
      ord <- order(x,-y)
      resampled_fit <-
        data.frame(x = x[ord], CEP_pav = isofit(y[ord])) %>%
        dplyr::distinct()
      if (nrow(resampled_fit) == 1L) {
        resampled_fit$CEP_pav
      } else { # interpolate
        dplyr::left_join(regions, resampled_fit, by = "x") %>%
          with(., approx(x, CEP_pav.y, xout = x)$y)
      }
    }) %>%
    unlist() %>%
    matrix(nrow = n.regions, ncol = n.boot) %>%
    apply(1L, stats::quantile,
      probs = 0.5 + c(-0.5, 0.5) * region.level,
      na.rm = TRUE
    ) %>%
    apply(1L, bound_correction, simplify = FALSE,
      x = regions$x,
      CEP_est = regions$CEP_pav,
      position = region.position
    ) %>%
    unlist() %>%
    matrix(nrow = n.regions, ncol = 2L)
  tibble::tibble(
    x = regions$x,
    lower = bounds[, 1L],
    upper = bounds[, 2L],
    n = regions$n,
    method = paste0("resampling_", n.boot),
    level = region.level,
    position = region.position
  )
}

restricted_resampling <- function(df.pava, df_bins, region.level, region.position, n.boot, ...) {
  ### experimental
  # m.asy <- 5*n.FC.unique # The "5" seems to be a compromise...
  m.asy <- 100
  # Preliminary results look like we would need m.asy to slowly increase with k !!!
  # For k=5, the asymptotic look good for n=100-200    ==> m.asy should be around 20-40
  # for k=10, the asymptotic look good for n=500-1000    ==> m.asy should be around 50-100
  # for k=20, the asymptotic start to look good for n=4000+    ==> m.asy should be 200+
  # for k=50, the asymptotic still looks bad for n=16000    ==> m.asy should be way larger than 300+++
  # This probably somehow makes sense due to the PAVA algorithm...
  x0 <- switch(region.position, diagonal = "x", estimate = "CEP_pav") %>%
    rlang::sym()
  df.pava %>%
    dplyr::group_by(.data$x) %>%
    dplyr::mutate(
      .,
      n = dplyr::n(),
      n_x_uniq = nrow(dplyr::group_keys(.))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_split(
      region.method = ifelse(
        dplyr::n() >= 2000 &
          .data$n >= max(
            10,
            .data$n_x_uniq, 4 * {{ x0 }} * (1 - {{ x0 }}) * m.asy),
        "discrete_asymptotics",
        "resampling"
      )) %>%
    lapply(function(df.pava) {
      region_method <- get(unique(df.pava$region.method))
      region_method(df.pava, region.level, region.position, n.boot, ...)
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(.data$x)
}
