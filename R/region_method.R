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
  # hlp = dplyr::group_by(df.pava, bin_id) %>%
  #   dplyr::mutate(avg = mean(x)) %>%
  #   dplyr::group_by(x) %>%
  #   dplyr::summarise(avg = unique(avg)) %>%
  #   dplyr::pull(avg)
  CEP_prime <- switch(
    region.position,
    diagonal = rlang::expr(1),
    estimate = rlang::expr(1)
    # {
    #   predict(stats::loess(CEP_pav ~ hlp)) %>%
    #     (function(t) c(
    #       diff(head(t, 2)) / diff(head(hlp, 2)),
    #       diff(t, 2) / diff(hlp, 2),
    #       diff(tail(t, 2)) / diff(tail(hlp, 2))
    #     ))
    # })
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
        `*`(4 / nrow(df_pav)) %>%
        `^`(1 / 3) %>%
        `*`(qchern(0.5 + 0.5 * region.level)),
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

resampling <- function(df.pava, df_bins, region.level, region.position, n.boot, ...)  {
  df.CB <- df.pava %>%
    dplyr::group_by(.data$x) %>%
    dplyr::summarise(
      CEP_pav = unique(.data$CEP_pav),
      n = dplyr::n(),
      .groups = "drop"
    )
  x0 <- switch(region.position, diagonal = "x", estimate = "CEP_pav") %>%
    rlang::sym()
  replicate(n.boot, {
    df.pava %>%
      dplyr::select(.data$x, .data$CEP_pav) %>%
      dplyr::slice_sample(., n = nrow(.), replace = TRUE) %>%
      dplyr::mutate(y = stats::rbinom(dplyr::n(), 1, {{ x0 }})) %>%
      with(.,
        stats::isoreg(x, y) %>%
          with(.,
            tibble::tibble(
              CEP_pav = yf,
              x = if (isOrd) x else x[ord]
            )
          )
      ) %>%
      dplyr::distinct() %>%
      dplyr::left_join(df.CB, ., by = "x") %>%
      tidyr::fill(.data$CEP_pav.y, .direction = "downup") %>%
      .$CEP_pav.y
  }) %>%
    apply(1, stats::quantile, 0.5 + c(-0.5, 0.5) * region.level) %>%
    (function(bounds) dplyr::mutate(
      df.CB,
      level = region.level,
      method = paste0("resampling_", n.boot),
      position = region.position,
      lower = bound_correction(bounds[1, ], .data$x, .data$CEP_pav, region.position),
      upper = bound_correction(bounds[2, ], .data$x, .data$CEP_pav, region.position)
    )) %>%
    dplyr::select(.data$x, .data$lower, .data$upper,
                  .data$n, .data$method, .data$level, .data$position)
}

restricted_resampling <- function(df.pava, df_bins, region.level, region.position, n.boot, ...) {
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
