# reliabilitydiag (0.2.0)

## Features

* Explicit support for `ggplot2::facet_*()` via the internal variable `forecast` (#2)
* If installed, `monotone::monotone()` is used for isotonic regression instead of `stats::isoreg()` (#3)

## Fixes

* Fixed a bug where the `"resampling"` method to calculate consistency/confidence regions could not be calculated for a constant forecast.

