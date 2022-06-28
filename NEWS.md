# reliabilitydiag (0.2.1)

This is a small patch release that fixes two minor bugs.

* Fixed a bug in `bound_correction` that occurred when "region.position == 'estimate'" and a prediction `x` has only one unique value.

* Fixed an inconsistency where the `"resampling"` method to calculate consistency/confidence regions required R 4.1, but DESCRIPTION stated R 3.3. The new consistent requirement is R 3.5.


# reliabilitydiag (0.2.0)

## Features

* Explicit support for `ggplot2::facet_*()` via the internal variable `forecast` (#2)
* If installed, `monotone::monotone()` is used for isotonic regression instead of `stats::isoreg()` (#3)

## Fixes

* Fixed a bug where the `"resampling"` method to calculate consistency/confidence regions could not be calculated for a constant forecast.
