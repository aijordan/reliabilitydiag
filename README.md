
# reliabilitydiag

This package implements reliability diagrams via the CORP approach,
which generates provably statistically Consistent, Optimally binned, and
Reproducible reliability diagrams in an automated way.

CORP is based on non-parametric isotonic regression and implemented via
the Pool-adjacent-violators (PAV) algorithm - essentially, the CORP
reliability diagram shows the graph of the PAV- (re)calibrated forecast
probabilities. The CORP approach allows for uncertainty quantification
via either resampling techniques or asymptotic theory, furnishes a new
numerical measure of miscalibration, and provides a CORP based Brier
score decomposition that generalizes to any proper scoring rule.

## Installation

``` r
# Install development version from GitHub
devtools::install_github("aijordan/reliabilitydiag")
```
