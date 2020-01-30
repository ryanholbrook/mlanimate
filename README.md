
# mlanimation

<!-- badges: start -->
<!-- badges: end -->

`mlanimate` is an R package for creating illustrative graphics for data science and machine learning topics.

## Installation

This is mostly a personal package at the moment with nothing guaranteed to work at any given moment. But, if you desire, you can install it from the repo the usual way.

```r
remotes::install_github("ryanholbrook/mlanimate")
```

## Example

``` r
library(mlanimate)

p <- 0.5
mu_0 <- c(0, 2)
sigma_0 <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
mu_1 <- c(2, 0)
sigma_1 <- matrix(c(1, -0.3, -0.3, 1), nrow = 2)

n <- 4000
set.seed(31415)
sample_mvn <- make_mvn_sample(n,
                              mu_0, sigma_0,
                              mu_1, sigma_1,
                              p)

ggplot() +
    gg_sample(sample_mvn) +
    coord_fixed()
```
