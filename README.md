

<!-- README.md is generated from README.Rmd. Please edit that file -->

# broom.asreml

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `broom.asreml` provides tidy format for extracting model components
for asreml objects.

## Installation

You can install the development version of broom.asreml from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("emitanaka/broom.asreml")
```

## Example

``` r
library(asreml)
library(broom.asreml)
fit <- asreml(yield ~ Variety, 
              random = ~Column + Row, 
              data = wheat)
```

## Single row summary of model

``` r
glance(fit)
#> # A tibble: 1 × 7
#>   sigma logLik deviance df.residual   AIC   BIC  nobs
#>   <dbl>  <dbl>    <dbl>       <int> <dbl> <dbl> <int>
#> 1  273.  -855.        0         136 2782. 5198.   670
```

## Tidy model parameters

### Fixed effects (E-BLUEs)

``` r
tidy(fit, "fixed")
#> # A tibble: 533 × 2
#>    term        estimate
#>    <chr>          <dbl>
#>  1 (Intercept)   3294. 
#>  2 Variety_1        0  
#>  3 Variety_2     -123. 
#>  4 Variety_3     -334. 
#>  5 Variety_4     -122. 
#>  6 Variety_5     -847. 
#>  7 Variety_6     -686. 
#>  8 Variety_7     -660. 
#>  9 Variety_8       26.5
#> 10 Variety_9     -264. 
#> # ℹ 523 more rows
```

### Random effects (E-BLUPs)

``` r
tidy(fit, "random")
#> # A tibble: 77 × 2
#>    term      estimate
#>    <chr>        <dbl>
#>  1 Column_1     99.5 
#>  2 Column_2    160.  
#>  3 Column_3    274.  
#>  4 Column_4    217.  
#>  5 Column_5    138.  
#>  6 Column_6     -9.68
#>  7 Column_7     33.2 
#>  8 Column_8   -125.  
#>  9 Column_9   -279.  
#> 10 Column_10  -509.  
#> # ℹ 67 more rows
```

### Variance components (REML estimates)

``` r
tidy(fit, "vcomp")
#> # A tibble: 3 × 5
#>   term    estimate std.error statistic constraint
#>   <chr>      <dbl>     <dbl>     <dbl> <chr>     
#> 1 Column    64035.    32779.      1.95 P         
#> 2 Row       11286.     8052.      1.40 P         
#> 3 units!R   74466.    10130.      7.35 P
```

### Wald test

``` r
tidy(fit, "wald")
#> # A tibble: 3 × 5
#>   term             df     sumsq statistic p.value
#>   <chr>         <dbl>     <dbl>     <dbl>   <dbl>
#> 1 (Intercept)       1 82441686.     1107.       0
#> 2 Variety         531 97555651.     1310.       0
#> 3 residual (MS)    NA    74466.       NA       NA
```

## Augment data with model information

``` r
augment(fit)
#> # A tibble: 670 × 9
#>    yield Column Row   Variety units mv    .fitted .resid  .hat
#>    <int> <fct>  <fct> <fct>   <fct> <fct>   <dbl>  <dbl> <dbl>
#>  1  2652 1      1     526     1     1       2538.  114.  8557.
#>  2  2691 2      1     526     2     1       2599.   92.5 9564.
#>  3  2770 3      1     526     3     1       2713.   57.0 8949.
#>  4  2896 4      1     526     4     1       2656.  240.  9546.
#>  5  2473 5      1     526     5     1       2576. -103.  9485.
#>  6  2317 6      1     526     6     1       2429. -112.  9562.
#>  7  2323 7      1     526     7     1       2472. -149.  9083.
#>  8  2261 8      1     526     8     1       2314.  -52.5 9327.
#>  9  2424 9      1     526     9     1       2160.  264.  9957.
#> 10  1899 10     1     526     10    1       1930.  -30.8 9310.
#> # ℹ 660 more rows
```
