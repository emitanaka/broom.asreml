

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
pak::pak("anu-aagi/broom.asreml")
```

## Example

``` r
library(asreml)
library(broom.asreml)
fit <- asreml(yield ~ Variety, random = ~ Column + Row, data = wheat)
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
#> # A tibble: 535 × 6
#>    term        group       level estimate std.error statistic
#>    <chr>       <chr>       <chr>    <dbl>     <dbl>     <dbl>
#>  1 (Intercept) (Intercept) <NA>    3294.       310.   10.6   
#>  2 Variety_1   Variety     1          0          0   NaN     
#>  3 Variety_2   Variety     2       -123.       398.   -0.308 
#>  4 Variety_3   Variety     3       -334.       397.   -0.842 
#>  5 Variety_4   Variety     4       -122.       398.   -0.305 
#>  6 Variety_5   Variety     5       -847.       398.   -2.13  
#>  7 Variety_6   Variety     6       -686.       398.   -1.72  
#>  8 Variety_7   Variety     7       -660.       397.   -1.66  
#>  9 Variety_8   Variety     8         26.5      398.    0.0667
#> 10 Variety_9   Variety     9       -264.       399.   -0.661 
#> # ℹ 525 more rows
```

### Random effects (E-BLUPs)

``` r
tidy(fit, "random")
#> # A tibble: 77 × 6
#>    term      group  level estimate std.error statistic
#>    <chr>     <chr>  <chr>    <dbl>     <dbl>     <dbl>
#>  1 Column_1  Column 1        99.5       101.    0.982 
#>  2 Column_2  Column 2       160.        106.    1.51  
#>  3 Column_3  Column 3       274.        103.    2.66  
#>  4 Column_4  Column 4       217.        106.    2.05  
#>  5 Column_5  Column 5       138.        106.    1.31  
#>  6 Column_6  Column 6        -9.68      106.   -0.0913
#>  7 Column_7  Column 7        33.2       104.    0.320 
#>  8 Column_8  Column 8      -125.        105.   -1.19  
#>  9 Column_9  Column 9      -279.        108.   -2.59  
#> 10 Column_10 Column 10     -509.        105.   -4.86  
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
#> A design matrix was not found in the asreml object. Building a design matrix.
#> ASReml Version 4.2 28/08/2026 18:33:22
#>           LogLik        Sigma2     DF     wall
#>  1     -855.0716      74463.85    136   18:33:22
#>  2     -855.0716      74463.88    136   18:33:22
#> # A tibble: 670 × 10
#>    yield Column Row   Variety units mv    .fitted .resid  .hat .fixed
#>    <int> <fct>  <fct> <fct>   <fct> <fct>   <dbl>  <dbl> <dbl>  <dbl>
#>  1  2652 1      1     526     1     1       2538.  114.  8557.  2390.
#>  2  2691 2      1     526     2     1       2599.   92.5 9564.  2390.
#>  3  2770 3      1     526     3     1       2713.   57.0 8949.  2390.
#>  4  2896 4      1     526     4     1       2656.  240.  9546.  2390.
#>  5  2473 5      1     526     5     1       2576. -103.  9485.  2390.
#>  6  2317 6      1     526     6     1       2429. -112.  9562.  2390.
#>  7  2323 7      1     526     7     1       2472. -149.  9083.  2390.
#>  8  2261 8      1     526     8     1       2314.  -52.5 9327.  2390.
#>  9  2424 9      1     526     9     1       2160.  264.  9957.  2390.
#> 10  1899 10     1     526     10    1       1930.  -30.8 9310.  2390.
#> # ℹ 660 more rows
```
