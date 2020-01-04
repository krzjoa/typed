
<!-- README.md is generated from README.Rmd. Please edit that file -->

# typed <img src='man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

<!-- badges: end -->

`typed` is an experimental R library, which introduces typed variables
in R.

## Installation

You can install the released version of typed from from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("krzjoa/typed")
```

## Example

``` r
library(typed)
#> 
#> Attaching package: 'typed'
#> The following object is masked from 'package:base':
#> 
#>     <-
x <- 10
x <- 2.5
print(x)
#> [1] 2.5
locked(z) <- 2
z <- 5
#> Error in do.call(.Primitive("<-"), list(var.name, value), envir = ref.env): nie można zmienić wartości zablokowanego połączenia dla 'z'
```
