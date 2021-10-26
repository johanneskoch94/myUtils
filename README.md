
<!-- README.md is generated from README.Rmd. Please edit that file -->

# myUtils

<!-- badges: start -->
<!-- badges: end -->

Bits and bobs to help analyze REMIND results

## Installation

Install the development version of myUtils from Github:

``` r
remotes::install_github("johanneskoch94/myUtils")
```

## Example

Use `explore_CEStree` to open a shiny app and compare CES parameters
(pm\_cesData) and final values of the production factors (vm\_ces) of
REMIND runs.

``` r
library(myUtils)
explore_CEStree(c("path1/fulldata.gdx", "path2/fulldata.gdx"))
```
