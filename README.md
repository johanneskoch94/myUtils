
<!-- README.md is generated from README.Rmd. Please edit that file -->

# myUtils

<!-- badges: start -->
<!-- badges: end -->

Load data from multiple gdx and mif files.

## Installation

Install the development version of myUtils from Github:

``` r
remotes::install_github("johanneskoch94/myUtils")
```

To load data from gdx files, the
[gamstransfer](https://www.gams.com/latest/docs/API_R_GAMSTRANSFER.html)
package has to be installed. (Tip when using renv: copy the package from
the gams source files, rebuild tar.gz file using devtools, and place it
in the renv cellar.)

``` r
install.packages("[PathToGAMS]/apifiles/R/gamstransfer/source/gamstransfer_r.tar.gz", dependencies = TRUE)
```

## Examples

Load data from multiple gdx files. Use the gams suffix notation to
specify the fields (level by default).

``` r
library(myUtils)
read_items_from_gdxs(c("path1/fulldata.gdx", "path2/fulldata.gdx"), c("var", "equa.m", "var2.l"))
```

Use `explore_CEStree` to open a shiny app and compare CES parameters
(pm_cesdata) and final values of the production factors (vm_ces) of
REMIND runs.

``` r
library(myUtils)
explore_CEStree(c("path1/fulldata.gdx", "path2/fulldata.gdx"))
```
