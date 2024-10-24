
![Project Status](https://www.repostatus.org/badges/latest/active.svg)
[![r-universe
name](https://ices-tools-prod.r-universe.dev/badges/:name)](https://ices-tools-prod.r-universe.dev)
[![version
number](https://ices-tools-prod.r-universe.dev/badges/icesDatsu)](https://ices-tools-prod.r-universe.dev/icesDatsu)
![branch version
number](https://img.shields.io/badge/branch_version-1.2.0-blue)
[![GitHub
release](https://img.shields.io/github/release/ices-tools-prod/icesDatsu.svg?maxAge=6000)]()
[![License](https://img.shields.io/badge/license-GPL%20(%3E%3D%202)-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

CRAN status:
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/icesDatsu)](https://cran.r-project.org/package=icesDatsu)
![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/icesDatsu) ![CRAN RStudio
mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/icesDatsu)

[<img align="right" alt="ICES Logo" width="17%" height="17%" src="http://ices.dk/_layouts/15/1033/images/icesimg/iceslogo.png">](http://ices.dk)

# icesDatsu

icesDatsu provides helper functions to access the ICES Data checking
utility [DATSU](http://datsu.ices.dk/).

icesDatsu is implemented as an [R](https://www.r-project.org) package
and is currently hosted on
[r-universe](https://ices-tools-prod.r-universe.dev) and available on
[CRAN](https://cran.r-project.org/package=icesDatsu).

### Installation

The stable version of icesDatsu can be installed from CRAN using the
`install.packages` command:

``` r
install.packages("icesDatsu", repos = "https://cloud.r-project.org")
```

or a potentially more recent, but less stable version installed from
r-universe:

``` r
install.packages("icesDatsu", repos = "https://ices-tools-prod.r-universe.dev")
```

### Usage

For a summary of the package:

``` r
library(icesDatsu)
?icesDatsu
```

### References

ICES DATSU site: <https://datsu.ices.dk>

ICES Stock Assessment Graphs web services:
<https://datsu.ices.dk/web/webservices.aspx>

## Development

icesDatsu is developed openly on
[GitHub](https://github.com/ices-tools-prod/icesDatsu).

Feel free to open an
[issue](https://github.com/ices-tools-prod/icesDatsu/issues) there if
you encounter problems or have suggestions for future versions.

``` r
library(devtools)
install_github("ices-tools-prod/icesDatsu@development")
```
