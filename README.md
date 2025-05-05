
<!-- 
<img src="https://raw.githubusercontent.com/aravind-j/rpcss/master/inst/extdata/rpcss.png" width="20%" />
-->

## `rpcss`: Constitution of Core Collections by Principal Component Scoring Strategy <img src="https://raw.githubusercontent.com/aravind-j/rpcss/master/inst/extdata/rpcss.png" align="right" alt="logo" width="173" height = "200" style = "border: none; float: right;">

###### Version : [0.1.0.9000](https://aravind-j.github.io/rpcss/); License: [GPL-2\|GPL-3](https://www.r-project.org/Licenses/)

##### Aravind, J.

Division of Germplasm Conservation, ICAR-National Bureau of Plant
Genetic Resources, New Delhi.

------------------------------------------------------------------------

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg?logo=R)](https://cran.r-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-last-release/rpcss)](https://cran.r-project.org/package=rpcss)
[![Dependencies](https://tinyverse.netlify.app/status/rpcss)](https://cran.r-project.org/package=rpcss)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/rpcss?color=green)](https://CRAN.R-project.org/package=rpcss)
[![develVersion](https://img.shields.io/badge/devel%20version-0.1.0.9000-orange.svg)](https://github.com/aravind-j/rpcss)
[![Github Code
Size](https://img.shields.io/github/languages/code-size/aravind-j/rpcss.svg)](https://github.com/aravind-j/rpcss)
[![R-CMD-check](https://github.com/aravind-j/rpcss/workflows/R-CMD-check/badge.svg)](https://github.com/aravind-j/rpcss/actions)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-maturing.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![Last-changedate](https://img.shields.io/badge/last%20change-2025--05--01-yellowgreen.svg)](https://github.com/aravind-j/rpcss/)
[![Zenodo
DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.zenodo.14889174.svg)](https://doi.org/10.5281/zenodo.14889174)
[![Website -
pkgdown](https://img.shields.io/website-up-down-green-red/https/aravind-j.github.io/rpcss.svg)](https://aravind-j.github.io/rpcss/)
[![.](https://pro-pulsar-193905.appspot.com/G-B9EKQHWBD9/welcome-page)](https://github.com/aravind-j/google-analytics-beacon)
[![GoatCounter](https://rpcss-gh.goatcounter.com/count?p=/test)](https://rpcss.goatcounter.com/)
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-0.2.3.3-orange.svg)](https://github.com/aravind-j/rpcss) -->
<!-- [![GitHub Download Count](https://github-basic-badges.herokuapp.com/downloads/aravind-j/rpcss/total.svg)] -->
<!-- [![Rdoc](http://www.rdocumentation.org/badges/version/rpcss)](http://www.rdocumentation.org/packages/rpcss) -->

------------------------------------------------------------------------

## Description

<!-- Generate a Core Collection with Principal Component Scoring Strategy (PCSS) using qualitative and/or quantitative trait data according to Hamon and Noirot (1990) <https://www.documentation.ird.fr/hor/fdi:36506>, Noirot et al. (1996) [doi:10.2307/2527837> and Noirot et al. (2003) <https://www.documentation.ird.fr/hor/fdi:010031886](https://doi.org/10.2307/2527837> and Noirot et al. (2003) <https://www.documentation.ird.fr/hor/fdi:010031886). -->

Generate a Core Collection with Principal Component ScoringStrategy
(PCSS) using qualitative and/or quantitative trait data accordingto
Hamon and Noirot (1990)
\<<a href='https://www.documentation.ird.fr/hor/fdi:36506'>https://www.documentation.ird.fr/hor/fdi:36506</a>\>,Noirot
et al. (1996)
\<<a href='https://doi.org/10.2307/2527837'>doi:10.2307/2527837</a>\>
and Noirot et
al. (2003)\<<a href='https://www.documentation.ird.fr/hor/fdi:010031886'>https://www.documentation.ird.fr/hor/fdi:010031886</a>\>.

<img src="man/figures/README-readme-plot-1.png" width="100%" />

## Installation

The package can be installed from CRAN as follows:

The development version can be installed from github as follows:

``` r
# Install development version from Github
devtools::install_github("aravind-j/rpcss")
```

<!-- ## Detailed tutorial
For a detailed tutorial (vignette) on how to used this package type:
&#10;
``` r
browseVignettes(package = 'rpcss')
```
The vignette for the latest version is also available [online](https://aravind-j.github.io/rpcss/articles.html).-->

## What’s new

To know whats new in this version type:

``` r
news(package='rpcss')
```

## Links

[CRAN page](https://cran.r-project.org/package=rpcss)

[Github page](https://github.com/aravind-j/rpcss)

[Documentation website](https://aravind-j.github.io/rpcss/)

[Zenodo DOI](https://doi.org/10.5281/zenodo.14889174)

<!--
## CRAN checks
&#10;
&#10;
&#10; [![Linux](https://img.shields.io/badge/Linux-FCC624?style=for-the-badge&logo=linux&logoColor=black)](https://cran.r-project.org/web/checks/check_results_rpcss.html) 
&#10;
+-----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Flavour                           | CRAN check                                                                                                                                                                                                |
+===================================+===========================================================================================================================================================================================================+
| r-devel-linux-x86_64-debian-clang | [![CRAN check -                                                                                                                                                                                           |
|                                   | r-devel-linux-x86_64-debian-clang](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-debian-clang/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html) |
+-----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| r-devel-linux-x86_64-debian-gcc   | [![CRAN check -                                                                                                                                                                                           |
|                                   | r-devel-linux-x86_64-debian-gcc](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-debian-gcc/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)     |
+-----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| r-devel-linux-x86_64-fedora-clang | [![CRAN check -                                                                                                                                                                                           |
|                                   | r-devel-linux-x86_64-fedora-clang](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-fedora-clang/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html) |
+-----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| r-devel-linux-x86_64-fedora-gcc   | [![CRAN check -                                                                                                                                                                                           |
|                                   | r-devel-linux-x86_64-fedora-gcc](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-fedora-gcc/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)     |
+-----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| r-patched-linux-x86_64            | [![CRAN check -                                                                                                                                                                                           |
|                                   | r-patched-linux-x86_64](https://badges.cranchecks.info/flavor/r-patched-linux-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)                       |
+-----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| r-release-linux-x86_64            | [![CRAN check -                                                                                                                                                                                           |
|                                   | r-release-linux-x86_64](https://badges.cranchecks.info/flavor/r-release-linux-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)                       |
+-----------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
&#10;
 [![Windows](https://img.shields.io/badge/Windows-0078D6?style=for-the-badge&logo=windows&logoColor=white)](https://cran.r-project.org/web/checks/check_results_rpcss.html) 
&#10;
+--------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Flavour                  | CRAN check                                                                                                                                                                              |
+==========================+=========================================================================================================================================================================================+
| r-devel-windows-x86_64   | [![CRAN check -                                                                                                                                                                         |
|                          | r-devel-windows-x86_64](https://badges.cranchecks.info/flavor/r-devel-windows-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)     |
+--------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| r-release-windows-x86_64 | [![CRAN check -                                                                                                                                                                         |
|                          | r-release-windows-x86_64](https://badges.cranchecks.info/flavor/r-release-windows-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html) |
+--------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| r-oldrel-windows-x86_64  | [![CRAN check -                                                                                                                                                                         |
|                          | r-oldrel-windows-x86_64](https://badges.cranchecks.info/flavor/r-oldrel-windows-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)   |
+--------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
&#10;
 [![MacOS](https://img.shields.io/badge/mac%20os-000000?style=for-the-badge&logo=apple&logoColor=white)](https://cran.r-project.org/web/checks/check_results_rpcss.html) 
&#10;
+------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Flavour                | CRAN check                                                                                                                                                                          |
+========================+=====================================================================================================================================================================================+
| r-release-macos-x86_64 | [![CRAN check -                                                                                                                                                                     |
|                        | r-release-macos-x86_64](https://badges.cranchecks.info/flavor/r-release-macos-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html) |
+------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| r-oldrel-macos-x86_64  | [![CRAN check -                                                                                                                                                                     |
|                        | r-oldrel-macos-x86_64](https://badges.cranchecks.info/flavor/r-oldrel-macos-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_j.aravind_at_icar.gov.in.html)   |
+------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
&#10;-->

## Citing `rpcss`

To cite the methods in the package use:

``` r
citation("rpcss")
```

    To cite the R package 'rpcss' in publications use:

      Aravind, J. ().  rpcss: Constitution of Core Collections by Principal Component Scoring Strategy. R
      package version 0.1.0.9000,
      https://aravind-j.github.io/rpcss/https://cran.r-project.org/package=rpcsshttps://doi.org/10.5281/zenodo.14889174.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {rpcss: Constitution of Core Collections by Principal Component Scoring Strategy},
        author = {J. Aravind},
        note = {R package version 0.1.0.9000 https://aravind-j.github.io/rpcss/ https://cran.r-project.org/package=rpcss https://doi.org/10.5281/zenodo.14889174},
      }

    This free and open-source software implements academic research by the authors and co-workers. If you use
    it, please support the project by citing the package.
