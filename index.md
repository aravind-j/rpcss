## `rpcss`: Constitution of Core Collections by Principal Component Scoring Strategy ![logo](https://raw.githubusercontent.com/aravind-j/rpcss/master/inst/extdata/rpcss.png)

###### Version : [0.1.1.9000](https://aravind-j.github.io/rpcss/); Copyright (C) 2024-2026: [ICAR-NBPGR](https://nbpgr.org.in/); License: [GPL-2\|GPL-3](https://www.r-project.org/Licenses/)

##### *Aravind, J. and Singh, Anju M.*

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
[![develVersion](https://img.shields.io/badge/devel%20version-0.1.1.9000-orange.svg)](https://github.com/aravind-j/rpcss)
[![Github Code
Size](https://img.shields.io/github/languages/code-size/aravind-j/rpcss.svg)](https://github.com/aravind-j/rpcss)
[![R-CMD-check](https://github.com/aravind-j/rpcss/workflows/R-CMD-check/badge.svg)](https://github.com/aravind-j/rpcss/actions)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-maturing.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![Last-changedate](https://img.shields.io/badge/last%20change-2026--04--30-yellowgreen.svg)](https://github.com/aravind-j/rpcss/)
[![Zenodo
DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.zenodo.14889174.svg)](https://doi.org/10.5281/zenodo.14889174)
[![Website -
pkgdown](https://img.shields.io/website-up-down-green-red/https/aravind-j.github.io/rpcss.svg)](https://aravind-j.github.io/rpcss/)
[![GoatCounter](https://rpcss-gh.goatcounter.com/count?p=/test)](https://rpcss-gh.goatcounter.com/)

------------------------------------------------------------------------

## Description

Generate a Core Collection with Principal Component Scoring Strategy
(PCSS) using qualitative and/or quantitative trait data according to
Hamon and Noirot (1990)
\<<https://www.documentation.ird.fr/hor/fdi:36506>\>, Noirot et
al. (1996) \<[doi:10.2307/2527837](https://doi.org/10.2307/2527837)\>
and Noirot et al. (2003)
\<<https://www.documentation.ird.fr/hor/fdi:010031886>\>.

![](reference/figures/README-readme-plot-1.png)

## Installation

The package can be installed from CRAN as follows:

The development version can be installed from github as follows:

``` r

# Install development version from Github
devtools::install_github("aravind-j/rpcss")
```

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

## CRAN checks

[![Linux](https://img.shields.io/badge/Linux-FCC624?style=for-the-badge&logo=linux&logoColor=black)](https://cran.r-project.org/web/checks/check_results_rpcss.html)

| Flavour | CRAN check |
|----|----|
| r-devel-linux-x86_64-debian-clang | [![CRAN check - r-devel-linux-x86_64-debian-clang](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-debian-clang/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_rpcss.html) |
| r-devel-linux-x86_64-debian-gcc | [![CRAN check - r-devel-linux-x86_64-debian-gcc](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-debian-gcc/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_rpcss.html) |
| r-devel-linux-x86_64-fedora-clang | [![CRAN check - r-devel-linux-x86_64-fedora-clang](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-fedora-clang/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_rpcss.html) |
| r-devel-linux-x86_64-fedora-gcc | [![CRAN check - r-devel-linux-x86_64-fedora-gcc](https://badges.cranchecks.info/flavor/r-devel-linux-x86_64-fedora-gcc/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_rpcss.html) |
| r-patched-linux-x86_64 | [![CRAN check - r-patched-linux-x86_64](https://badges.cranchecks.info/flavor/r-patched-linux-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_rpcss.html) |
| r-release-linux-x86_64 | [![CRAN check - r-release-linux-x86_64](https://badges.cranchecks.info/flavor/r-release-linux-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_rpcss.html) |

[![Windows](https://img.shields.io/badge/Windows-0078D6?style=for-the-badge&logo=windows&logoColor=white)](https://cran.r-project.org/web/checks/check_results_rpcss.html)

| Flavour | CRAN check |
|----|----|
| r-devel-windows-x86_64 | [![CRAN check - r-devel-windows-x86_64](https://badges.cranchecks.info/flavor/r-devel-windows-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_rpcss.html) |
| r-release-windows-x86_64 | [![CRAN check - r-release-windows-x86_64](https://badges.cranchecks.info/flavor/r-release-windows-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_rpcss.html) |
| r-oldrel-windows-x86_64 | [![CRAN check - r-oldrel-windows-x86_64](https://badges.cranchecks.info/flavor/r-oldrel-windows-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_rpcss.html) |

[![MacOS](https://img.shields.io/badge/mac%20os-000000?style=for-the-badge&logo=apple&logoColor=white)](https://cran.r-project.org/web/checks/check_results_rpcss.html)

| Flavour | CRAN check |
|----|----|
| r-release-macos-x86_64 | [![CRAN check - r-release-macos-x86_64](https://badges.cranchecks.info/flavor/r-release-macos-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_rpcss.html) |
| r-oldrel-macos-x86_64 | [![CRAN check - r-oldrel-macos-x86_64](https://badges.cranchecks.info/flavor/r-oldrel-macos-x86_64/rpcss.svg)](https://cran.r-project.org/web/checks/check_results_rpcss.html) |

## Citing `rpcss`

To cite the methods in the package use:

``` r

citation("rpcss")
```

``` R
To cite the R package 'rpcss' in publications use:

  Aravind, J. (2026).  rpcss: Constitution of Core Collections by Principal
  Component Scoring Strategy. R package version 0.1.1.9000,
  https://aravind-j.github.io/rpcss/https://cran.r-project.org/package=rpcsshttps://doi.org/10.5281/zenodo.14889174.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {rpcss: Constitution of Core Collections by Principal Component Scoring Strategy},
    author = {J. Aravind and Anju Mahendru Singh},
    note = {R package version 0.1.1.9000 https://aravind-j.github.io/rpcss/ https://cran.r-project.org/package=rpcss https://doi.org/10.5281/zenodo.14889174},
    year = {2026},
  }

This free and open-source software implements academic research by the authors and
co-workers. If you use it, please support the project by citing the package.
```
