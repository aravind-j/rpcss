
wlcm <- paste0("\n",
               "--------------------------------------------------------------------------------\n",
               "Welcome to rpcss version ", utils::packageDescription("rpcss")$Version, "\n",
               "\n", "\n",
               # "# To know how to use this package type:", "\n",
               # "  browseVignettes(package = 'rpcss')", "\n", "  for the package vignette.", "\n",
               # "\n",
               "# To know whats new in this version type:", "\n",
               "  news(package='rpcss')", "\n", "  for the NEWS file.", "\n",
               "\n",
               "# To cite the methods in the package type:", "\n",
               "  citation(package='rpcss')", "\n",
               "\n",
               "# To suppress this message use:", "\n",
               "  suppressPackageStartupMessages(library(rpcss))", "\n",
               "--------------------------------------------------------------------------------\n")

.onAttach <- function(lib, pkg, ...) {
  packageStartupMessage(wlcm)


}

