# Plot Contribution or Loadings of Traits for each Dimension/Factor from `pcss.core` Output

`contrib.pcss.core` generates bar plots of contributions or loadings
("right singular vectors") of traits for each dimension/factor from the
output of `pcss.core`.

## Usage

``` r
contrib(x, ...)

# Default S3 method
contrib(
  x,
  ndim = NULL,
  plot.loadings = FALSE,
  use.sign = TRUE,
  sort.value = TRUE,
  ...
)

# S3 method for class 'pcss.core'
contrib(
  x,
  ndim = NULL,
  plot.loadings = FALSE,
  use.sign = TRUE,
  sort.value = TRUE,
  ...
)

contrib(x, ...)
```

## Arguments

- x:

  An object of class `pcss.core`.

- ...:

  Unused.

- ndim:

  The number of dimensions for which contribution or loadings of traits
  are to be plotted.

- plot.loadings:

  If `TRUE`, the loadings or "right singular vectors" are plotted
  instead of contributions. Default is `FALSE`.

- use.sign:

  If `TRUE`, contributions of variables are given the sign of their
  corresponding coordinates. Default is `TRUE`.

- sort.value:

  If `TRUE`, the bars are sorted according to their value.

## Value

The contributions/loadings bar plot as a `ggplot` object.

## See also

[`pcss.core`](https://aravind-j.github.io/rpcss/reference/pcss.core.md)

## Examples

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare example data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

suppressPackageStartupMessages(library(EvaluateCore))

# Get data from EvaluateCore

data("cassava_EC", package = "EvaluateCore")
data = cbind(Genotypes = rownames(cassava_EC), cassava_EC)
quant <- c("NMSR", "TTRN", "TFWSR", "TTRW", "TFWSS", "TTSW", "TTPW", "AVPW",
           "ARSR", "SRDM")
qual <- c("CUAL", "LNGS", "PTLC", "DSTA", "LFRT", "LBTEF", "CBTR", "NMLB",
          "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
          "PSTR")
rownames(data) <- NULL

# Convert qualitative data columns to factor
data[, qual] <- lapply(data[, qual], as.factor)


library(FactoMineR)
suppressPackageStartupMessages(library(factoextra))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# With quantitative data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out1 <- pcss.core(data = data, names = "Genotypes",
                  quantitative = quant,
                  qualitative = NULL, eigen.threshold = NULL, size = 0.2,
                  var.threshold = 0.75)

# \donttest{
# Plot contributions of genotypes - with sign - sorted
contrib(x = out1, ndim = 5)


# Plot contributions of genotypes - without sign - sorted
contrib(x = out1, ndim = 5, use.sign = FALSE)


# Plot loadings/coordinates of genotypes - with sign - sorted
contrib(x = out1, ndim = 5, plot.loadings = TRUE)


# Plot contributions of genotypes - with sign - unsorted
contrib(x = out1, ndim = 5, sort.value = FALSE)


# Plot biplot with factoextra
fviz_contrib(out1$raw.out, choice = "var", axes = 1)

fviz_contrib(out1$raw.out, choice = "var", axes = 2)

# }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get core sets with PCSS (qualitative data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out2 <- pcss.core(data = data, names = "Genotypes", quantitative = NULL,
                  qualitative = qual, eigen.threshold = NULL,
                  size = 0.2, var.threshold = 0.75)

# \donttest{
# Plot contributions of genotypes - with sign - sorted
contrib(x = out2, ndim = 5)


# Plot contributions of genotypes - without sign - sorted
contrib(x = out2, ndim = 5, use.sign = FALSE)


# Plot loadings/coordinates of genotypes - with sign - sorted
contrib(x = out2, ndim = 5, plot.loadings = TRUE)


# Plot contributions of genotypes - with sign - unsorted
contrib(x = out2, ndim = 5, sort.value = FALSE)


# Plot biplot with factoextra
fviz_contrib(out2$raw.out, choice = "var", axes = 1)

fviz_contrib(out2$raw.out, choice = "var", axes = 2)

# }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get core sets with PCSS (quantitative and qualitative data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out3 <- pcss.core(data = data, names = "Genotypes",
                  quantitative = quant,
                  qualitative = qual, eigen.threshold = NULL)

# \donttest{
# Plot contributions of genotypes - sorted
contrib(x = out3, ndim = 5)


# Plot contributions of genotypes - without sign - sorted
contrib(x = out3, ndim = 5, use.sign = FALSE)


# Plot loadings/coordinates of genotypes - sorted
contrib(x = out3, ndim = 5, plot.loadings = TRUE)


# Plot contributions of genotypes - with sign - unsorted
contrib(x = out3, ndim = 5, sort.value = FALSE)


# Plot biplot with factoextra
# fviz_contrib(out3$raw.out, choice = "quanti.var", axes = 1)
# fviz_contrib(out3$raw.out, choice = "quali.var", axes = 1)
# fviz_contrib(out3$raw.out, choice = "quanti.var", axes = 2)
# fviz_contrib(out3$raw.out, choice = "quali.var", axes = 2)
# }
```
