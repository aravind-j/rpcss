# Principal Component Scoring to Generate Core collections

Generate a Core Collection with Principal Component Scoring Strategy
(PCSS) (Hamon and Noirot 1990; Noirot et al. 1996; Noirot et al. 2003)
using qualitative and/or quantitative trait data.

## Usage

``` r
pcss.core(
  data,
  names,
  quantitative,
  qualitative,
  eigen.threshold = NULL,
  size = 0.2,
  var.threshold = 0.75,
  always.selected = NULL
)
```

## Arguments

- data:

  The data as a data frame object. The data frame should possess one row
  per individual and columns with the individual names and multiple
  trait/character data.

- names:

  Name of column with the individual/genotype names as a character
  string.

- quantitative:

  Name of columns with the quantitative traits as a character vector.

- qualitative:

  Name of columns with the qualitative traits as a character vector.

- eigen.threshold:

  The lower limit of the eigen value of factors to be included in the
  estimation. The default value is the average of all the eigen values.

- size:

  The desired core set size proportion.

- var.threshold:

  The desired proportion of total variability to be sampled.

- always.selected:

  Names of genotypes to be always included in the core set as a
  character vector.

## Value

A list of class `pcss.core` with the following components.

- details:

  The details of the core set generation process.

- raw.out:

  The original output of
  [`PCA`](https://rdrr.io/pkg/FactoMineR/man/PCA.html),
  [`CA`](https://rdrr.io/pkg/FactoMineR/man/CA.html) and
  [`FAMD`](https://rdrr.io/pkg/FactoMineR/man/FAMD.html) functions of
  [`FactoMineR`](https://rdrr.io/pkg/FactoMineR/man/FactoMineR-package.html)

- eigen:

  A data frame with eigen values and their partial and cumulative
  contribution to percentage of variance.

- eigen.threshold:

  The threshold eigen value used.

- rotation:

  A matrix of rotation values or loadings.

- scores:

  A matrix of scores from PCA, CA or FAMD.

- variability.ret:

  A data frame of individuals/genotypes ordered by variability retained.

- cores.info:

  A data frame of core set size and percentage variability retained
  according to the method used.

## Details

A core collection is constituted from an entire collection of \\N\\
genotypes using quantitative data of \\J\\ traits using Principal
Component Scoring Strategy (PCSS) (Hamon and Noirot 1990; Noirot et al.
1996; Noirot et al. 2003) as follows:

1.  Principal Component Analysis (PCA) is performed on the standardized
    genotype \\\times\\ trait data. This takes care of multicollinearity
    between the traits to generate \\J\\ standardized and independent
    variables or factors or principal component.

2.  Considering only a subset of factors \\K\\, the Generalized Sum of
    Squares (GSS) of N individuals in K factorial spaces is computed as
    \\N \times K\\.

    \\K\\ can be the number of factors for which the eigen value
    \\\lambda\\ is greater than a threshold value such as 1
    (Kaiser-Guttman criterion) or the average of all the eigen values.

3.  The contribution of the \\i\\th genotype to GSS (\\P\_{i}\\) or
    total variability is calculated as below.

    \\P\_{i} = \sum\_{j = 1}^{K} x\_{ij}^{2}\\

    Where \\x\_{ij}\\ is the component score or coordinate of the
    \\i\\th genotype on the \\j\\th principal component.

4.  For each genotype, its relative contribution to GSS or total
    variability is computed as below.

    \\CR\_{i} = \frac{P\_{i}}{N \times K}\\

5.  The genotypes are sorted in descending order of magnitude of their
    contribution to GSS and then the cumulative contribution of
    successive genotypes to GSS is computed.

6.  The core collection can then be selected by three different methods.

    1.  Selection of fixed proportion or percentage or number of the top
        accessions.

    2.  Selection of the top accessions that contribute up to a fixed
        percentage of the GSS.

    3.  Fitting a logistic regression model of the following form to the
        cumulative contribution of successive genotypes to GSS
        (Balakrishnan et al. 2000) .

        \\\frac{y}{A-y} = e^{a + bn}\\

        The above equation can be reparameterized as below.

        \\\log\_{e} \left ( {\frac{y}{A-y}} \right ) = a + bn\\

        Where, \\a\\ and \\b\\ are the intercept and regression
        coefficient, respectively; \\y\\ is the cumulative contribution
        of successive genotypes to GSS; \\n\\ is the rank of the
        genotype when sorted according to the contribution to GSS and
        \\A\\ is the asymptote of the curve (\\A = 100\\).

        The rate of increase in the successive contribution of genotypes
        to GSS can be computed by the following equation to find the
        point of inflection where the rate of increase starts declining.

        \\\frac{\mathrm{d} y}{\mathrm{d} x} = by(A-y)\\

        The number of accessions included till the peak or infection
        point are selected to constitute the core collection.

Similarly for qualitative traits, standardized and independent variables
or factors can be obtained by Correspondence Analysis (CA) on complete
disjunctive table of genotype \\\times\\ trait data or to be specific
Multiple Correspondence Analysis (MCA). In `rpcss`, this has also been
extended for data sets having both quantitative and qualitative traits
by implementing Factor Analysis for Mixed Data (FAMD) for obtaining
standardized and independent variables or factors.

In `rpcss`, PCA, MCA and FAMD are implemented via the
[`FactoMineR`](https://rdrr.io/pkg/FactoMineR/man/FactoMineR-package.html)
package. (Le et al. 2008; Husson et al. 2017) .

## References

Balakrishnan R, Nair NV, Sreenivasan TV (2000). “A method for
establishing a core collection of *Saccharum officinarum* L. germplasm
based on quantitative-morphological data.” *Genetic Resources and Crop
Evolution*, **47**, 1–9. ISBN: 0925-9864 Publisher: Springer.  
  
Hamon S, Noirot M (1990). “Some proposed procedures for obtaining a core
collection using quantitative plant characterization data.” In *Report
of An International Workshop on Okra Genetic Resources held at National
Bureau of Plant Genetic Resources (NBPGR), New Delhi, India, 8-12
October,1990*, number 5 in International Crop Network Series.
International Board for Plant Genetic Resources, Rome.  
  
Husson F, Le S, Pages J (2017). *Exploratory Multivariate Analysis by
Example Using R*, Second edition edition. CRC Press, Boca Raton. ISBN
978-1-138-19634-6.  
  
Le S, Josse J, Husson F (2008). “FactoMineR : An R package for
multivariate analysis.” *Journal of Statistical Software*, **25**(1).  
  
Noirot M, Anthony F, Dussert S, Hamon S (2003). “A method for building
core collections.” In *Genetic Diversity of Cultivated Tropical Plants*,
81–92. CRC Press.  
  
Noirot M, Hamon S, Anthony F (1996). “The principal component scoring: A
new method of constituting a core collection using quantitative data.”
*Genetic Resources and Crop Evolution*, **43**(1), 1–6.

## See also

[`PCA`](https://rdrr.io/pkg/FactoMineR/man/PCA.html),
[`CA`](https://rdrr.io/pkg/FactoMineR/man/CA.html) and
[`FAMD`](https://rdrr.io/pkg/FactoMineR/man/FAMD.html)

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get core sets with PCSS (quantitative data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out1 <- pcss.core(data = data, names = "Genotypes",
                  quantitative = quant,
                  qualitative = NULL, eigen.threshold = NULL, size = 0.2,
                  var.threshold = 0.75)

out1
#> 
#> Method
#> ========================
#> [1] "PCA"
#> 
#> Details
#> ========================
#>                                  Detail
#> 1 Total number of individuals/genotypes
#> 2                   Quantitative traits
#> 3                    Qualitative traits
#> 4                                Method
#> 5                 Threshold eigen value
#> 6       Number of eigen values selected
#> 7                        Threshold size
#> 8                Threshold variance (%)
#>                                                          Value
#> 1                                                         1684
#> 2 NMSR, TTRN, TFWSR, TTRW, TFWSS, TTSW, TTPW, AVPW, ARSR, SRDM
#> 3                                                             
#> 4                                                          PCA
#> 5                                                            1
#> 6                                                            2
#> 7                                                          0.2
#> 8                                                           75
#> 
#> Core sets
#> =========================
#>                   Method Size   VarRet
#> 1      By size specified  337 62.79839
#> 2  By threshold variance  532 75.00000
#> 3 By logistic regression  189 50.03205

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get core sets with PCSS (qualitative data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out2 <- pcss.core(data = data, names = "Genotypes", quantitative = NULL,
                  qualitative = qual, eigen.threshold = NULL,
                  size = 0.2, var.threshold = 0.75)

out2
#> 
#> Method
#> ========================
#> [1] "MCA"
#> 
#> Details
#> ========================
#>                                  Detail
#> 1 Total number of individuals/genotypes
#> 2                   Quantitative traits
#> 3                    Qualitative traits
#> 4                                Method
#> 5                 Threshold eigen value
#> 6       Number of eigen values selected
#> 7                        Threshold size
#> 8                Threshold variance (%)
#>                                                                                                  Value
#> 1                                                                                                 1684
#> 2                                                                                                     
#> 3 CUAL, LNGS, PTLC, DSTA, LFRT, LBTEF, CBTR, NMLB, ANGB, CUAL9M, LVC9M, TNPR9M, PL9M, STRP, STRC, PSTR
#> 4                                                                                                  MCA
#> 5                                                                                               0.0625
#> 6                                                                                                   24
#> 7                                                                                                  0.2
#> 8                                                                                                   75
#> 
#> Core sets
#> =========================
#>                   Method Size   VarRet
#> 1      By size specified  337 51.05444
#> 2  By threshold variance  822 75.00000
#> 3 By logistic regression  322 50.00157

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get core sets with PCSS (quantitative and qualitative data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out3 <- pcss.core(data = data, names = "Genotypes",
                  quantitative = quant,
                  qualitative = qual, eigen.threshold = NULL)

out3
#> 
#> Method
#> ========================
#> [1] "FAMD"
#> 
#> Details
#> ========================
#>                                  Detail
#> 1 Total number of individuals/genotypes
#> 2                   Quantitative traits
#> 3                    Qualitative traits
#> 4                                Method
#> 5                 Threshold eigen value
#> 6       Number of eigen values selected
#> 7                        Threshold size
#> 8                Threshold variance (%)
#>                                                                                                  Value
#> 1                                                                                                 1684
#> 2                                         NMSR, TTRN, TFWSR, TTRW, TFWSS, TTSW, TTPW, AVPW, ARSR, SRDM
#> 3 CUAL, LNGS, PTLC, DSTA, LFRT, LBTEF, CBTR, NMLB, ANGB, CUAL9M, LVC9M, TNPR9M, PL9M, STRP, STRC, PSTR
#> 4                                                                                                 FAMD
#> 5                                                                                              1.63208
#> 6                                                                                                    6
#> 7                                                                                                  0.2
#> 8                                                                                                   75
#> 
#> Core sets
#> =========================
#>                   Method Size   VarRet
#> 1      By size specified  337 44.63182
#> 2  By threshold variance  859 75.00000
#> 3 By logistic regression  412 50.02493

```
