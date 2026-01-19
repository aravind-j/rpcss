# Fetch the names of individuals/genotypes in the core set generated from `pcss.core` Output

`subset.pcss.core` returns names of individuals/genotypes in the core
collection from `pcss.core` Output.

## Usage

``` r
# S3 method for class 'pcss.core'
subset(x, criterion = c("size", "variance", "logistic"), ...)
```

## Arguments

- x:

  An object of class `pcss.core`.

- criterion:

  The core collection generation criterion. Either `"size"`,
  `"variance"`, or `"logistic"`. See **Details**.

- ...:

  Unused.

## Value

The names of individuals/genotypes in the core collection as a character
vector.

## Details

Use `"size"` to return names of individuals/genotypes in the core
collection according to the threshold `size` criterion or use
`"variance"` to return names according to the variability threshold
criterion or use `"logistic"` to return names according to inflection
point of rate of progress of cumulative variability retained identified
by logistic regression.

## See also

[`pcss.core`](https://aravind-j.github.io/rpcss/reference/pcss.core.md)

## Examples

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare example data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (requireNamespace('EvaluateCore', quietly = TRUE)) {

  suppressPackageStartupMessages(library(EvaluateCore))

  # Get data from EvaluateCore

  data("cassava_EC", package = "EvaluateCore")
  data = cbind(Genotypes = rownames(cassava_EC), cassava_EC)
  quant <- c("NMSR", "TTRN", "TFWSR", "TTRW", "TFWSS", "TTSW", "TTPW",
             "AVPW", "ARSR", "SRDM")
  qual <- c("CUAL", "LNGS", "PTLC", "DSTA", "LFRT", "LBTEF", "CBTR", "NMLB",
            "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
            "PSTR")
  rownames(data) <- NULL

  # Convert qualitative data columns to factor
  data[, qual] <- lapply(data[, qual], as.factor)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# With quantitative data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  out1 <- pcss.core(data = data, names = "Genotypes",
                    quantitative = quant,
                    qualitative = NULL, eigen.threshold = NULL, size = 0.2,
                    var.threshold = 0.75)

  # Core sets
  out1$cores.info

  # Fetch genotype names of core set by size criterion
  subset(x = out1, criterion = "size")

  # Fetch genotype names of core set by variance criterion
  subset(x = out1, criterion = "variance")

  # Fetch genotype names of core set by logistic regression criterion
  subset(x = out1, criterion = "logistic")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get core sets with PCSS (qualitative data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  out2 <- pcss.core(data = data, names = "Genotypes", quantitative = NULL,
                    qualitative = qual, eigen.threshold = NULL,
                    size = 0.2, var.threshold = 0.75)

  # Core sets
  out2$cores.info

  # Fetch genotype names of core set by size criterion
  subset(x = out2, criterion = "size")

  # Fetch genotype names of core set by variance criterion
  subset(x = out2, criterion = "variance")

  # Fetch genotype names of core set by logistic regression criterion
  subset(x = out2, criterion = "logistic")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get core sets with PCSS (quantitative and qualitative data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  out3 <- pcss.core(data = data, names = "Genotypes",
                    quantitative = quant,
                    qualitative = qual, eigen.threshold = NULL)

  # Core sets
  out3$cores.info

  # Fetch genotype names of core set by size criterion
  subset(x = out3, criterion = "size")

  # Fetch genotype names of core set by variance criterion
  subset(x = out3, criterion = "variance")

  # Fetch genotype names of core set by logistic regression criterion
  subset(x = out3, criterion = "logistic")

} else {
  message('Package "EvaluateCore" is required to run these examples.')
}
#>   [1] "TMe-3163" "TMe-603"  "TMe-3685" "TMe-3223" "TMe-2967" "TMe-3667"
#>   [7] "TMe-3736" "TMe-3292" "TMe-399"  "TMe-3605" "TMe-3705" "TMe-3800"
#>  [13] "TMe-3475" "TMe-2604" "TMe-3573" "TMe-3628" "TMe-616"  "TMe-1985"
#>  [19] "TMe-3392" "TMe-2943" "TMe-3065" "TMe-3396" "TMe-3319" "TMe-901" 
#>  [25] "TMe-2996" "TMe-2853" "TMe-2050" "TMe-3730" "TMe-3249" "TMe-3095"
#>  [31] "TMe-1769" "TMe-3387" "TMe-3398" "TMe-3701" "TMe-2035" "TMe-1730"
#>  [37] "TMe-390"  "TMe-3466" "TMe-812"  "TMe-761"  "TMe-41"   "TMe-608" 
#>  [43] "TMe-3549" "TMe-3281" "TMe-3323" "TMe-731"  "TMe-2531" "TMe-3266"
#>  [49] "TMe-3116" "TMe-2064" "TMe-3115" "TMe-3282" "TMe-696"  "TMe-756" 
#>  [55] "TMe-1416" "TMe-707"  "TMe-3264" "TMe-1661" "TMe-3389" "TMe-3694"
#>  [61] "TMe-3766" "TMe-13"   "TMe-2033" "TMe-3141" "TMe-3297" "TMe-2983"
#>  [67] "TMe-867"  "TMe-412"  "TMe-1239" "TMe-3140" "TMe-1723" "TMe-2352"
#>  [73] "TMe-2010" "TMe-2196" "TMe-138"  "TMe-1403" "TMe-2993" "TMe-606" 
#>  [79] "TMe-798"  "TMe-1919" "TMe-3252" "TMe-2195" "TMe-1124" "TMe-1294"
#>  [85] "TMe-1232" "TMe-717"  "TMe-3437" "TMe-997"  "TMe-2985" "TMe-1506"
#>  [91] "TMe-1307" "TMe-929"  "TMe-3054" "TMe-373"  "TMe-2308" "TMe-2963"
#>  [97] "TMe-1518" "TMe-1902" "TMe-35"   "TMe-620"  "TMe-2045" "TMe-2913"
#> [103] "TMe-241"  "TMe-2955" "TMe-1809" "TMe-588"  "TMe-3641" "TMe-514" 
#> [109] "TMe-3330" "TMe-1608" "TMe-1283" "TMe-1011" "TMe-2952" "TMe-1428"
#> [115] "TMe-1744" "TMe-3025" "TMe-1564" "TMe-3314" "TMe-1383" "TMe-527" 
#> [121] "TMe-3089" "TMe-432"  "TMe-2510" "TMe-623"  "TMe-3272" "TMe-1261"
#> [127] "TMe-705"  "TMe-3805" "TMe-3040" "TMe-465"  "TMe-3690" "TMe-3034"
#> [133] "TMe-3698" "TMe-728"  "TMe-2441" "TMe-3485" "TMe-2439" "TMe-1339"
#> [139] "TMe-86"   "TMe-3581" "TMe-1158" "TMe-2204" "TMe-427"  "TMe-1633"
#> [145] "TMe-659"  "TMe-751"  "TMe-725"  "TMe-3415" "TMe-861"  "TMe-815" 
#> [151] "TMe-421"  "TMe-2329" "TMe-2916" "TMe-1137" "TMe-2043" "TMe-289" 
#> [157] "TMe-93"   "TMe-3467" "TMe-3200" "TMe-3382" "TMe-3406" "TMe-85"  
#> [163] "TMe-1945" "TMe-27"   "TMe-2968" "TMe-1646" "TMe-1992" "TMe-3592"
#> [169] "TMe-1738" "TMe-1248" "TMe-2897" "TMe-5"    "TMe-267"  "TMe-584" 
#> [175] "TMe-3401" "TMe-3230" "TMe-926"  "TMe-3234" "TMe-2733" "TMe-3222"
#> [181] "TMe-3071" "TMe-3565" "TMe-3046" "TMe-3571" "TMe-832"  "TMe-3007"
#> [187] "TMe-1004" "TMe-2984" "TMe-3596" "TMe-2751" "TMe-1775" "TMe-2843"
#> [193] "TMe-925"  "TMe-3299" "TMe-3329" "TMe-1388" "TMe-1273" "TMe-3721"
#> [199] "TMe-3663" "TMe-1098" "TMe-7"    "TMe-2906" "TMe-2151" "TMe-2940"
#> [205] "TMe-2756" "TMe-745"  "TMe-3572" "TMe-3451" "TMe-742"  "TMe-3130"
#> [211] "TMe-3659" "TMe-3277" "TMe-473"  "TMe-2995" "TMe-2998" "TMe-3493"
#> [217] "TMe-2976" "TMe-863"  "TMe-3087" "TMe-2802" "TMe-3066" "TMe-3302"
#> [223] "TMe-835"  "TMe-1579" "TMe-698"  "TMe-2914" "TMe-1600" "TMe-1580"
#> [229] "TMe-64"   "TMe-842"  "TMe-3631" "TMe-3346" "TMe-3085" "TMe-3255"
#> [235] "TMe-3544" "TMe-853"  "TMe-1817" "TMe-438"  "TMe-3533" "TMe-589" 
#> [241] "TMe-886"  "TMe-2862" "TMe-2119" "TMe-365"  "TMe-3207" "TMe-2811"
#> [247] "TMe-2361" "TMe-44"   "TMe-2355" "TMe-1160" "TMe-3599" "TMe-3531"
#> [253] "TMe-39"   "TMe-635"  "TMe-1218" "TMe-6"    "TMe-1725" "TMe-3498"
#> [259] "TMe-1286" "TMe-3238" "TMe-3118" "TMe-3352" "TMe-1958" "TMe-1398"
#> [265] "TMe-694"  "TMe-1401" "TMe-2981" "TMe-203"  "TMe-773"  "TMe-2901"
#> [271] "TMe-304"  "TMe-3101" "TMe-1444" "TMe-1792" "TMe-3088" "TMe-2040"
#> [277] "TMe-3133" "TMe-1079" "TMe-2912" "TMe-34"   "TMe-3575" "TMe-3608"
#> [283] "TMe-312"  "TMe-787"  "TMe-3440" "TMe-3433" "TMe-700"  "TMe-601" 
#> [289] "TMe-2551" "TMe-2004" "TMe-394"  "TMe-2518" "TMe-946"  "TMe-3707"
#> [295] "TMe-1838" "TMe-3026" "TMe-2374" "TMe-3434" "TMe-2748" "TMe-617" 
#> [301] "TMe-2910" "TMe-3043" "TMe-400"  "TMe-2966" "TMe-1875" "TMe-976" 
#> [307] "TMe-3112" "TMe-360"  "TMe-2860" "TMe-3480" "TMe-3209" "TMe-2957"
#> [313] "TMe-196"  "TMe-3569" "TMe-2158" "TMe-2304" "TMe-363"  "TMe-1472"
#> [319] "TMe-3772" "TMe-43"   "TMe-1461" "TMe-1988" "TMe-3055" "TMe-15"  
#> [325] "TMe-656"  "TMe-434"  "TMe-3324" "TMe-391"  "TMe-1147" "TMe-33"  
#> [331] "TMe-154"  "TMe-3443" "TMe-148"  "TMe-59"   "TMe-3601" "TMe-1198"
#> [337] "TMe-3114" "TMe-3032" "TMe-160"  "TMe-298"  "TMe-1311" "TMe-3185"
#> [343] "TMe-2128" "TMe-3458" "TMe-3781" "TMe-1819" "TMe-830"  "TMe-1814"
#> [349] "TMe-1787" "TMe-3148" "TMe-3359" "TMe-3357" "TMe-340"  "TMe-2814"
#> [355] "TMe-2530" "TMe-47"   "TMe-645"  "TMe-2383" "TMe-2809" "TMe-2973"
#> [361] "TMe-1622" "TMe-1026" "TMe-3639" "TMe-729"  "TMe-25"   "TMe-3633"
#> [367] "TMe-2937" "TMe-2226" "TMe-404"  "TMe-1006" "TMe-1101" "TMe-3445"
#> [373] "TMe-383"  "TMe-4"    "TMe-2997" "TMe-2285" "TMe-1074" "TMe-1272"
#> [379] "TMe-3326" "TMe-477"  "TMe-1250" "TMe-3383" "TMe-1312" "TMe-736" 
#> [385] "TMe-3284" "TMe-280"  "TMe-995"  "TMe-1505" "TMe-3256" "TMe-878" 
#> [391] "TMe-930"  "TMe-2257" "TMe-826"  "TMe-3679" "TMe-1420" "TMe-1521"
#> [397] "TMe-1766" "TMe-1042" "TMe-3020" "TMe-3128" "TMe-279"  "TMe-123" 
#> [403] "TMe-3557" "TMe-1762" "TMe-2217" "TMe-1227" "TMe-1790" "TMe-600" 
#> [409] "TMe-2956" "TMe-754"  "TMe-3481" "TMe-2242"
```
