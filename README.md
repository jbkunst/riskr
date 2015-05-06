<!-- README.md is generated from README.Rmd. Please edit that file -->
riskr
=====

riskr is the next ite

You can install the latest development version from github with

``` r
devtools::install_github("jbkunst/riskr")
```

Some functions
--------------

### Odds Table

``` r
library("riskr")

data("predictions")

score <- round(predictions$score * 1000)
target <- predictions$target
```

``` r
odds_table(score, target, nclass = 4)
```

| class     |  count|  percent|  count\_target|  rate\_target|  percent\_target|      odds|
|:----------|------:|--------:|--------------:|-------------:|----------------:|---------:|
| [1,199]   |   2502|   0.2502|           1304|     0.5211831|        0.1865522|  1.088481|
| (199,430] |   2504|   0.2504|           1661|     0.6633387|        0.2376252|  1.970344|
| (430,683] |   2502|   0.2502|           1894|     0.7569944|        0.2709585|  3.115132|
| (683,996] |   2492|   0.2492|           2131|     0.8551364|        0.3048641|  5.903047|
