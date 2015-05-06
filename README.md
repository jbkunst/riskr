<!-- README.md is generated from README.Rmd. Please edit that file -->
riskr
=====

riskr is the next ite

You can install the latest development version from github with

    ```R
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

knitr::kable(odds_table(score, target, nclass = 4))
```

| class     |  count|  percent|  count\_target|  rate\_target|  percent\_target|      odds|
|:----------|------:|--------:|--------------:|-------------:|----------------:|---------:|
| [1,199]   |   2502|   0.2502|           1304|     0.5211831|        0.1865522|  1.088481|
| (199,430] |   2504|   0.2504|           1661|     0.6633387|        0.2376252|  1.970344|
| (430,683] |   2502|   0.2502|           1894|     0.7569944|        0.2709585|  3.115132|
| (683,996] |   2492|   0.2492|           2131|     0.8551364|        0.3048641|  5.903047|

``` r

knitr::kable(odds_table(score, target, nclass = 4, quantile = FALSE))
```

| class     |  count|  percent|  count\_target|  rate\_target|  percent\_target|      odds|
|:----------|------:|--------:|--------------:|-------------:|----------------:|---------:|
| [1,250]   |   3111|   0.3111|           1686|     0.5419479|        0.2412017|  1.183158|
| (250,498] |   2586|   0.2586|           1782|     0.6890951|        0.2549356|  2.216418|
| (498,747] |   2403|   0.2403|           1874|     0.7798585|        0.2680973|  3.542533|
| (747,996] |   1900|   0.1900|           1648|     0.8673684|        0.2357654|  6.539682|

``` r

knitr::kable(odds_table(score, target, breaks = c(-Inf, 250, 750, Inf)))
```

| class      |  count|  percent|  count\_target|  rate\_target|  percent\_target|      odds|
|:-----------|------:|--------:|--------------:|-------------:|----------------:|---------:|
| (-Inf,250] |   3118|   0.3118|           1691|     0.5423348|        0.2419170|  1.185004|
| (250,750]  |   5005|   0.5005|           3669|     0.7330669|        0.5248927|  2.746258|
| (750, Inf] |   1877|   0.1877|           1630|     0.8684070|        0.2331903|  6.599190|
