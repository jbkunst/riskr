# riskr
<!-- README.md is generated from README.Rmd -->



## Introduction

The `riskr` package facilitate *credit scoring* tasks such as measure the scores/models performance and make easy the scoring modelling process.

There are function to:

 1. Measure in a simple way the performance of models via wrapper or shortcuts from ROCR functions.
 2. Visualize relationships between variables.
 4. Compute usual values in the credit scoring PSI, WOE, IV, KS, AUCROC, among others.
 5. Make easier the scoring modelling process. 

## Assumption

`riskr` assume the target variable is *binary* with numeric values: 0 and 1. Usually 1 means the characteristic of interest.

## Installation

You can install the latest development version from github with:


```r
devtools::install_github("jbkunst/riskr")
```

## Functions

Usually we have a data frame with a *target* variable and a *score* (or probability) like this:


```r
library("riskr")

data("predictions")

head(predictions)
```



| score| target|
|-----:|------:|
| 0.202|      1|
| 0.806|      1|
| 0.513|      1|
| 0.052|      0|
| 0.329|      1|
| 0.246|      0|

```r

str(predictions)
> 'data.frame':	10000 obs. of  2 variables:
>  $ score : num  0.2023 0.8058 0.5134 0.0525 0.3288 ...
>  $ target: num  1 1 1 0 1 0 1 0 1 1 ...

score <- predictions$score

target <- predictions$target
```

### Performance Indicators

The main statistics or indicators are KS, AUCROC so:


```r
ks(score, target)
> [1] 0.254

aucroc(score, target)
> [1] 0.676

gini(score, target)
> [1] 0.353

score_indicators(score, target)
```



| count| target_count| target_rate|    ks| aucroc|  gini|
|-----:|------------:|-----------:|-----:|------:|-----:|
| 10000|         6990|       0.699| 0.254|  0.676| 0.353|

There are some functions to plot the score/model performance (based on ggplot package).


```r
plot_roc(score, target)
```

<img src="vignettes/figures/unnamed-chunk-5-1.png" title="" alt="" style="display: block; margin: auto;" />

```r

plot_gain(score, target)
```

<img src="vignettes/figures/unnamed-chunk-5-2.png" title="" alt="" style="display: block; margin: auto;" />

```r

plot_ks(1000 * score, target) +
  ggthemes::theme_hc() +
  ggtitle(sprintf("The KS statistics is %.0f%%", 100 * ks(score, target)))
```

<img src="vignettes/figures/unnamed-chunk-5-3.png" title="" alt="" style="display: block; margin: auto;" />

### Odds Tables

The odds tables are other way to show how a score/model performs.


```r
score <- round(predictions$score * 1000)

odds_table(score, target, nclass = 5) # default is (nclass =) 10 groups of equal size
```



|variable  | count| percent| target_count| target_rate| target_percent| odds|
|:---------|-----:|-------:|------------:|-----------:|--------------:|----:|
|[1,164]   |  2009|   0.201|         1010|       0.503|          0.144| 1.01|
|(164,331] |  1991|   0.199|         1255|       0.630|          0.180| 1.71|
|(331,526] |  2008|   0.201|         1429|       0.712|          0.204| 2.47|
|(526,738] |  2000|   0.200|         1573|       0.786|          0.225| 3.68|
|(738,996] |  1992|   0.199|         1723|       0.865|          0.246| 6.41|

```r

odds_table(score, target, breaks = c(0, 300, 700, 999))
```



|variable  | count| percent| target_count| target_rate| target_percent| odds|
|:---------|-----:|-------:|------------:|-----------:|--------------:|----:|
|(0,300]   |  3675|   0.368|         2052|       0.558|          0.294| 1.26|
|(300,700] |  3978|   0.398|         2926|       0.736|          0.419| 2.78|
|(700,999] |  2347|   0.235|         2012|       0.857|          0.288| 6.01|

### Confusion Matrix

The `conf_matrix` function return a list with the next elements:


```r
score_cat <- ifelse(score < 500, 0, 1)

cm <- conf_matrix(score_cat, target)
```

- The confusion matrix:

```r
cm$confusion.matrix
```



|   |class  | pred 0| pred 1|
|:--|:------|------:|------:|
|0  |true 0 |   2230|    780|
|1  |true 1 |   3476|   3514|

- The confusion matrix statistics

```r
cm$indicators
```



|term                                  |term.short | value|
|:-------------------------------------|:----------|-----:|
|Accuracy                              |AC         | 0.574|
|Recall &#124; True Positive rate (GG) |Recall     | 0.503|
|False Positive rate                   |FP         | 0.259|
|True Negative rate (BB)               |TN         | 0.741|
|False Negative rate                   |FN         | 0.497|
|Precision                             |P          | 0.818|

```r

cm$indicators.t
```



|    AC| Recall|    FP|    TN|    FN|     P|
|-----:|------:|-----:|-----:|-----:|-----:|
| 0.574|  0.503| 0.259| 0.741| 0.497| 0.818|


### Bivariate Tables


```r
data("credit")

str(credit)
> 'data.frame':	49694 obs. of  17 variables:
>  $ id_client          : int  1 7 9 12 14 19 22 26 28 30 ...
>  $ sex                : chr  "F" "F" "F" "F" ...
>  $ marital_status     : chr  "O" "S" "S" "C" ...
>  $ age                : int  44 22 27 32 36 46 17 20 71 46 ...
>  $ flag_res_phone     : chr  "N" "Y" "Y" "Y" ...
>  $ area_code_res_phone: int  31 31 31 31 31 50 50 50 31 31 ...
>  $ payment_day        : int  12 12 20 12 12 12 12 12 18 8 ...
>  $ residence_type     : chr  "P" "A" "A" "P" ...
>  $ months_in_residence: int  12 0 0 24 120 360 12 12 96 72 ...
>  $ months_in_the_job  : int  48 48 0 0 36 120 12 24 12 12 ...
>  $ profession_code    : int  731 999 950 165 15 704 38 39 13 801 ...
>  $ flag_other_card    : chr  "N" "N" "N" "N" ...
>  $ flag_mobile_phone  : chr  "N" "N" "N" "N" ...
>  $ flag_contact_phone : chr  "N" "N" "N" "N" ...
>  $ personal_net_income: num  300 410 1000 700 1987 ...
>  $ quant_add_cards    : int  0 0 0 0 1 0 0 0 0 0 ...
>  $ bad                : int  0 0 1 0 0 0 1 1 0 0 ...

ft(credit$marital_status)
```



|class | count| percent|
|:-----|-----:|-------:|
|C     | 17097|   0.344|
|D     |  2142|   0.043|
|O     |  2776|   0.056|
|S     | 25249|   0.508|
|V     |  2430|   0.049|

```r

bt(credit$marital_status, credit$bad)
```



|variable | count| percent| target_count| target_rate| target_percent|  odds|
|:--------|-----:|-------:|------------:|-----------:|--------------:|-----:|
|C        | 17097|   0.344|         2483|       0.145|          0.253| 0.170|
|D        |  2142|   0.043|          322|       0.150|          0.033| 0.177|
|O        |  2776|   0.056|          660|       0.238|          0.067| 0.312|
|S        | 25249|   0.508|         6059|       0.240|          0.617| 0.316|
|V        |  2430|   0.049|          289|       0.119|          0.029| 0.135|

```r

library("ggplot2")

credit$age_bin <- cut_interval(credit$age, 4)

bt(credit$age_bin, credit$bad)
```



|variable | count| percent| target_count| target_rate| target_percent|  odds|
|:--------|-----:|-------:|------------:|-----------:|--------------:|-----:|
|[15,35]  | 28377|   0.571|         7015|       0.247|          0.715| 0.328|
|(35,55]  | 17425|   0.351|         2473|       0.142|          0.252| 0.165|
|(55,75]  |  3767|   0.076|          313|       0.083|          0.032| 0.091|
|(75,95]  |   125|   0.003|           12|       0.096|          0.001| 0.106|

```r

plot_bt(credit$age_bin, credit$bad) + ggtitle("Age")
```

<img src="vignettes/figures/unnamed-chunk-10-1.png" title="" alt="" style="display: block; margin: auto;" />

```r

plot_bt(credit$flag_res_phone, credit$bad,
        count.labels = TRUE, target.labels = TRUE) +
  ggtitle("Flag Response Phone")
```

<img src="vignettes/figures/unnamed-chunk-10-2.png" title="" alt="" style="display: block; margin: auto;" />

## Related work

1. [woe](github.com/tomasgreif/woe) package by [tomasgreif](github.com/tomasgreif)
2. [smbinning](http://cran.r-project.org/web/packages/smbinning) package by [Herman Jopia](github.com/hjopia). [Github repository](https://github.com/cran/smbinning).
