---
title: "Morts Collisions"
date: "2025-08-01"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
---

# Importations / nettoyage


``` r
library(ggfortify)
library(lmtest)
library(dplyr)
library(ggplot2)
```



``` r
wd <- set_wd()
```



``` r
Total_et_Diro <- readr::read_csv(paste0(wd$data, 
                                        "derived/Total_et_Diro.csv"),
                                 locale = readr::locale(encoding = "UTF-8"))
```

```
## New names:
## Rows: 966902 Columns: 16
## ── Column specification
## ───────────────────────────────────────────────────────────────────────────────────────────── Delimiter: "," chr
## (5): Code_10km, paysage_ID, paysage_nom, famille_paysage, etat_biologique dbl (10): ...1, X_10km, Y_10km, cd_nom,
## cd_nom_grp_must, Indice_Diversite, Densite_Cultures, Distance_EcotoneArb... date (1): date
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ Specify the column types or set
## `show_col_types = FALSE` to quiet this message.
## • `` -> `...1`
```

``` r
Total_et_Diro <- Total_et_Diro %>%
  dplyr::select(-`...1`)
```

# Mortalit?s routi?res - Toutes donn?es

``` r
Donnees <- Total_et_Diro %>%
  dplyr::filter(etat_biologique == "Trouvé mort : impact routier")
```

## Lapin

### Lapin - Sanglier

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 61714,
                   espece_benchmark = 60981)
```



``` r
liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -465.507
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + X_10km + Ind_Diversite + Dnst_Cultures + Dist_Ecotone + Dist_Littoral + prop_tmoins1 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Ind_Diversite + 
##     Dnst_Cultures + Dist_Ecotone + Dist_Littoral + prop_tmoins1 + 
##     year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.288e+04  2.712e+03   4.750 2.26e-06 ***
## year          -1.272e+01  2.688e+00  -4.733 2.46e-06 ***
## X_10km        -1.160e-01  1.454e-02  -7.979 3.25e-15 ***
## Ind_Diversite  4.460e-01  9.458e-02   4.716 2.67e-06 ***
## Dnst_Cultures  5.897e-01  1.573e-01   3.750 0.000185 ***
## Dist_Ecotone   5.285e-03  8.419e-04   6.277 4.70e-10 ***
## Dist_Littoral  4.456e-03  7.821e-04   5.698 1.50e-08 ***
## prop_tmoins1   1.716e-01  2.377e-02   7.219 8.94e-13 ***
## year2          3.141e-03  6.661e-04   4.715 2.68e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1173963)
## 
##     Null deviance: 227.64  on 1297  degrees of freedom
## Residual deviance: 151.32  on 1289  degrees of freedom
## AIC: 913.96
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 6.9345, df = 1, p-value = 0.008455
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 31.809, df = 8, p-value = 0.0001008
```

```
##          year        X_10km Ind_Diversite Dnst_Cultures  Dist_Ecotone Dist_Littoral  prop_tmoins1         year2 
##  1.201113e+06  1.941869e+00  2.948496e+00  2.462200e+00  1.103397e+00  2.281362e+00  1.169420e+00  1.201139e+06
```


### Lapin - Renard


``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 61714,
                   espece_benchmark = 60585)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "qualitative"
## 
## $min_bic
## [1] -719.632
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)



``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year   + Dist_Littoral + prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Dist_Littoral + prop_tmoins1 + 
##     prop_tmoins2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    0.1338852  0.0389847   3.434 0.000606 ***
## year2011       0.0092822  0.0475941   0.195 0.845390    
## year2012      -0.0541655  0.0476910  -1.136 0.256194    
## year2013      -0.1190101  0.0479806  -2.480 0.013206 *  
## year2014       0.1500649  0.0472520   3.176 0.001517 ** 
## year2015       0.1849145  0.0490890   3.767 0.000170 ***
## year2016       0.0010400  0.0497601   0.021 0.983327    
## year2017       0.0184133  0.0499463   0.369 0.712419    
## year2018       0.0443687  0.0500759   0.886 0.375709    
## year2019       0.0727563  0.0511132   1.423 0.154766    
## year2020       0.0976862  0.0514856   1.897 0.057926 .  
## year2021       0.0279876  0.0525967   0.532 0.594704    
## year2022      -0.0022390  0.0515566  -0.043 0.965365    
## year2023       0.0228052  0.0526237   0.433 0.664798    
## year2024      -0.0644338  0.0523166  -1.232 0.218239    
## year2025      -0.3346137  0.0687775  -4.865 1.23e-06 ***
## Dist_Littoral  0.0017440  0.0004679   3.727 0.000199 ***
## prop_tmoins1   0.3536948  0.0234982  15.052  < 2e-16 ***
## prop_tmoins2   0.2329948  0.0250089   9.316  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.129139)
## 
##     Null deviance: 383.87  on 2014  degrees of freedom
## Residual deviance: 257.76  on 1996  degrees of freedom
## AIC: 1614.8
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 3.1589, df = 1, p-value = 0.07551
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 54.598, df = 18, p-value = 1.48e-05
```

```
##                    GVIF Df GVIF^(1/(2*Df))
## year          0.6765112 15       0.9870576
## Dist_Littoral 2.6024634  1       1.6132152
## prop_tmoins1  2.7182400  1       1.6487086
## prop_tmoins2  2.7240603  1       1.6504727
```

### Lapin - Chevreuil


``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 61714,
                   espece_benchmark = 61057)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -446.8622
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + X_10km  + Dnst_Cultures + Dist_Ecotone  + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dnst_Cultures + 
##     Dist_Ecotone + prop_tmoins1 + prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    2.098e+04  2.371e+03   8.849  < 2e-16 ***
## year          -2.075e+01  2.350e+00  -8.833  < 2e-16 ***
## X_10km        -4.135e-02  1.024e-02  -4.039 5.63e-05 ***
## Dnst_Cultures  2.390e-01  9.803e-02   2.438  0.01488 *  
## Dist_Ecotone   5.928e-03  7.602e-04   7.798 1.18e-14 ***
## prop_tmoins1   1.501e-01  2.427e-02   6.183 8.12e-10 ***
## prop_tmoins2   8.563e-02  2.524e-02   3.393  0.00071 ***
## year2          5.133e-03  5.823e-04   8.816  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1074528)
## 
##     Null deviance: 222.83  on 1482  degrees of freedom
## Residual deviance: 158.49  on 1475  degrees of freedom
## AIC: 910.42
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 12.827, df = 1, p-value = 0.0003416
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 42.784, df = 7, p-value = 3.672e-07
```

```
##          year        X_10km Dnst_Cultures  Dist_Ecotone  prop_tmoins1  prop_tmoins2         year2 
##  1.150401e+06  1.189791e+00  1.198093e+00  1.075757e+00  1.165040e+00  1.190092e+00  1.150371e+06
```

### Lapin - Blaireau


``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 61714,
                   espece_benchmark = 60636)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -385.5912
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png)



``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year  + Ind_Diversite + Dist_Littoral + prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Ind_Diversite + Dist_Littoral + 
##     prop_tmoins1 + prop_tmoins2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   35.6849378  2.7279335  13.081  < 2e-16 ***
## year          -0.0177765  0.0013522 -13.146  < 2e-16 ***
## Ind_Diversite  0.1371118  0.0403605   3.397 0.000692 ***
## Dist_Littoral  0.0016922  0.0003863   4.381 1.23e-05 ***
## prop_tmoins1   0.1887340  0.0197779   9.543  < 2e-16 ***
## prop_tmoins2   0.1964484  0.0201291   9.759  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.08111557)
## 
##     Null deviance: 237.47  on 2461  degrees of freedom
## Residual deviance: 199.22  on 2456  degrees of freedom
## AIC: 810.6
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 4.6189, df = 1, p-value = 0.03162
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 254.33, df = 5, p-value < 2.2e-16
```

```
##          year Ind_Diversite Dist_Littoral  prop_tmoins1  prop_tmoins2 
##      1.011351      1.401660      1.420254      1.109175      1.109988
```

## Herisson

### Herisson - Sanglier

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60015,
                   espece_benchmark = 60981)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -580.384
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year  + X_10km + Dnst_Cultures + Dist_Ecotone + Dist_Littoral + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dnst_Cultures + 
##     Dist_Ecotone + Dist_Littoral + prop_tmoins1 + prop_tmoins2 + 
##     year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.864e+04  1.313e+03  14.197  < 2e-16 ***
## year          -1.846e+01  1.301e+00 -14.184  < 2e-16 ***
## X_10km        -3.409e-02  7.804e-03  -4.368 1.30e-05 ***
## Dnst_Cultures -3.038e-01  6.001e-02  -5.061 4.44e-07 ***
## Dist_Ecotone   1.833e-03  3.598e-04   5.095 3.72e-07 ***
## Dist_Littoral  1.709e-03  3.937e-04   4.340 1.48e-05 ***
## prop_tmoins1   2.154e-01  1.552e-02  13.882  < 2e-16 ***
## prop_tmoins2   6.566e-02  1.510e-02   4.350 1.41e-05 ***
## year2          4.569e-03  3.225e-04  14.171  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.07432806)
## 
##     Null deviance: 257.42  on 2738  degrees of freedom
## Residual deviance: 202.92  on 2730  degrees of freedom
## AIC: 664.54
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-30](figure/unnamed-chunk-30-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 25.174, df = 1, p-value = 5.238e-07
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 269.85, df = 8, p-value < 2.2e-16
```

```
##          year        X_10km Dnst_Cultures  Dist_Ecotone Dist_Littoral  prop_tmoins1  prop_tmoins2         year2 
##  1.216228e+06  2.011004e+00  1.297551e+00  1.143132e+00  1.949767e+00  1.374611e+00  1.624175e+00  1.215901e+06
```



### Herisson - Renard


``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60015,
                   espece_benchmark = 60585)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "qualitative"
## 
## $min_bic
## [1] -209.4285
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-33](figure/unnamed-chunk-33-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year  + X_10km + Y_10km + Dist_Ecotone + prop_tmoins1 + prop_tmoins2 )
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Y_10km + Dist_Ecotone + 
##     prop_tmoins1 + prop_tmoins2, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   4.5266990  0.9577796   4.726 2.39e-06 ***
## year2011      0.0116033  0.0391522   0.296 0.766973    
## year2012      0.0061287  0.0395622   0.155 0.876901    
## year2013     -0.1069756  0.0409692  -2.611 0.009070 ** 
## year2014     -0.0192134  0.0404588  -0.475 0.634902    
## year2015     -0.0143612  0.0424891  -0.338 0.735389    
## year2016     -0.0660909  0.0430372  -1.536 0.124726    
## year2017      0.0098551  0.0423201   0.233 0.815878    
## year2018      0.0814820  0.0415810   1.960 0.050136 .  
## year2019      0.1545651  0.0417786   3.700 0.000220 ***
## year2020      0.1143825  0.0427767   2.674 0.007537 ** 
## year2021      0.1277271  0.0431242   2.962 0.003082 ** 
## year2022      0.1144934  0.0429157   2.668 0.007675 ** 
## year2023      0.0958650  0.0435086   2.203 0.027646 *  
## year2024      0.0719818  0.0430337   1.673 0.094495 .  
## year2025      0.0798604  0.0480563   1.662 0.096656 .  
## X_10km        0.0282742  0.0072479   3.901 9.79e-05 ***
## Y_10km       -0.0819416  0.0200027  -4.097 4.31e-05 ***
## Dist_Ecotone  0.0014384  0.0004189   3.434 0.000604 ***
## prop_tmoins1  0.1009301  0.0181757   5.553 3.05e-08 ***
## prop_tmoins2  0.0599650  0.0182682   3.282 0.001041 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.122626)
## 
##     Null deviance: 407.63  on 3003  degrees of freedom
## Residual deviance: 365.79  on 2983  degrees of freedom
## AIC: 2243.7
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-35](figure/unnamed-chunk-35-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 4.7931, df = 1, p-value = 0.02857
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 153.12, df = 20, p-value < 2.2e-16
```

```
##                    GVIF Df GVIF^(1/(2*Df))
## year         -0.3860833 15             NaN
## X_10km       -3.0725061  1             NaN
## Y_10km       -2.6609312  1             NaN
## Dist_Ecotone -2.6754295  1             NaN
## prop_tmoins1 -3.9405793  1             NaN
## prop_tmoins2 -4.5285759  1             NaN
```


### Herisson - Chevreuil

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60015,
                   espece_benchmark = 61057)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -859.524
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-37-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-38](figure/unnamed-chunk-38-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year   + Dnst_Cultures + Dist_Ecotone + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Dnst_Cultures + Dist_Ecotone + 
##     prop_tmoins1 + prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    2.673e+04  1.395e+03  19.163  < 2e-16 ***
## year          -2.648e+01  1.383e+00 -19.149  < 2e-16 ***
## Dnst_Cultures -2.515e-01  5.890e-02  -4.270 2.02e-05 ***
## Dist_Ecotone   2.605e-03  3.802e-04   6.851 8.94e-12 ***
## prop_tmoins1   2.873e-01  1.622e-02  17.710  < 2e-16 ***
## prop_tmoins2   7.908e-02  1.609e-02   4.914 9.41e-07 ***
## year2          6.556e-03  3.426e-04  19.135  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.0929708)
## 
##     Null deviance: 363.88  on 2843  degrees of freedom
## Residual deviance: 263.76  on 2837  degrees of freedom
## AIC: 1324.1
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-40](figure/unnamed-chunk-40-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 58.427, df = 1, p-value = 2.11e-14
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 219.7, df = 6, p-value < 2.2e-16
```

```
##          year Dnst_Cultures  Dist_Ecotone  prop_tmoins1  prop_tmoins2         year2 
##  1.122100e+06  1.028890e+00  1.028776e+00  1.350727e+00  1.513219e+00  1.121935e+06
```


### Herisson - Blaireau

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60015,
                   espece_benchmark = 60636)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -323.177
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-42](figure/unnamed-chunk-42-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-43](figure/unnamed-chunk-43-1.png)



``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + X_10km + Dnst_Cultures + Dist_Ecotone + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dnst_Cultures + 
##     Dist_Ecotone + prop_tmoins1 + prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.283e+04  1.507e+03   8.512  < 2e-16 ***
## year          -1.270e+01  1.494e+00  -8.500  < 2e-16 ***
## X_10km         5.363e-02  7.678e-03   6.985 3.44e-12 ***
## Dnst_Cultures -5.726e-01  7.256e-02  -7.891 4.05e-15 ***
## Dist_Ecotone   2.475e-03  4.214e-04   5.873 4.69e-09 ***
## prop_tmoins1   1.533e-01  1.681e-02   9.119  < 2e-16 ***
## prop_tmoins2   5.658e-02  1.709e-02   3.310 0.000944 ***
## year2          3.142e-03  3.702e-04   8.488  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1349149)
## 
##     Null deviance: 498.88  on 3294  degrees of freedom
## Residual deviance: 443.47  on 3287  degrees of freedom
## AIC: 2760.5
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-45](figure/unnamed-chunk-45-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 14.441, df = 1, p-value = 0.0001446
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 27.444, df = 7, p-value = 0.0002772
```

```
##          year        X_10km Dnst_Cultures  Dist_Ecotone  prop_tmoins1  prop_tmoins2         year2 
##  1.080092e+06  1.290904e+00  1.214899e+00  1.138351e+00  1.110128e+00  1.189957e+00  1.079997e+06
```


## Putois

### Putois - Sanglier

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60731,
                   espece_benchmark = 60981)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -252.7188
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-47](figure/unnamed-chunk-47-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-48](figure/unnamed-chunk-48-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year  + X_10km + Dist_Ecotone + prop_tmoins1 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dist_Ecotone + 
##     prop_tmoins1 + year2, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   3.065e+04  3.393e+03   9.035  < 2e-16 ***
## year         -3.035e+01  3.362e+00  -9.026  < 2e-16 ***
## X_10km       -7.974e-02  1.337e-02  -5.966 3.32e-09 ***
## Dist_Ecotone  1.693e-03  1.019e-03   1.661    0.097 .  
## prop_tmoins1  2.228e-01  3.075e-02   7.246 8.29e-13 ***
## year2         7.511e-03  8.330e-04   9.017  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1445097)
## 
##     Null deviance: 201.10  on 1059  degrees of freedom
## Residual deviance: 152.31  on 1054  degrees of freedom
## AIC: 965.66
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-50](figure/unnamed-chunk-50-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 3.8472, df = 1, p-value = 0.04983
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 29.957, df = 5, p-value = 1.504e-05
```

```
##         year       X_10km Dist_Ecotone prop_tmoins1        year2 
## 1.145774e+06 1.098791e+00 1.032899e+00 1.052894e+00 1.145752e+06
```

### Putois - Renard


``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60731,
                   espece_benchmark = 60585)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "qualitative"
## 
## $min_bic
## [1] -60.71499
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-52](figure/unnamed-chunk-52-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-53](figure/unnamed-chunk-53-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year   + prop_tmoins1)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + prop_tmoins1, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.100000   0.037074   2.697  0.00706 ** 
## year2011     -0.021091   0.046481  -0.454  0.65007    
## year2012     -0.014730   0.046348  -0.318  0.75066    
## year2013     -0.044214   0.046833  -0.944  0.34527    
## year2014      0.078493   0.046497   1.688  0.09157 .  
## year2015      0.081432   0.050345   1.617  0.10596    
## year2016      0.119862   0.048878   2.452  0.01430 *  
## year2017      0.003373   0.050618   0.067  0.94688    
## year2018      0.088713   0.049791   1.782  0.07497 .  
## year2019      0.100960   0.052074   1.939  0.05270 .  
## year2020      0.172509   0.053208   3.242  0.00121 ** 
## year2021      0.173524   0.052765   3.289  0.00103 ** 
## year2022      0.292060   0.049710   5.875 5.08e-09 ***
## year2023      0.178150   0.052209   3.412  0.00066 ***
## year2024      0.066053   0.051030   1.294  0.19570    
## year2025      0.016859   0.062925   0.268  0.78880    
## prop_tmoins1  0.079268   0.027340   2.899  0.00379 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.115454)
## 
##     Null deviance: 209.59  on 1690  degrees of freedom
## Residual deviance: 193.27  on 1674  degrees of freedom
## AIC: 1167.1
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-55](figure/unnamed-chunk-55-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 1.1152, df = 1, p-value = 0.291
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 111.97, df = 16, p-value < 2.2e-16
```

```
##                  GVIF Df GVIF^(1/(2*Df))
## year         0.533688 15       0.9792861
## prop_tmoins1 0.533688  1       0.7305395
```


### Putois - Chevreuil

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60731,
                   espece_benchmark = 61057)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -243.655
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-57](figure/unnamed-chunk-57-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-58](figure/unnamed-chunk-58-1.png)



``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year   + Dist_Ecotone + Dist_Littoral + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Dist_Ecotone + Dist_Littoral + 
##     prop_tmoins1 + prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    2.443e+04  2.619e+03   9.329  < 2e-16 ***
## year          -2.419e+01  2.595e+00  -9.322  < 2e-16 ***
## Dist_Ecotone   2.440e-03  8.034e-04   3.037  0.00243 ** 
## Dist_Littoral -2.147e-03  5.161e-04  -4.159 3.38e-05 ***
## prop_tmoins1   2.554e-01  2.910e-02   8.774  < 2e-16 ***
## prop_tmoins2   1.006e-01  3.320e-02   3.032  0.00247 ** 
## year2          5.989e-03  6.430e-04   9.315  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1185592)
## 
##     Null deviance: 205.78  on 1416  degrees of freedom
## Residual deviance: 167.17  on 1410  degrees of freedom
## AIC: 1008.7
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-60](figure/unnamed-chunk-60-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 10.113, df = 1, p-value = 0.001472
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 88.521, df = 6, p-value < 2.2e-16
```

```
##          year  Dist_Ecotone Dist_Littoral  prop_tmoins1  prop_tmoins2         year2 
##  1.115341e+06  1.009039e+00  1.038932e+00  1.118044e+00  1.111462e+00  1.115320e+06
```


### Putois - Blaireau

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60731,
                   espece_benchmark = 60636)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -8.083715
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-62](figure/unnamed-chunk-62-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-63](figure/unnamed-chunk-63-1.png)



``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year   + prop_tmoins1 )
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + prop_tmoins1, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  11.027184   2.266871   4.864 1.22e-06 ***
## year         -0.005422   0.001123  -4.828 1.47e-06 ***
## prop_tmoins1  0.061226   0.021283   2.877  0.00405 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.05335957)
## 
##     Null deviance: 128.84  on 2385  degrees of freedom
## Residual deviance: 127.16  on 2383  degrees of freedom
## AIC: -216.48
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-65](figure/unnamed-chunk-65-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 8.3756, df = 1, p-value = 0.003803
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 22.995, df = 2, p-value = 1.016e-05
```

```
##         year prop_tmoins1 
##            1            1
```


## Hermine

### Hermine - Sanglier

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60686,
                   espece_benchmark = 60981)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -67.65313
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-67](figure/unnamed-chunk-67-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-68](figure/unnamed-chunk-68-1.png)



``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + X_10km  + Dist_Littoral + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dist_Littoral + 
##     year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.179e+04  2.548e+03   4.626 4.35e-06 ***
## year          -1.166e+01  2.524e+00  -4.619 4.49e-06 ***
## X_10km        -4.401e-02  9.883e-03  -4.453 9.68e-06 ***
## Dist_Littoral  8.823e-04  4.735e-04   1.863   0.0628 .  
## year2          2.883e-03  6.251e-04   4.613 4.63e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.03627921)
## 
##     Null deviance: 32.871  on 803  degrees of freedom
## Residual deviance: 28.987  on 799  degrees of freedom
## AIC: -377.84
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-70](figure/unnamed-chunk-70-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 0.63783, df = 1, p-value = 0.4245
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 87.413, df = 4, p-value < 2.2e-16
```

```
##          year        X_10km Dist_Littoral         year2 
##  1.515306e+06  1.742716e+00  1.736049e+00  1.515317e+06
```


### Hermine - Renard

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60686,
                   espece_benchmark = 60585)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "qualitative"
## 
## $min_bic
## [1] -3.300864
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-72](figure/unnamed-chunk-72-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-73](figure/unnamed-chunk-73-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year   + prop_tmoins1)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + prop_tmoins1, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)   0.021097   0.018638   1.132  0.25784   
## year2011     -0.020977   0.023313  -0.900  0.36839   
## year2012     -0.017705   0.023222  -0.762  0.44593   
## year2013     -0.019022   0.023341  -0.815  0.41522   
## year2014      0.051367   0.023372   2.198  0.02812 * 
## year2015      0.039185   0.025271   1.551  0.12121   
## year2016      0.022565   0.024955   0.904  0.36602   
## year2017      0.060731   0.025191   2.411  0.01604 * 
## year2018      0.014796   0.025235   0.586  0.55773   
## year2019      0.027987   0.026642   1.050  0.29367   
## year2020      0.031437   0.027869   1.128  0.25948   
## year2021      0.001926   0.027483   0.070  0.94413   
## year2022      0.014168   0.026556   0.534  0.59376   
## year2023     -0.009695   0.027425  -0.354  0.72374   
## year2024     -0.024307   0.025984  -0.935  0.34971   
## year2025     -0.021097   0.032147  -0.656  0.51175   
## prop_tmoins1  0.089876   0.029351   3.062  0.00224 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.02744253)
## 
##     Null deviance: 42.267  on 1502  degrees of freedom
## Residual deviance: 40.780  on 1486  degrees of freedom
## AIC: -1120
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-75](figure/unnamed-chunk-75-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 0.27612, df = 1, p-value = 0.5993
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 48.619, df = 16, p-value = 3.795e-05
```

```
##                   GVIF Df GVIF^(1/(2*Df))
## year         0.5031592 15       0.9773651
## prop_tmoins1 0.5031592  1       0.7093372
```


### Hermine - Chevreuil

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60686,
                   espece_benchmark = 61057)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -17.70485
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-77](figure/unnamed-chunk-77-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-78](figure/unnamed-chunk-78-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year   + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + year2, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  3.276e+03  1.220e+03   2.684  0.00737 **
## year        -3.240e+00  1.209e+00  -2.679  0.00748 **
## year2        8.010e-04  2.995e-04   2.674  0.00760 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.01692391)
## 
##     Null deviance: 20.706  on 1186  degrees of freedom
## Residual deviance: 20.038  on 1184  degrees of freedom
## AIC: -1468.2
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-80](figure/unnamed-chunk-80-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 0.02438, df = 1, p-value = 0.8759
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 32.48, df = 2, p-value = 8.853e-08
```

```
##    year   year2 
## 1226299 1226299
```


### Hermine - Blaireau

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60686,
                   espece_benchmark = 60636)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] 1.606498
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-82](figure/unnamed-chunk-82-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-83](figure/unnamed-chunk-83-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year2, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.403e+00  3.738e-01   3.753 0.000179 ***
## year2       -3.420e-07  9.176e-08  -3.728 0.000198 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.005487565)
## 
##     Null deviance: 12.621  on 2287  degrees of freedom
## Residual deviance: 12.545  on 2286  degrees of freedom
## AIC: -5412.6
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-85](figure/unnamed-chunk-85-1.png)

``` r
bgtest(reg); bptest(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 0.97838, df = 1, p-value = 0.3226
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 8.3927, df = 1, p-value = 0.003767
```


## Belette


### Belette - Sanglier

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60716,
                   espece_benchmark = 60981)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -187.7034
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-87](figure/unnamed-chunk-87-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-88](figure/unnamed-chunk-88-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + X_10km + Dnst_Cultures + Dist_Ecotone + Dist_Eau + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dnst_Cultures + 
##     Dist_Ecotone + Dist_Eau + year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    2.216e+04  3.412e+03   6.495 1.32e-10 ***
## year          -2.192e+01  3.381e+00  -6.484 1.41e-10 ***
## X_10km        -3.995e-02  1.456e-02  -2.745 0.006163 ** 
## Dnst_Cultures -6.779e-01  1.436e-01  -4.722 2.68e-06 ***
## Dist_Ecotone   4.189e-03  1.103e-03   3.797 0.000156 ***
## Dist_Eau       1.002e-03  2.783e-04   3.601 0.000333 ***
## year2          5.422e-03  8.376e-04   6.474 1.51e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1316313)
## 
##     Null deviance: 163.71  on 985  degrees of freedom
## Residual deviance: 128.87  on 979  degrees of freedom
## AIC: 807.76
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-90](figure/unnamed-chunk-90-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 2.1107, df = 1, p-value = 0.1463
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 37.005, df = 6, p-value = 1.757e-06
```

```
##          year        X_10km Dnst_Cultures  Dist_Ecotone      Dist_Eau         year2 
##  1.120692e+06  1.285607e+00  1.388575e+00  1.133976e+00  1.145289e+00  1.120678e+06
```


### Belette - Renard

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60716,
                   espece_benchmark = 60585)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "qualitative"
## 
## $min_bic
## [1] -87.84112
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-92](figure/unnamed-chunk-92-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-93](figure/unnamed-chunk-93-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + X_10km + prop_tmoins1 + prop_tmoins2 )
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + prop_tmoins1 + 
##     prop_tmoins2, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.173502   0.045371   3.824 0.000136 ***
## year2011     -0.020876   0.044504  -0.469 0.639070    
## year2012     -0.034549   0.044670  -0.773 0.439378    
## year2013     -0.090631   0.044997  -2.014 0.044155 *  
## year2014      0.064516   0.044572   1.447 0.147960    
## year2015      0.106576   0.047294   2.253 0.024361 *  
## year2016      0.114972   0.046694   2.462 0.013908 *  
## year2017      0.048587   0.048164   1.009 0.313224    
## year2018      0.088171   0.047693   1.849 0.064675 .  
## year2019      0.094879   0.049991   1.898 0.057877 .  
## year2020      0.160823   0.050801   3.166 0.001575 ** 
## year2021      0.105226   0.050833   2.070 0.038605 *  
## year2022     -0.021229   0.050965  -0.417 0.677065    
## year2023      0.053067   0.051164   1.037 0.299795    
## year2024     -0.006088   0.049549  -0.123 0.902231    
## year2025      0.026291   0.059542   0.442 0.658867    
## X_10km        0.027174   0.008877   3.061 0.002240 ** 
## prop_tmoins1  0.132587   0.027333   4.851 1.34e-06 ***
## prop_tmoins2  0.147530   0.030794   4.791 1.81e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1052551)
## 
##     Null deviance: 192.41  on 1670  degrees of freedom
## Residual deviance: 173.88  on 1652  degrees of freedom
## AIC: 1000.9
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-95](figure/unnamed-chunk-95-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 0.49732, df = 1, p-value = 0.4807
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 114.4, df = 18, p-value = 4.734e-16
```

```
##                      GVIF Df GVIF^(1/(2*Df))
## year         -0.005788057 15             NaN
## X_10km        0.336801041  1       0.5803456
## prop_tmoins1  0.350383773  1       0.5919322
## prop_tmoins2  0.346826934  1       0.5889201
```

### Belette - Chevreuil

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60716,
                   espece_benchmark = 61057)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -165.6966
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-97](figure/unnamed-chunk-97-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-98](figure/unnamed-chunk-98-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year   + Dist_Ecotone + Dist_Littoral + prop_tmoins1  + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Dist_Ecotone + Dist_Littoral + 
##     prop_tmoins1 + year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    2.093e+04  2.356e+03   8.883  < 2e-16 ***
## year          -2.072e+01  2.335e+00  -8.875  < 2e-16 ***
## Dist_Ecotone   2.674e-03  7.491e-04   3.569 0.000371 ***
## Dist_Littoral -2.448e-03  4.550e-04  -5.380 8.82e-08 ***
## prop_tmoins1   1.066e-01  3.046e-02   3.498 0.000484 ***
## year2          5.129e-03  5.784e-04   8.867  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.08760732)
## 
##     Null deviance: 134.15  on 1311  degrees of freedom
## Residual deviance: 114.42  on 1306  degrees of freedom
## AIC: 536.7
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-100](figure/unnamed-chunk-100-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 7.0847, df = 1, p-value = 0.007775
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 130.12, df = 5, p-value < 2.2e-16
```

```
##          year  Dist_Ecotone Dist_Littoral  prop_tmoins1         year2 
##  1.091833e+06  1.004869e+00  1.023269e+00  1.018856e+00  1.091827e+06
```


### Belette - Blaireau

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60716,
                   espece_benchmark = 60636)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -26.32505
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-102](figure/unnamed-chunk-102-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-103](figure/unnamed-chunk-103-1.png)

``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year   + Dnst_Cultures)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Dnst_Cultures, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   11.774340   2.057378   5.723 1.18e-08 ***
## year          -0.005760   0.001019  -5.652 1.78e-08 ***
## Dnst_Cultures -0.203133   0.046380  -4.380 1.24e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.04375811)
## 
##     Null deviance: 105.77  on 2369  degrees of freedom
## Residual deviance: 103.58  on 2367  degrees of freedom
## AIC: -685.15
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-105](figure/unnamed-chunk-105-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 1.1352, df = 1, p-value = 0.2867
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 51.315, df = 2, p-value = 7.197e-12
```

```
##          year Dnst_Cultures 
##      1.000453      1.000453
```

## Fouine

### Fouine - Sanglier

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60674,
                   espece_benchmark = 60981)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -286.2294
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-107](figure/unnamed-chunk-107-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-108](figure/unnamed-chunk-108-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + Ind_Diversite + Dist_Ecotone + Dist_Littoral + Dist_Eau + prop_tmoins1 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Ind_Diversite + Dist_Ecotone + 
##     Dist_Littoral + Dist_Eau + prop_tmoins1 + year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.232e+04  3.101e+03   3.973 7.52e-05 ***
## year          -1.216e+01  3.073e+00  -3.958 8.01e-05 ***
## Ind_Diversite  4.222e-01  8.005e-02   5.274 1.59e-07 ***
## Dist_Ecotone   5.721e-03  1.075e-03   5.320 1.24e-07 ***
## Dist_Littoral  1.870e-03  7.552e-04   2.476   0.0134 *  
## Dist_Eau       1.233e-03  2.568e-04   4.801 1.78e-06 ***
## prop_tmoins1   1.297e-01  2.658e-02   4.879 1.21e-06 ***
## year2          3.002e-03  7.615e-04   3.942 8.55e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.138528)
## 
##     Null deviance: 218.82  on 1192  degrees of freedom
## Residual deviance: 164.16  on 1185  degrees of freedom
## AIC: 1037.4
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-110](figure/unnamed-chunk-110-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 9.0779, df = 1, p-value = 0.002587
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 21.486, df = 7, p-value = 0.003113
```

```
##          year Ind_Diversite  Dist_Ecotone Dist_Littoral      Dist_Eau  prop_tmoins1         year2 
##  1.150276e+06  1.664282e+00  1.008340e+00  1.624606e+00  1.136317e+00  1.094612e+00  1.150292e+06
```


### Fouine - Renard

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60674,
                   espece_benchmark = 60585)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "qualitative"
## 
## $min_bic
## [1] -503.8788
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-112](figure/unnamed-chunk-112-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-113](figure/unnamed-chunk-113-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + X_10km + prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + prop_tmoins1 + 
##     prop_tmoins2, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.388516   0.049502   7.849 6.95e-15 ***
## year2011     -0.046293   0.049280  -0.939 0.347655    
## year2012     -0.059439   0.049158  -1.209 0.226758    
## year2013     -0.110814   0.049573  -2.235 0.025509 *  
## year2014      0.129941   0.048732   2.666 0.007730 ** 
## year2015      0.238480   0.050326   4.739 2.31e-06 ***
## year2016      0.100554   0.050920   1.975 0.048441 *  
## year2017      0.100319   0.051022   1.966 0.049422 *  
## year2018      0.069957   0.051747   1.352 0.176564    
## year2019      0.124153   0.053136   2.337 0.019567 *  
## year2020      0.154598   0.054006   2.863 0.004247 ** 
## year2021      0.188909   0.052948   3.568 0.000369 ***
## year2022      0.092344   0.053519   1.725 0.084611 .  
## year2023      0.010613   0.055811   0.190 0.849206    
## year2024     -0.140323   0.055283  -2.538 0.011219 *  
## year2025     -0.096578   0.067398  -1.433 0.152034    
## X_10km        0.078249   0.009807   7.979 2.52e-15 ***
## prop_tmoins1  0.228099   0.024159   9.442  < 2e-16 ***
## prop_tmoins2  0.177240   0.025804   6.869 8.73e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1332842)
## 
##     Null deviance: 351.20  on 1932  degrees of freedom
## Residual deviance: 255.11  on 1914  degrees of freedom
## AIC: 1611
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-115](figure/unnamed-chunk-115-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 3.8186, df = 1, p-value = 0.05069
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 102.5, df = 18, p-value = 7.702e-14
```

```
##                    GVIF Df GVIF^(1/(2*Df))
## year         -0.2306717 15             NaN
## X_10km        0.1760049  1       0.4195294
## prop_tmoins1  0.1270890  1       0.3564954
## prop_tmoins2  0.1428323  1       0.3779316
```

### Fouine - Chevreuil

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60674,
                   espece_benchmark = 61057)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -244.9157
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-117](figure/unnamed-chunk-117-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-118](figure/unnamed-chunk-118-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + X_10km + Dist_Ecotone + Dist_Littoral + Dist_Eau + prop_tmoins1 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dist_Ecotone + 
##     Dist_Littoral + Dist_Eau + prop_tmoins1 + year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.646e+04  2.535e+03   6.493 1.15e-10 ***
## year          -1.628e+01  2.512e+00  -6.480 1.25e-10 ***
## X_10km         6.516e-02  1.366e-02   4.771 2.01e-06 ***
## Dist_Ecotone   4.672e-03  9.145e-04   5.109 3.67e-07 ***
## Dist_Littoral -2.630e-03  6.574e-04  -4.000 6.65e-05 ***
## Dist_Eau       8.523e-04  2.023e-04   4.213 2.68e-05 ***
## prop_tmoins1   1.317e-01  2.596e-02   5.073 4.42e-07 ***
## year2          4.026e-03  6.224e-04   6.467 1.36e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1218901)
## 
##     Null deviance: 216.52  on 1448  degrees of freedom
## Residual deviance: 175.64  on 1441  degrees of freedom
## AIC: 1072.4
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-120](figure/unnamed-chunk-120-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 3.154, df = 1, p-value = 0.07574
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 68.39, df = 7, p-value = 3.123e-12
```

```
##          year        X_10km  Dist_Ecotone Dist_Littoral      Dist_Eau  prop_tmoins1         year2 
##  1.081407e+06  1.720307e+00  1.017864e+00  1.685360e+00  1.038811e+00  1.059038e+00  1.081403e+06
```


### Fouine - Blaireau

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60674,
                   espece_benchmark = 60636)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -224.37
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-122](figure/unnamed-chunk-122-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-123](figure/unnamed-chunk-123-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~   X_10km + Ind_Diversite + Dnst_Cultures + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ X_10km + Ind_Diversite + Dnst_Cultures + 
##     prop_tmoins1 + prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.252e+01  1.330e+00   9.415  < 2e-16 ***
## X_10km         6.137e-02  6.786e-03   9.043  < 2e-16 ***
## Ind_Diversite  2.552e-01  5.056e-02   5.047 4.82e-07 ***
## Dnst_Cultures  2.662e-01  9.068e-02   2.935  0.00337 ** 
## prop_tmoins1   1.146e-01  2.050e-02   5.590 2.53e-08 ***
## prop_tmoins2   7.062e-02  2.121e-02   3.330  0.00088 ***
## year2         -3.153e-06  3.251e-07  -9.699  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.07435608)
## 
##     Null deviance: 202.30  on 2432  degrees of freedom
## Residual deviance: 180.39  on 2426  degrees of freedom
## AIC: 590.45
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-125](figure/unnamed-chunk-125-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 2.8128, df = 1, p-value = 0.09352
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 147.28, df = 6, p-value < 2.2e-16
```

```
##        X_10km Ind_Diversite Dnst_Cultures  prop_tmoins1  prop_tmoins2         year2 
##      1.274696      2.324565      2.275947      1.093374      1.093169      1.008972
```

## Temoins

### Sanglier - Renard

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60981,
                   espece_benchmark = 60585)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "qualitative"
## 
## $min_bic
## [1] -1231.308
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-127](figure/unnamed-chunk-127-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-128](figure/unnamed-chunk-128-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + X_10km + prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + prop_tmoins1 + 
##     prop_tmoins2, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.135590   0.043534   3.115 0.001869 ** 
## year2011      0.014981   0.044179   0.339 0.734581    
## year2012     -0.008607   0.044159  -0.195 0.845489    
## year2013      0.006837   0.044239   0.155 0.877193    
## year2014      0.242783   0.043544   5.576 2.81e-08 ***
## year2015      0.261279   0.045780   5.707 1.33e-08 ***
## year2016      0.160117   0.045384   3.528 0.000428 ***
## year2017      0.177714   0.045402   3.914 9.38e-05 ***
## year2018      0.166615   0.045562   3.657 0.000262 ***
## year2019      0.193583   0.046770   4.139 3.64e-05 ***
## year2020      0.295662   0.046869   6.308 3.49e-10 ***
## year2021      0.241756   0.047290   5.112 3.50e-07 ***
## year2022      0.175833   0.046852   3.753 0.000180 ***
## year2023      0.188433   0.047360   3.979 7.18e-05 ***
## year2024      0.159521   0.046429   3.436 0.000603 ***
## year2025     -0.130575   0.059639  -2.189 0.028685 *  
## X_10km        0.043284   0.008048   5.378 8.44e-08 ***
## prop_tmoins1  0.387182   0.023994  16.137  < 2e-16 ***
## prop_tmoins2  0.266607   0.025546  10.436  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.09825233)
## 
##     Null deviance: 378.29  on 1951  degrees of freedom
## Residual deviance: 189.92  on 1933  degrees of freedom
## AIC: 1031.4
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-130](figure/unnamed-chunk-130-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 12.14, df = 1, p-value = 0.0004935
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 199.33, df = 18, p-value < 2.2e-16
```

```
##                    GVIF Df GVIF^(1/(2*Df))
## year          1.7085879 15        1.018016
## X_10km       -0.6928078  1             NaN
## prop_tmoins1 -0.7546431  1             NaN
## prop_tmoins2 -0.7989216  1             NaN
```

### Sanglier - Chevreuil

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60981,
                   espece_benchmark = 61057)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "qualitative"
## 
## $min_bic
## [1] -67.916
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-132](figure/unnamed-chunk-132-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-133](figure/unnamed-chunk-133-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + X_10km + Dist_Littoral + prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dist_Littoral + 
##     prop_tmoins1 + prop_tmoins2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    0.2293953  0.1462659   1.568  0.11705    
## year2011       0.2559816  0.1626899   1.573  0.11587    
## year2012       0.0572882  0.1546126   0.371  0.71105    
## year2013       0.0879175  0.1513886   0.581  0.56152    
## year2014       0.3246888  0.1415703   2.293  0.02198 *  
## year2015       0.2309593  0.1420030   1.626  0.10411    
## year2016       0.1465902  0.1418549   1.033  0.30162    
## year2017       0.1994991  0.1417965   1.407  0.15969    
## year2018       0.2184730  0.1415080   1.544  0.12287    
## year2019       0.1958139  0.1419908   1.379  0.16812    
## year2020       0.2513929  0.1415922   1.775  0.07606 .  
## year2021       0.2095540  0.1414799   1.481  0.13881    
## year2022       0.3776144  0.1419111   2.661  0.00789 ** 
## year2023       0.2343846  0.1421339   1.649  0.09939 .  
## year2024       0.2939227  0.1419569   2.071  0.03861 *  
## year2025       0.2404299  0.1566266   1.535  0.12502    
## X_10km         0.0609917  0.0130918   4.659 3.52e-06 ***
## Dist_Littoral -0.0025615  0.0006317  -4.055 5.32e-05 ***
## prop_tmoins1   0.1370304  0.0309400   4.429 1.03e-05 ***
## prop_tmoins2   0.1025316  0.0328218   3.124  0.00183 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.0954693)
## 
##     Null deviance: 134.98  on 1282  degrees of freedom
## Residual deviance: 120.58  on 1263  degrees of freedom
## AIC: 649.14
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-135](figure/unnamed-chunk-135-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 2.3066, df = 1, p-value = 0.1288
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 65.982, df = 19, p-value = 4.225e-07
```

```
##                       GVIF Df GVIF^(1/(2*Df))
## year            0.02721716 15       0.8868049
## X_10km          5.64291270  1       2.3754816
## Dist_Littoral   4.81510038  1       2.1943337
## prop_tmoins1    0.39786775  1       0.6307676
## prop_tmoins2  113.91843576  1      10.6732580
```

### Sanglier - Blaireau

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60981,
                   espece_benchmark = 60636)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "qualitative"
## 
## $min_bic
## [1] -535.5606
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-137](figure/unnamed-chunk-137-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-138](figure/unnamed-chunk-138-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + X_10km  + prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + prop_tmoins1 + 
##     prop_tmoins2, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.113462   0.034434   3.295 0.000999 ***
## year2011      0.035740   0.038711   0.923 0.355968    
## year2012      0.011575   0.037940   0.305 0.760328    
## year2013      0.019758   0.037513   0.527 0.598455    
## year2014      0.155268   0.035476   4.377 1.26e-05 ***
## year2015      0.081360   0.036047   2.257 0.024095 *  
## year2016      0.079757   0.035649   2.237 0.025361 *  
## year2017      0.119001   0.035705   3.333 0.000873 ***
## year2018      0.144145   0.035421   4.069 4.87e-05 ***
## year2019      0.055526   0.035164   1.579 0.114460    
## year2020      0.137519   0.035421   3.882 0.000106 ***
## year2021      0.119856   0.035385   3.387 0.000718 ***
## year2022      0.092705   0.035098   2.641 0.008314 ** 
## year2023      0.128777   0.035574   3.620 0.000301 ***
## year2024      0.074951   0.035028   2.140 0.032481 *  
## year2025     -0.044103   0.037384  -1.180 0.238226    
## X_10km        0.038429   0.005203   7.386 2.09e-13 ***
## prop_tmoins1  0.259656   0.021406  12.130  < 2e-16 ***
## prop_tmoins2  0.188531   0.022884   8.239 2.87e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.04938447)
## 
##     Null deviance: 151.10  on 2344  degrees of freedom
## Residual deviance: 114.87  on 2326  degrees of freedom
## AIC: -378.3
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-140](figure/unnamed-chunk-140-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 6.2571, df = 1, p-value = 0.01237
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 115.65, df = 18, p-value = 2.768e-16
```

```
##                      GVIF Df GVIF^(1/(2*Df))
## year         -0.367008173 15             NaN
## X_10km       -0.006618221  1             NaN
## prop_tmoins1 -0.017044621  1             NaN
## prop_tmoins2 -0.010353522  1             NaN
```


### Renard - Chevreuil

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60585,
                   espece_benchmark = 61057)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -1147.459
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-142](figure/unnamed-chunk-142-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-143](figure/unnamed-chunk-143-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year  + Dist_Ecotone + Dist_Littoral + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Dist_Ecotone + Dist_Littoral + 
##     prop_tmoins1 + prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    3.627e+04  1.838e+03  19.736  < 2e-16 ***
## year          -3.592e+01  1.822e+00 -19.715  < 2e-16 ***
## Dist_Ecotone   2.743e-03  5.280e-04   5.195 2.24e-07 ***
## Dist_Littoral -2.350e-03  4.323e-04  -5.436 6.06e-08 ***
## prop_tmoins1   3.158e-01  1.916e-02  16.480  < 2e-16 ***
## prop_tmoins2   1.014e-01  1.950e-02   5.200 2.17e-07 ***
## year2          8.893e-03  4.515e-04  19.695  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1205656)
## 
##     Null deviance: 455.42  on 2188  degrees of freedom
## Residual deviance: 263.07  on 2182  degrees of freedom
## AIC: 1590.1
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-145](figure/unnamed-chunk-145-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 19.932, df = 1, p-value = 8.026e-06
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 96.343, df = 6, p-value < 2.2e-16
```

```
##          year  Dist_Ecotone Dist_Littoral  prop_tmoins1  prop_tmoins2         year2 
##  1.097773e+06  1.021077e+00  1.056587e+00  1.415572e+00  1.430218e+00  1.097745e+06
```


### Renard - Blaireau

``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 60585,
                   espece_benchmark = 60636)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "quantitative"
## 
## $min_bic
## [1] -624.2308
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-147](figure/unnamed-chunk-147-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-148](figure/unnamed-chunk-148-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + Dnst_Cultures + Dist_Eau + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Dnst_Cultures + Dist_Eau + 
##     prop_tmoins1 + prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.710e+04  1.580e+03  10.826  < 2e-16 ***
## year          -1.692e+01  1.566e+00 -10.805  < 2e-16 ***
## Dnst_Cultures -3.996e-01  7.271e-02  -5.496 4.22e-08 ***
## Dist_Eau       4.381e-04  1.305e-04   3.355 0.000803 ***
## prop_tmoins1   1.057e-01  1.769e-02   5.977 2.56e-09 ***
## prop_tmoins2   6.444e-02  1.807e-02   3.567 0.000367 ***
## year2          4.185e-03  3.881e-04  10.783  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1266005)
## 
##     Null deviance: 458.87  on 2865  degrees of freedom
## Residual deviance: 361.95  on 2859  degrees of freedom
## AIC: 2219.1
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-150](figure/unnamed-chunk-150-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 10.734, df = 1, p-value = 0.001052
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 74.369, df = 6, p-value = 5.176e-14
```

```
##          year Dnst_Cultures      Dist_Eau  prop_tmoins1  prop_tmoins2         year2 
##  1.078080e+06  1.032443e+00  1.032468e+00  1.104598e+00  1.111186e+00  1.078105e+06
```


### Chevreuil - Blaireau


``` r
bdd_reg <- tab_glm(Donnees, 
                   espece_interet = 61057,
                   espece_benchmark = 60636)

liste <- choisi_forme_year(bdd = bdd_reg)
```

```
## $choice
## [1] "qualitative"
## 
## $min_bic
## [1] -623.9254
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-152](figure/unnamed-chunk-152-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-153](figure/unnamed-chunk-153-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + X_10km + prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + prop_tmoins1 + 
##     prop_tmoins2, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.201699   0.041696   4.837  1.4e-06 ***
## year2011      0.002699   0.047090   0.057 0.954292    
## year2012      0.026320   0.045647   0.577 0.564267    
## year2013      0.037639   0.045199   0.833 0.405070    
## year2014      0.149116   0.043029   3.465 0.000539 ***
## year2015      0.121364   0.043695   2.778 0.005520 ** 
## year2016      0.124997   0.043515   2.872 0.004108 ** 
## year2017      0.107340   0.043632   2.460 0.013959 *  
## year2018      0.134913   0.043426   3.107 0.001914 ** 
## year2019      0.040537   0.042989   0.943 0.345789    
## year2020      0.101893   0.043336   2.351 0.018792 *  
## year2021      0.141254   0.043031   3.283 0.001043 ** 
## year2022     -0.017666   0.042892  -0.412 0.680469    
## year2023      0.128744   0.043423   2.965 0.003058 ** 
## year2024      0.007942   0.042680   0.186 0.852403    
## year2025     -0.131530   0.045667  -2.880 0.004009 ** 
## X_10km        0.041494   0.006286   6.601  5.0e-11 ***
## prop_tmoins1  0.284694   0.020749  13.721  < 2e-16 ***
## prop_tmoins2  0.184130   0.021765   8.460  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.07749643)
## 
##     Null deviance: 252.40  on 2433  degrees of freedom
## Residual deviance: 187.15  on 2415  degrees of freedom
## AIC: 703.31
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-155](figure/unnamed-chunk-155-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 5.8301, df = 1, p-value = 0.01575
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 59.363, df = 18, p-value = 2.592e-06
```

```
##                    GVIF Df GVIF^(1/(2*Df))
## year          0.7688369 15       0.9912757
## X_10km       -0.1951470  1             NaN
## prop_tmoins1 -0.2983212  1             NaN
## prop_tmoins2 -0.3295191  1             NaN
```

# Mortalite routieres - DIRO uniquement

Il me manque la colonne bdd_originale... Ou est elle passe?
