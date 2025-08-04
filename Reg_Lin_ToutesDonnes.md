---
title: "Toutes donn�es (Vu FN, GN, et Diro)"
date: "`r format(Sys.time(), '%Y-%m-%d')`"
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

# Mortalites routieres - Toutes donnees

``` r
Donnees <- Total_et_Diro
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
## [1] -743.5494
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
           formula = proportion_interet ~ year + X_10km + Ind_Diversite + Dist_Ecotone + Dist_Littoral + prop_tmoins1 +prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Ind_Diversite + 
##     Dist_Ecotone + Dist_Littoral + prop_tmoins1 + prop_tmoins2 + 
##     year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    5.747e+03  1.547e+03   3.716 0.000206 ***
## year          -5.664e+00  1.533e+00  -3.694 0.000224 ***
## X_10km        -6.005e-02  8.333e-03  -7.206 7.16e-13 ***
## Ind_Diversite  1.728e-01  3.715e-02   4.651 3.44e-06 ***
## Dist_Ecotone   1.098e-03  2.044e-04   5.371 8.39e-08 ***
## Dist_Littoral  1.157e-03  4.844e-04   2.388 0.017000 *  
## prop_tmoins1   1.463e-01  1.648e-02   8.878  < 2e-16 ***
## prop_tmoins2   1.035e-01  1.656e-02   6.248 4.71e-10 ***
## year2          1.396e-03  3.799e-04   3.673 0.000244 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1135187)
## 
##     Null deviance: 464.30  on 3170  degrees of freedom
## Residual deviance: 358.95  on 3162  degrees of freedom
## AIC: 2110.5
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
## LM test = 17.071, df = 1, p-value = 3.601e-05
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 195.94, df = 8, p-value < 2.2e-16
```

```
##          year        X_10km Ind_Diversite  Dist_Ecotone Dist_Littoral  prop_tmoins1  prop_tmoins2         year2 
##  1.246718e+06  1.831949e+00  1.459723e+00  1.104345e+00  2.130879e+00  1.279166e+00  1.439067e+00  1.246503e+06
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
## [1] -601.6783
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
           formula = proportion_interet ~ year + X_10km + Ind_Diversite + Dist_Ecotone + prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Ind_Diversite + 
##     Dist_Ecotone + prop_tmoins1 + prop_tmoins2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    0.0171122  0.0685189   0.250 0.802798    
## year2011      -0.0244455  0.0348141  -0.702 0.482613    
## year2012      -0.0794562  0.0342263  -2.321 0.020312 *  
## year2013      -0.1046885  0.0347554  -3.012 0.002611 ** 
## year2014      -0.0544444  0.0344658  -1.580 0.114266    
## year2015      -0.0375647  0.0356003  -1.055 0.291410    
## year2016      -0.0536979  0.0356180  -1.508 0.131738    
## year2017      -0.0288498  0.0356623  -0.809 0.418582    
## year2018      -0.0309540  0.0353737  -0.875 0.381599    
## year2019      -0.0115817  0.0357058  -0.324 0.745679    
## year2020       0.0311562  0.0358755   0.868 0.385201    
## year2021       0.0056987  0.0358614   0.159 0.873749    
## year2022      -0.0407533  0.0359964  -1.132 0.257642    
## year2023      -0.0804731  0.0362052  -2.223 0.026294 *  
## year2024      -0.1384131  0.0358190  -3.864 0.000113 ***
## year2025      -0.1995851  0.0372586  -5.357 8.97e-08 ***
## X_10km         0.0265825  0.0065022   4.088 4.44e-05 ***
## Ind_Diversite  0.1443108  0.0320952   4.496 7.12e-06 ***
## Dist_Ecotone   0.0015613  0.0002046   7.630 2.95e-14 ***
## prop_tmoins1   0.2220871  0.0164981  13.461  < 2e-16 ***
## prop_tmoins2   0.1789714  0.0171396  10.442  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1218676)
## 
##     Null deviance: 560.79  on 3857  degrees of freedom
## Residual deviance: 467.61  on 3837  degrees of freedom
## AIC: 2851.1
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
## LM test = 6.21, df = 1, p-value = 0.0127
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 71.234, df = 20, p-value = 1.144e-07
```

```
##                      GVIF Df GVIF^(1/(2*Df))
## year          -0.03454361 15             NaN
## X_10km         1.19701423  1       1.0940815
## Ind_Diversite  1.14460939  1       1.0698642
## Dist_Ecotone   1.10369757  1       1.0505701
## prop_tmoins1   0.89688164  1       0.9470384
## prop_tmoins2   0.88818235  1       0.9424343
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
## [1] -1244.16
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
           formula = proportion_interet ~ year + Dist_Ecotone + Dist_Littoral  + prop_tmoins1 + prop_tmoins2 + year2)
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
## (Intercept)    6.397e+03  1.035e+03   6.178 7.10e-10 ***
## year          -6.324e+00  1.026e+00  -6.162 7.86e-10 ***
## Dist_Ecotone   1.526e-03  1.541e-04   9.901  < 2e-16 ***
## Dist_Littoral -1.971e-03  2.522e-04  -7.813 7.02e-15 ***
## prop_tmoins1   2.655e-01  1.511e-02  17.564  < 2e-16 ***
## prop_tmoins2   1.847e-01  1.538e-02  12.005  < 2e-16 ***
## year2          1.563e-03  2.543e-04   6.146 8.70e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.08318124)
## 
##     Null deviance: 474.89  on 4190  degrees of freedom
## Residual deviance: 348.03  on 4184  degrees of freedom
## AIC: 1480.6
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
## LM test = 28.312, df = 1, p-value = 1.033e-07
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 434.34, df = 6, p-value < 2.2e-16
```

```
##          year  Dist_Ecotone Dist_Littoral  prop_tmoins1  prop_tmoins2         year2 
##  1.061125e+06  1.101060e+00  1.072313e+00  1.278979e+00  1.307111e+00  1.061105e+06
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
## [1] -821.1633
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
           formula = proportion_interet ~ year  + Dnst_Cultures + Dist_Ecotone + prop_tmoins1 + prop_tmoins2+ year2)
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
## (Intercept)    4.165e+03  1.343e+03   3.101  0.00194 ** 
## year          -4.106e+00  1.331e+00  -3.084  0.00206 ** 
## Dnst_Cultures -4.399e-01  5.557e-02  -7.917 3.18e-15 ***
## Dist_Ecotone   1.787e-03  1.997e-04   8.949  < 2e-16 ***
## prop_tmoins1   2.237e-01  1.597e-02  14.010  < 2e-16 ***
## prop_tmoins2   1.431e-01  1.629e-02   8.784  < 2e-16 ***
## year2          1.012e-03  3.299e-04   3.068  0.00217 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1184905)
## 
##     Null deviance: 557.06  on 3718  degrees of freedom
## Residual deviance: 439.84  on 3712  degrees of freedom
## AIC: 2630.7
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
## LM test = 17.028, df = 1, p-value = 3.683e-05
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 105.34, df = 6, p-value < 2.2e-16
```

```
##          year Dnst_Cultures  Dist_Ecotone  prop_tmoins1  prop_tmoins2         year2 
##  1.095669e+06  1.066227e+00  1.064761e+00  1.225961e+00  1.300987e+00  1.095565e+06
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
## [1] -436.291
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
           formula = proportion_interet ~ year  + X_10km + Y_10km + Dist_Littoral + Dist_Eau + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Y_10km + Dist_Littoral + 
##     Dist_Eau + prop_tmoins1 + prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.003e+04  1.275e+03   7.861 5.00e-15 ***
## year          -9.912e+00  1.264e+00  -7.841 5.84e-15 ***
## X_10km        -3.554e-02  7.106e-03  -5.001 5.98e-07 ***
## Y_10km        -7.305e-02  1.692e-02  -4.317 1.63e-05 ***
## Dist_Littoral  1.059e-03  3.661e-04   2.894 0.003828 ** 
## Dist_Eau       3.492e-04  9.886e-05   3.532 0.000417 ***
## prop_tmoins1   1.508e-01  1.496e-02  10.080  < 2e-16 ***
## prop_tmoins2   9.843e-02  1.487e-02   6.619 4.16e-11 ***
## year2          2.451e-03  3.132e-04   7.825 6.63e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.09318855)
## 
##     Null deviance: 385.20  on 3595  degrees of freedom
## Residual deviance: 334.27  on 3587  degrees of freedom
## AIC: 1682.2
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
## LM test = 9.4064, df = 1, p-value = 0.002162
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 168.21, df = 8, p-value < 2.2e-16
```

```
##          year        X_10km        Y_10km Dist_Littoral      Dist_Eau  prop_tmoins1  prop_tmoins2         year2 
##  1.244858e+06  1.771759e+00  1.240587e+00  1.772384e+00  1.224418e+00  1.236623e+00  1.478759e+00  1.244533e+06
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
## [1] -467.9491
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
           formula = proportion_interet ~ year  + X_10km  + prop_tmoins1 + prop_tmoins2 )
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
## (Intercept)   0.602057   0.030128  19.983  < 2e-16 ***
## year2011      0.008948   0.032667   0.274 0.784161    
## year2012     -0.111268   0.033075  -3.364 0.000775 ***
## year2013     -0.129581   0.033495  -3.869 0.000111 ***
## year2014     -0.139686   0.032980  -4.235 2.33e-05 ***
## year2015     -0.138144   0.034133  -4.047 5.28e-05 ***
## year2016     -0.173815   0.034010  -5.111 3.36e-07 ***
## year2017     -0.085986   0.033881  -2.538 0.011190 *  
## year2018     -0.033811   0.033486  -1.010 0.312696    
## year2019      0.048701   0.033564   1.451 0.146854    
## year2020      0.057860   0.034451   1.679 0.093141 .  
## year2021      0.030946   0.034321   0.902 0.367287    
## year2022      0.003236   0.034688   0.093 0.925679    
## year2023     -0.054635   0.034771  -1.571 0.116200    
## year2024     -0.063821   0.034367  -1.857 0.063375 .  
## year2025     -0.113211   0.035541  -3.185 0.001457 ** 
## X_10km        0.052655   0.005682   9.266  < 2e-16 ***
## prop_tmoins1  0.156880   0.015957   9.831  < 2e-16 ***
## prop_tmoins2  0.136176   0.016415   8.296  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1163944)
## 
##     Null deviance: 548.38  on 4089  degrees of freedom
## Residual deviance: 473.84  on 4071  degrees of freedom
## AIC: 2831.2
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
## LM test = 11.463, df = 1, p-value = 0.0007098
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 82.443, df = 18, p-value = 3.191e-10
```

```
##                   GVIF Df GVIF^(1/(2*Df))
## year         0.2027652 15       0.9481996
## X_10km       0.5201392  1       0.7212067
## prop_tmoins1 0.5596394  1       0.7480905
## prop_tmoins2 0.5590414  1       0.7476907
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
## [1] -603.0621
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
           formula = proportion_interet ~ year + Dist_Eau + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Dist_Eau + prop_tmoins1 + 
##     prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   1.119e+04  1.102e+03  10.153  < 2e-16 ***
## year         -1.108e+01  1.092e+00 -10.140  < 2e-16 ***
## Dist_Eau      3.365e-04  8.018e-05   4.197 2.75e-05 ***
## prop_tmoins1  2.348e-01  1.490e-02  15.760  < 2e-16 ***
## prop_tmoins2  1.219e-01  1.516e-02   8.044 1.12e-15 ***
## year2         2.741e-03  2.707e-04  10.126  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1006969)
## 
##     Null deviance: 509.01  on 4356  degrees of freedom
## Residual deviance: 438.13  on 4351  degrees of freedom
## AIC: 2370.5
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
## LM test = 64.234, df = 1, p-value = 1.105e-15
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 181.37, df = 5, p-value < 2.2e-16
```

```
##         year     Dist_Eau prop_tmoins1 prop_tmoins2        year2 
## 1.052513e+06 1.011555e+00 1.131104e+00 1.188056e+00 1.052466e+06
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
## [1] -303.0281
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
## (Intercept)    7.308e+03  1.262e+03   5.790 7.60e-09 ***
## year          -7.228e+00  1.251e+00  -5.777 8.20e-09 ***
## X_10km         4.991e-02  6.468e-03   7.716 1.51e-14 ***
## Dnst_Cultures -4.780e-01  5.986e-02  -7.985 1.84e-15 ***
## Dist_Ecotone   1.553e-03  3.083e-04   5.038 4.92e-07 ***
## prop_tmoins1   1.413e-01  1.540e-02   9.178  < 2e-16 ***
## prop_tmoins2   7.296e-02  1.549e-02   4.710 2.57e-06 ***
## year2          1.787e-03  3.100e-04   5.764 8.82e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1162438)
## 
##     Null deviance: 500.59  on 3927  degrees of freedom
## Residual deviance: 455.68  on 3920  degrees of freedom
## AIC: 2703.9
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
## LM test = 20.398, df = 1, p-value = 6.288e-06
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 87.911, df = 7, p-value = 3.32e-16
```

```
##          year        X_10km Dnst_Cultures  Dist_Ecotone  prop_tmoins1  prop_tmoins2         year2 
##  1.080346e+06  1.276208e+00  1.184822e+00  1.111552e+00  1.118288e+00  1.227811e+00  1.080200e+06
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
## [1] -224.9018
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
           formula = proportion_interet ~ year  + X_10km  + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + prop_tmoins1 + 
##     prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   9.631e+03  1.975e+03   4.876 1.18e-06 ***
## year         -9.521e+00  1.958e+00  -4.863 1.25e-06 ***
## X_10km       -4.900e-02  8.833e-03  -5.548 3.30e-08 ***
## prop_tmoins1  1.494e-01  2.335e-02   6.400 1.95e-10 ***
## prop_tmoins2  7.974e-02  2.478e-02   3.218  0.00131 ** 
## year2         2.353e-03  4.850e-04   4.851 1.33e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1216938)
## 
##     Null deviance: 264.26  on 1887  degrees of freedom
## Residual deviance: 229.03  on 1882  degrees of freedom
## AIC: 1389.3
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
## LM test = 1.4402, df = 1, p-value = 0.2301
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 127.5, df = 5, p-value < 2.2e-16
```

```
##         year       X_10km prop_tmoins1 prop_tmoins2        year2 
## 1.023307e+06 1.059777e+00 1.079791e+00 1.084215e+00 1.023297e+06
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
## [1] -63.24613
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
           formula = proportion_interet ~ year   + Ind_Diversite + prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Ind_Diversite + prop_tmoins1 + 
##     prop_tmoins2, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    0.245413   0.049445   4.963 7.28e-07 ***
## year2011      -0.043189   0.023720  -1.821  0.06873 .  
## year2012      -0.058204   0.023287  -2.499  0.01249 *  
## year2013      -0.063564   0.023630  -2.690  0.00718 ** 
## year2014      -0.026877   0.023166  -1.160  0.24606    
## year2015      -0.028231   0.024243  -1.165  0.24430    
## year2016      -0.029289   0.023949  -1.223  0.22143    
## year2017      -0.048956   0.024362  -2.010  0.04456 *  
## year2018      -0.003397   0.023883  -0.142  0.88690    
## year2019      -0.017128   0.024064  -0.712  0.47666    
## year2020       0.016361   0.024656   0.664  0.50703    
## year2021       0.013147   0.024488   0.537  0.59140    
## year2022       0.075133   0.023966   3.135  0.00173 ** 
## year2023      -0.003520   0.024053  -0.146  0.88367    
## year2024      -0.023325   0.023852  -0.978  0.32818    
## year2025      -0.076328   0.025221  -3.026  0.00249 ** 
## Ind_Diversite -0.071876   0.022142  -3.246  0.00118 ** 
## prop_tmoins1   0.096763   0.018542   5.219 1.92e-07 ***
## prop_tmoins2   0.064268   0.020261   3.172  0.00153 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.05077256)
## 
##     Null deviance: 172.21  on 3263  degrees of freedom
## Residual deviance: 164.76  on 3245  degrees of freedom
## AIC: -444.25
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
## LM test = 1.7473, df = 1, p-value = 0.1862
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 113.45, df = 18, p-value = 7.149e-16
```

```
##                      GVIF Df GVIF^(1/(2*Df))
## year           0.09592267 15       0.9248345
## Ind_Diversite -0.47835009  1             NaN
## prop_tmoins1  -0.43682056  1             NaN
## prop_tmoins2  -0.44012813  1             NaN
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
## [1] -145.8882
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
           formula = proportion_interet ~ year   + X_10km + Y_10km  + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Y_10km + prop_tmoins1 + 
##     prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.496e+03  6.188e+02   4.034 5.59e-05 ***
## year         -2.472e+00  6.133e-01  -4.030 5.69e-05 ***
## X_10km       -1.028e-02  2.870e-03  -3.582 0.000345 ***
## Y_10km        3.749e-02  8.375e-03   4.476 7.82e-06 ***
## prop_tmoins1  1.368e-01  1.638e-02   8.353  < 2e-16 ***
## prop_tmoins2  3.470e-02  1.684e-02   2.060 0.039429 *  
## year2         6.114e-04  1.520e-04   4.023 5.87e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.02842325)
## 
##     Null deviance: 115.22  on 3851  degrees of freedom
## Residual deviance: 109.29  on 3845  degrees of freedom
## AIC: -2774.7
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
## LM test = 3.0738, df = 1, p-value = 0.07956
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 157.71, df = 6, p-value < 2.2e-16
```

```
##         year       X_10km       Y_10km prop_tmoins1 prop_tmoins2        year2 
## 9.980618e+05 1.031976e+00 1.037128e+00 1.034300e+00 1.034319e+00 9.980623e+05
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
## [1] -18.83158
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
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  10.5408204  1.9499244   5.406 6.96e-08 ***
## year         -0.0051795  0.0009661  -5.361 8.90e-08 ***
## prop_tmoins1  0.0716851  0.0187672   3.820 0.000136 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.05406844)
## 
##     Null deviance: 164.86  on 3008  degrees of freedom
## Residual deviance: 162.53  on 3006  degrees of freedom
## AIC: -234.6
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
## LM test = 8.1403, df = 1, p-value = 0.004329
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 31.377, df = 2, p-value = 1.537e-07
```

```
##         year prop_tmoins1 
##     1.000026     1.000026
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
## [1] -147.9292
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
           formula = proportion_interet ~ year + X_10km  + Dist_Ecotone + prop_tmoins1 + year2)
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
## (Intercept)   5.139e+03  1.325e+03   3.878 0.000110 ***
## year         -5.078e+00  1.313e+00  -3.867 0.000115 ***
## X_10km       -2.741e-02  5.756e-03  -4.763 2.07e-06 ***
## Dist_Ecotone  1.141e-03  3.545e-04   3.218 0.001317 ** 
## prop_tmoins1  9.312e-02  2.470e-02   3.769 0.000169 ***
## year2         1.254e-03  3.254e-04   3.855 0.000120 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.04623204)
## 
##     Null deviance: 86.004  on 1662  degrees of freedom
## Residual deviance: 76.606  on 1657  degrees of freedom
## AIC: -384.82
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
## LM test = 1.6129, df = 1, p-value = 0.2041
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 181.8, df = 5, p-value < 2.2e-16
```

```
##         year       X_10km Dist_Ecotone prop_tmoins1        year2 
## 1.016150e+06 1.068050e+00 1.064323e+00 1.036538e+00 1.016177e+06
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
## [1] "quantitative"
## 
## $min_bic
## [1] -0.1333714
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
           formula = proportion_interet ~ Y_10km   + prop_tmoins1)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ Y_10km + prop_tmoins1, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.047669   0.328398  -3.190 0.001435 ** 
## Y_10km        0.022192   0.006816   3.256 0.001142 ** 
## prop_tmoins1  0.064325   0.018253   3.524 0.000431 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.01650682)
## 
##     Null deviance: 52.564  on 3162  degrees of freedom
## Residual deviance: 52.162  on 3160  degrees of freedom
## AIC: -3999.7
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
## LM test = 8.9455, df = 1, p-value = 0.002782
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 18.298, df = 2, p-value = 0.0001063
```

```
##       Y_10km prop_tmoins1 
##     1.003149     1.003149
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
## [1] -36.28959
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
           formula = proportion_interet ~ year + Y_10km + Dist_Ecotone + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Y_10km + Dist_Ecotone + 
##     prop_tmoins2, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   3.264e+00  6.881e-01   4.744 2.18e-06 ***
## year         -2.012e-03  3.234e-04  -6.220 5.52e-10 ***
## Y_10km        1.658e-02  4.319e-03   3.838 0.000126 ***
## Dist_Ecotone  1.824e-04  5.836e-05   3.125 0.001791 ** 
## prop_tmoins2  5.548e-02  1.749e-02   3.172 0.001528 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.007649725)
## 
##     Null deviance: 29.537  on 3787  degrees of freedom
## Residual deviance: 28.939  on 3783  degrees of freedom
## AIC: -7702.4
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
## LM test = 2.6017, df = 1, p-value = 0.1067
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 55.712, df = 4, p-value = 2.304e-11
```

```
##         year       Y_10km Dist_Ecotone prop_tmoins2 
##     1.002631     1.007793     1.006126     1.004367
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
## [1] -62.40324
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
           formula = proportion_interet ~ year + Y_10km +  Dist_Ecotone + Dist_Eau + prop_tmoins1)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Y_10km + Dist_Ecotone + 
##     Dist_Eau + prop_tmoins1, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   5.672e+00  1.236e+00   4.589 4.65e-06 ***
## year         -3.512e-03  5.691e-04  -6.171 7.71e-10 ***
## Y_10km        2.947e-02  9.082e-03   3.244  0.00119 ** 
## Dist_Ecotone  6.375e-04  1.457e-04   4.375 1.26e-05 ***
## Dist_Eau     -5.758e-05  5.557e-05  -1.036  0.30020    
## prop_tmoins1  9.313e-02  1.908e-02   4.881 1.11e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.01790358)
## 
##     Null deviance: 54.105  on 2922  degrees of freedom
## Residual deviance: 52.225  on 2917  degrees of freedom
## AIC: -3455.4
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-85](figure/unnamed-chunk-85-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 10.922, df = 1, p-value = 0.0009501
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 79.096, df = 5, p-value = 1.297e-15
```

```
##         year       Y_10km Dist_Ecotone     Dist_Eau prop_tmoins1 
##     1.007292     1.417800     1.036648     1.429147     1.015068
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
## [1] -512.5662
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
           formula = proportion_interet ~ year + X_10km + Dnst_Cultures + Dist_Ecotone + Dist_Eau + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dnst_Cultures + 
##     Dist_Ecotone + Dist_Eau + prop_tmoins1 + prop_tmoins2 + year2, 
##     data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    5.725e+03  2.029e+03   2.822 0.004818 ** 
## year          -5.641e+00  2.011e+00  -2.805 0.005070 ** 
## X_10km        -7.088e-02  9.542e-03  -7.428 1.53e-13 ***
## Dnst_Cultures -2.854e-01  8.257e-02  -3.456 0.000557 ***
## Dist_Ecotone   1.157e-03  3.330e-04   3.474 0.000522 ***
## Dist_Eau       4.569e-04  1.319e-04   3.464 0.000542 ***
## prop_tmoins1   1.086e-01  2.012e-02   5.399 7.36e-08 ***
## prop_tmoins2   9.087e-02  2.078e-02   4.373 1.28e-05 ***
## year2          1.389e-03  4.982e-04   2.789 0.005333 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1538018)
## 
##     Null deviance: 463.26  on 2362  degrees of freedom
## Residual deviance: 362.05  on 2354  degrees of freedom
## AIC: 2293.1
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
## LM test = 0.74516, df = 1, p-value = 0.388
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 36.808, df = 8, p-value = 1.248e-05
```

```
##          year        X_10km Dnst_Cultures  Dist_Ecotone      Dist_Eau  prop_tmoins1  prop_tmoins2         year2 
##  1.106392e+06  1.311119e+00  1.168134e+00  1.379918e+00  1.304437e+00  1.196033e+00  1.230421e+00  1.106315e+06
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
## [1] -193.7704
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
           formula = proportion_interet ~ year + Ind_Diversite + Dist_Ecotone + Dist_Eau + prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Ind_Diversite + Dist_Ecotone + 
##     Dist_Eau + prop_tmoins1 + prop_tmoins2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   -2.594e-01  6.673e-02  -3.888 0.000103 ***
## year2011      -1.982e-02  3.201e-02  -0.619 0.535916    
## year2012      -4.985e-03  3.136e-02  -0.159 0.873705    
## year2013      -5.566e-02  3.215e-02  -1.731 0.083500 .  
## year2014       1.719e-02  3.131e-02   0.549 0.583052    
## year2015       3.796e-02  3.271e-02   1.160 0.245971    
## year2016       4.805e-02  3.221e-02   1.492 0.135775    
## year2017       1.865e-02  3.276e-02   0.569 0.569244    
## year2018       6.515e-02  3.245e-02   2.008 0.044771 *  
## year2019       5.790e-02  3.255e-02   1.778 0.075411 .  
## year2020       7.647e-02  3.332e-02   2.295 0.021806 *  
## year2021       1.085e-01  3.289e-02   3.297 0.000985 ***
## year2022       7.683e-02  3.285e-02   2.339 0.019411 *  
## year2023       6.256e-03  3.273e-02   0.191 0.848427    
## year2024       1.246e-02  3.240e-02   0.385 0.700630    
## year2025      -4.985e-02  3.412e-02  -1.461 0.144065    
## Ind_Diversite  1.144e-01  2.860e-02   4.000 6.48e-05 ***
## Dist_Ecotone   1.252e-03  2.133e-04   5.870 4.76e-09 ***
## Dist_Eau       3.546e-04  8.688e-05   4.081 4.59e-05 ***
## prop_tmoins1   9.607e-02  1.789e-02   5.371 8.36e-08 ***
## prop_tmoins2   7.416e-02  1.915e-02   3.873 0.000110 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.09470619)
## 
##     Null deviance: 355.12  on 3464  degrees of freedom
## Residual deviance: 326.17  on 3444  degrees of freedom
## AIC: 1689.3
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
## LM test = 3.1558, df = 1, p-value = 0.07566
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 138.88, df = 20, p-value < 2.2e-16
```

```
##                    GVIF Df GVIF^(1/(2*Df))
## year          0.1179141 15       0.9312198
## Ind_Diversite 1.2255199  1       1.1070320
## Dist_Ecotone  1.4257256  1       1.1940375
## Dist_Eau      1.4537208  1       1.2057035
## prop_tmoins1  1.1172699  1       1.0570099
## prop_tmoins2  1.1269285  1       1.0615689
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
## [1] -560.5691
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
           formula = proportion_interet ~ year + X_10km + Dist_Ecotone + Dist_Littoral + Dist_Eau + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dist_Ecotone + 
##     Dist_Littoral + Dist_Eau + prop_tmoins1 + prop_tmoins2 + 
##     year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.893e+03  8.823e+02   2.146   0.0320 *  
## year          -1.868e+00  8.746e-01  -2.136   0.0327 *  
## X_10km        -1.454e-02  5.234e-03  -2.778   0.0055 ** 
## Dist_Ecotone   1.193e-03  1.588e-04   7.514 7.03e-14 ***
## Dist_Littoral -1.075e-03  2.732e-04  -3.935 8.46e-05 ***
## Dist_Eau       3.652e-04  6.698e-05   5.452 5.28e-08 ***
## prop_tmoins1   1.352e-01  1.602e-02   8.435  < 2e-16 ***
## prop_tmoins2   9.887e-02  1.672e-02   5.914 3.62e-09 ***
## year2          4.609e-04  2.167e-04   2.127   0.0335 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.05821119)
## 
##     Null deviance: 272.31  on 3999  degrees of freedom
## Residual deviance: 232.32  on 3991  degrees of freedom
## AIC: -12.212
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
## LM test = 7.4458, df = 1, p-value = 0.006358
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 423.07, df = 8, p-value < 2.2e-16
```

```
##          year        X_10km  Dist_Ecotone Dist_Littoral      Dist_Eau  prop_tmoins1  prop_tmoins2         year2 
##  1.026222e+06  1.778911e+00  1.383801e+00  1.725693e+00  1.331250e+00  1.133587e+00  1.141304e+00  1.026214e+06
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
## [1] -468.4456
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
           formula = proportion_interet ~ year + Dnst_Cultures + Dist_Ecotone + Dist_Eau +
             prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Dnst_Cultures + Dist_Ecotone + 
##     Dist_Eau + prop_tmoins1 + prop_tmoins2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    2.551e+01  2.599e+00   9.813  < 2e-16 ***
## year          -1.256e-02  1.287e-03  -9.753  < 2e-16 ***
## Dnst_Cultures -5.395e-01  5.724e-02  -9.426  < 2e-16 ***
## Dist_Ecotone   2.045e-03  2.428e-04   8.422  < 2e-16 ***
## Dist_Eau       4.308e-04  9.569e-05   4.502 6.96e-06 ***
## prop_tmoins1   1.357e-01  1.751e-02   7.751 1.20e-14 ***
## prop_tmoins2   6.408e-02  1.801e-02   3.557  0.00038 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1032856)
## 
##     Null deviance: 398.79  on 3299  degrees of freedom
## Residual deviance: 340.12  on 3293  degrees of freedom
## AIC: 1882.1
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
## LM test = 4.0919, df = 1, p-value = 0.04309
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 178.53, df = 6, p-value < 2.2e-16
```

```
##          year Dnst_Cultures  Dist_Ecotone      Dist_Eau  prop_tmoins1  prop_tmoins2 
##      1.019597      1.059909      1.240535      1.215041      1.126190      1.124207
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
## [1] -335.2041
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
           formula = proportion_interet ~ year +
             Ind_Diversite + Dnst_Cultures +
             prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Ind_Diversite + Dnst_Cultures + 
##     prop_tmoins1 + prop_tmoins2, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   68.731109   4.070639  16.885  < 2e-16 ***
## year          -0.034341   0.002018 -17.017  < 2e-16 ***
## Ind_Diversite  0.300777   0.070731   4.252 2.21e-05 ***
## Dnst_Cultures  0.616502   0.132299   4.660 3.37e-06 ***
## prop_tmoins1   0.127784   0.021391   5.974 2.73e-09 ***
## prop_tmoins2   0.126371   0.021961   5.754 1.00e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1415281)
## 
##     Null deviance: 347.25  on 2041  degrees of freedom
## Residual deviance: 288.15  on 2036  degrees of freedom
## AIC: 1810.3
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
## LM test = 2.7392, df = 1, p-value = 0.09791
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 143.27, df = 5, p-value < 2.2e-16
```

```
##          year Ind_Diversite Dnst_Cultures  prop_tmoins1  prop_tmoins2 
##      1.026072      2.352944      2.367143      1.086315      1.090865
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
## [1] -764.6309
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
           formula = proportion_interet ~ year + X_10km + Dnst_Cultures +
             prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dnst_Cultures + 
##     prop_tmoins1 + prop_tmoins2, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    0.150556   0.036203   4.159 3.28e-05 ***
## year2011      -0.048601   0.028298  -1.717 0.085988 .  
## year2012      -0.073510   0.027884  -2.636 0.008420 ** 
## year2013      -0.079699   0.028262  -2.820 0.004830 ** 
## year2014       0.016909   0.027651   0.612 0.540907    
## year2015       0.077243   0.028609   2.700 0.006970 ** 
## year2016      -0.018716   0.028562  -0.655 0.512334    
## year2017       0.012765   0.028604   0.446 0.655432    
## year2018       0.003966   0.028448   0.139 0.889144    
## year2019       0.011780   0.028686   0.411 0.681353    
## year2020       0.034032   0.029402   1.157 0.247169    
## year2021       0.068854   0.028739   2.396 0.016637 *  
## year2022      -0.006965   0.028784  -0.242 0.808820    
## year2023      -0.083061   0.028925  -2.872 0.004110 ** 
## year2024      -0.102721   0.028622  -3.589 0.000337 ***
## year2025      -0.116665   0.030174  -3.866 0.000113 ***
## X_10km         0.038734   0.005380   7.200 7.38e-13 ***
## Dnst_Cultures  0.216753   0.050455   4.296 1.79e-05 ***
## prop_tmoins1   0.256546   0.017367  14.772  < 2e-16 ***
## prop_tmoins2   0.182266   0.018174  10.029  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.07255263)
## 
##     Null deviance: 319.13  on 3402  degrees of freedom
## Residual deviance: 245.45  on 3383  degrees of freedom
## AIC: 751.66
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
## LM test = 1.9699, df = 1, p-value = 0.1605
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 234.13, df = 19, p-value < 2.2e-16
```

```
##                    GVIF Df GVIF^(1/(2*Df))
## year          0.1828791 15       0.9449427
## X_10km        1.2627740  1       1.1237322
## Dnst_Cultures 1.1986316  1       1.0948203
## prop_tmoins1  1.1872232  1       1.0895977
## prop_tmoins2  1.1983305  1       1.0946829
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
## [1] -347.8022
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
           formula = proportion_interet ~ year + X_10km +
             Ind_Diversite + Dnst_Cultures + Dist_Eau +
             prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Ind_Diversite + 
##     Dnst_Cultures + Dist_Eau + prop_tmoins1 + prop_tmoins2 + 
##     year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   -1.080e+02  7.157e+02  -0.151 0.880090    
## year           1.137e-01  7.094e-01   0.160 0.872675    
## X_10km         1.135e-02  3.636e-03   3.123 0.001803 ** 
## Ind_Diversite  1.009e-01  2.608e-02   3.867 0.000112 ***
## Dnst_Cultures  2.746e-01  4.888e-02   5.619 2.06e-08 ***
## Dist_Eau       2.027e-04  5.494e-05   3.690 0.000227 ***
## prop_tmoins1   1.740e-01  1.613e-02  10.784  < 2e-16 ***
## prop_tmoins2   1.146e-01  1.639e-02   6.989 3.25e-12 ***
## year2         -2.989e-05  1.758e-04  -0.170 0.864977    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.03672347)
## 
##     Null deviance: 157.58  on 3863  degrees of freedom
## Residual deviance: 141.57  on 3855  degrees of freedom
## AIC: -1791.4
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
## LM test = 8.7065, df = 1, p-value = 0.003171
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 182.35, df = 8, p-value < 2.2e-16
```

```
##          year        X_10km Ind_Diversite Dnst_Cultures      Dist_Eau  prop_tmoins1  prop_tmoins2         year2 
##  1.030860e+06  1.275662e+00  2.426668e+00  2.455537e+00  1.063430e+00  1.092809e+00  1.097525e+00  1.030863e+06
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
## [1] -233.328
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
           formula = proportion_interet ~ year + X_10km +
             Ind_Diversite + Dnst_Cultures +
             prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Ind_Diversite + 
##     Dnst_Cultures + prop_tmoins1 + prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   -2.556e+03  1.158e+03  -2.207 0.027365 *  
## year           2.544e+00  1.148e+00   2.217 0.026717 *  
## X_10km         5.264e-02  5.916e-03   8.899  < 2e-16 ***
## Ind_Diversite  2.205e-01  4.452e-02   4.952 7.75e-07 ***
## Dnst_Cultures  2.507e-01  8.029e-02   3.122 0.001810 ** 
## prop_tmoins1   1.096e-01  1.831e-02   5.984 2.44e-09 ***
## prop_tmoins2   6.944e-02  1.881e-02   3.693 0.000226 ***
## year2         -6.332e-04  2.844e-04  -2.226 0.026068 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.07493417)
## 
##     Null deviance: 252.77  on 3072  degrees of freedom
## Residual deviance: 229.67  on 3065  degrees of freedom
## AIC: 768.2
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
## LM test = 0.60221, df = 1, p-value = 0.4377
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 132.07, df = 7, p-value < 2.2e-16
```

```
##          year        X_10km Ind_Diversite Dnst_Cultures  prop_tmoins1  prop_tmoins2         year2 
##  1.043894e+06  1.254070e+00  2.327168e+00  2.290791e+00  1.088410e+00  1.098843e+00  1.043872e+06
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
## [1] -1060.57
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
           formula = proportion_interet ~ year + X_10km +
             prop_tmoins1 + prop_tmoins2)
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
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.23472    0.03041   7.719 1.51e-14 ***
## year2011     -0.01766    0.03244  -0.544 0.586193    
## year2012     -0.04881    0.03188  -1.531 0.125880    
## year2013     -0.05122    0.03230  -1.586 0.112884    
## year2014      0.07800    0.03138   2.485 0.012988 *  
## year2015      0.06723    0.03284   2.047 0.040734 *  
## year2016      0.04965    0.03250   1.528 0.126676    
## year2017      0.10641    0.03253   3.271 0.001080 ** 
## year2018      0.10888    0.03245   3.355 0.000802 ***
## year2019      0.11300    0.03259   3.467 0.000532 ***
## year2020      0.21786    0.03294   6.614 4.31e-11 ***
## year2021      0.20296    0.03283   6.182 7.04e-10 ***
## year2022      0.16891    0.03310   5.103 3.51e-07 ***
## year2023      0.14109    0.03317   4.254 2.16e-05 ***
## year2024      0.11969    0.03263   3.668 0.000248 ***
## year2025      0.08692    0.03398   2.558 0.010574 *  
## X_10km        0.05144    0.00561   9.170  < 2e-16 ***
## prop_tmoins1  0.25948    0.01773  14.636  < 2e-16 ***
## prop_tmoins2  0.17450    0.01882   9.273  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.09403153)
## 
##     Null deviance: 462.18  on 3551  degrees of freedom
## Residual deviance: 332.21  on 3533  degrees of freedom
## AIC: 1703.7
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
## LM test = 7.5474, df = 1, p-value = 0.00601
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 286.55, df = 18, p-value < 2.2e-16
```

```
##                     GVIF Df GVIF^(1/(2*Df))
## year          0.01160071 15       0.8619515
## X_10km       -0.81388010  1             NaN
## prop_tmoins1 -0.94723390  1             NaN
## prop_tmoins2 -0.94424561  1             NaN
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
## [1] -199.4537
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
           formula = proportion_interet ~ year + Dnst_Cultures +
             prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Dnst_Cultures + prop_tmoins1 + 
##     prop_tmoins2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    0.0242991  0.0219660   1.106   0.2687    
## year2011      -0.0001973  0.0234234  -0.008   0.9933    
## year2012      -0.0189542  0.0231851  -0.818   0.4137    
## year2013      -0.0398235  0.0234139  -1.701   0.0891 .  
## year2014       0.0348741  0.0226847   1.537   0.1243    
## year2015       0.0115532  0.0228572   0.505   0.6133    
## year2016      -0.0062960  0.0226451  -0.278   0.7810    
## year2017       0.0030824  0.0226367   0.136   0.8917    
## year2018       0.0196435  0.0224409   0.875   0.3814    
## year2019      -0.0040407  0.0223276  -0.181   0.8564    
## year2020       0.0394455  0.0223505   1.765   0.0777 .  
## year2021       0.0205912  0.0220454   0.934   0.3503    
## year2022       0.0560697  0.0222606   2.519   0.0118 *  
## year2023       0.0220989  0.0221996   0.995   0.3196    
## year2024       0.0421686  0.0222299   1.897   0.0579 .  
## year2025       0.0148647  0.0229454   0.648   0.5171    
## Dnst_Cultures  0.1491470  0.0339353   4.395 1.14e-05 ***
## prop_tmoins1   0.1611545  0.0170172   9.470  < 2e-16 ***
## prop_tmoins2   0.1138267  0.0177986   6.395 1.80e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.04256823)
## 
##     Null deviance: 175.29  on 3854  degrees of freedom
## Residual deviance: 163.29  on 3836  degrees of freedom
## AIC: -1207.9
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
## LM test = 4.8726, df = 1, p-value = 0.02729
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 45.457, df = 18, p-value = 0.0003559
```

```
##                      GVIF Df GVIF^(1/(2*Df))
## year          -0.15136578 15             NaN
## Dnst_Cultures  0.09428647  1       0.3070610
## prop_tmoins1   0.09872164  1       0.3142000
## prop_tmoins2   0.09780651  1       0.3127403
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
## [1] -248.077
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
           formula = proportion_interet ~ year + X_10km + Dnst_Cultures +
             prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dnst_Cultures + 
##     prop_tmoins1 + prop_tmoins2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    0.4668279  0.0494233   9.445  < 2e-16 ***
## year2011       0.0240958  0.0421576   0.572  0.56766    
## year2012      -0.0073660  0.0413201  -0.178  0.85853    
## year2013      -0.0602275  0.0414542  -1.453  0.14636    
## year2014       0.0250580  0.0397142   0.631  0.52811    
## year2015      -0.0001815  0.0407718  -0.004  0.99645    
## year2016       0.0095609  0.0403891   0.237  0.81289    
## year2017       0.0425383  0.0399324   1.065  0.28684    
## year2018       0.0874844  0.0399147   2.192  0.02847 *  
## year2019      -0.0048970  0.0398041  -0.123  0.90209    
## year2020       0.0987014  0.0396902   2.487  0.01294 *  
## year2021       0.0865935  0.0395677   2.188  0.02871 *  
## year2022       0.1035929  0.0394489   2.626  0.00868 ** 
## year2023       0.1158291  0.0398254   2.908  0.00366 ** 
## year2024       0.0363072  0.0394860   0.919  0.35791    
## year2025       0.0221901  0.0405070   0.548  0.58386    
## X_10km         0.0623979  0.0066507   9.382  < 2e-16 ***
## Dnst_Cultures -0.2954371  0.0644660  -4.583 4.76e-06 ***
## prop_tmoins1   0.1278614  0.0186951   6.839 9.52e-12 ***
## prop_tmoins2   0.0968736  0.0196290   4.935 8.42e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1002081)
## 
##     Null deviance: 352.60  on 3177  degrees of freedom
## Residual deviance: 316.46  on 3158  degrees of freedom
## AIC: 1729.7
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
## LM test = 3.1092, df = 1, p-value = 0.07785
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 66.918, df = 19, p-value = 2.969e-07
```

```
##                    GVIF Df GVIF^(1/(2*Df))
## year          -0.189882 15             NaN
## X_10km         1.306186  1        1.142885
## Dnst_Cultures  1.202463  1        1.096569
## prop_tmoins1   1.119084  1        1.057868
## prop_tmoins2   1.095088  1        1.046464
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
## [1] -1229.348
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
           formula = proportion_interet ~ year + X_10km + Dist_Eau +
             prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dist_Eau + 
##     prop_tmoins1 + prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   1.529e+04  1.054e+03  14.514  < 2e-16 ***
## year         -1.514e+01  1.044e+00 -14.492  < 2e-16 ***
## X_10km       -5.360e-02  4.879e-03 -10.985  < 2e-16 ***
## Dist_Eau      4.067e-04  7.592e-05   5.357 8.93e-08 ***
## prop_tmoins1  1.943e-01  1.459e-02  13.318  < 2e-16 ***
## prop_tmoins2  1.136e-01  1.481e-02   7.670 2.11e-14 ***
## year2         3.745e-03  2.588e-04  14.471  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.08629524)
## 
##     Null deviance: 500.24  on 4304  degrees of freedom
## Residual deviance: 370.90  on 4298  degrees of freedom
## AIC: 1678.9
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
## LM test = 22.29, df = 1, p-value = 2.344e-06
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 157.74, df = 6, p-value < 2.2e-16
```

```
##         year       X_10km     Dist_Eau prop_tmoins1 prop_tmoins2        year2 
## 1.107323e+06 1.107833e+00 1.047281e+00 1.234158e+00 1.284714e+00 1.107372e+06
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
## [1] -758.7898
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
           formula = proportion_interet ~ year + Dnst_Cultures +
             Dist_Ecotone + prop_tmoins1 + prop_tmoins2 + year2)
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
## (Intercept)    1.381e+04  1.282e+03  10.775  < 2e-16 ***
## year          -1.367e+01  1.271e+00 -10.755  < 2e-16 ***
## Dnst_Cultures -4.907e-01  5.620e-02  -8.731  < 2e-16 ***
## Dist_Ecotone   9.220e-04  2.449e-04   3.765 0.000169 ***
## prop_tmoins1   1.715e-01  1.509e-02  11.364  < 2e-16 ***
## prop_tmoins2   9.894e-02  1.520e-02   6.509 8.52e-11 ***
## year2          3.380e-03  3.149e-04  10.735  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1159999)
## 
##     Null deviance: 561.09  on 3937  degrees of freedom
## Residual deviance: 456.00  on 3931  degrees of freedom
## AIC: 2701.4
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
## LM test = 2.3276, df = 1, p-value = 0.1271
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 26.379, df = 6, p-value = 0.0001892
```

```
##          year Dnst_Cultures  Dist_Ecotone  prop_tmoins1  prop_tmoins2         year2 
##  1.118421e+06  1.033724e+00  1.006498e+00  1.174905e+00  1.262761e+00  1.118372e+06
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
## [1] -389.2782
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
           formula = proportion_interet ~ year + X_10km + Dnst_Cultures +
             Dist_Eau + prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dnst_Cultures + 
##     Dist_Eau + prop_tmoins1 + prop_tmoins2, data = bdd)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.094e+00  4.188e-02  26.129  < 2e-16 ***
## year2011      -1.924e-02  3.138e-02  -0.613 0.539864    
## year2012      -1.117e-01  3.170e-02  -3.522 0.000433 ***
## year2013      -1.433e-01  3.239e-02  -4.423 1.00e-05 ***
## year2014      -1.712e-01  3.209e-02  -5.335 1.01e-07 ***
## year2015      -1.557e-01  3.235e-02  -4.813 1.54e-06 ***
## year2016      -1.175e-01  3.233e-02  -3.635 0.000282 ***
## year2017      -1.418e-01  3.238e-02  -4.377 1.23e-05 ***
## year2018      -1.148e-01  3.247e-02  -3.536 0.000411 ***
## year2019      -1.457e-01  3.215e-02  -4.532 6.00e-06 ***
## year2020      -1.145e-01  3.248e-02  -3.526 0.000426 ***
## year2021      -9.334e-02  3.205e-02  -2.913 0.003604 ** 
## year2022      -1.490e-01  3.234e-02  -4.609 4.18e-06 ***
## year2023      -8.884e-02  3.221e-02  -2.758 0.005834 ** 
## year2024      -1.837e-01  3.222e-02  -5.701 1.28e-08 ***
## year2025      -1.826e-01  3.264e-02  -5.596 2.33e-08 ***
## X_10km         5.673e-02  5.401e-03  10.504  < 2e-16 ***
## Dnst_Cultures -5.335e-01  5.221e-02 -10.218  < 2e-16 ***
## Dist_Eau      -2.599e-04  8.057e-05  -3.226 0.001266 ** 
## prop_tmoins1   1.496e-01  1.514e-02   9.882  < 2e-16 ***
## prop_tmoins2   1.024e-01  1.504e-02   6.805 1.16e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.08786384)
## 
##     Null deviance: 410.53  on 4152  degrees of freedom
## Residual deviance: 363.05  on 4132  degrees of freedom
## AIC: 1708.7
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
## LM test = 2.5891, df = 1, p-value = 0.1076
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 346.05, df = 20, p-value < 2.2e-16
```

```
##                    GVIF Df GVIF^(1/(2*Df))
## year          -4.584654 15             NaN
## X_10km        13.811905  1        3.716437
## Dnst_Cultures 13.213404  1        3.635025
## Dist_Eau      11.311008  1        3.363184
## prop_tmoins1   9.317967  1        3.052534
## prop_tmoins2  10.152320  1        3.186270
```
