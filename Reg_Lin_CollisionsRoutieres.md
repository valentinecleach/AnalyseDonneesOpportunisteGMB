---
title: "Morts Collisions"
date: "2025-08-27"
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
## Rows: 153117 Columns: 15
## ── Column specification
## ──────────────────────────────────────────────────────────────────────────── Delimiter: "," chr
## (5): Code_10km, bdd_originale, Nom_paysage, Famille_paysage, etat_biologique dbl (9): ...1, X_10km,
## Y_10km, cd_nom, Ind_Diversite, Dnst_Cultures, Dist_EcotoneArbore, Dist_L... date (1): date
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ Specify the column types
## or set `show_col_types = FALSE` to quiet this message.
## • `` -> `...1`
```

``` r
Total_et_Diro <- Total_et_Diro %>%
  dplyr::select(-`...1`)%>%
  dplyr::filter(date < as.Date("2025-01-01"))
```

# Mortalites routieres - Toutes donnees

``` r
Donnees <- Total_et_Diro %>%
  dplyr::filter(etat_biologique == "Trouve mort : impact routier")
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
## [1] -30.71749
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
## (Intercept)  2.575e+01  3.719e+00   6.922 1.73e-11 ***
## year2       -6.128e-06  9.151e-07  -6.696 7.08e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1113645)
## 
##     Null deviance: 50.542  on 410  degrees of freedom
## Residual deviance: 45.548  on 409  degrees of freedom
## AIC: 268.24
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png)

``` r
bgtest(reg); bptest(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 0.33903, df = 1, p-value = 0.5604
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 33.331, df = 1, p-value = 7.772e-09
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
## [1] "quantitative"
## 
## $min_bic
## [1] -17.83721
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
           formula = proportion_interet ~ year + Famille_paysage_m + Dnst_Cultures + prop_tmoins1)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Famille_paysage_m + 
##     Dnst_Cultures + prop_tmoins1, data = bdd)
## 
## Coefficients:
##                                                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                                            5.699079   3.769436   1.512   0.1308    
## year                                                  -0.002721   0.001870  -1.455   0.1458    
## Famille_paysage_mPaysage boise et de bosquets         -0.042520   0.120693  -0.352   0.7247    
## Famille_paysage_mPaysage cultive a ragosses            0.044463   0.120398   0.369   0.7120    
## Famille_paysage_mPaysage cultive avec talus           -0.012721   0.120289  -0.106   0.9158    
## Famille_paysage_mPaysage de bocage a maille elargie   -0.031203   0.120142  -0.260   0.7951    
## Famille_paysage_mPaysage de bocage dense sur collines -0.049054   0.121131  -0.405   0.6856    
## Famille_paysage_mPaysage de cultures legumieres       -0.005516   0.127065  -0.043   0.9654    
## Famille_paysage_mPaysage de littoral urbanise          0.007315   0.120357   0.061   0.9515    
## Dnst_Cultures                                         -0.203776   0.082307  -2.476   0.0134 *  
## prop_tmoins1                                           0.141673   0.026838   5.279 1.48e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.09790356)
## 
##     Null deviance: 158.30  on 1573  degrees of freedom
## Residual deviance: 153.02  on 1563  degrees of freedom
##   (3 observations deleted due to missingness)
## AIC: 822.16
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
## LM test = 1.3595, df = 1, p-value = 0.2436
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 31.234, df = 10, p-value = 0.0005368
```

```
##                       GVIF Df GVIF^(1/(2*Df))
## year              1.019313  1        1.009610
## Famille_paysage_m 1.581812  7        1.033297
## Dnst_Cultures     1.562237  1        1.249895
## prop_tmoins1      1.022939  1        1.011405
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
## [1] -45.35879
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
           formula = proportion_interet ~ year + Dist_Ecotone + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Dist_Ecotone + prop_tmoins2, 
##     data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  75.192425   8.990697   8.363 4.91e-16 ***
## year         -0.037085   0.004462  -8.312 7.25e-16 ***
## Dist_Ecotone  0.002974   0.001023   2.906  0.00381 ** 
## prop_tmoins2  0.131435   0.046638   2.818  0.00500 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1978057)
## 
##     Null deviance: 124.97  on 560  degrees of freedom
## Residual deviance: 110.18  on 557  degrees of freedom
##   (2 observations deleted due to missingness)
## AIC: 688.95
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
## LM test = 0.3878, df = 1, p-value = 0.5335
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 9.1203, df = 3, p-value = 0.02773
```

```
##         year Dist_Ecotone prop_tmoins2 
##     1.107476     1.005042     1.105578
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
## [1] -142.1491
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
           formula = proportion_interet ~ year  + Famille_paysage_m + Dist_Littoral + prop_tmoins1 + prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Famille_paysage_m + 
##     Dist_Littoral + prop_tmoins1 + prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                                                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                                            9.606e+03  1.838e+03   5.225 1.94e-07 ***
## year                                                  -9.507e+00  1.823e+00  -5.216 2.04e-07 ***
## Famille_paysage_mPaysage boise et de bosquets         -3.958e-02  1.097e-01  -0.361 0.718348    
## Famille_paysage_mPaysage cultive a ragosses           -1.198e-02  1.093e-01  -0.110 0.912703    
## Famille_paysage_mPaysage cultive avec talus           -2.433e-02  1.087e-01  -0.224 0.822874    
## Famille_paysage_mPaysage de bocage a maille elargie   -3.254e-03  1.091e-01  -0.030 0.976205    
## Famille_paysage_mPaysage de bocage dense sur collines -3.801e-02  1.104e-01  -0.344 0.730688    
## Famille_paysage_mPaysage de cultures legumieres       -8.861e-03  1.139e-01  -0.078 0.937991    
## Famille_paysage_mPaysage de littoral urbanise          7.456e-02  1.096e-01   0.680 0.496426    
## Dist_Littoral                                          1.154e-03  4.914e-04   2.347 0.019024 *  
## prop_tmoins1                                           1.205e-01  2.359e-02   5.105 3.66e-07 ***
## prop_tmoins2                                           9.021e-02  2.472e-02   3.649 0.000271 ***
## year2                                                  2.352e-03  4.518e-04   5.206 2.15e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.09226427)
## 
##     Null deviance: 184.92  on 1809  degrees of freedom
## Residual deviance: 165.80  on 1797  degrees of freedom
##   (2 observations deleted due to missingness)
## AIC: 838.1
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
## LM test = 0.30839, df = 1, p-value = 0.5787
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 137.87, df = 12, p-value < 2.2e-16
```

```
##                           GVIF Df GVIF^(1/(2*Df))
## year              1.189840e+06  1     1090.797873
## Famille_paysage_m 1.562201e+00  7        1.032377
## Dist_Littoral     1.542727e+00  1        1.242066
## prop_tmoins1      1.045244e+00  1        1.022372
## prop_tmoins2      1.050813e+00  1        1.025091
## year2             1.189826e+06  1     1090.791601
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
## [1] "qualitative"
## 
## $min_bic
## [1] 4.636311
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
           formula = proportion_interet ~ year)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year, data = bdd)
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.000000   0.010921  91.568  < 2e-16 ***
## year2011    -0.008349   0.013592  -0.614  0.53910    
## year2012    -0.002264   0.013448  -0.168  0.86629    
## year2013    -0.015432   0.013755  -1.122  0.26202    
## year2014    -0.026389   0.013853  -1.905  0.05691 .  
## year2015    -0.049426   0.015141  -3.264  0.00111 ** 
## year2016    -0.009901   0.015213  -0.651  0.51523    
## year2017    -0.005747   0.014729  -0.390  0.69643    
## year2018    -0.011640   0.013787  -0.844  0.39861    
## year2019    -0.002039   0.013486  -0.151  0.87984    
## year2020    -0.017565   0.013803  -1.273  0.20331    
## year2021    -0.011982   0.013649  -0.878  0.38011    
## year2022    -0.018281   0.013565  -1.348  0.17789    
## year2023    -0.028272   0.013679  -2.067  0.03886 *  
## year2024    -0.030824   0.013739  -2.243  0.02497 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.01133008)
## 
##     Null deviance: 25.801  on 2263  degrees of freedom
## Residual deviance: 25.481  on 2249  degrees of freedom
## AIC: -3701.5
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
## LM test = 0.18663, df = 1, p-value = 0.6657
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 24.41, df = 14, p-value = 0.04086
```

```
## Error in vif.default(reg): model contains fewer than 2 terms
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
## [1] -153.9818
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
           formula = proportion_interet ~ year + Y_10km + prop_tmoins1 + prop_tmoins2 )
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Y_10km + prop_tmoins1 + 
##     prop_tmoins2, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   5.344024   1.033133   5.173 2.48e-07 ***
## year2011      0.022295   0.041034   0.543  0.58695    
## year2012      0.025915   0.041534   0.624  0.53272    
## year2013     -0.080137   0.043039  -1.862  0.06271 .  
## year2014     -0.044929   0.042908  -1.047  0.29514    
## year2015     -0.087426   0.045849  -1.907  0.05665 .  
## year2016     -0.101498   0.045435  -2.234  0.02557 *  
## year2017      0.002732   0.044841   0.061  0.95143    
## year2018      0.075065   0.043716   1.717  0.08607 .  
## year2019      0.177294   0.043902   4.038 5.53e-05 ***
## year2020      0.146033   0.045138   3.235  0.00123 ** 
## year2021      0.148932   0.045291   3.288  0.00102 ** 
## year2022      0.130926   0.045254   2.893  0.00384 ** 
## year2023      0.122696   0.045736   2.683  0.00735 ** 
## year2024      0.072800   0.045594   1.597  0.11045    
## Y_10km       -0.099187   0.021438  -4.627 3.89e-06 ***
## prop_tmoins1  0.083074   0.019283   4.308 1.71e-05 ***
## prop_tmoins2  0.035193   0.019501   1.805  0.07125 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.134388)
## 
##     Null deviance: 400.23  on 2723  degrees of freedom
## Residual deviance: 363.65  on 2706  degrees of freedom
##   (12 observations deleted due to missingness)
## AIC: 2283.2
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
## LM test = 1.0911, df = 1, p-value = 0.2962
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 117.74, df = 17, p-value < 2.2e-16
```

```
##                  GVIF Df GVIF^(1/(2*Df))
## year         1.623631 14        1.017460
## Y_10km       1.019484  1        1.009695
## prop_tmoins1 1.318975  1        1.148466
## prop_tmoins2 1.455547  1        1.206461
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
## [1] 7.973115
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
           formula = proportion_interet ~ year)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  5.7343385  1.8165258   3.157  0.00162 **
## year        -0.0023735  0.0009005  -2.636  0.00845 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.03572605)
## 
##     Null deviance: 82.668  on 2308  degrees of freedom
## Residual deviance: 82.420  on 2307  degrees of freedom
## AIC: -1136.6
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
## LM test = 0.097082, df = 1, p-value = 0.7554
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 6.2868, df = 1, p-value = 0.01216
```

```
## Error in vif.default(reg): model contains fewer than 2 terms
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
## [1] "qualitative"
## 
## $min_bic
## [1] -114.3753
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
           formula = proportion_interet ~ year + X_10km + Famille_paysage_m + prop_tmoins1 + prop_tmoins2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Famille_paysage_m + 
##     prop_tmoins1 + prop_tmoins2, data = bdd)
## 
## Coefficients:
##                                                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                                            0.90577    0.10814   8.376  < 2e-16 ***
## year2011                                               0.03471    0.04407   0.788 0.430975    
## year2012                                              -0.04926    0.04491  -1.097 0.272785    
## year2013                                              -0.17543    0.04607  -3.808 0.000143 ***
## year2014                                              -0.17353    0.04628  -3.750 0.000181 ***
## year2015                                              -0.25532    0.04905  -5.205 2.08e-07 ***
## year2016                                              -0.30136    0.04784  -6.299 3.46e-10 ***
## year2017                                              -0.22823    0.04702  -4.853 1.28e-06 ***
## year2018                                              -0.15012    0.04553  -3.297 0.000990 ***
## year2019                                              -0.18535    0.04521  -4.100 4.25e-05 ***
## year2020                                              -0.19834    0.04607  -4.305 1.72e-05 ***
## year2021                                              -0.15783    0.04575  -3.450 0.000570 ***
## year2022                                              -0.20450    0.04536  -4.508 6.81e-06 ***
## year2023                                              -0.18426    0.04557  -4.044 5.40e-05 ***
## year2024                                              -0.25957    0.04564  -5.687 1.43e-08 ***
## X_10km                                                 0.05816    0.01323   4.397 1.14e-05 ***
## Famille_paysage_mPaysage boise et de bosquets         -0.05174    0.10051  -0.515 0.606703    
## Famille_paysage_mPaysage cultive a ragosses           -0.05960    0.09961  -0.598 0.549642    
## Famille_paysage_mPaysage cultive avec talus           -0.02404    0.10269  -0.234 0.814898    
## Famille_paysage_mPaysage de bocage a maille elargie   -0.04390    0.10188  -0.431 0.666564    
## Famille_paysage_mPaysage de bocage dense sur collines -0.07591    0.10142  -0.748 0.454228    
## Famille_paysage_mPaysage de cultures legumieres       -0.03349    0.10597  -0.316 0.752005    
## Famille_paysage_mPaysage de littoral urbanise          0.05704    0.10138   0.563 0.573758    
## prop_tmoins1                                           0.12276    0.01867   6.575 5.77e-11 ***
## prop_tmoins2                                           0.07045    0.01900   3.708 0.000213 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1456183)
## 
##     Null deviance: 442.78  on 2834  degrees of freedom
## Residual deviance: 409.19  on 2810  degrees of freedom
##   (9 observations deleted due to missingness)
## AIC: 2609.9
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
## LM test = 2.2651, df = 1, p-value = 0.1323
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 145.22, df = 24, p-value < 2.2e-16
```

```
##                       GVIF Df GVIF^(1/(2*Df))
## year              1.415121 14        1.012478
## X_10km            3.040063  1        1.743578
## Famille_paysage_m 3.126693  7        1.084834
## prop_tmoins1      1.205696  1        1.098042
## prop_tmoins2      1.333723  1        1.154869
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
## [1] "qualitative"
## 
## $min_bic
## [1] 3.486645
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
           formula = proportion_interet ~ year)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year, data = bdd)
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.00000    0.09536  10.486  < 2e-16 ***
## year2011    -0.08696    0.11931  -0.729  0.46651    
## year2012    -0.07407    0.11607  -0.638  0.52371    
## year2013    -0.13043    0.11931  -1.093  0.27491    
## year2014    -0.20000    0.11417  -1.752  0.08056 .  
## year2015    -0.25000    0.11679  -2.141  0.03290 *  
## year2016    -0.02299    0.11476  -0.200  0.84133    
## year2017    -0.16667    0.13764  -1.211  0.22665    
## year2018    -0.15625    0.11309  -1.382  0.16782    
## year2019    -0.06818    0.12028  -0.567  0.57112    
## year2020    -0.19828    0.11476  -1.728  0.08480 .  
## year2021    -0.21429    0.11540  -1.857  0.06403 .  
## year2022    -0.11017    0.10535  -1.046  0.29627    
## year2023    -0.21795    0.11011  -1.979  0.04845 *  
## year2024    -0.32812    0.11309  -2.902  0.00391 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1182205)
## 
##     Null deviance: 51.315  on 423  degrees of freedom
## Residual deviance: 48.352  on 409  degrees of freedom
## AIC: 314.66
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
## LM test = 0.42262, df = 1, p-value = 0.5156
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 30.368, df = 14, p-value = 0.006789
```

```
## Error in vif.default(reg): model contains fewer than 2 terms
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
## [1] "quantitative"
## 
## $min_bic
## [1] -64.87981
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
           formula = proportion_interet ~ year)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -34.144915   3.799997  -8.986   <2e-16 ***
## year          0.017013   0.001885   9.028   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1062808)
## 
##     Null deviance: 179.77  on 1611  degrees of freedom
## Residual deviance: 171.11  on 1610  degrees of freedom
## AIC: 965.08
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
## LM test = 3.6465, df = 1, p-value = 0.05619
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 86.231, df = 1, p-value < 2.2e-16
```

```
## Error in vif.default(reg): model contains fewer than 2 terms
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
## [1] "qualitative"
## 
## $min_bic
## [1] -0.8240282
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
           formula = proportion_interet ~ year   + Y_10km)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Y_10km, data = bdd)
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -7.93289    2.77729  -2.856  0.00444 **
## year2011    -0.04044    0.13850  -0.292  0.77039   
## year2012    -0.16751    0.13276  -1.262  0.20756   
## year2013    -0.25977    0.13261  -1.959  0.05061 . 
## year2014    -0.12165    0.13210  -0.921  0.35753   
## year2015    -0.02756    0.14119  -0.195  0.84533   
## year2016    -0.04221    0.13261  -0.318  0.75039   
## year2017    -0.34903    0.14328  -2.436  0.01515 * 
## year2018    -0.08970    0.13126  -0.683  0.49468   
## year2019    -0.12506    0.13622  -0.918  0.35895   
## year2020    -0.07796    0.13487  -0.578  0.56346   
## year2021    -0.20134    0.13204  -1.525  0.12786   
## year2022     0.03202    0.12361   0.259  0.79570   
## year2023    -0.11638    0.12815  -0.908  0.36420   
## year2024    -0.24683    0.13162  -1.875  0.06126 . 
## Y_10km       0.17953    0.05756   3.119  0.00191 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.2182762)
## 
##     Null deviance: 131.54  on 582  degrees of freedom
## Residual deviance: 123.76  on 567  degrees of freedom
##   (1 observation deleted due to missingness)
## AIC: 784.94
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
## LM test = 0.00080005, df = 1, p-value = 0.9774
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 12.348, df = 15, p-value = 0.6525
```

```
##            GVIF Df GVIF^(1/(2*Df))
## year   1.059125 14        1.002054
## Y_10km 1.059125  1        1.029138
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
## [1] "qualitative"
## 
## $min_bic
## [1] 2.029749
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
           formula = proportion_interet ~ year )
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year, data = bdd)
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.17760    0.03736   4.753 2.17e-06 ***
## year2011    -0.02866    0.04798  -0.597  0.55037    
## year2012    -0.03345    0.04651  -0.719  0.47211    
## year2013    -0.05917    0.04629  -1.278  0.20134    
## year2014    -0.02954    0.04589  -0.644  0.51984    
## year2015    -0.06027    0.04788  -1.259  0.20828    
## year2016    -0.01905    0.04595  -0.415  0.67849    
## year2017    -0.13100    0.04698  -2.788  0.00536 ** 
## year2018    -0.02927    0.04602  -0.636  0.52484    
## year2019    -0.09424    0.04449  -2.118  0.03429 *  
## year2020    -0.06537    0.04529  -1.443  0.14908    
## year2021    -0.05266    0.04570  -1.152  0.24933    
## year2022     0.03343    0.04427   0.755  0.45028    
## year2023    -0.01402    0.04507  -0.311  0.75579    
## year2024    -0.09964    0.04445  -2.242  0.02510 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.08516393)
## 
##     Null deviance: 152.32  on 1767  degrees of freedom
## Residual deviance: 149.29  on 1753  degrees of freedom
## AIC: 679.41
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
## LM test = 3.6064, df = 1, p-value = 0.05756
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 28.889, df = 14, p-value = 0.01082
```

```
## Error in vif.default(reg): model contains fewer than 2 terms
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
## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in = force.in, : 1 linear
## dependencies found
```

```
## Reordering variables and trying again:
```

```
## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in = force.in, : 1 linear
## dependencies found
```

```
## $choice
## [1] "qualitative"
## 
## $min_bic
## [1] -5.855631
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
           formula = proportion_interet ~ year + Famille_paysage_m + Dist_Ecotone)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Famille_paysage_m + 
##     Dist_Ecotone, data = bdd)
## 
## Coefficients:
##                                                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                                            1.130843   0.222303   5.087 3.51e-06 ***
## year2011                                              -0.708969   0.228077  -3.108 0.002822 ** 
## year2012                                              -1.108243   0.266723  -4.155 9.98e-05 ***
## year2013                                              -0.827745   0.226843  -3.649 0.000536 ***
## year2014                                              -0.979857   0.212809  -4.604 2.06e-05 ***
## year2015                                              -0.882191   0.207869  -4.244 7.35e-05 ***
## year2016                                              -0.292115   0.239727  -1.219 0.227566    
## year2017                                              -0.778124   0.244362  -3.184 0.002256 ** 
## year2018                                              -1.030809   0.217762  -4.734 1.29e-05 ***
## year2019                                              -1.101749   0.266801  -4.129 0.000109 ***
## year2020                                              -1.024936   0.210261  -4.875 7.72e-06 ***
## year2021                                              -0.931702   0.210483  -4.427 3.88e-05 ***
## year2022                                              -1.018831   0.205298  -4.963 5.58e-06 ***
## year2023                                              -1.010195   0.202886  -4.979 5.25e-06 ***
## year2024                                              -1.017492   0.203503  -5.000 4.86e-06 ***
## Famille_paysage_mPaysage cultive a ragosses            0.077807   0.102539   0.759 0.450799    
## Famille_paysage_mPaysage cultive avec talus            0.110722   0.106242   1.042 0.301316    
## Famille_paysage_mPaysage de bocage a maille elargie   -0.001241   0.114264  -0.011 0.991370    
## Famille_paysage_mPaysage de bocage dense sur collines  0.222338   0.132032   1.684 0.097137 .  
## Famille_paysage_mPaysage de cultures legumieres        1.184715   0.329631   3.594 0.000639 ***
## Famille_paysage_mPaysage de littoral urbanise          0.093387   0.119018   0.785 0.435602    
## Dist_Ecotone                                          -0.003790   0.002023  -1.873 0.065653 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.0646408)
## 
##     Null deviance: 8.8235  on 84  degrees of freedom
## Residual deviance: 4.0724  on 63  degrees of freedom
## AIC: 28.953
## 
## Number of Fisher Scoring iterations: 2
```

```
## Warning: Removed 6 rows containing missing values or values outside the scale range
## (`geom_line()`).
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range (`geom_line()`).
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
## LM test = 0.28486, df = 1, p-value = 0.5935
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 32.933, df = 21, p-value = 0.04697
```

```
##                       GVIF Df GVIF^(1/(2*Df))
## year              3.800140 14        1.048835
## Famille_paysage_m 3.558044  6        1.111564
## Dist_Ecotone      1.223471  1        1.106106
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
## [1] 11.01472
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
           formula = proportion_interet ~ year)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year, data = bdd)
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  0.021097   0.006717   3.141  0.00172 **
## year2011    -0.019668   0.008401  -2.341  0.01936 * 
## year2012    -0.017601   0.008369  -2.103  0.03563 * 
## year2013    -0.018699   0.008412  -2.223  0.02637 * 
## year2014    -0.021097   0.008504  -2.481  0.01323 * 
## year2015    -0.019245   0.009204  -2.091  0.03671 * 
## year2016    -0.005791   0.009027  -0.642  0.52128   
## year2017    -0.009986   0.009204  -1.085  0.27813   
## year2018    -0.021097   0.009157  -2.304  0.02137 * 
## year2019    -0.021097   0.009727  -2.169  0.03026 * 
## year2020    -0.021097   0.010176  -2.073  0.03832 * 
## year2021    -0.006172   0.009915  -0.622  0.53375   
## year2022    -0.021097   0.009625  -2.192  0.02855 * 
## year2023    -0.021097   0.009915  -2.128  0.03353 * 
## year2024    -0.021097   0.009357  -2.255  0.02430 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.003564072)
## 
##     Null deviance: 5.0949  on 1427  degrees of freedom
## Residual deviance: 5.0360  on 1413  degrees of freedom
## AIC: -3980
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
## LM test = 0.43709, df = 1, p-value = 0.5085
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 17.178, df = 14, p-value = 0.2468
```

```
## Error in vif.default(reg): model contains fewer than 2 terms
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
## [1] 3.385466
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
           formula = proportion_interet ~ year)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year, data = bdd)
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 15.344218   5.498393   2.791  0.00565 **
## year        -0.007586   0.002725  -2.784  0.00577 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.03579459)
## 
##     Null deviance: 9.6198  on 262  degrees of freedom
## Residual deviance: 9.3424  on 261  degrees of freedom
## AIC: -125.43
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
## LM test = 1.3236, df = 1, p-value = 0.25
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 8.0988, df = 1, p-value = 0.00443
```

```
## Error in vif.default(reg): model contains fewer than 2 terms
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
## [1] 8.550649
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
           formula = proportion_interet ~ year)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)  1.9011670  0.7581255   2.508   0.0122 *
## year        -0.0009399  0.0003757  -2.502   0.0125 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.004066064)
## 
##     Null deviance: 6.6572  on 1632  degrees of freedom
## Residual deviance: 6.6318  on 1631  degrees of freedom
## AIC: -4351.5
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-100](figure/unnamed-chunk-100-1.png)

``` r
bgtest(reg); bptest(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 0.13848, df = 1, p-value = 0.7098
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 5.1954, df = 1, p-value = 0.02265
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
## [1] -7.53767
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
           formula = proportion_interet ~ year)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year, data = bdd)
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 53.676668  12.368045   4.340 2.10e-05 ***
## year        -0.026257   0.006131  -4.283 2.67e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1861219)
## 
##     Null deviance: 48.269  on 242  degrees of freedom
## Residual deviance: 44.855  on 241  degrees of freedom
## AIC: 285.03
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
## LM test = 0.47277, df = 1, p-value = 0.4917
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 15.455, df = 1, p-value = 8.451e-05
```

```
## Error in vif.default(reg): model contains fewer than 2 terms
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
## [1] 0.4204149
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
           formula = proportion_interet ~ year + Ind_Diversite )
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Ind_Diversite, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)   
## (Intercept)   -0.113504   0.071990  -1.577  0.11509   
## year2011      -0.016189   0.033726  -0.480  0.63129   
## year2012      -0.018669   0.033806  -0.552  0.58087   
## year2013      -0.075978   0.034069  -2.230  0.02589 * 
## year2014       0.016559   0.034066   0.486  0.62697   
## year2015      -0.048988   0.037133  -1.319  0.18728   
## year2016      -0.004308   0.036209  -0.119  0.90532   
## year2017      -0.048496   0.037430  -1.296  0.19530   
## year2018       0.004703   0.036691   0.128  0.89803   
## year2019       0.008673   0.039041   0.222  0.82422   
## year2020       0.053995   0.040141   1.345  0.17879   
## year2021       0.036752   0.039213   0.937  0.34879   
## year2022      -0.041169   0.038671  -1.065  0.28722   
## year2023       0.015571   0.039328   0.396  0.69222   
## year2024       0.041419   0.037227   1.113  0.26605   
## Ind_Diversite  0.097731   0.032358   3.020  0.00257 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.06046576)
## 
##     Null deviance: 92.646  on 1506  degrees of freedom
## Residual deviance: 90.154  on 1491  degrees of freedom
##   (4 observations deleted due to missingness)
## AIC: 66.438
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
## LM test = 0.033726, df = 1, p-value = 0.8543
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 37.519, df = 15, p-value = 0.001062
```

```
##                   GVIF Df GVIF^(1/(2*Df))
## year          1.010774 14        1.000383
## Ind_Diversite 1.010774  1        1.005372
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
## [1] "qualitative"
## 
## $min_bic
## [1] 0.006884307
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
           formula = proportion_interet ~ year   + Famille_paysage_m +  Dist_Littoral)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Famille_paysage_m + 
##     Dist_Littoral, data = bdd)
## 
## Coefficients:
##                                                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                                            0.606423   0.294880   2.057 0.040414 *  
## year2011                                              -0.057890   0.151897  -0.381 0.703328    
## year2012                                              -0.163940   0.143395  -1.143 0.253642    
## year2013                                              -0.515702   0.147387  -3.499 0.000522 ***
## year2014                                              -0.106920   0.142417  -0.751 0.453263    
## year2015                                              -0.310332   0.169678  -1.829 0.068189 .  
## year2016                                              -0.243332   0.150511  -1.617 0.106770    
## year2017                                              -0.424795   0.154376  -2.752 0.006212 ** 
## year2018                                              -0.225421   0.145961  -1.544 0.123325    
## year2019                                              -0.253688   0.152879  -1.659 0.097858 .  
## year2020                                              -0.208688   0.150201  -1.389 0.165524    
## year2021                                              -0.367938   0.145495  -2.529 0.011846 *  
## year2022                                              -0.575676   0.150975  -3.813 0.000160 ***
## year2023                                              -0.371740   0.144859  -2.566 0.010663 *  
## year2024                                              -0.293767   0.140785  -2.087 0.037586 *  
## Famille_paysage_mPaysage boise et de bosquets          0.270874   0.278585   0.972 0.331507    
## Famille_paysage_mPaysage cultive a ragosses            0.181602   0.276938   0.656 0.512380    
## Famille_paysage_mPaysage cultive avec talus            0.148193   0.276587   0.536 0.592415    
## Famille_paysage_mPaysage de bocage a maille elargie    0.097351   0.275801   0.353 0.724301    
## Famille_paysage_mPaysage de bocage dense sur collines  0.018143   0.282163   0.064 0.948764    
## Famille_paysage_mPaysage de cultures legumieres        0.314544   0.313804   1.002 0.316806    
## Famille_paysage_mPaysage de littoral urbanise          0.108012   0.273325   0.395 0.692933    
## Dist_Littoral                                         -0.004143   0.001731  -2.393 0.017190 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.2103986)
## 
##     Null deviance: 91.903  on 403  degrees of freedom
## Residual deviance: 80.162  on 381  degrees of freedom
##   (2 observations deleted due to missingness)
## AIC: 541.09
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
## LM test = 3.7757, df = 1, p-value = 0.052
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 31.616, df = 22, p-value = 0.08419
```

```
##                       GVIF Df GVIF^(1/(2*Df))
## year              1.281568 14        1.008900
## Famille_paysage_m 2.117077  7        1.055035
## Dist_Littoral     1.738640  1        1.318575
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
## [1] -13.11844
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
           formula = proportion_interet ~ year  + Famille_paysage_m)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Famille_paysage_m, 
##     data = bdd)
## 
## Coefficients:
##                                                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                                           11.736891   2.674186   4.389 1.21e-05 ***
## year                                                  -0.005760   0.001326  -4.344 1.48e-05 ***
## Famille_paysage_mPaysage boise et de bosquets         -0.038520   0.083346  -0.462    0.644    
## Famille_paysage_mPaysage cultive a ragosses           -0.057911   0.082588  -0.701    0.483    
## Famille_paysage_mPaysage cultive avec talus           -0.064592   0.082809  -0.780    0.435    
## Famille_paysage_mPaysage de bocage a maille elargie   -0.051655   0.083123  -0.621    0.534    
## Famille_paysage_mPaysage de bocage dense sur collines -0.060846   0.083817  -0.726    0.468    
## Famille_paysage_mPaysage de cultures legumieres       -0.044068   0.086855  -0.507    0.612    
## Famille_paysage_mPaysage de littoral urbanise          0.024369   0.083603   0.291    0.771    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.05360175)
## 
##     Null deviance: 93.590  on 1714  degrees of freedom
## Residual deviance: 91.445  on 1706  degrees of freedom
##   (2 observations deleted due to missingness)
## AIC: -140.45
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
## LM test = 0.074419, df = 1, p-value = 0.785
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 34.659, df = 8, p-value = 3.085e-05
```

```
##                       GVIF Df GVIF^(1/(2*Df))
## year              1.003161  1        1.001579
## Famille_paysage_m 1.003161  7        1.000225
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
## [1] -7.171759
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
## (Intercept)  1.820e+01  3.939e+00   4.619 5.08e-06 ***
## year2       -4.266e-06  9.681e-07  -4.406 1.33e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1250978)
## 
##     Null deviance: 56.596  on 434  degrees of freedom
## Residual deviance: 54.167  on 433  degrees of freedom
## AIC: 334.26
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
## LM test = 0.92999, df = 1, p-value = 0.3349
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 18.059, df = 1, p-value = 2.142e-05
```

```
## Error in vif.default(reg): model contains fewer than 2 terms
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
## [1] "quantitative"
## 
## $min_bic
## [1] -24.53798
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
           formula = proportion_interet ~ year + Famille_paysage_m)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Famille_paysage_m, 
##     data = bdd)
## 
## Coefficients:
##                                                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                                           -16.819279   3.858480  -4.359 1.39e-05 ***
## year                                                    0.008372   0.001913   4.376 1.29e-05 ***
## Famille_paysage_mPaysage boise et de bosquets           0.081027   0.133868   0.605    0.545    
## Famille_paysage_mPaysage cultive a ragosses             0.159255   0.132853   1.199    0.231    
## Famille_paysage_mPaysage cultive avec talus             0.052264   0.133367   0.392    0.695    
## Famille_paysage_mPaysage de bocage a maille elargie     0.040841   0.133630   0.306    0.760    
## Famille_paysage_mPaysage de bocage dense sur collines   0.089992   0.134284   0.670    0.503    
## Famille_paysage_mPaysage de cultures legumieres         0.016810   0.139812   0.120    0.904    
## Famille_paysage_mPaysage de littoral urbanise           0.147515   0.133920   1.102    0.271    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1044807)
## 
##     Null deviance: 171.89  on 1594  degrees of freedom
## Residual deviance: 165.71  on 1586  degrees of freedom
##   (4 observations deleted due to missingness)
## AIC: 934.68
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
## LM test = 0.23531, df = 1, p-value = 0.6276
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 55.591, df = 8, p-value = 3.388e-09
```

```
##                       GVIF Df GVIF^(1/(2*Df))
## year              1.005703  1        1.002847
## Famille_paysage_m 1.005703  7        1.000406
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
## [1] -1.990942
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
           formula = proportion_interet ~ prop_tmoins2 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ prop_tmoins2 + year2, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   1.994e+01  4.734e+00   4.213 2.93e-05 ***
## prop_tmoins2  1.114e-01  4.945e-02   2.253   0.0246 *  
## year2        -4.759e-06  1.164e-06  -4.089 4.96e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.215839)
## 
##     Null deviance: 127.77  on 576  degrees of freedom
## Residual deviance: 123.89  on 574  degrees of freedom
## AIC: 757.78
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
## LM test = 0.45813, df = 1, p-value = 0.4985
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 7.2032, df = 2, p-value = 0.02728
```

```
## prop_tmoins2        year2 
##     1.089702     1.089702
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
## [1] -46.7018
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
           formula = proportion_interet ~   year + X_10km + Famille_paysage_m)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Famille_paysage_m, 
##     data = bdd)
## 
## Coefficients:
##                                                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                                           18.477885   3.468314   5.328 1.12e-07 ***
## year                                                  -0.009066   0.001719  -5.274 1.49e-07 ***
## X_10km                                                 0.031434   0.014130   2.225   0.0262 *  
## Famille_paysage_mPaysage boise et de bosquets          0.022386   0.110569   0.202   0.8396    
## Famille_paysage_mPaysage cultive a ragosses            0.072061   0.109607   0.657   0.5110    
## Famille_paysage_mPaysage cultive avec talus            0.025263   0.112686   0.224   0.8226    
## Famille_paysage_mPaysage de bocage a maille elargie    0.027338   0.111753   0.245   0.8068    
## Famille_paysage_mPaysage de bocage dense sur collines  0.043381   0.111354   0.390   0.6969    
## Famille_paysage_mPaysage de cultures legumieres        0.002003   0.116249   0.017   0.9863    
## Famille_paysage_mPaysage de littoral urbanise          0.141655   0.111463   1.271   0.2039    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.09447917)
## 
##     Null deviance: 177.21  on 1801  degrees of freedom
## Residual deviance: 169.31  on 1792  degrees of freedom
##   (1 observation deleted due to missingness)
## AIC: 874.23
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
## LM test = 0.20201, df = 1, p-value = 0.6531
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 63.926, df = 9, p-value = 2.333e-10
```

```
##                       GVIF Df GVIF^(1/(2*Df))
## year              1.004609  1        1.002302
## X_10km            3.141231  1        1.772352
## Famille_paysage_m 3.152834  7        1.085479
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
## [1] "quantitative"
## 
## $min_bic
## [1] -31.81588
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
           formula = proportion_interet ~ prop_tmoins1 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ prop_tmoins1 + year2, data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -5.756e+00  9.908e-01  -5.809 7.69e-09 ***
## prop_tmoins1  1.205e-01  3.282e-02   3.672 0.000249 ***
## year2         1.424e-06  2.438e-07   5.840 6.43e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.02516581)
## 
##     Null deviance: 37.966  on 1456  degrees of freedom
## Residual deviance: 36.591  on 1454  degrees of freedom
## AIC: -1225.3
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
## LM test = 3.0124, df = 1, p-value = 0.08263
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 39.459, df = 2, p-value = 2.701e-09
```

```
## prop_tmoins1        year2 
##     1.019748     1.019748
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
## [1] 5.891631
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
           formula = proportion_interet ~ year)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year, data = bdd)
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -2.319e-16  1.740e-01   0.000   1.0000  
## year2011     2.308e-01  2.047e-01   1.127   0.2605  
## year2012     7.500e-02  1.945e-01   0.386   0.7001  
## year2013     1.000e-01  1.906e-01   0.525   0.6002  
## year2014     2.800e-01  1.906e-01   1.469   0.1428  
## year2015     4.375e-01  1.993e-01   2.195   0.0289 *
## year2016     6.667e-02  2.009e-01   0.332   0.7402  
## year2017     1.053e-01  1.955e-01   0.538   0.5907  
## year2018     1.875e-01  1.945e-01   0.964   0.3358  
## year2019     1.176e-01  1.979e-01   0.594   0.5527  
## year2020     3.250e-01  1.945e-01   1.671   0.0958 .
## year2021     1.667e-01  1.900e-01   0.877   0.3810  
## year2022     2.500e-01  1.884e-01   1.327   0.1855  
## year2023     2.917e-01  1.879e-01   1.552   0.1217  
## year2024     2.917e-01  1.871e-01   1.559   0.1200  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1513309)
## 
##     Null deviance: 48.233  on 311  degrees of freedom
## Residual deviance: 44.945  on 297  degrees of freedom
## AIC: 312.9
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
## LM test = 0.66941, df = 1, p-value = 0.4133
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 23.997, df = 14, p-value = 0.04587
```

```
## Error in vif.default(reg): model contains fewer than 2 terms
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
## [1] 6.34305
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
           formula = proportion_interet ~ prop_tmoins1 )
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ prop_tmoins1, data = bdd)
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.027389   0.003806   7.197 9.29e-13 ***
## prop_tmoins1 0.079010   0.027087   2.917  0.00358 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.02341076)
## 
##     Null deviance: 39.061  on 1661  degrees of freedom
## Residual deviance: 38.862  on 1660  degrees of freedom
## AIC: -1519.5
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
## LM test = 0.26384, df = 1, p-value = 0.6075
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 5.0588, df = 1, p-value = 0.0245
```

```
## Error in vif.default(reg): model contains fewer than 2 terms
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
## [1] -59.03815
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-157](figure/unnamed-chunk-157-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-158](figure/unnamed-chunk-158-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year  + Y_10km)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + Y_10km, data = bdd)
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 25.312358   3.613190   7.006 3.67e-12 ***
## year        -0.013646   0.001662  -8.209 4.67e-16 ***
## Y_10km       0.064133   0.021272   3.015  0.00261 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.07675578)
## 
##     Null deviance: 124.35  on 1539  degrees of freedom
## Residual deviance: 117.97  on 1537  degrees of freedom
##   (3 observations deleted due to missingness)
## AIC: 421.95
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-160](figure/unnamed-chunk-160-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 6.9054, df = 1, p-value = 0.008594
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 88.619, df = 2, p-value < 2.2e-16
```

```
##     year   Y_10km 
## 1.012706 1.012706
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
## [1] -226.7401
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-162](figure/unnamed-chunk-162-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-163](figure/unnamed-chunk-163-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + prop_tmoins1 + year2)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + prop_tmoins1 + year2, 
##     data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   7.476e+03  2.154e+03   3.470 0.000529 ***
## year         -7.381e+00  2.136e+00  -3.456 0.000558 ***
## prop_tmoins1  7.064e-02  1.984e-02   3.560 0.000378 ***
## year2         1.822e-03  5.295e-04   3.442 0.000588 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1641132)
## 
##     Null deviance: 430.49  on 2354  degrees of freedom
## Residual deviance: 385.83  on 2351  degrees of freedom
## AIC: 2433.2
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-165](figure/unnamed-chunk-165-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 2.7215, df = 1, p-value = 0.09901
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 7.9574, df = 3, p-value = 0.0469
```

```
##         year prop_tmoins1        year2 
## 1.211775e+06 1.039592e+00 1.211798e+06
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
## [1] 1.123909
```

``` r
regfit <- liste[1]$regfit
bdd <- liste[2]$bdd
```


``` r
reg.summary <- summary(regfit); plot_regsubsets(reg.summary)
```

![plot of chunk unnamed-chunk-167](figure/unnamed-chunk-167-1.png)


``` r
par(mfrow=c(1 ,1)) ; plot(regfit, scale ="bic")
```

![plot of chunk unnamed-chunk-168](figure/unnamed-chunk-168-1.png)


``` r
reg <- glm(data = bdd,
           formula = proportion_interet ~ year + X_10km + Dnst_Cultures)
```


``` r
summary(reg); autoplot(reg)
```

```
## 
## Call:
## glm(formula = proportion_interet ~ year + X_10km + Dnst_Cultures, 
##     data = bdd)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    0.228184   0.048485   4.706 2.72e-06 ***
## year2011       0.010310   0.043855   0.235  0.81417    
## year2012       0.039466   0.042505   0.928  0.35328    
## year2013       0.061916   0.042074   1.472  0.14132    
## year2014       0.019342   0.042317   0.457  0.64768    
## year2015      -0.008100   0.043899  -0.185  0.85363    
## year2016      -0.021629   0.042640  -0.507  0.61205    
## year2017       0.015418   0.042696   0.361  0.71806    
## year2018       0.016178   0.042253   0.383  0.70186    
## year2019      -0.011432   0.040795  -0.280  0.77934    
## year2020      -0.003312   0.041598  -0.080  0.93655    
## year2021       0.042773   0.041765   1.024  0.30591    
## year2022       0.025629   0.040937   0.626  0.53136    
## year2023       0.042826   0.041496   1.032  0.30220    
## year2024      -0.002951   0.040666  -0.073  0.94216    
## X_10km         0.020310   0.007069   2.873  0.00411 ** 
## Dnst_Cultures -0.228414   0.055130  -4.143 3.59e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.06721804)
## 
##     Null deviance: 117.72  on 1732  degrees of freedom
## Residual deviance: 115.35  on 1716  degrees of freedom
##   (2 observations deleted due to missingness)
## AIC: 258.18
## 
## Number of Fisher Scoring iterations: 2
```

![plot of chunk unnamed-chunk-170](figure/unnamed-chunk-170-1.png)

``` r
bgtest(reg); bptest(reg); car::vif(reg)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  reg
## LM test = 0.7113, df = 1, p-value = 0.399
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  reg
## BP = 28.888, df = 16, p-value = 0.0247
```

```
##                   GVIF Df GVIF^(1/(2*Df))
## year          1.014634 14        1.000519
## X_10km        1.073576  1        1.036135
## Dnst_Cultures 1.074796  1        1.036724
```

# Mortalite routieres - DIRO uniquement

Il me manque la colonne bdd_originale... Ou est elle passee
