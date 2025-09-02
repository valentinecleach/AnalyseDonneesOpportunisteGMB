---
title: "Fin Stage"
date: "2025-09-02"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
---







On a utilise definit nos sites par des mailles 10x10 sur l'ensemble de la Bretagne. On a retire les sites dont l'aire est strictement infereur a $10km^2$. On a donc eu 317 sites au total.


``` r
VariablesSite%>%
  ggplot()+geom_sf()+labs(title = "Sites utilises pour nos analyses")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

# Regressions

On a fait des regressions lineaires avec la proportion de l'espece sur un site chaque annee $t$ en question avec d'autres especes. On a inclut divers variables tel que :

-   l'annee en qualitatif ou en quantitatif selon le cas, ainsi que l'annee au carre si applicable.

-   Des variables d'environnement tel que la distance au littoral, la densite de cultures, etc.

-   La proportion a l'annee precedente $t-1$, ainsi que l'annee qui precede celle ci $t-2$, afin de reduire l'autocorrelation.

Nous avons utilise le BIC pour choisir le modele optimale. Cependant, bien que la plupart des resultats etaient coherents avec ce qui peux être observe, les modeles ne semblait pas valider les hypotheses necessaire aux GLM (residus et differents tests).

De plus, comme nous utilisons une espece temoin, ceci peut affecter le modele. Nous sommes donc parti sur des modeles d'occupancy.

# Modeles Occupancy

On a fait differents modele d'occupancy sur differentes especes. Les especes qui nous ont interesse le plus etaient:

-   Le sanglier

-   Le renard

-   Le lapin

-   Le herisson

-   Le putois

-   L'hermine

-   La fouine (pas presente ici)

    En lisant de la biblio, les especes moins frequentamment reperes ont des meilleurs resultats avec des periodes plus longues. On a donc separe les especes soit en 5 periodes de 3 ans, ou alors 3 periodes de 5 ans. On a les mêmes variables d'environnement que pour les GLM. On ajoute la variable pression d'observation qui prends en compte le nombre d'observation autre que l'espece de notre modele, par site et par annee.

    Variables pour definir les annees :


``` r
quali_periodes_matrice_5 <- 
      matrix(c('2010 - 2012', '2013 - 2015', 
               '2016 - 2018', '2019-2021', '2022 - 2024'),
             nrow = 317, 
             ncol = 5, 
             byrow = TRUE)

    quanti_periodes_matrice_5 <- matrix(
      1:5, 
      nrow=317,
      ncol=5, 
      byrow=TRUE)
```



``` r
quali_periodes_matrice_3 <- 
      matrix(c('2010 - 2014','2015 - 2019', '2020 - 2024'),
             nrow = 317, 
             ncol = 3, 
             byrow = TRUE)

    quanti_periodes_matrice_3 <- matrix(
      1:3, 
      nrow=317,
      ncol=3, 
      byrow=TRUE)
```



``` r
site_covs_periodes_5 <- list(
      quanti_periodes = quanti_periodes_matrice_5,
      quali_periodes = quanti_periodes_matrice_5
    )

    site_covs_periodes_3 <- list(
      quanti_periodes = quanti_periodes_matrice_3,
      quali_periodes = quanti_periodes_matrice_3
    )
```

## Le sanglier


``` r
    detection_matrice <- matrice_occu_detections(num_cd_nom = 60981)

    umf <- unmarkedFrameOccu(y = detection_matrice, 
                             siteCovs = site_info)
```

    On observe que les sites sont de plus en plus nombreux :


``` r
    plot(umf,
         main="Detection/Non Detection de Sanglier sur les sites de 2010 a 2024 inclus")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

``` r
    print(colSums(detection_matrice))
```

```
## 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 
##   20   41   45   32   59   38   51   53   73   71   94   90  116  114  120
```


``` r
    pression_obs <- pression_obs_bdd(num_cd_nom = 60981)

    umf <- unmarked::unmarkedMultFrame(
      y = detection_matrice,
      siteCovs = site_info,
      obsCovs = list(pression_obs = detection_autres),
      yearlySiteCovs = site_covs_periodes_5,
      numPrimary = 5
    )

    fm <- unmarked::colext(
      psiformula = ~ X_10km + Dnst_Cultures,     # initial occupancy
      gammaformula =  ~ 1,  # colonization
      epsilonformula = ~ 1, # extinction
      pformula = ~ pression_obs,  # detection
      data = umf, # data
      control = list(trace = 1),
      se = TRUE)
```

```
## initial  value 2390.684310 
## iter  10 value 2069.759253
## iter  20 value 2060.362740
## final  value 2060.323358 
## converged
```

    coefficients : 
    

``` r
    fm
```

```
## 
## Call:
## unmarked::colext(psiformula = ~X_10km + Dnst_Cultures, gammaformula = ~1, 
##     epsilonformula = ~1, pformula = ~pression_obs, data = umf, 
##     se = TRUE, control = list(trace = 1))
## 
## Initial (logit-scale):
##               Estimate    SE      z P(>|z|)
## (Intercept)    -0.0659 0.207 -0.318  0.7505
## X_10km          0.6869 0.231  2.967  0.0030
## Dnst_Cultures  -0.5969 0.230 -2.590  0.0096
## 
## Colonization (logit-scale):
##  Estimate    SE    z P(>|z|)
##    -0.216 0.166 -1.3   0.193
## 
## Extinction (logit-scale):
##  Estimate    SE     z  P(>|z|)
##     -2.97 0.439 -6.78 1.17e-11
## 
## Detection (logit-scale):
##              Estimate      SE     z   P(>|z|)
## (Intercept)   -1.8035 0.07951 -22.7 6.70e-114
## pression_obs   0.0379 0.00223  17.0  1.17e-64
## 
## AIC: 4134.647 
## Number of sites: 317
```

``` r
    backTransform(fm, "col"); confint(backTransform(fm, "col"))
```

```
## Backtransformed linear combination(s) of Colonization estimate(s)
## 
##  Estimate    SE LinComb (Intercept)
##     0.446 0.041  -0.216           1
## 
## Transformation: logistic
```

```
##      0.025     0.975
##  0.3679646 0.5273005
```

``` r
    backTransform(fm, "ext"); confint(backTransform(fm, "ext"))
```

```
## Backtransformed linear combination(s) of Extinction estimate(s)
## 
##  Estimate     SE LinComb (Intercept)
##    0.0486 0.0203   -2.97           1
## 
## Transformation: logistic
```

```
##       0.025     0.975
##  0.02116279 0.1076269
```

    graphiques :


``` r
    plot <- proba_graphique_5(fm, couleur=couleur_sanglier)
```

```
## initial  value 2386.289861 
## iter  10 value 2061.328506
## iter  20 value 2032.942836
## final  value 2028.989640 
## converged
## initial  value 2452.206598 
## iter  10 value 2083.999045
## iter  20 value 2065.533606
## final  value 2065.309486 
## converged
## initial  value 2450.009374 
## iter  10 value 2121.791833
## iter  20 value 2103.847733
## final  value 2103.753688 
## converged
## initial  value 2357.725942 
## iter  10 value 2100.747309
## final  value 2097.966397 
## converged
## initial  value 2294.006429 
## iter  10 value 1993.945755
## iter  20 value 1986.089078
## final  value 1986.057777 
## converged
## initial  value 2366.514840 
## iter  10 value 2039.104864
## iter  20 value 2008.434629
## final  value 2008.095439 
## converged
## initial  value 2419.248230 
## iter  10 value 2088.967869
## iter  20 value 2082.223226
## iter  30 value 2056.679354
## iter  30 value 2056.679345
## iter  30 value 2056.679339
## final  value 2056.679339 
## converged
## initial  value 2351.134268 
## iter  10 value 2012.336453
## iter  20 value 2004.669769
## final  value 2004.534475 
## converged
## initial  value 2428.037128 
## iter  10 value 2069.205883
## iter  20 value 2050.264270
## iter  30 value 2030.530840
## final  value 2030.530384 
## converged
## initial  value 2421.445454 
## iter  10 value 2102.478962
## iter  20 value 2096.211060
## final  value 2096.178577 
## converged
## initial  value 2408.262107 
## iter  10 value 2096.460478
## iter  20 value 2088.502522
## final  value 2088.472160 
## converged
## initial  value 2410.459331 
## iter  10 value 2118.922328
## iter  20 value 2087.690092
## final  value 2086.059847 
## converged
## initial  value 2384.092636 
## iter  10 value 2062.209005
## iter  20 value 2049.286463
## final  value 2049.274990 
## converged
## initial  value 2408.262107 
## iter  10 value 2097.552870
## iter  20 value 2086.281014
## final  value 2086.224211 
## converged
## initial  value 2379.698187 
## iter  10 value 2013.997633
## iter  20 value 2005.395955
## final  value 2005.355419 
## converged
## initial  value 2417.051005 
## iter  10 value 2079.419406
## iter  20 value 2071.996610
## final  value 2071.961261 
## converged
## initial  value 2368.712064 
## iter  10 value 2055.427850
## iter  20 value 2042.708360
## final  value 2031.215856 
## converged
## initial  value 2322.570348 
## iter  10 value 2017.189039
## iter  20 value 2012.052210
## final  value 2012.038626 
## converged
## initial  value 2335.753696 
## iter  10 value 1994.931930
## iter  20 value 1983.745209
## final  value 1983.709514 
## converged
## initial  value 2395.078759 
## iter  10 value 2034.769192
## iter  20 value 2031.847286
## final  value 2031.842574 
## converged
## initial  value 2333.556471 
## iter  10 value 1992.190899
## iter  20 value 1985.925648
## final  value 1985.808820 
## converged
## initial  value 2324.767573 
## iter  10 value 2025.914048
## iter  20 value 2012.040836
## iter  30 value 1984.520880
## final  value 1984.520491 
## converged
## initial  value 2425.839903 
## iter  10 value 2119.203338
## iter  20 value 2093.230418
## iter  30 value 2086.652144
## iter  30 value 2086.652144
## final  value 2086.652144 
## converged
## initial  value 2414.853781 
## iter  10 value 2057.772877
## iter  20 value 2052.034451
## final  value 2052.031477 
## converged
## initial  value 2377.500963 
## iter  10 value 2016.121900
## iter  20 value 1998.175143
## final  value 1998.147880 
## converged
## initial  value 2381.895412 
## iter  10 value 2072.230009
## iter  20 value 2064.937938
## final  value 2064.682317 
## converged
## initial  value 2309.387001 
## iter  10 value 1994.024115
## iter  20 value 1954.827739
## final  value 1953.991525 
## converged
## initial  value 2579.645624 
## iter  10 value 2190.804721
## iter  20 value 2186.477126
## final  value 2185.777505 
## converged
## initial  value 2478.573293 
## iter  10 value 2167.309234
## iter  20 value 2157.266418
## final  value 2156.999412 
## converged
## initial  value 2432.431577 
## iter  10 value 2097.078981
## final  value 2086.189963 
## converged
## initial  value 2395.078759 
## iter  10 value 2047.220731
## iter  20 value 2031.190655
## final  value 2030.825904 
## converged
## initial  value 2276.428632 
## iter  10 value 1993.322808
## final  value 1992.308070 
## converged
## initial  value 2346.739819 
## iter  10 value 1983.915045
## iter  20 value 1974.447273
## final  value 1974.429892 
## converged
## initial  value 2414.853781 
## iter  10 value 2125.372378
## iter  20 value 2116.296665
## final  value 2115.998727 
## converged
## initial  value 2340.148145 
## iter  10 value 2059.806097
## final  value 2052.726603 
## converged
## initial  value 2410.459331 
## iter  10 value 2090.673118
## iter  20 value 2077.046770
## iter  30 value 2067.586392
## final  value 2067.585836 
## converged
## initial  value 2414.853781 
## iter  10 value 2092.469264
## iter  20 value 2083.155263
## iter  30 value 2069.806470
## final  value 2069.777733 
## converged
## initial  value 2408.262107 
## iter  10 value 2050.092456
## iter  20 value 2044.361911
## final  value 2042.953971 
## converged
## initial  value 2353.331492 
## iter  10 value 2068.119227
## iter  20 value 2060.293841
## iter  30 value 2043.936113
## iter  40 value 2029.275403
## final  value 2029.263526 
## converged
## initial  value 2487.362192 
## iter  10 value 2177.841158
## iter  20 value 2152.972132
## final  value 2151.758373 
## converged
## initial  value 2390.684310 
## iter  10 value 2033.705562
## iter  20 value 1993.906791
## final  value 1992.821745 
## converged
## initial  value 2419.248230 
## iter  10 value 2155.772778
## iter  20 value 2147.628450
## final  value 2147.627587 
## converged
## initial  value 2337.950920 
## iter  10 value 2037.769308
## final  value 2035.885668 
## converged
## initial  value 2392.881535 
## iter  10 value 2059.134412
## iter  20 value 2051.126710
## final  value 2050.995372 
## converged
## initial  value 2399.473209 
## iter  10 value 2058.770686
## iter  20 value 2044.725150
## final  value 2044.556843 
## converged
## initial  value 2342.345370 
## iter  10 value 2035.626611
## iter  20 value 1997.809858
## final  value 1996.438551 
## converged
## initial  value 2298.400878 
## iter  10 value 1947.295365
## iter  20 value 1936.494780
## final  value 1936.483495 
## converged
## initial  value 2320.373124 
## iter  10 value 2073.011406
## iter  20 value 2063.423044
## final  value 2063.420099 
## converged
## initial  value 2454.403823 
## iter  10 value 2171.122929
## iter  20 value 2155.841709
## final  value 2155.730668 
## converged
## initial  value 2344.542594 
## iter  10 value 2048.891921
## iter  20 value 2034.790250
## final  value 2034.780685 
## converged
## initial  value 2434.628802 
## iter  10 value 2083.285318
## iter  20 value 2073.543246
## final  value 2073.300977 
## converged
## initial  value 2417.051005 
## iter  10 value 2084.007930
## iter  20 value 2065.893955
## final  value 2060.477144 
## converged
## initial  value 2348.937043 
## iter  10 value 2055.395887
## iter  20 value 2043.568895
## final  value 2043.526223 
## converged
## initial  value 2439.023251 
## iter  10 value 2125.075858
## iter  20 value 2117.258374
## iter  30 value 2091.845323
## final  value 2084.284128 
## converged
## initial  value 2375.303738 
## iter  10 value 2069.752431
## iter  20 value 2058.655922
## iter  30 value 2051.287164
## final  value 2051.286972 
## converged
## initial  value 2450.009374 
## iter  10 value 2164.219953
## iter  20 value 2150.168039
## iter  30 value 2140.083525
## final  value 2140.083445 
## converged
## initial  value 2379.698187 
## iter  10 value 2101.900882
## iter  20 value 2088.781935
## iter  30 value 2071.733203
## final  value 2071.704898 
## converged
## initial  value 2417.051005 
## iter  10 value 2066.491067
## iter  20 value 2051.918996
## final  value 2051.391730 
## converged
## initial  value 2428.037128 
## iter  10 value 2111.478933
## iter  20 value 2101.787224
## final  value 2101.768635 
## converged
## initial  value 2346.739819 
## iter  10 value 1975.751329
## iter  20 value 1969.959812
## final  value 1969.957637 
## converged
## initial  value 2366.514840 
## iter  10 value 2061.286542
## iter  20 value 2049.387747
## final  value 2049.328157 
## converged
## initial  value 2439.023251 
## iter  10 value 2087.981452
## iter  20 value 2075.014327
## final  value 2074.565301 
## converged
## initial  value 2355.528717 
## iter  10 value 2086.702676
## iter  20 value 2078.433890
## final  value 2078.413188 
## converged
## initial  value 2399.473209 
## iter  10 value 2083.970143
## iter  20 value 2078.259440
## iter  30 value 2040.787455
## final  value 2037.259971 
## converged
## initial  value 2366.514840 
## iter  10 value 2032.508734
## iter  20 value 2020.887892
## final  value 2020.762946 
## converged
## initial  value 2417.051005 
## iter  10 value 2029.662027
## iter  20 value 2028.372963
## final  value 2028.334700 
## converged
## initial  value 2469.784395 
## iter  10 value 2121.917806
## iter  20 value 2097.120067
## final  value 2087.662390 
## converged
## initial  value 2454.403823 
## iter  10 value 2128.538921
## iter  20 value 2112.352062
## final  value 2104.701112 
## converged
## initial  value 2447.812149 
## iter  10 value 2127.759794
## iter  20 value 2119.441974
## final  value 2119.440037 
## converged
## initial  value 2482.967742 
## iter  10 value 2129.194825
## iter  20 value 2116.117522
## final  value 2116.084318 
## converged
## initial  value 2443.417700 
## iter  10 value 2158.315504
## iter  20 value 2153.501927
## iter  30 value 2139.586341
## final  value 2137.984074 
## converged
## initial  value 2425.839903 
## iter  10 value 2138.908887
## iter  20 value 2110.562589
## final  value 2108.710185 
## converged
## initial  value 2353.331492 
## iter  10 value 2043.425002
## iter  20 value 2040.871376
## iter  30 value 2031.848596
## iter  40 value 2027.206775
## final  value 2027.205315 
## converged
## initial  value 2351.134268 
## iter  10 value 2033.302440
## iter  20 value 2004.766779
## final  value 2003.155829 
## converged
## initial  value 2375.303738 
## iter  10 value 2056.423515
## final  value 2052.893031 
## converged
## initial  value 2434.628802 
## iter  10 value 2087.182902
## iter  20 value 2080.694601
## iter  30 value 2050.686172
## final  value 2049.712043 
## converged
## initial  value 2463.192721 
## iter  10 value 2109.164095
## iter  20 value 2091.220456
## final  value 2091.184323 
## converged
## initial  value 2392.881535 
## iter  10 value 2071.923697
## iter  20 value 2069.302037
## iter  30 value 2056.371899
## iter  40 value 2051.000554
## final  value 2051.000502 
## converged
## initial  value 2276.428632 
## iter  10 value 1959.563716
## iter  20 value 1950.095052
## final  value 1950.022624 
## converged
## initial  value 2335.753696 
## iter  10 value 1921.315067
## iter  20 value 1904.551831
## final  value 1904.537113 
## converged
## initial  value 2368.712064 
## iter  10 value 2018.288767
## iter  20 value 2001.319696
## final  value 2001.276370 
## converged
## initial  value 2491.756641 
## iter  10 value 2125.338089
## iter  20 value 2115.626467
## final  value 2115.409275 
## converged
## initial  value 2300.598103 
## iter  10 value 1973.314930
## iter  20 value 1968.163428
## final  value 1968.160475 
## converged
## initial  value 2283.020306 
## iter  10 value 1996.763576
## iter  20 value 1987.565268
## final  value 1987.544025 
## converged
## initial  value 2335.753696 
## iter  10 value 2078.288985
## iter  20 value 2040.229564
## final  value 2039.989243 
## converged
## initial  value 2384.092636 
## iter  10 value 2090.813181
## iter  20 value 2085.458960
## final  value 2085.458008 
## converged
## initial  value 2302.795327 
## iter  10 value 1991.550080
## iter  20 value 1965.716574
## final  value 1965.376307 
## converged
## initial  value 2355.528717 
## iter  10 value 2073.175311
## iter  20 value 2027.181792
## final  value 2024.801429 
## converged
## initial  value 2454.403823 
## iter  10 value 2107.305948
## iter  20 value 2103.518351
## final  value 2103.517006 
## converged
## initial  value 2456.601048 
## iter  10 value 2102.626525
## iter  20 value 2094.608762
## final  value 2094.082957 
## converged
## initial  value 2375.303738 
## iter  10 value 2067.983415
## iter  20 value 2047.327805
## iter  30 value 2029.023964
## final  value 2029.019804 
## converged
## initial  value 2419.248230 
## iter  10 value 2058.536785
## iter  20 value 2025.665390
## final  value 2024.570307 
## converged
## initial  value 2392.881535 
## iter  10 value 2035.352442
## iter  20 value 2028.127932
## final  value 2028.077391 
## converged
## initial  value 2500.545539 
## iter  10 value 2218.218640
## iter  20 value 2178.377042
## final  value 2177.297203 
## converged
## initial  value 2390.684310 
## iter  10 value 2065.743272
## iter  20 value 2056.835418
## final  value 2056.824982 
## converged
## initial  value 2432.431577 
## iter  10 value 2113.986642
## iter  20 value 2102.330515
## final  value 2102.166135 
## converged
## initial  value 2476.376069 
## iter  10 value 2121.895432
## iter  20 value 2107.023648
## final  value 2106.699619 
## converged
## initial  value 2381.895412 
## iter  10 value 2088.625370
## iter  20 value 2078.590879
## final  value 2078.578265 
## converged
## initial  value 2375.303738 
## iter  10 value 2048.138207
## iter  20 value 2034.433763
## final  value 2034.387655 
## converged
## initial  value 2348.937043 
## iter  10 value 2089.945219
## final  value 2082.767153 
## converged
```

``` r
    plot +
      labs(title = "Probabilite d'occupation du Sanglier a travers les saisons",
           x = "Saisons", y ="Probabilite d'occupation lisee")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)



``` r
    plot <- carte_graphique_5(fm=fm, couleur=couleur_sanglier)
    ggpubr::annotate_figure(plot, 
                              top = ggpubr::text_grob("Presence du Sanglier"))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
    
     tests :


``` r
    pb <- parboot(fm, statistic=chisq, nsim=40, parallel=FALSE); pb
```

```
## initial  value 2065.068789 
## iter  10 value 2062.063261
## final  value 2062.058213 
## converged
## initial  value 2128.290004 
## iter  10 value 2125.132367
## final  value 2125.127412 
## converged
## initial  value 2030.665173 
## iter  10 value 2026.442106
## iter  10 value 2026.442086
## iter  10 value 2026.442079
## final  value 2026.442079 
## converged
## initial  value 2093.616901 
## iter  10 value 2083.284838
## final  value 2083.041599 
## converged
## initial  value 2131.086610 
## iter  10 value 2126.618005
## final  value 2126.599480 
## converged
## initial  value 2088.606935 
## iter  10 value 2084.856629
## final  value 2084.856256 
## converged
## initial  value 2014.701459 
## iter  10 value 2011.502322
## iter  10 value 2011.502313
## iter  10 value 2011.502307
## final  value 2011.502307 
## converged
## initial  value 2119.847486 
## iter  10 value 2116.895948
## final  value 2116.894379 
## converged
## initial  value 2001.829341 
## iter  10 value 1995.689991
## final  value 1995.621011 
## converged
## initial  value 2039.546143 
## iter  10 value 2035.173259
## final  value 2035.173117 
## converged
## initial  value 1980.682321 
## iter  10 value 1977.644773
## final  value 1977.622720 
## converged
## initial  value 1952.613811 
## iter  10 value 1948.549089
## final  value 1948.536356 
## converged
## initial  value 2104.448638 
## iter  10 value 2098.655136
## final  value 2098.654419 
## converged
## initial  value 2097.946083 
## iter  10 value 2096.225077
## final  value 2096.223964 
## converged
## initial  value 2035.538216 
## iter  10 value 2031.550751
## final  value 2031.550670 
## converged
## initial  value 2040.021082 
## iter  10 value 2037.189522
## final  value 2037.183096 
## converged
## initial  value 2044.155336 
## iter  10 value 2037.919478
## final  value 2037.919133 
## converged
## initial  value 2069.171418 
## iter  10 value 2066.529000
## final  value 2066.528788 
## converged
## initial  value 2063.087506 
## iter  10 value 2061.885489
## final  value 2061.885339 
## converged
## initial  value 2063.712983 
## iter  10 value 2061.797397
## final  value 2061.797226 
## converged
## initial  value 1997.580850 
## iter  10 value 1994.080023
## final  value 1994.079619 
## converged
## initial  value 2062.463703 
## iter  10 value 2060.861646
## final  value 2060.860893 
## converged
## initial  value 2124.341219 
## iter  10 value 2122.382863
## final  value 2122.353010 
## converged
## initial  value 2041.233031 
## iter  10 value 2039.647141
## final  value 2039.622141 
## converged
## initial  value 2096.619296 
## iter  10 value 2094.643049
## iter  10 value 2094.643044
## iter  10 value 2094.643044
## final  value 2094.643044 
## converged
## initial  value 2086.124145 
## iter  10 value 2081.241371
## final  value 2081.239776 
## converged
## initial  value 2115.408110 
## iter  10 value 2112.876022
## final  value 2112.875944 
## converged
## initial  value 2048.587035 
## iter  10 value 2045.727927
## final  value 2045.727861 
## converged
## initial  value 2115.791273 
## iter  10 value 2110.613917
## final  value 2110.612347 
## converged
## initial  value 2051.332860 
## iter  10 value 2050.591202
## iter  10 value 2050.591198
## iter  10 value 2050.591198
## final  value 2050.591198 
## converged
## initial  value 2123.568117 
## iter  10 value 2117.479841
## final  value 2117.479098 
## converged
## initial  value 2001.676927 
## iter  10 value 1999.105355
## final  value 1999.097740 
## converged
## initial  value 2071.804850 
## iter  10 value 2070.109127
## final  value 2070.109065 
## converged
## initial  value 2098.768676 
## iter  10 value 2096.708603
## final  value 2096.708092 
## converged
## initial  value 2062.345560 
## iter  10 value 2060.231461
## final  value 2060.207315 
## converged
## initial  value 2034.592900 
## iter  10 value 2027.950562
## final  value 2027.950192 
## converged
## initial  value 2109.363562 
## iter  10 value 2100.583527
## final  value 2099.836028 
## converged
## initial  value 2084.304132 
## iter  10 value 2079.460196
## final  value 2079.459592 
## converged
## initial  value 2097.893263 
## iter  10 value 2095.747604
## final  value 2095.744599 
## converged
## initial  value 2091.985735 
## iter  10 value 2088.527352
## final  value 2088.519248 
## converged
```

```
## 
## Call: parboot(object = fm, statistic = chisq, nsim = 40, parallel = FALSE)
## 
## Parametric Bootstrap Statistics:
##     t0 mean(t0 - t_B) StdDev(t0 - t_B) Pr(t_B > t0)
## 1 4378           -383             59.8        0.976
## 
## t_B quantiles:
##        0% 2.5%  25%  50%  75% 97.5% 100%
## [1,] 4619 4674 4730 4747 4792  4885 4903
## 
## t0 = Original statistic computed from data
## t_B = Vector of bootstrap samples
```


``` r
    AICcmodavg::mb.gof.test(fm)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

```
## 
## Goodness-of-fit for dynamic occupancy model
## 
## Number of seasons:  5 
## 
## Chi-square statistic:
## Season 1 Season 2 Season 3 Season 4 Season 5 
##  13.9910  16.3399  18.2582  14.1132  48.6259 
## 
## Total chi-square = 111.3281 
## Number of bootstrap samples = 5
## P-value = 0
## 
## Quantiles of bootstrapped statistics:
##   0%  25%  50%  75% 100% 
##   18   25   29   32   35 
## 
## Estimate of c-hat = 4.01
```

## Le renard roux

``` r
    detection_matrice <- matrice_occu_detections(num_cd_nom = 60585)

    umf <- unmarkedFrameOccu(y = detection_matrice, 
                             siteCovs = site_info)
```



``` r
    plot(umf,
         main="Detection/Non Detection du renard sur les sites de 2010 a 2024 inclus")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)

``` r
    print(colSums(detection_matrice))
```

```
## 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 
##  144  211  232  213  231  188  200  186  200  191  167  175  189  199  208
```


``` r
    pression_obs <- pression_obs_bdd(num_cd_nom = 60585)

    umf <- unmarked::unmarkedMultFrame(
      y = detection_matrice,
      siteCovs = site_info,
      obsCovs = list(pression_obs = detection_autres),
      yearlySiteCovs = site_covs_periodes_5,
      numPrimary = 5
    )

    fm <- unmarked::colext(
    psiformula = ~ 1,     # initial occupancy
    gammaformula =  ~ 1,  # colonization
    epsilonformula = ~ 1, # extinction
    pformula = ~ pression_obs,  # detection
    data = umf, # data
    se = TRUE)
```

    coefficients : 
    

``` r
    fm
```

```
## 
## Call:
## unmarked::colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, 
##     pformula = ~pression_obs, data = umf, se = TRUE)
## 
## Initial (logit-scale):
##  Estimate    SE    z  P(>|z|)
##       3.3 0.518 6.38 1.75e-10
## 
## Colonization (logit-scale):
##  Estimate    SE      z P(>|z|)
##   -0.0969 0.362 -0.267   0.789
## 
## Extinction (logit-scale):
##  Estimate    SE     z P(>|z|)
##     -3.57 0.311 -11.5 1.8e-30
## 
## Detection (logit-scale):
##              Estimate      SE     z  P(>|z|)
## (Intercept)   -0.3294 0.05982 -5.51 3.65e-08
## pression_obs   0.0598 0.00322 18.56 7.03e-77
## 
## AIC: 5456.153 
## Number of sites: 317
```

``` r
    backTransform(fm, "col"); confint(backTransform(fm, "col"))
```

```
## Backtransformed linear combination(s) of Colonization estimate(s)
## 
##  Estimate     SE LinComb (Intercept)
##     0.476 0.0904 -0.0969           1
## 
## Transformation: logistic
```

```
##      0.025     0.975
##  0.3084968 0.6487255
```

``` r
    backTransform(fm, "ext"); confint(backTransform(fm, "ext"))
```

```
## Backtransformed linear combination(s) of Extinction estimate(s)
## 
##  Estimate      SE LinComb (Intercept)
##    0.0274 0.00828   -3.57           1
## 
## Transformation: logistic
```

```
##       0.025      0.975
##  0.01505073 0.04921592
```

    tests :


``` r
    pb <- parboot(fm, statistic=chisq, nsim=40, parallel=FALSE); pb
```

```
## 
## Call: parboot(object = fm, statistic = chisq, nsim = 40, parallel = FALSE)
## 
## Parametric Bootstrap Statistics:
##     t0 mean(t0 - t_B) StdDev(t0 - t_B) Pr(t_B > t0)
## 1 4427           -350              106        0.976
## 
## t_B quantiles:
##        0% 2.5%  25%  50%  75% 97.5% 100%
## [1,] 4491 4563 4715 4768 4836  4932 5041
## 
## t0 = Original statistic computed from data
## t_B = Vector of bootstrap samples
```


``` r
    AICcmodavg::mb.gof.test(fm)
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png)

```
## 
## Goodness-of-fit for dynamic occupancy model
## 
## Number of seasons:  5 
## 
## Chi-square statistic:
## Season 1 Season 2 Season 3 Season 4 Season 5 
##  92.1906  50.3443  17.5839  34.7673   7.1825 
## 
## Total chi-square = 202.0686 
## Number of bootstrap samples = 5
## P-value = 0
## 
## Quantiles of bootstrapped statistics:
##   0%  25%  50%  75% 100% 
##   29   30   31   32   35 
## 
## Estimate of c-hat = 6.48
```

    graphiques :


``` r
    plot <- proba_graphique_5(fm = fm, couleur=couleur_renard)
    plot +
      labs(title = "Probabilite d'occupation du Renard a travers les saisons",
           x = "Saisons", y ="Probabilite d'occupation lisee")
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-1.png)

``` r
    plot <- carte_graphique_5(fm=fm, couleur=couleur_renard)
    ggpubr::annotate_figure(plot, 
                              top = ggpubr::text_grob("Presence du Renard"))
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-2.png)

## Le lapin de garenne


``` r
    detection_matrice <- matrice_occu_detections(num_cd_nom = 61714)

    umf <- unmarkedFrameOccu(y = detection_matrice, 
                             siteCovs = site_info)
```


``` r
    plot(umf,
         main="Detection/Non Detection de lapins sur les sites de 2010 a 2024 inclus")
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png)

``` r
    print(colSums(detection_matrice))
```

```
## 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 
##   95  165  179  158  166  123  140  134  147  143  158  153  152  143  122
```


``` r
    pression_obs <- pression_obs_bdd(num_cd_nom = 61714)

    umf <- unmarked::unmarkedMultFrame(
      y = detection_matrice,
      siteCovs = site_info,
      obsCovs = list(pression_obs = detection_autres),
      yearlySiteCovs = site_covs_periodes_5,
      numPrimary = 5
    )

  fm <- unmarked::colext(
    psiformula = ~ Dnst_Cultures + Dist_Littoral, # initial occupancy
    gammaformula =  ~ 1,  # colonization
    epsilonformula = ~ 1, # extinction
    pformula = ~ pression_obs,  # detection
    data = umf, # data
    control = list(trace = 1),
    se = TRUE)
```

```
## initial  value 3425.577086 
## iter  10 value 2759.081999
## iter  20 value 2753.701509
## final  value 2753.690634 
## converged
```
    
    coefficients : 
    

``` r
    fm
```

```
## 
## Call:
## unmarked::colext(psiformula = ~Dnst_Cultures + Dist_Littoral, 
##     gammaformula = ~1, epsilonformula = ~1, pformula = ~pression_obs, 
##     data = umf, se = TRUE, control = list(trace = 1))
## 
## Initial (logit-scale):
##               Estimate    SE     z P(>|z|)
## (Intercept)      4.538 1.630  2.78 0.00538
## Dnst_Cultures   -1.648 1.016 -1.62 0.10463
## Dist_Littoral   -0.868 0.435 -1.99 0.04605
## 
## Colonization (logit-scale):
##  Estimate    SE     z P(>|z|)
##    -0.394 0.302 -1.31   0.192
## 
## Extinction (logit-scale):
##  Estimate    SE     z  P(>|z|)
##      -2.5 0.203 -12.3 8.29e-35
## 
## Detection (logit-scale):
##              Estimate      SE     z  P(>|z|)
## (Intercept)   -0.9126 0.06234 -14.6 1.62e-48
## pression_obs   0.0536 0.00272  19.7 3.94e-86
## 
## AIC: 5521.381 
## Number of sites: 317
```

``` r
    backTransform(fm, "col"); confint(backTransform(fm, "col"))
```

```
## Backtransformed linear combination(s) of Colonization estimate(s)
## 
##  Estimate     SE LinComb (Intercept)
##     0.403 0.0725  -0.394           1
## 
## Transformation: logistic
```

```
##      0.025     0.975
##  0.2719704 0.5491959
```

``` r
    backTransform(fm, "ext"); confint(backTransform(fm, "ext"))
```

```
## Backtransformed linear combination(s) of Extinction estimate(s)
## 
##  Estimate     SE LinComb (Intercept)
##    0.0757 0.0142    -2.5           1
## 
## Transformation: logistic
```

```
##       0.025     0.975
##  0.05208577 0.1086882
```

    tests :


``` r
    pb <- parboot(fm, statistic=chisq, nsim=40, parallel=FALSE); pb
```

```
## initial  value 2825.533962 
## iter  10 value 2815.711824
## final  value 2815.563417 
## converged
## initial  value 2839.147070 
## iter  10 value 2834.953726
## final  value 2834.949282 
## converged
## initial  value 2830.836334 
## iter  10 value 2828.667028
## final  value 2828.666848 
## converged
## initial  value 2785.482492 
## iter  10 value 2780.673705
## final  value 2780.639968 
## converged
## initial  value 2852.671970 
## iter  10 value 2846.998297
## final  value 2846.881334 
## converged
## initial  value 2788.863950 
## iter  10 value 2787.066977
## final  value 2787.066596 
## converged
## initial  value 2813.903891 
## final  value 2812.903256 
## converged
## initial  value 2793.956063 
## iter  10 value 2786.827602
## final  value 2786.820180 
## converged
## initial  value 2794.981360 
## iter  10 value 2790.575322
## final  value 2790.555883 
## converged
## initial  value 2832.479919 
## iter  10 value 2829.752895
## final  value 2829.750554 
## converged
## initial  value 2809.687420 
## iter  10 value 2804.581750
## final  value 2804.510993 
## converged
## initial  value 2765.594196 
## iter  10 value 2762.407666
## final  value 2762.343210 
## converged
## initial  value 2780.851598 
## iter  10 value 2778.249973
## iter  10 value 2778.249943
## iter  10 value 2778.249941
## final  value 2778.249941 
## converged
## initial  value 2854.025032 
## iter  10 value 2849.305668
## final  value 2848.906788 
## converged
## initial  value 2800.576369 
## iter  10 value 2798.383090
## iter  20 value 2797.172873
## final  value 2797.010045 
## converged
## initial  value 2773.825309 
## iter  10 value 2771.043105
## final  value 2771.027198 
## converged
## initial  value 2732.163231 
## iter  10 value 2724.503338
## final  value 2724.500790 
## converged
## initial  value 2781.109923 
## iter  10 value 2777.738783
## iter  20 value 2777.613050
## final  value 2777.460863 
## converged
## initial  value 2856.537559 
## iter  10 value 2850.675769
## final  value 2850.476661 
## converged
## initial  value 2814.724851 
## iter  10 value 2812.272487
## final  value 2812.249574 
## converged
## initial  value 2781.434742 
## iter  10 value 2778.469130
## final  value 2778.467776 
## converged
## initial  value 2785.854916 
## iter  10 value 2780.948136
## final  value 2780.797168 
## converged
## initial  value 2807.918829 
## iter  10 value 2802.226989
## final  value 2802.017113 
## converged
## initial  value 2764.835872 
## iter  10 value 2761.051662
## final  value 2761.042209 
## converged
## initial  value 2830.759592 
## iter  10 value 2829.102183
## final  value 2829.101554 
## converged
## initial  value 2821.863234 
## iter  10 value 2820.706159
## final  value 2820.704563 
## converged
## initial  value 2822.564626 
## iter  10 value 2817.115205
## final  value 2817.110916 
## converged
## initial  value 2835.339687 
## iter  10 value 2831.853231
## final  value 2831.816020 
## converged
## initial  value 2805.071095 
## iter  10 value 2804.657409
## final  value 2804.655082 
## converged
## initial  value 2836.290976 
## iter  10 value 2830.666372
## final  value 2830.554032 
## converged
## initial  value 2808.588096 
## iter  10 value 2804.293281
## final  value 2802.917490 
## converged
## initial  value 2836.824048 
## iter  10 value 2832.165849
## final  value 2832.116973 
## converged
## initial  value 2850.207546 
## iter  10 value 2847.388344
## final  value 2847.380622 
## converged
## initial  value 2785.810855 
## iter  10 value 2780.666102
## final  value 2780.619434 
## converged
## initial  value 2770.711599 
## iter  10 value 2768.153795
## iter  10 value 2768.153772
## iter  10 value 2768.153769
## final  value 2768.153769 
## converged
## initial  value 2796.797798 
## iter  10 value 2795.484467
## iter  10 value 2795.484464
## iter  10 value 2795.484462
## final  value 2795.484462 
## converged
## initial  value 2845.144680 
## iter  10 value 2838.504038
## final  value 2838.487584 
## converged
## initial  value 2867.383268 
## iter  10 value 2854.612205
## final  value 2854.505084 
## converged
## initial  value 2812.675979 
## iter  10 value 2811.173943
## final  value 2811.173890 
## converged
## initial  value 2833.121124 
## iter  10 value 2829.324815
## final  value 2829.316292 
## converged
```

```
## 
## Call: parboot(object = fm, statistic = chisq, nsim = 40, parallel = FALSE)
## 
## Parametric Bootstrap Statistics:
##     t0 mean(t0 - t_B) StdDev(t0 - t_B) Pr(t_B > t0)
## 1 4243           -523             65.5        0.976
## 
## t_B quantiles:
##        0% 2.5%  25%  50%  75% 97.5% 100%
## [1,] 4659 4667 4704 4771 4813  4876 4887
## 
## t0 = Original statistic computed from data
## t_B = Vector of bootstrap samples
```


``` r
    AICcmodavg::mb.gof.test(fm)
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-1.png)

```
## 
## Goodness-of-fit for dynamic occupancy model
## 
## Number of seasons:  5 
## 
## Chi-square statistic:
## Season 1 Season 2 Season 3 Season 4 Season 5 
##  75.4974  30.5561  17.5884  11.7653  17.0179 
## 
## Total chi-square = 152.4252 
## Number of bootstrap samples = 5
## P-value = 0
## 
## Quantiles of bootstrapped statistics:
##   0%  25%  50%  75% 100% 
##   23   26   32   35   41 
## 
## Estimate of c-hat = 4.83
```

    graphiques :


``` r
    plot <- proba_graphique_5(fm, couleur=couleur_lapin)
```

```
## initial  value 3383.829819 
## iter  10 value 2751.772889
## iter  20 value 2743.186380
## final  value 2743.169949 
## converged
## initial  value 3403.604840 
## iter  10 value 2743.524839
## iter  20 value 2737.184083
## final  value 2736.478753 
## converged
## initial  value 3353.068675 
## iter  10 value 2744.522166
## final  value 2739.843875 
## converged
## initial  value 3372.843696 
## iter  10 value 2753.975228
## final  value 2746.707570 
## converged
## initial  value 3407.999290 
## iter  10 value 2744.607670
## final  value 2734.004177 
## converged
## initial  value 3328.899205 
## iter  10 value 2715.608248
## final  value 2698.046815 
## converged
## initial  value 3445.352107 
## iter  10 value 2691.303507
## iter  20 value 2645.883453
## iter  30 value 2638.272094
## final  value 2638.213966 
## converged
## initial  value 3397.013167 
## iter  10 value 2740.052960
## final  value 2733.204711 
## converged
## initial  value 3491.493823 
## iter  10 value 2821.037196
## iter  20 value 2815.863911
## iter  30 value 2814.044164
## iter  40 value 2783.585823
## iter  50 value 2758.594119
## iter  60 value 2758.335486
## final  value 2758.171776 
## converged
## initial  value 3381.632595 
## iter  10 value 2714.500046
## iter  20 value 2711.609335
## final  value 2711.367455 
## converged
## initial  value 3377.238145 
## iter  10 value 2765.954520
## iter  20 value 2761.105466
## iter  30 value 2734.366559
## iter  40 value 2722.028813
## iter  50 value 2719.845280
## final  value 2719.845224 
## converged
## initial  value 3375.040921 
## iter  10 value 2755.967164
## iter  20 value 2746.164718
## final  value 2746.064234 
## converged
## initial  value 3392.618717 
## iter  10 value 2696.693457
## iter  20 value 2688.228986
## iter  30 value 2685.983742
## iter  40 value 2685.594128
## final  value 2685.169602 
## converged
## initial  value 3377.238145 
## iter  10 value 2765.557490
## iter  20 value 2763.794490
## final  value 2763.726658 
## converged
## initial  value 3386.027044 
## iter  10 value 2720.468762
## final  value 2718.761876 
## converged
## initial  value 3418.985412 
## iter  10 value 2795.167425
## iter  20 value 2792.611395
## final  value 2792.575061 
## converged
## initial  value 3504.677171 
## iter  10 value 2763.343170
## iter  20 value 2750.585087
## iter  30 value 2744.576160
## final  value 2744.575959 
## converged
## initial  value 3377.238145 
## iter  10 value 2684.928904
## iter  20 value 2680.034171
## iter  30 value 2679.026976
## iter  30 value 2679.026947
## iter  30 value 2679.026945
## final  value 2679.026945 
## converged
## initial  value 3489.296599 
## iter  10 value 2873.555443
## iter  20 value 2865.773628
## final  value 2863.492351 
## converged
## initial  value 3407.999290 
## iter  10 value 2703.406345
## iter  20 value 2692.717318
## final  value 2692.305386 
## converged
## initial  value 3412.393739 
## iter  10 value 2776.366915
## final  value 2774.943872 
## converged
## initial  value 3348.674226 
## iter  10 value 2753.902432
## iter  20 value 2719.082485
## final  value 2718.729722 
## converged
## initial  value 3348.674226 
## iter  10 value 2648.899496
## iter  20 value 2644.079887
## final  value 2643.907429 
## converged
## initial  value 3427.774311 
## iter  10 value 2707.158377
## iter  20 value 2697.001014
## final  value 2696.977541 
## converged
## initial  value 3438.760434 
## iter  10 value 2799.756267
## iter  20 value 2777.252677
## iter  30 value 2732.217055
## final  value 2732.127626 
## converged
## initial  value 3451.943781 
## iter  10 value 2779.534636
## iter  20 value 2776.907163
## final  value 2776.676869 
## converged
## initial  value 3386.027044 
## iter  10 value 2746.721815
## iter  20 value 2741.326941
## final  value 2741.240844 
## converged
## initial  value 3456.338230 
## iter  10 value 2801.493895
## iter  20 value 2797.868641
## final  value 2796.921318 
## converged
## initial  value 3392.618717 
## iter  10 value 2706.243445
## iter  20 value 2702.224448
## iter  30 value 2701.574697
## iter  40 value 2700.206879
## final  value 2700.091334 
## converged
## initial  value 3350.871451 
## iter  10 value 2755.469802
## iter  20 value 2746.209390
## final  value 2746.032228 
## converged
## initial  value 3335.490878 
## iter  10 value 2650.730502
## iter  20 value 2640.490866
## final  value 2637.218980 
## converged
## initial  value 3447.549332 
## iter  10 value 2772.735003
## iter  20 value 2766.915087
## final  value 2765.780934 
## converged
## initial  value 3443.154883 
## iter  10 value 2812.487888
## final  value 2811.328880 
## converged
## initial  value 3397.013167 
## iter  10 value 2753.023740
## iter  20 value 2750.783316
## final  value 2749.458378 
## converged
## initial  value 3432.168760 
## iter  10 value 2799.930658
## final  value 2799.121992 
## converged
## initial  value 3375.040921 
## iter  10 value 2682.778662
## iter  20 value 2671.472614
## final  value 2671.448239 
## converged
## initial  value 3465.127129 
## iter  10 value 2791.228355
## final  value 2784.963309 
## converged
## initial  value 3425.577086 
## iter  10 value 2795.385481
## iter  20 value 2782.600682
## final  value 2780.647326 
## converged
## initial  value 3476.113251 
## iter  10 value 2787.130241
## iter  20 value 2782.687876
## iter  30 value 2782.331047
## final  value 2782.330232 
## converged
## initial  value 3346.477001 
## iter  10 value 2710.351127
## iter  20 value 2704.589568
## final  value 2704.157342 
## converged
## initial  value 3410.196514 
## iter  10 value 2780.734259
## iter  20 value 2777.412299
## final  value 2777.375419 
## converged
## initial  value 3511.268845 
## iter  10 value 2781.692899
## final  value 2780.072765 
## converged
## initial  value 3366.252023 
## iter  10 value 2760.627148
## iter  20 value 2756.151903
## final  value 2756.133883 
## converged
## initial  value 3432.168760 
## iter  10 value 2758.708274
## iter  20 value 2756.800025
## final  value 2755.192913 
## converged
## initial  value 3440.957658 
## iter  10 value 2801.223226
## iter  20 value 2797.066355
## final  value 2796.525319 
## converged
## initial  value 3418.985412 
## iter  10 value 2769.335886
## iter  20 value 2765.461420
## final  value 2765.177651 
## converged
## initial  value 3361.857573 
## iter  10 value 2692.521022
## final  value 2685.731139 
## converged
## initial  value 3361.857573 
## iter  10 value 2723.789302
## iter  20 value 2720.339159
## iter  30 value 2720.034165
## iter  40 value 2716.280961
## iter  40 value 2716.280956
## final  value 2716.280956 
## converged
## initial  value 3449.746556 
## iter  10 value 2719.324476
## iter  20 value 2711.087816
## final  value 2711.049240 
## converged
## initial  value 3443.154883 
## iter  10 value 2769.514579
## iter  20 value 2744.904215
## final  value 2744.485743 
## converged
## initial  value 3364.054798 
## iter  10 value 2798.775765
## iter  20 value 2797.169370
## iter  30 value 2795.676416
## final  value 2795.675953 
## converged
## initial  value 3445.352107 
## iter  10 value 2835.311832
## final  value 2829.701532 
## converged
## initial  value 3401.407616 
## iter  10 value 2781.969161
## iter  20 value 2777.470839
## final  value 2777.468112 
## converged
## initial  value 3381.632595 
## iter  10 value 2828.961032
## iter  20 value 2787.748943
## iter  30 value 2763.103826
## iter  40 value 2762.910428
## final  value 2762.891487 
## converged
## initial  value 3473.916027 
## iter  10 value 2781.462440
## iter  20 value 2764.724709
## final  value 2764.684221 
## converged
## initial  value 3414.590963 
## iter  10 value 2743.213627
## iter  20 value 2727.499159
## final  value 2727.491076 
## converged
## initial  value 3509.071620 
## iter  10 value 2840.026412
## iter  20 value 2837.453630
## iter  30 value 2795.183877
## final  value 2792.299416 
## converged
## initial  value 3456.338230 
## iter  10 value 2782.165847
## iter  20 value 2773.840154
## final  value 2773.487008 
## converged
## initial  value 3320.110306 
## iter  10 value 2742.096455
## final  value 2737.587414 
## converged
## initial  value 3401.407616 
## iter  10 value 2851.450077
## iter  20 value 2795.721989
## iter  30 value 2789.613582
## final  value 2789.009239 
## converged
## initial  value 3412.393739 
## iter  10 value 2779.979706
## final  value 2778.421827 
## converged
## initial  value 3326.701980 
## iter  10 value 2719.166739
## iter  20 value 2717.716930
## final  value 2717.372317 
## converged
## initial  value 3443.154883 
## iter  10 value 2845.956079
## iter  20 value 2834.684139
## iter  30 value 2808.592829
## iter  40 value 2807.864135
## iter  40 value 2807.864109
## iter  40 value 2807.864107
## final  value 2807.864107 
## converged
## initial  value 3383.829819 
## iter  10 value 2710.794523
## iter  20 value 2694.706453
## final  value 2694.706188 
## converged
## initial  value 3443.154883 
## iter  10 value 2747.538940
## final  value 2745.678633 
## converged
## initial  value 3491.493823 
## iter  10 value 2768.150998
## iter  20 value 2765.660170
## iter  30 value 2765.642586
## iter  40 value 2760.966657
## iter  50 value 2752.474440
## iter  60 value 2731.572341
## iter  70 value 2731.214329
## final  value 2731.213628 
## converged
## initial  value 3432.168760 
## iter  10 value 2801.736272
## iter  20 value 2794.595044
## final  value 2794.589559 
## converged
## initial  value 3506.874396 
## iter  10 value 2830.703937
## iter  20 value 2807.608110
## final  value 2807.491542 
## converged
## initial  value 3478.310476 
## iter  10 value 2748.980032
## iter  20 value 2740.322335
## final  value 2738.032999 
## converged
## initial  value 3386.027044 
## iter  10 value 2782.437019
## iter  20 value 2776.776154
## final  value 2776.567986 
## converged
## initial  value 3383.829819 
## iter  10 value 2725.342903
## iter  20 value 2718.219819
## final  value 2717.448517 
## converged
## initial  value 3410.196514 
## iter  10 value 2734.194552
## iter  20 value 2720.590268
## final  value 2720.454917 
## converged
## initial  value 3495.888273 
## iter  10 value 2774.173994
## iter  20 value 2773.941110
## final  value 2773.932517 
## converged
## initial  value 3454.141006 
## iter  10 value 2787.757299
## final  value 2776.682326 
## converged
## initial  value 3436.563209 
## iter  10 value 2776.696016
## iter  20 value 2774.578737
## final  value 2773.193798 
## converged
## initial  value 3449.746556 
## iter  10 value 2770.724640
## iter  20 value 2766.131429
## final  value 2765.904757 
## converged
## initial  value 3368.449247 
## iter  10 value 2691.238296
## final  value 2690.311014 
## converged
## initial  value 3407.999290 
## iter  10 value 2749.509913
## iter  20 value 2743.072073
## final  value 2743.048677 
## converged
## initial  value 3421.182637 
## iter  10 value 2738.303586
## iter  20 value 2736.670678
## final  value 2736.461004 
## converged
## initial  value 3309.124184 
## iter  10 value 2666.397327
## iter  20 value 2659.763797
## final  value 2659.498176 
## converged
## initial  value 3484.902150 
## iter  10 value 2720.057433
## final  value 2718.012135 
## converged
## initial  value 3405.802065 
## iter  10 value 2770.868308
## final  value 2770.023526 
## converged
## initial  value 3440.957658 
## iter  10 value 2867.773151
## iter  20 value 2847.023420
## final  value 2833.945308 
## converged
## initial  value 3386.027044 
## iter  10 value 2721.159399
## iter  20 value 2714.615393
## final  value 2714.568667 
## converged
## initial  value 3484.902150 
## iter  10 value 2786.481814
## final  value 2779.862623 
## converged
## initial  value 3405.802065 
## iter  10 value 2705.034172
## iter  20 value 2696.932243
## final  value 2696.821435 
## converged
## initial  value 3410.196514 
## iter  10 value 2778.246211
## iter  20 value 2770.844899
## final  value 2770.823467 
## converged
## initial  value 3445.352107 
## iter  10 value 2748.001987
## iter  20 value 2747.115224
## final  value 2746.790925 
## converged
## initial  value 3458.535455 
## iter  10 value 2913.793251
## final  value 2911.151389 
## converged
## initial  value 3331.096429 
## iter  10 value 2720.438508
## iter  20 value 2717.222447
## final  value 2717.005531 
## converged
## initial  value 3564.002235 
## iter  10 value 2824.393927
## final  value 2817.098320 
## converged
## initial  value 3421.182637 
## iter  10 value 2771.857001
## iter  20 value 2770.638897
## final  value 2770.550594 
## converged
## initial  value 3471.718802 
## iter  10 value 2820.158003
## iter  20 value 2813.651022
## final  value 2812.475833 
## converged
## initial  value 3489.296599 
## iter  10 value 2817.015175
## iter  20 value 2779.334875
## iter  30 value 2771.590277
## iter  40 value 2770.421003
## iter  50 value 2770.247161
## final  value 2770.075167 
## converged
## initial  value 3443.154883 
## iter  10 value 2827.441706
## final  value 2826.486746 
## converged
## initial  value 3458.535455 
## iter  10 value 2769.136819
## iter  20 value 2762.853952
## final  value 2762.123327 
## converged
## initial  value 3425.577086 
## iter  10 value 2755.981453
## iter  20 value 2752.891363
## iter  30 value 2751.280992
## iter  30 value 2751.280991
## final  value 2751.280991 
## converged
## initial  value 3456.338230 
## iter  10 value 2780.679609
## iter  20 value 2776.874267
## final  value 2776.624654 
## converged
## initial  value 3478.310476 
## iter  10 value 2822.584130
## iter  20 value 2816.542920
## final  value 2816.531600 
## converged
## initial  value 3425.577086 
## iter  10 value 2822.783302
## iter  20 value 2817.827135
## final  value 2817.723359 
## converged
```

``` r
    plot +
      labs(title = "Probabilite d'occupation du Lapin a travers les saisons",
           x = "Saisons", y ="Probabilite d'occupation lisee")
```

![plot of chunk unnamed-chunk-29](figure/unnamed-chunk-29-1.png)

``` r
    plot <- carte_graphique_5(fm=fm, couleur=couleur_lapin)
    ggpubr::annotate_figure(plot, 
                              top = ggpubr::text_grob("Presence du Lapin"))
```

![plot of chunk unnamed-chunk-29](figure/unnamed-chunk-29-2.png)
    
## Le herisson
    

``` r
    detection_matrice <- matrice_occu_detections(num_cd_nom = 60015)

    umf <- unmarkedFrameOccu(y = detection_matrice, 
                             siteCovs = site_info)
```


``` r
    plot(umf,
         main="Detection/Non Detection de Herissons sur les sites de 2010 a 2024 inclus")
```

![plot of chunk unnamed-chunk-31](figure/unnamed-chunk-31-1.png)

``` r
    print(colSums(detection_matrice))
```

```
## 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 
##  133  211  211  205  203  145  149  168  200  225  222  232  230  215  221
```


``` r
    pression_obs <- pression_obs_bdd(num_cd_nom = 60015)

    umf <- unmarked::unmarkedMultFrame(
      y = detection_matrice,
      siteCovs = site_info,
      obsCovs = list(pression_obs = detection_autres),
      yearlySiteCovs = site_covs_periodes_5,
      numPrimary = 5
    )

  fm <- unmarked::colext(
    psiformula = ~ X_10km + Dist_EcotoneArbore, # initial occupancy
    gammaformula =  ~ 1,  # colonization
    epsilonformula = ~ 1, # extinction
    pformula = ~ pression_obs,  # detection
    data = umf, # data
    control = list(trace = 1),
    se = TRUE)
```

```
## initial  value 3911.163718 
## iter  10 value 2629.649836
## iter  20 value 2617.468678
## final  value 2616.578271 
## converged
```

    coefficients : 
    

``` r
    fm
```

```
## 
## Call:
## unmarked::colext(psiformula = ~X_10km + Dist_EcotoneArbore, gammaformula = ~1, 
##     epsilonformula = ~1, pformula = ~pression_obs, data = umf, 
##     se = TRUE, control = list(trace = 1))
## 
## Initial (logit-scale):
##                    Estimate    SE     z P(>|z|)
## (Intercept)            7.20 2.139  3.37 0.00076
## X_10km                -4.44 1.415 -3.14 0.00172
## Dist_EcotoneArbore    -1.97 0.635 -3.10 0.00193
## 
## Colonization (logit-scale):
##  Estimate    SE   z P(>|z|)
##      0.82 0.341 2.4  0.0162
## 
## Extinction (logit-scale):
##  Estimate    SE   z  P(>|z|)
##     -3.15 0.225 -14 1.03e-44
## 
## Detection (logit-scale):
##              Estimate      SE     z  P(>|z|)
## (Intercept)   -0.3908 0.05957 -6.56 5.39e-11
## pression_obs   0.0723 0.00366 19.76 7.19e-87
## 
## AIC: 5247.157 
## Number of sites: 317
```

``` r
    backTransform(fm, "col"); confint(backTransform(fm, "col"))
```

```
## Backtransformed linear combination(s) of Colonization estimate(s)
## 
##  Estimate     SE LinComb (Intercept)
##     0.694 0.0724    0.82           1
## 
## Transformation: logistic
```

```
##      0.025     0.975
##  0.5377841 0.8158356
```

``` r
    backTransform(fm, "ext"); confint(backTransform(fm, "ext"))
```

```
## Backtransformed linear combination(s) of Extinction estimate(s)
## 
##  Estimate      SE LinComb (Intercept)
##    0.0409 0.00882   -3.15           1
## 
## Transformation: logistic
```

```
##       0.025      0.975
##  0.02671133 0.06214582
```

    tests :


``` r
    pb <- parboot(fm, statistic=chisq, nsim=40, parallel=FALSE); pb
```

```
## initial  value 2657.249391 
## iter  10 value 2653.297706
## final  value 2653.263828 
## converged
## initial  value 2669.800392 
## iter  10 value 2667.896624
## final  value 2667.872382 
## converged
## initial  value 2657.432189 
## iter  10 value 2654.216864
## final  value 2654.199369 
## converged
## initial  value 2662.252985 
## iter  10 value 2660.873540
## final  value 2660.847581 
## converged
## initial  value 2668.773273 
## iter  10 value 2666.058698
## final  value 2665.867264 
## converged
## initial  value 2684.026359 
## iter  10 value 2681.319889
## final  value 2681.285093 
## converged
## initial  value 2650.718777 
## iter  10 value 2647.149656
## final  value 2646.192059 
## converged
## initial  value 2667.561924 
## iter  10 value 2664.337585
## final  value 2664.312563 
## converged
## initial  value 2667.938671 
## iter  10 value 2665.852353
## final  value 2665.834488 
## converged
## initial  value 2691.571001 
## iter  10 value 2688.826210
## final  value 2688.826056 
## converged
## initial  value 2738.809246 
## iter  10 value 2729.564639
## final  value 2729.454979 
## converged
## initial  value 2659.076993 
## iter  10 value 2653.375215
## final  value 2653.141849 
## converged
## initial  value 2718.570517 
## iter  10 value 2714.688098
## final  value 2714.538829 
## converged
## initial  value 2688.697539 
## iter  10 value 2686.156372
## final  value 2686.155002 
## converged
## initial  value 2693.684706 
## iter  10 value 2691.540626
## final  value 2691.517557 
## converged
## initial  value 2648.227339 
## iter  10 value 2645.787062
## final  value 2645.785091 
## converged
## initial  value 2685.528334 
## iter  10 value 2681.782293
## final  value 2681.754314 
## converged
## initial  value 2711.230526 
## iter  10 value 2706.360371
## final  value 2706.304407 
## converged
## initial  value 2671.331334 
## iter  10 value 2669.386038
## final  value 2669.359861 
## converged
## initial  value 2697.860402 
## iter  10 value 2695.247811
## final  value 2695.176218 
## converged
## initial  value 2648.020026 
## iter  10 value 2644.191615
## final  value 2644.173054 
## converged
## initial  value 2637.740055 
## iter  10 value 2635.409387
## final  value 2635.308869 
## converged
## initial  value 2712.019587 
## iter  10 value 2709.144880
## final  value 2709.138511 
## converged
## initial  value 2637.969269 
## iter  10 value 2635.553481
## final  value 2635.540845 
## converged
## initial  value 2716.863637 
## iter  10 value 2710.644880
## final  value 2710.628692 
## converged
## initial  value 2690.086744 
## iter  10 value 2687.746293
## final  value 2687.722496 
## converged
## initial  value 2651.464916 
## iter  10 value 2644.988838
## final  value 2644.897105 
## converged
## initial  value 2638.562883 
## iter  10 value 2636.273916
## final  value 2636.233953 
## converged
## initial  value 2663.404129 
## iter  10 value 2660.827753
## final  value 2660.781532 
## converged
## initial  value 2634.585167 
## iter  10 value 2630.266052
## final  value 2630.098869 
## converged
## initial  value 2684.905086 
## iter  10 value 2682.637062
## final  value 2682.609802 
## converged
## initial  value 2642.299470 
## iter  10 value 2640.260393
## final  value 2640.255560 
## converged
## initial  value 2653.227375 
## iter  10 value 2649.311823
## final  value 2649.256282 
## converged
## initial  value 2635.321403 
## iter  10 value 2633.409841
## final  value 2633.392108 
## converged
## initial  value 2622.212191 
## iter  10 value 2620.963773
## final  value 2620.880815 
## converged
## initial  value 2628.733163 
## iter  10 value 2624.085419
## final  value 2624.040855 
## converged
## initial  value 2672.696112 
## iter  10 value 2670.604191
## final  value 2670.601763 
## converged
## initial  value 2653.669482 
## iter  10 value 2652.245653
## final  value 2652.215812 
## converged
## initial  value 2716.399090 
## iter  10 value 2711.419946
## final  value 2711.297437 
## converged
## initial  value 2683.809492 
## iter  10 value 2681.038421
## final  value 2680.897600 
## converged
```

```
## 
## Call: parboot(object = fm, statistic = chisq, nsim = 40, parallel = FALSE)
## 
## Parametric Bootstrap Statistics:
##     t0 mean(t0 - t_B) StdDev(t0 - t_B) Pr(t_B > t0)
## 1 4396           -376              143        0.976
## 
## t_B quantiles:
##        0% 2.5%  25%  50%  75% 97.5% 100%
## [1,] 4487 4587 4675 4728 4857  5041 5188
## 
## t0 = Original statistic computed from data
## t_B = Vector of bootstrap samples
```


``` r
    AICcmodavg::mb.gof.test(fm)
```

![plot of chunk unnamed-chunk-35](figure/unnamed-chunk-35-1.png)

```
## 
## Goodness-of-fit for dynamic occupancy model
## 
## Number of seasons:  5 
## 
## Chi-square statistic:
## Season 1 Season 2 Season 3 Season 4 Season 5 
##  62.0918  51.7383  51.7405  16.2428  13.2655 
## 
## Total chi-square = 195.0789 
## Number of bootstrap samples = 5
## P-value = 0
## 
## Quantiles of bootstrapped statistics:
##   0%  25%  50%  75% 100% 
##   19   26   35   40   46 
## 
## Estimate of c-hat = 5.89
```

    graphiques :































