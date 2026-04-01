# Aula 7 - Summaries dos Modelos

## Regressao de Poisson
```r

Call:
glm(formula = formula_contagem, family = "poisson", data = dados_treino_poisson)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -2.31555    0.58605  -3.951 7.78e-05 ***
DIA_SEMANA  -0.04770    0.03516  -1.356    0.175    
MES          0.83042    0.12155   6.832 8.37e-12 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 127.418  on 60  degrees of freedom
Residual deviance:  64.462  on 58  degrees of freedom
AIC: 244.69

Number of Fisher Scoring iterations: 5

```

## Regressao Quase-Poisson
```r

Call:
glm(formula = formula_contagem, family = "quasipoisson", data = dados_treino_poisson)

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -2.31555    0.64044  -3.616 0.000629 ***
DIA_SEMANA  -0.04770    0.03843  -1.241 0.219508    
MES          0.83042    0.13283   6.252 5.22e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for quasipoisson family taken to be 1.194239)

    Null deviance: 127.418  on 60  degrees of freedom
Residual deviance:  64.462  on 58  degrees of freedom
AIC: NA

Number of Fisher Scoring iterations: 5

```

## Regressao Binomial Negativa (oficial com overdispersao)
```r

Call:
MASS::glm.nb(formula = formula_contagem, data = dados_treino_poisson, 
    init.theta = 15.23380583, link = log)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -2.26329    0.62237  -3.637 0.000276 ***
DIA_SEMANA  -0.04505    0.03972  -1.134 0.256716    
MES          0.81661    0.12974   6.294 3.09e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for Negative Binomial(15.2338) family taken to be 1)

    Null deviance: 101.750  on 60  degrees of freedom
Residual deviance:  50.322  on 58  degrees of freedom
AIC: 244.6

Number of Fisher Scoring iterations: 1


              Theta:  15.2 
          Std. Err.:  12.8 

 2 x log-likelihood:  -236.597 
```

## Kaplan-Meier por Sexo
```r
Call: survfit(formula = obj_surv ~ AP_SEXO, data = base_sobrevida)

                AP_SEXO=Masculino 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1   2004       2    0.999 0.000705        0.998        1.000
   90   1820       1    0.998 0.000893        0.997        1.000
  153   1721       1    0.998 0.001065        0.996        1.000
  184   1629       1    0.997 0.001228        0.995        1.000
  214   1623       1    0.997 0.001372        0.994        0.999
  425   1443       1    0.996 0.001535        0.993        0.999
  456   1410       1    0.995 0.001689        0.992        0.999
  457   1397       1    0.995 0.001832        0.991        0.998
  550   1266       1    0.994 0.001992        0.990        0.998
  580   1247       2    0.992 0.002285        0.988        0.997
  641   1145       3    0.990 0.002728        0.984        0.995
  669   1104       3    0.987 0.003131        0.981        0.993
  700   1013      29    0.959 0.005999        0.947        0.970

                AP_SEXO=Feminino 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
  121   2011       1    1.000 0.000497        0.999        1.000
  122   2010       1    0.999 0.000703        0.998        1.000
  153   1927       4    0.997 0.001251        0.994        0.999
  181   1834       2    0.996 0.001467        0.993        0.999
  183   1832       1    0.995 0.001564        0.992        0.998
  214   1797       2    0.994 0.001747        0.991        0.998
  425   1565       1    0.994 0.001858        0.990        0.997
  427   1551       2    0.992 0.002065        0.988        0.996
  517   1445       1    0.992 0.002174        0.987        0.996
  547   1402       1    0.991 0.002285        0.986        0.995
  578   1369       1    0.990 0.002395        0.985        0.995
  580   1349       2    0.989 0.002607        0.984        0.994
  609   1292       1    0.988 0.002715        0.983        0.993
  610   1272       1    0.987 0.002822        0.982        0.993
  670   1129       1    0.986 0.002951        0.981        0.992
  700   1074      41    0.949 0.006428        0.936        0.961

```

## Kaplan-Meier por Raca/Cor
```r
Call: survfit(formula = obj_surv ~ AP_RACACOR, data = base_sobrevida)

                AP_RACACOR=01 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
  121   1455       1    0.999 0.000687        0.998        1.000
  122   1454       1    0.999 0.000971        0.997        1.000
  181   1338       1    0.998 0.001224        0.995        1.000
  214   1323       1    0.997 0.001437        0.994        1.000
  427   1132       1    0.996 0.001684        0.993        1.000
  580    991       2    0.994 0.002200        0.990        0.999
  610    933       1    0.993 0.002443        0.988        0.998
  641    908       1    0.992 0.002674        0.987        0.997
  669    875       2    0.990 0.003111        0.984        0.996
  700    794      29    0.954 0.007239        0.940        0.968

                AP_RACACOR=02 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
   90    186       1    0.995 0.00536        0.984         1.00
  580    116       1    0.986 0.01006        0.967         1.00
  700     98       4    0.946 0.02194        0.904         0.99

                AP_RACACOR=03 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1   2127       2    0.999 0.000665        0.998        1.000
  153   1819       5    0.996 0.001394        0.994        0.999
  181   1723       1    0.996 0.001508        0.993        0.999
  183   1719       1    0.995 0.001615        0.992        0.998
  184   1704       1    0.995 0.001716        0.991        0.998
  214   1692       2    0.993 0.001905        0.990        0.997
  425   1502       2    0.992 0.002120        0.988        0.996
  427   1490       1    0.991 0.002220        0.987        0.996
  456   1460       1    0.991 0.002320        0.986        0.995
  457   1454       1    0.990 0.002417        0.985        0.995
  517   1379       1    0.989 0.002519        0.984        0.994
  547   1336       1    0.989 0.002624        0.983        0.994
  550   1305       1    0.988 0.002729        0.982        0.993
  578   1299       1    0.987 0.002831        0.982        0.993
  580   1280       1    0.986 0.002932        0.981        0.992
  609   1232       1    0.985 0.003037        0.980        0.991
  641   1155       2    0.984 0.003263        0.977        0.990
  670   1060       1    0.983 0.003389        0.976        0.990
  700   1018      32    0.952 0.006298        0.940        0.964

                AP_RACACOR=04 
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
    700.0000      68.0000       3.0000       0.9559       0.0249       0.9083       1.0000 

                AP_RACACOR=99 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
  669    121       1    0.992 0.00823        0.976            1
  700    109       2    0.974 0.01509        0.944            1

```

## Kaplan-Meier por Faixa Etaria
```r
Call: survfit(formula = obj_surv ~ FAIXA_ETARIA, data = base_sobrevida)

                FAIXA_ETARIA=<20 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1   1401       1    0.999 0.000714        0.998        1.000
  122   1255       1    0.998 0.001069        0.996        1.000
  153   1202       2    0.997 0.001586        0.994        1.000
  181   1146       2    0.995 0.002004        0.991        0.999
  425    987       1    0.994 0.002242        0.990        0.998
  427    977       1    0.993 0.002459        0.988        0.998
  456    959       1    0.992 0.002666        0.987        0.997
  457    952       1    0.991 0.002860        0.985        0.997
  517    910       1    0.990 0.003057        0.984        0.996
  578    853       1    0.989 0.003266        0.982        0.995
  580    839       3    0.985 0.003840        0.978        0.993
  641    768       1    0.984 0.004043        0.976        0.992
  669    745       1    0.983 0.004248        0.974        0.991
  670    715       1    0.981 0.004459        0.973        0.990
  700    684      21    0.951 0.007783        0.936        0.966

                FAIXA_ETARIA=20-40 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
  153   1439       1    0.999 0.000695        0.998        1.000
  184   1350       1    0.999 0.001015        0.997        1.000
  214   1342       2    0.997 0.001460        0.994        1.000
  547   1065       1    0.996 0.001733        0.993        1.000
  580   1027       1    0.995 0.001984        0.991        0.999
  609    989       1    0.994 0.002223        0.990        0.999
  641    932       1    0.993 0.002463        0.988        0.998
  669    896       1    0.992 0.002698        0.987        0.997
  700    807      33    0.951 0.007384        0.937        0.966

                FAIXA_ETARIA=40-60 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    941       1    0.999 0.00106        0.997        1.000
   90    853       1    0.998 0.00158        0.995        1.000
  183    771       1    0.996 0.00204        0.992        1.000
  214    765       1    0.995 0.00242        0.990        1.000
  425    674       1    0.994 0.00283        0.988        0.999
  427    671       1    0.992 0.00319        0.986        0.998
  550    597       1    0.991 0.00359        0.984        0.998
  610    559       1    0.989 0.00400        0.981        0.997
  641    539       1    0.987 0.00439        0.978        0.996
  700    482      14    0.958 0.00867        0.941        0.975

                FAIXA_ETARIA=60+ 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
  121    213       1    0.995 0.00468        0.986        1.000
  153    201       2    0.985 0.00837        0.969        1.000
  669    124       1    0.977 0.01147        0.955        1.000
  700    114       2    0.960 0.01648        0.929        0.993

```

## Cox Proportional Hazards
```r
Call:
coxph(formula = Surv(TEMPO_TRAT_DIAS, EVENTO_TRAT) ~ FAIXA_ETARIA + 
    AP_SEXO + AP_RACACOR + AM_TRANSPL + AP_CIDPRI, data = base_sobrevida)

  n= 4228, number of events= 110 

                        coef  exp(coef)   se(coef)      z Pr(>|z|)  
FAIXA_ETARIA20-40 -2.077e-01  8.125e-01  2.306e-01 -0.900   0.3679  
FAIXA_ETARIA40-60 -3.285e-01  7.200e-01  2.792e-01 -1.177   0.2394  
FAIXA_ETARIA60+   -3.376e-01  7.135e-01  4.592e-01 -0.735   0.4622  
AP_SEXOFeminino    2.296e-01  1.258e+00  1.948e-01  1.179   0.2384  
AP_RACACOR02       2.041e-01  1.226e+00  4.420e-01  0.462   0.6443  
AP_RACACOR03       1.485e-01  1.160e+00  2.070e-01  0.717   0.4733  
AP_RACACOR04      -2.030e-02  9.799e-01  6.006e-01 -0.034   0.9730  
AP_RACACOR99      -5.789e-01  5.605e-01  5.997e-01 -0.965   0.3344  
AM_TRANSPLSim             NA         NA  0.000e+00     NA       NA  
AP_CIDPRIE101     -1.149e+00  3.171e-01  1.093e+00 -1.051   0.2932  
AP_CIDPRIE102     -1.839e+00  1.589e-01  1.428e+00 -1.288   0.1976  
AP_CIDPRIE103     -3.910e-01  6.763e-01  1.175e+00 -0.333   0.7392  
AP_CIDPRIE104     -1.624e+01  8.847e-08  2.312e+03 -0.007   0.9944  
AP_CIDPRIE105     -1.613e+01  9.869e-08  3.962e+03 -0.004   0.9968  
AP_CIDPRIE106     -7.326e-01  4.807e-01  1.423e+00 -0.515   0.6066  
AP_CIDPRIE107     -2.623e+00  7.259e-02  1.426e+00 -1.840   0.0658 .
AP_CIDPRIE108     -1.318e+00  2.676e-01  1.045e+00 -1.262   0.2069  
AP_CIDPRIE109     -1.939e+00  1.438e-01  1.021e+00 -1.900   0.0574 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

                  exp(coef) exp(-coef) lower .95 upper .95
FAIXA_ETARIA20-40 8.125e-01  1.231e+00  0.517006     1.277
FAIXA_ETARIA40-60 7.200e-01  1.389e+00  0.416615     1.244
FAIXA_ETARIA60+   7.135e-01  1.402e+00  0.290097     1.755
AP_SEXOFeminino   1.258e+00  7.948e-01  0.858878     1.843
AP_RACACOR02      1.226e+00  8.154e-01  0.515698     2.917
AP_RACACOR03      1.160e+00  8.620e-01  0.773134     1.741
AP_RACACOR04      9.799e-01  1.021e+00  0.301956     3.180
AP_RACACOR99      5.605e-01  1.784e+00  0.173011     1.816
AM_TRANSPLSim            NA         NA        NA        NA
AP_CIDPRIE101     3.171e-01  3.154e+00  0.037233     2.700
AP_CIDPRIE102     1.589e-01  6.293e+00  0.009678     2.609
AP_CIDPRIE103     6.763e-01  1.479e+00  0.067666     6.760
AP_CIDPRIE104     8.847e-08  1.130e+07  0.000000       Inf
AP_CIDPRIE105     9.869e-08  1.013e+07  0.000000       Inf
AP_CIDPRIE106     4.807e-01  2.080e+00  0.029564     7.815
AP_CIDPRIE107     7.259e-02  1.378e+01  0.004438     1.187
AP_CIDPRIE108     2.676e-01  3.737e+00  0.034533     2.073
AP_CIDPRIE109     1.438e-01  6.953e+00  0.019458     1.063

Concordance= 0.62  (se = 0.027 )
Likelihood ratio test= 18.57  on 17 df,   p=0.4
Wald test            = 20.22  on 17 df,   p=0.3
Score (logrank) test = 22.9  on 17 df,   p=0.2

```
