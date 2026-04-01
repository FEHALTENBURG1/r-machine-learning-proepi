# Modelo - Regressao Linear Stepwise

- Transformacao da target usada: `original`
- Modelo vencedor global (menor RMSE): `Regressao_Linear_Stepwise`

## Metricas no teste

| Modelo | RMSE | MAE | R2 |
|---|---:|---:|---:|
| Regressao_Linear_Stepwise | 5.2048 | 3.1560 | 1.0000 |

## Summary do modelo

```r

Call:
lm(formula = VAL_TOT_TRANSF ~ VAL_SH + VAL_SP, data = dados_treino)

Residuals:
    Min      1Q  Median      3Q     Max 
-236.37   -2.18   -1.94   -1.59  985.92 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 2.428360   5.131606   0.473    0.636    
VAL_SH      0.991634   0.007029 141.071   <2e-16 ***
VAL_SP      1.052335   0.042280  24.889   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 57.11 on 326 degrees of freedom
Multiple R-squared:  0.9954,	Adjusted R-squared:  0.9953 
F-statistic: 3.506e+04 on 2 and 326 DF,  p-value: < 2.2e-16

```
