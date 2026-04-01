# Regressao Logistica Completa (Uppersample)

## Summary de Precision, Recall e Accuracy (teste)

| Modelo | Precision | Recall | Accuracy |
|---|---:|---:|---:|
| Regressao_Logistica_Completa | 0.0482 | 0.5025 | 0.5538 |

## Metricas de classificacao no teste

| Modelo | AUC | Acuracia | Sensibilidade | Especificidade | Precisao | F1 |
|---|---:|---:|---:|---:|---:|---:|
| Regressao_Logistica_Completa | 0.5228 | 0.5538 | 0.5025 | 0.5560 | 0.0482 | 0.0879 |

## Summary do modelo

```r

Call:
glm(formula = TARGET_I219 ~ RACACOR + IDADEanos, family = "binomial", 
    data = dados_treino_uppersample)

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)   
(Intercept)      -0.134844   0.077231  -1.746   0.0808 . 
RACACORPreta      0.031400   0.050046   0.627   0.5304   
RACACORAmarela   -0.077440   0.183273  -0.423   0.6726   
RACACORParda      0.096513   0.029575   3.263   0.0011 **
RACACORIndigena -12.514935  90.062571  -0.139   0.8895   
IDADEanos         0.001308   0.001031   1.268   0.2047   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 29356  on 21175  degrees of freedom
Residual deviance: 29327  on 21170  degrees of freedom
AIC: 29339

Number of Fisher Scoring iterations: 11

```
