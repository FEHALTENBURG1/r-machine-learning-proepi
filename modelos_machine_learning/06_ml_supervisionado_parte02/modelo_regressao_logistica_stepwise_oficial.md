# Regressao Logistica Stepwise (MODELO OFICIAL)

## Summary de Precision, Recall e Accuracy (teste)

| Modelo | Precision | Recall | Accuracy |
|---|---:|---:|---:|
| Regressao_Logistica_Stepwise_Oficial | 0.0477 | 0.4778 | 0.5696 |

## Metricas de classificacao no teste

| Modelo | AUC | Acuracia | Sensibilidade | Especificidade | Precisao | F1 |
|---|---:|---:|---:|---:|---:|---:|
| Regressao_Logistica_Stepwise_Oficial | 0.5247 | 0.5696 | 0.4778 | 0.5737 | 0.0477 | 0.0868 |

## Summary do modelo

```r

Call:
glm(formula = TARGET_I219 ~ RACACOR, family = "binomial", data = dados_treino_uppersample)

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)   
(Intercept)      -0.04039    0.02041  -1.979  0.04781 * 
RACACORPreta      0.02396    0.04970   0.482  0.62972   
RACACORAmarela   -0.07544    0.18326  -0.412  0.68058   
RACACORParda      0.08889    0.02896   3.070  0.00214 **
RACACORIndigena -12.52567   90.06770  -0.139  0.88940   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 29356  on 21175  degrees of freedom
Residual deviance: 29328  on 21171  degrees of freedom
AIC: 29338

Number of Fisher Scoring iterations: 11

```
