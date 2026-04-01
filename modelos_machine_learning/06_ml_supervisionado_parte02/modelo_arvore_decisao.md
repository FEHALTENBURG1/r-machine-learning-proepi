# Arvore de Decisao (Treino Original)

## Summary de Precision, Recall e Accuracy (teste)

| Modelo | Precision | Recall | Accuracy |
|---|---:|---:|---:|
| Arvore_Decisao_Treino_Original |    NA | 0.0000 | 0.9572 |

## Metricas de classificacao no teste

| Modelo | AUC | Acuracia | Sensibilidade | Especificidade | Precisao | F1 |
|---|---:|---:|---:|---:|---:|---:|
| Arvore_Decisao_Treino_Original | 0.5000 | 0.9572 | 0.0000 | 1.0000 |    NA |    NA |

## Summary do modelo

```r
Call:
rpart(formula = TARGET_FATOR ~ RACACOR + IDADEanos, data = dados_treino, 
    method = "class", parms = list(split = "gini"), control = rpart.control(cp = 0.001, 
        minsplit = 20, maxdepth = 6))
  n= 11068 

  CP nsplit rel error xerror xstd
1  0      0         1      0    0

Node number 1: 11068 observations
  predicted class=0  expected loss=0.04336827  P(node) =1
    class counts: 10588   480
   probabilities: 0.957 0.043 

```
