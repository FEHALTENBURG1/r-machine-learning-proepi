# Modelo - Arvore de Regressao

- Transformacao da target usada: `original`
- Modelo vencedor global (menor RMSE): `Regressao_Linear_Stepwise`

## Metricas no teste

| Modelo | RMSE | MAE | R2 |
|---|---:|---:|---:|
| Arvore_Regressao | 743.4870 | 181.0752 | 0.7000 |

## Summary do modelo

```r
Call:
rpart(formula = formula_arvore, data = dados_treino, method = "anova", 
    control = rpart.control(cp = 0.001, minsplit = 20, maxdepth = 6))
  n= 329 

            CP nsplit rel error    xerror       xstd
1  0.594477495      0 1.0000000 1.0087036 0.25574870
2  0.232043118      1 0.4055225 0.5130082 0.14777255
3  0.023449520      2 0.1734794 0.1662733 0.04932738
4  0.007698496      3 0.1500299 0.1353078 0.04913069
5  0.004520448      4 0.1423314 0.1287504 0.04911685
6  0.004146984      5 0.1378109 0.1265147 0.04919315
7  0.002529165      6 0.1336639 0.1218295 0.04920759
8  0.001273772      7 0.1311348 0.1201317 0.04921765
9  0.001262859      8 0.1298610 0.1191248 0.04921401
10 0.001000000      9 0.1285981 0.1189188 0.04921524

Variable importance
VAL_SH VAL_SP  MORTE 
    66     32      2 

Node number 1: 329 observations,    complexity param=0.5944775
  mean=1073.167, MSE=698305.2 
  left son=2 (315 obs) right son=3 (14 obs)
  Primary splits:
      VAL_SH < 2238.45  to the left,  improve=0.594477500, (0 missing)
      VAL_SP < 138.33   to the left,  improve=0.502745200, (0 missing)
      MORTE  splits as  LR,           improve=0.031746770, (0 missing)
      SEXO   splits as  RL,           improve=0.006174449, (0 missing)
  Surrogate splits:
      VAL_SP < 513.595  to the left,  agree=0.976, adj=0.429, (0 split)

Node number 2: 315 observations,    complexity param=0.2320431
  mean=937.3358, MSE=206916 
  left son=4 (253 obs) right son=5 (62 obs)
  Primary splits:
      VAL_SH < 1123.06  to the left,  improve=8.179093e-01, (0 missing)
      VAL_SP < 117.38   to the left,  improve=7.053893e-01, (0 missing)
      MORTE  splits as  LR,           improve=7.537798e-02, (0 missing)
      SEXO   splits as  RL,           improve=7.497868e-06, (0 missing)
  Surrogate splits:
      VAL_SP < 126.725  to the left,  agree=0.924, adj=0.613, (0 split)
      MORTE  splits as  LR,           agree=0.822, adj=0.097, (0 split)

Node number 3: 14 observations
  mean=4129.366, MSE=1999083 

Node number 4: 253 observations,    complexity param=0.02344952
  mean=733.6854, MSE=35192.61 
  left son=8 (218 obs) right son=9 (35 obs)
  Primary splits:
      VAL_SH < 759.41   to the left,  improve=0.6050665000, (0 missing)
      VAL_SP < 100.115  to the left,  improve=0.4904179000, (0 missing)
      SEXO   splits as  RL,           improve=0.0006545968, (0 missing)
  Surrogate splits:
      VAL_SP < 107.54   to the left,  agree=0.933, adj=0.514, (0 split)

Node number 5: 62 observations,    complexity param=0.007698496
  mean=1768.361, MSE=47817.34 
  left son=10 (12 obs) right son=11 (50 obs)
  Primary splits:
      VAL_SH < 1437.585 to the left,  improve=0.596581700, (0 missing)
      VAL_SP < 136.395  to the left,  improve=0.306741400, (0 missing)
      SEXO   splits as  LR,           improve=0.012834220, (0 missing)
      MORTE  splits as  RL,           improve=0.004917946, (0 missing)
  Surrogate splits:
      VAL_SP < 136.395  to the left,  agree=0.887, adj=0.417, (0 split)

Node number 8: 218 observations,    complexity param=0.004520448
  mean=675.2154, MSE=11558.34 
  left son=16 (111 obs) right son=17 (107 obs)
  Primary splits:
      VAL_SH < 563.415  to the left,  improve=0.412164400, (0 missing)
      VAL_SP < 88.025   to the left,  improve=0.377799400, (0 missing)
      SEXO   splits as  LR,           improve=0.001408151, (0 missing)
  Surrogate splits:
      VAL_SP < 88.025   to the left,  agree=0.821, adj=0.636, (0 split)
      SEXO   splits as  LR,           agree=0.528, adj=0.037, (0 split)
      MORTE  splits as  LR,           agree=0.518, adj=0.019, (0 split)

Node number 9: 35 observations,    complexity param=0.002529165
  mean=1097.87, MSE=28476.08 
  left son=18 (27 obs) right son=19 (8 obs)
  Primary splits:
      VAL_SP < 154.31   to the left,  improve=0.58300210, (0 missing)
      VAL_SH < 861.72   to the left,  improve=0.29243650, (0 missing)
      SEXO   splits as  LR,           improve=0.05649064, (0 missing)

Node number 10: 12 observations
  mean=1423.597, MSE=7503.144 

Node number 11: 50 observations,    complexity param=0.004146984
  mean=1851.104, MSE=22119.33 
  left son=22 (42 obs) right son=23 (8 obs)
  Primary splits:
      VAL_SH < 1830.36  to the left,  improve=0.86145310, (0 missing)
      VAL_SP < 241.6552 to the left,  improve=0.13455040, (0 missing)
      SEXO   splits as  LR,           improve=0.05042282, (0 missing)
      MORTE  splits as  RL,           improve=0.01833602, (0 missing)
  Surrogate splits:
      VAL_SP < 284.185  to the left,  agree=0.9, adj=0.375, (0 split)

Node number 16: 111 observations
  mean=607.4492, MSE=329.8342 

Node number 17: 107 observations,    complexity param=0.001273772
  mean=745.515, MSE=13500.65 
  left son=34 (91 obs) right son=35 (16 obs)
  Primary splits:
      VAL_SP < 98.18    to the left,  improve=0.2025790000, (0 missing)
      VAL_SH < 666.28   to the left,  improve=0.1045094000, (0 missing)
      SEXO   splits as  RL,           improve=0.0001881979, (0 missing)
  Surrogate splits:
      VAL_SH < 715.78   to the left,  agree=0.869, adj=0.125, (0 split)

Node number 18: 27 observations,    complexity param=0.001262859
  mean=1027.734, MSE=13068.43 
  left son=36 (16 obs) right son=37 (11 obs)
  Primary splits:
      VAL_SH < 938.605  to the left,  improve=0.82225990, (0 missing)
      VAL_SP < 116.635  to the left,  improve=0.04879872, (0 missing)
      SEXO   splits as  LR,           improve=0.03138205, (0 missing)
  Surrogate splits:
      VAL_SP < 86.09    to the right, agree=0.667, adj=0.182, (0 split)
      MORTE  splits as  LR,           agree=0.630, adj=0.091, (0 split)

Node number 19: 8 observations
  mean=1334.578, MSE=7844.846 

Node number 22: 42 observations
  mean=1790.859, MSE=2635.151 

Node number 23: 8 observations
  mean=2167.391, MSE=5318.977 

Node number 34: 91 observations
  mean=723.5863, MSE=2436.85 

Node number 35: 16 observations
  mean=870.2347, MSE=58136.02 

Node number 36: 16 observations
  mean=941.7831, MSE=2007.858 

Node number 37: 11 observations
  mean=1152.755, MSE=2780.855 

```
