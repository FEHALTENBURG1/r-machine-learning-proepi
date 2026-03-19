# Curso de Análise de Dados e Machine Learning com R para Saúde

> Imersão prática em Ciência de Dados aplicada à saúde pública usando R e dados reais do DATASUS.

**ProEpi 2026** · Autora: Danielly Xavier · [Lattes](https://lattes.cnpq.br/2373300932860701)

---

## Sobre o Curso

O treinamento cobre todo o ciclo de vida dos dados em saúde: desde a **extração** via DATASUS, passando por **pré-processamento**, **análise exploratória** e **estatística inferencial**, até **modelagem preditiva** com Machine Learning. Utiliza dados reais dos sistemas SINASC, SIM, SINAN, SIA e SIH.

- **Carga horária:** 20 horas (10 encontros de 2h)
- **Público:** estudantes e profissionais que desejam análise de dados e ML em saúde pública

---

## Início Rápido

### Pré-requisitos

- R 4.x ou superior
- RStudio (recomendado)
- Conexão com internet (para download dos dados)

### Execução

Abra o R no diretório do projeto e execute os scripts em ordem:

```r
source("01_preparacao_R_sistemas_SUS.r")        # ~5 min
source("02_preprocessamento_parte1.r")          # ~2 min
source("03_preprocessamento_parte2.r")          # ~2 min
source("04_analise_exploratoria.r")             # ~3 min
source("05_estatistica_inferencial.r")          # ~2 min
source("06_kmeans_hierarquico.r")               # ~3 min
source("07_kprototypes.r")                      # ~3 min
source("08_regressao_linear_arvore.r")          # ~3 min
source("09_classificacao_logistica_arvore.r")   # ~3 min
source("10_poisson_kaplanmeier.r")              # ~2 min
```

Os diretórios `dados_originais_datasus` e `dados_processados_datasus` são criados automaticamente pela Aula 1.

---

## Estrutura das Aulas

| Aula | Script | Conteúdo |
|:----:|--------|----------|
| 1 | `01_preparacao_R_sistemas_SUS.r` | Instalação de pacotes, download DATASUS (SINASC, SIM, SINAN, SIA, SIH), dados glifage SIA 2024 |
| 2 | `02_preprocessamento_parte1.r` | Tipos de variáveis, NAs, outliers (IQR), imputação mediana/moda |
| 3 | `03_preprocessamento_parte2.r` | Padronização Z-score, discretização (binning), one-hot encoding |
| 4 | `04_analise_exploratoria.r` | Frequências, tendência central, dispersão, correlação Pearson/Spearman |
| 5 | `05_estatistica_inferencial.r` | t-Student, Wilcoxon, Kolmogorov-Smirnov, Qui-quadrado, Fisher, ANOVA |
| 6 | `06_kmeans_hierarquico.r` | K-Means e clustering hierárquico (SIA) |
| 7 | `07_kprototypes.r` | K-Prototypes para dados mistos (SINASC) |
| 8 | `08_regressao_linear_arvore.r` | Regressão linear e árvore de regressão (SIH) |
| 9 | `09_classificacao_logistica_arvore.r` | Regressão logística e árvore de classificação (SINAN) |
| 10 | `10_poisson_kaplanmeier.r` | Poisson (SIM) e Kaplan-Meier (glifage SIA 2024) |

---

## Fluxo de Dados

```
Aula 1:  DATASUS (API) → dados_originais_datasus/
         ├── SINASC, SIM, SINAN, SIA, SIH (*_completo.csv, *_selecionadas.csv)
         └── SIA_glifage_2024_DF.csv (para Kaplan-Meier na Aula 10)

Aula 2:  dados_originais_datasus → dados_processados_datasus/*_intermediario.csv

Aula 3:  intermediário → dados_processados_datasus/*_processado.csv

Aulas 4-5:  Análise exploratória e inferencial (dados originais)
Aulas 6-10: Machine Learning (dados processados)
```

---

## O que você vai aprender

- **Extrair e manipular dados públicos** com `microdatasus`
- **Pré-processar dados** para ML: outliers, NAs, normalização, codificação
- **Análise exploratória** com visualizações e estatística descritiva
- **Testes de hipótese** (t-Student, Wilcoxon, Qui-quadrado, ANOVA)
- **Clustering** não-supervisionado (K-Means, hierárquico, K-Prototypes)
- **Modelos supervisionados** (regressão linear/logística, árvores, Poisson, Kaplan-Meier)

---

## Pacotes

Instalados automaticamente na Aula 1:

`tidyverse` · `janitor` · `microdatasus` · `cluster` · `factoextra` · `clustMixType` · `rpart` · `glmnet` · `caret` · `pROC` · `survival` · `survminer`

---

## Cronograma

| Data | Aula |
|------|------|
| 23/03 | Preparação do R e extração de dados |
| 24/03 | Pré-processamento Parte 1 |
| 25/03 | Pré-processamento Parte 2 |
| 26/03 | Análise exploratória |
| 27/03 | Estatística inferencial |
| 30/03 | Modelos não-supervisionados (K-Means, hierárquico) |
| 31/03 | K-Prototypes |
| 01/04 | Regressão linear e árvore |
| 02/04 | Regressão logística e árvore |
| 03/04 | Poisson e Kaplan-Meier |

---

## Certificação

- **Frequência:** mínimo 70% das aulas
- **Desafio final:** questionário prático em R (prazo: 1 semana após o curso)

---

## Observações

1. **Sem internet:** a Aula 1 gera dados simulados para fins didáticos.
2. **Reprodutibilidade:** `set.seed(123)` em todos os scripts.
3. **Aula 8:** usa o mesmo dataset para regressão linear e árvore (comparação direta).
4. **Aula 10:** Kaplan-Meier usa obrigatoriamente os dados glifage importados na Aula 1.
