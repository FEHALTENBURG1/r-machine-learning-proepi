# Curso de Analise de Dados e Machine Learning com R para Saude

Formacao aplicada para profissionais da saude, epidemiologia e vigilancia que desejam transformar dados publicos em evidencia analitica, com foco em reproducibilidade, interpretacao e suporte a decisao.

**ProEpi 2026**  
Autora: Danielly Xavier · [Lattes](https://lattes.cnpq.br/2373300932860701) · [LinkedIn](https://www.linkedin.com/in/danielly-bx/)

---

## Ementa - Curso de Analise de Dados e Machine Learning com R para Saude

### 1. Informacoes Gerais

- **Carga horaria:** 20 horas  
- **Modalidade:** Online (Zoom)  
- **Publico-alvo:** estudantes e profissionais interessados em analise de dados em saude  
- **Pre-requisitos:** conhecimentos basicos de informatica (nao e necessario saber programar)

### 2. Objetivos

Capacitar os participantes a coletar, preparar, analisar e modelar dados de saude utilizando a linguagem R, com foco em bases publicas (DATASUS) e aplicacao de tecnicas de machine learning.

### 3. Visao Geral do Conteudo

Este repositorio contem o material didatico completo do curso, organizado em scripts sequenciais (`01` a `08`) que percorrem o pipeline de ciencia de dados em saude:

- preparacao do ambiente e download de dados;
- tratamento e padronizacao;
- analise exploratoria;
- testes de hipotese;
- modelagem supervisionada (regressao, classificacao, contagem e sobrevida);
- modelagem nao supervisionada (clusterizacao).

---

## Estrutura do Repositorio

```text
.
├── 01_preparacao_download_dados.r
├── 02_tratamento_dados.r
├── 03_analise_exploratoria.r
├── 04_introducao_testes_hipotese.r
├── 05_machine_learning_supervisionado_parte01.r
├── 06_machine_learning_supervisionado_parte02.r
├── 07_machine_learning_supervisionado_parte03.r
├── 08_machine_learning_nao_supervisionado.r
├── dados_originais_datasus/
│   ├── dicionarios/
│   ├── sia/
│   ├── sih/
│   ├── sim/
│   ├── sinan/
│   └── sinasc/
├── dados_processados_datasus/
│   ├── sia_modelagem.RData
│   ├── sih_modelagem.RData
│   ├── sim_modelagem.RData
│   ├── sinan_modelagem.RData
│   └── sinasc_modelagem.RData
├── modelos_machine_learning/
│   ├── 05_ml_supervisionado_parte01/
│   ├── 06_ml_supervisionado_parte02/
│   ├── 07_ml_supervisionado_parte03/
│   └── 08_ml_nao_supervisionado/
├── r-machine-learning-proepi.Rproj
├── LICENSE
└── README.md
```

### Detalhamento das pastas

- `dados_originais_datasus/`: armazenamento bruto por sistema (arquivos de origem do DATASUS).
- `dados_originais_datasus/dicionarios/`: dicionarios e metadados de apoio para codigos/variaveis.
- `dados_processados_datasus/`: bases consolidadas e prontas para analise/modelagem.
- `modelos_machine_learning/`: saidas geradas pelas aulas de ML (summaries `.md` e figuras `.png`).
- `modelos_machine_learning/05_ml_supervisionado_parte01/`: resultados da aula de regressao supervisionada (SIH).
- `modelos_machine_learning/06_ml_supervisionado_parte02/`: resultados da aula de classificacao supervisionada (SIM).
- `modelos_machine_learning/07_ml_supervisionado_parte03/`: resultados da aula de contagem e sobrevida (SIH/SIA).
- `modelos_machine_learning/08_ml_nao_supervisionado/`: resultados da aula de clusterizacao (SINAN/SINASC).

> Observacao: os diretorios de `dados_originais_datasus` estao no `.gitignore` para evitar versionamento de arquivos brutos pesados/sensiveis.

---

## Cronograma e Conteudo

| Data       | Aula                                                          | Conteudo principal |
| ---------- | ------------------------------------------------------------- | ------------------ |
| 23/03/2026 | Preparacao do R e Sistemas de Informacao em Saude no SUS (2h) | Instalacao de pacotes; introducao ao `microdatasus`; download de bases DATASUS (SINASC, SIM, SINAN, SIA, SIH). |
| 24/03/2026 | Pre-processamento de dados (2h)                               | Tipos de variaveis, limpeza, missing, outliers, padronizacao/normalizacao, discretizacao e codificacao. |
| 25/03/2026 | Analise exploratoria (3h)                                     | Frequencias, medidas-resumo, visualizacoes e correlacoes (Pearson/Spearman). |
| 26/03/2026 | Estatistica inferencial (3h)                                  | Qui-quadrado, Fisher, ANOVA, teste t, Wilcoxon e Kolmogorov-Smirnov. |
| 30/03/2026 | Modelos supervisionados - Parte 1 (2h)                        | Regressao Linear (incluindo stepwise) e Arvore de Decisao para regressao. |
| 31/03/2026 | Modelos supervisionados - Parte 2 (2h)                        | Regressao Logistica (com stepwise), amostragem de classes e Arvore para classificacao. |
| 01/04/2026 | Modelos supervisionados - Parte 3 (3h)                        | Regressao de contagem (Poisson/NegBin) e Analise de Sobrevida (Kaplan-Meier/Cox). |
| 02/04/2026 | Modelos nao supervisionados (3h)                              | Hierarquico aglomerativo, K-Means e K-Prototypes com avaliacao de clusters. |

---

## Scripts e Objetivos por Aula

- `01_preparacao_download_dados.r`: configuracao do ambiente e ingestao inicial de dados do DATASUS.
- `02_tratamento_dados.r`: limpeza, transformacao, tipagem e padronizacao das bases.
- `03_analise_exploratoria.r`: EDA com metricas descritivas e visualizacoes.
- `04_introducao_testes_hipotese.r`: comparacoes inferenciais e interpretacao estatistica.
- `05_machine_learning_supervisionado_parte01.r`: regressao supervisionada com foco em `VAL_TOT` (SIH).
- `06_machine_learning_supervisionado_parte02.r`: classificacao supervisionada para evento `I219` (SIM).
- `07_machine_learning_supervisionado_parte03.r`: contagem (internacoes diarias) e sobrevida (SIA).
- `08_machine_learning_nao_supervisionado.r`: clusterizacao SINAN/SINASC com avaliacao de qualidade (silhueta, BSS/TSS, perfis e dendrograma).

---

## Dados de Entrada Esperados

Os scripts de ML utilizam arquivos em `dados_processados_datasus/`, especialmente:

- `sih_modelagem.RData`
- `sim_modelagem.RData`
- `sia_modelagem.RData`
- `sinan_modelagem.RData`
- `sinasc_modelagem.RData`

Se os dados ainda nao estiverem disponiveis, execute as aulas iniciais de preparacao e tratamento antes das aulas de modelagem.

---

## Como Executar

### 1) Abrir o projeto no RStudio/Posit

Use o diretorio raiz do repositorio como working directory.

### 2) Rodar em sequencia didatica

Execute os scripts na ordem numerica (`01` -> `08`) para garantir reproducibilidade e consistencia de insumos.

### 3) Execucao por script (terminal)

```bash
Rscript 01_preparacao_download_dados.r
Rscript 02_tratamento_dados.r
Rscript 03_analise_exploratoria.r
Rscript 04_introducao_testes_hipotese.r
Rscript 05_machine_learning_supervisionado_parte01.r
Rscript 06_machine_learning_supervisionado_parte02.r
Rscript 07_machine_learning_supervisionado_parte03.r
Rscript 08_machine_learning_nao_supervisionado.r
```

---

## Saidas Geradas

As saidas de modelagem ficam em `modelos_machine_learning/`, em subpastas por aula.

- Aula 5: `modelos_machine_learning/05_ml_supervisionado_parte01`
- Aula 6: `modelos_machine_learning/06_ml_supervisionado_parte02`
- Aula 7: `modelos_machine_learning/07_ml_supervisionado_parte03`
- Aula 8: `modelos_machine_learning/08_ml_nao_supervisionado`

Padrao atual das aulas mais recentes:

- **summaries:** arquivos `.md`
- **graficos:** arquivos `.png`
- **sem salvamento `.rds`** nas aulas 7 e 8

---

## Pacotes Principais Utilizados

- Manipulacao e visualizacao: `tidyverse`, `dplyr`, `tidyr`, `ggplot2`, `lubridate`
- Modelagem supervisionada: `rsample`, `caret`, `rpart`, `rpart.plot`, `pROC`, `MASS`, `AER`, `survival`, `survminer`
- Modelagem nao supervisionada: `cluster`, `factoextra`, `clustMixType`

---

## Publico-alvo e Certificacao

- **Publico:** estudantes e profissionais de epidemiologia, vigilancia e gestao em saude.
- **Carga horaria:** 20 horas.
- **Certificacao:** frequencia minima de 70% + entrega da atividade pratica final.

