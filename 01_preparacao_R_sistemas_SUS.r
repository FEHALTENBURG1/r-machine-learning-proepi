# ==============================================================================
# AULA 1: PREPARACAO DO R - SISTEMAS SUS
# Curso: Analise de Dados e Machine Learning com R - ProEpi 2026
# ==============================================================================

# ==== SESSAO 1: INSTALACAO DIRETA DOS PACOTES ====
pacotes_necessarios <- c(
  "tidyverse",
  "janitor",
  "tidymodels",
  "cluster",
  "factoextra",
  "clustMixType",
  "rpart",
  "glmnet",
  "caret",
  "pROC",
  "survival",
  "survminer",
  "remotes"
)

install.packages(pacotes_necessarios)

# ==== SESSAO 2: INSTALAR microdatasus DIRETO DO GITHUB ====
remotes::install_github("rfsaldanha/microdatasus")
