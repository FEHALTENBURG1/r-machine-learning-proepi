# ==============================================================================
# AULA 3: PRÉ-PROCESSAMENTO DE DADOS – PARTE 2
# ==============================================================================
# Autor: Cientista de Dados Sênior
# Data de Criação: 25/03/2026
# Objetivo: Padronização, normalização, discretização e codificação de variáveis
#
# ENTRADA: Arquivos CSV intermediários da Aula 2
#   - dados_processados_datasus/SINASC_2024_06_DF_intermediario.csv
#   - dados_processados_datasus/SIM_2024_06_DF_intermediario.csv
#   - dados_processados_datasus/SINAN_2024_06_DF_intermediario.csv
#   - dados_processados_datasus/SIA_2024_06_DF_intermediario.csv
#   - dados_processados_datasus/SIH_2024_06_DF_intermediario.csv
#
# SAÍDA: Arquivos CSV processados (input Aulas 4-10)
#   - dados_processados_datasus/SINASC_2024_06_DF_processado.csv
#   - dados_processados_datasus/SIM_2024_06_DF_processado.csv
#   - dados_processados_datasus/SINAN_2024_06_DF_processado.csv
#   - dados_processados_datasus/SIA_2024_06_DF_processado.csv
#   - dados_processados_datasus/SIH_2024_06_DF_processado.csv
#
# DEPENDÊNCIAS: tidyverse, janitor, recipes (tidymodels)
# ==============================================================================

# ==== CARREGAMENTO DE BIBLIOTECAS ====
library(tidyverse)  # Manipulação e visualização
library(janitor)    # Limpeza de dados
library(recipes)    # Pré-processamento (normalização, one-hot encoding, etc)

# ==== CONFIGURAÇÃO DE DIRETÓRIOS ====
dir_dados_processados <- "dados_processados_datasus"

# ==== FUNÇÕES AUXILIARES ====

# Função: Normalização Min-Max
# Transforma valores para o intervalo [0, 1]
# Fórmula: (x - min) / (max - min)
normalizar_minmax <- function(x) {
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  (x - min_val) / (max_val - min_val)
}

# Função: Padronização Z-Score
# Transforma dados para média 0 e desvio padrão 1
# Fórmula: (x - média) / desvio_padrão
padronizar_zscore <- function(x) {
  media <- mean(x, na.rm = TRUE)
  desvio <- sd(x, na.rm = TRUE)
  (x - media) / desvio
}

# Função: Discretização por Binning (igual width)
# Divide uma variável contínua em n_bins categorias com largura igual
discretizar_binning <- function(x, n_bins = 5) {
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  breaks <- seq(min_val, max_val, length.out = n_bins + 1)
  cut(x, breaks = breaks, labels = paste0("bin_", 1:n_bins), include.lowest = TRUE)
}

# Função: One-Hot Encoding
# Converte variável categórica em múltiplas colunas binárias
one_hot_encoding <- function(dados, col_nome) {
  # Cria uma cópia do dataframe
  dados_encoded <- dados

  # Obtém valores únicos da coluna
  categorias <- unique(dados[[col_nome]])

  # Para cada categoria, cria coluna binária
  for (cat in categorias) {
    nova_col <- paste0(col_nome, "_", cat)
    dados_encoded[[nova_col]] <- ifelse(dados[[col_nome]] == cat, 1, 0)
  }

  # Remove coluna original
  dados_encoded[[col_nome]] <- NULL

  return(dados_encoded)
}

# ==== PRÉ-PROCESSAMENTO SINASC ====

sinasc <- read_csv(file.path(dir_dados_processados, "SINASC_2024_06_DF_intermediario.csv"),
                   show_col_types = FALSE)

# Identifica tipo de cada coluna

# ==== Padronização de variáveis numéricas ====
# Aplica Z-Score standardization para centralizar distribuição

colunas_numéricas <- colnames(sinasc)[sapply(sinasc, is.numeric)]
for (col in colunas_numéricas) {
  sinasc[[paste0(col, "_padronizado")]] <- padronizar_zscore(sinasc[[col]])
}

# ==== One-Hot Encoding de variáveis categóricas ====
# Converte categorias em variáveis binárias (0/1)

colunas_categoricas <- colnames(sinasc)[sapply(sinasc, is.character)]
for (col in colunas_categoricas) {
  sinasc <- one_hot_encoding(sinasc, col)
}

# Remove colunas numéricas originais (mantém apenas versões padronizadas)
sinasc <- sinasc %>%
  select(-all_of(colunas_numéricas))

# Renomeia colunas padronizadas removendo sufixo repetido
sinasc <- sinasc %>%
  rename_with(~str_remove(., "_padronizado_padronizado"), ends_with("_padronizado"))


write_csv(sinasc,
          file.path(dir_dados_processados, "SINASC_2024_06_DF_processado.csv"))

# ==== PRÉ-PROCESSAMENTO SIM ====

sim <- read_csv(file.path(dir_dados_processados, "SIM_2024_06_DF_intermediario.csv"),
                show_col_types = FALSE)

colunas_numéricas_sim <- colnames(sim)[sapply(sim, is.numeric)]
for (col in colunas_numéricas_sim) {
  sim[[paste0(col, "_padronizado")]] <- padronizar_zscore(sim[[col]])
}

colunas_categoricas_sim <- colnames(sim)[sapply(sim, is.character)]
for (col in colunas_categoricas_sim) {
  sim <- one_hot_encoding(sim, col)
}

sim <- sim %>%
  select(-all_of(colunas_numéricas_sim))

write_csv(sim,
          file.path(dir_dados_processados, "SIM_2024_06_DF_processado.csv"))

# ==== PRÉ-PROCESSAMENTO SINAN ====

sinan <- read_csv(file.path(dir_dados_processados, "SINAN_2024_06_DF_intermediario.csv"),
                  show_col_types = FALSE)

colunas_numéricas_sinan <- colnames(sinan)[sapply(sinan, is.numeric)]
for (col in colunas_numéricas_sinan) {
  sinan[[paste0(col, "_padronizado")]] <- padronizar_zscore(sinan[[col]])
}

colunas_categoricas_sinan <- colnames(sinan)[sapply(sinan, is.character)]
for (col in colunas_categoricas_sinan) {
  sinan <- one_hot_encoding(sinan, col)
}

sinan <- sinan %>%
  select(-all_of(colunas_numéricas_sinan))

write_csv(sinan,
          file.path(dir_dados_processados, "SINAN_2024_06_DF_processado.csv"))

# ==== PRÉ-PROCESSAMENTO SIA ====

sia <- read_csv(file.path(dir_dados_processados, "SIA_2024_06_DF_intermediario.csv"),
                show_col_types = FALSE)

# Para dados com muita variação, aplicamos discretização antes de normalizar
# Identifica coluna de valor (assumindo coluna com "valor" no nome)
col_valor <- colnames(sia)[grep("valor", colnames(sia), ignore.case = TRUE)]

if (length(col_valor) > 0 && is.numeric(sia[[col_valor[1]]])) {
  sia[[paste0(col_valor[1], "_discretizado")]] <- discretizar_binning(sia[[col_valor[1]]], n_bins = 5)
}

colunas_numéricas_sia <- colnames(sia)[sapply(sia, is.numeric)]
for (col in colunas_numéricas_sia) {
  sia[[paste0(col, "_padronizado")]] <- padronizar_zscore(sia[[col]])
}

# One-Hot Encoding
colunas_categoricas_sia <- colnames(sia)[sapply(sia, is.character) | sapply(sia, is.factor)]
for (col in colunas_categoricas_sia) {
  sia <- one_hot_encoding(sia, col)
}

sia <- sia %>%
  select(-all_of(colunas_numéricas_sia))

write_csv(sia,
          file.path(dir_dados_processados, "SIA_2024_06_DF_processado.csv"))

# ==== PRÉ-PROCESSAMENTO SIH ====

sih <- read_csv(file.path(dir_dados_processados, "SIH_2024_06_DF_intermediario.csv"),
                show_col_types = FALSE)

# Discretização para tempo_permanencia (variável chave para SIH)
if ("tempo_permanencia" %in% colnames(sih)) {
  sih$tempo_permanencia_discretizado <- discretizar_binning(sih$tempo_permanencia, n_bins = 4)
}

colunas_numéricas_sih <- colnames(sih)[sapply(sih, is.numeric)]
for (col in colunas_numéricas_sih) {
  sih[[paste0(col, "_padronizado")]] <- padronizar_zscore(sih[[col]])
}

colunas_categoricas_sih <- colnames(sih)[sapply(sih, is.character) | sapply(sih, is.factor)]
for (col in colunas_categoricas_sih) {
  sih <- one_hot_encoding(sih, col)
}

sih <- sih %>%
  select(-all_of(colunas_numéricas_sih))

write_csv(sih,
          file.path(dir_dados_processados, "SIH_2024_06_DF_processado.csv"))

# ==== RESUMO FINAL ====
