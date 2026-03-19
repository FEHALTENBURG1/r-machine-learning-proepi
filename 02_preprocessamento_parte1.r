# ==============================================================================
# AULA 2: PRÉ-PROCESSAMENTO DE DADOS – PARTE 1
# ==============================================================================
# Autor: Cientista de Dados Sênior
# Data de Criação: 24/03/2026
# Objetivo: Tratamento de tipos de variáveis, valores perdidos e outliers
#
# ENTRADA: Arquivos CSV da Aula 1
#   - dados_originais_datasus/SINASC_2024_06_DF_selecionadas.csv
#   - dados_originais_datasus/SIM_2024_06_DF_selecionadas.csv
#   - dados_originais_datasus/SINAN_2024_06_DF_selecionadas.csv
#   - dados_originais_datasus/SIA_2024_06_DF_selecionadas.csv
#   - dados_originais_datasus/SIH_2024_06_DF_selecionadas.csv
#
# SAÍDA: Arquivos CSV intermediários (input Aula 3)
#   - dados_processados_datasus/SINASC_2024_06_DF_intermediario.csv
#   - dados_processados_datasus/SIM_2024_06_DF_intermediario.csv
#   - dados_processados_datasus/SINAN_2024_06_DF_intermediario.csv
#   - dados_processados_datasus/SIA_2024_06_DF_intermediario.csv
#   - dados_processados_datasus/SIH_2024_06_DF_intermediario.csv
#
# DEPENDÊNCIAS: tidyverse, janitor
# ==============================================================================

# ==== CARREGAMENTO DE BIBLIOTECAS ====
library(tidyverse)  # Manipulação de dados e visualização
library(janitor)    # Funções de limpeza de dados

# ==== CONFIGURAÇÃO DE DIRETÓRIOS ====
dir_dados_originais <- "dados_originais_datasus"
dir_dados_processados <- "dados_processados_datasus"

# Cria diretório de dados processados se não existir
if (!dir.exists(dir_dados_processados)) dir.create(dir_dados_processados)

# ==== FUNÇÃO AUXILIAR: DETECTAR E TRATAR OUTLIERS ====
# Utiliza o método IQR (Interquartile Range) para identificar valores discrepantes
# Para cada variável numérica:
#   - Calcula Q1 (1º quartil) e Q3 (3º quartil)
#   - Limites: Q1 - 1.5*IQR e Q3 + 1.5*IQR
#   - Valores fora dos limites são substituídos pela mediana ou recortados

tratar_outliers_iqr <- function(dados, col, metodo = "mediana") {
  # Parâmetros:
  #   dados: dataframe
  #   col: nome da coluna (como string)
  #   metodo: "mediana" (substitui) ou "recorte" (limita aos limites)

  valores <- dados[[col]]

  # Calcula quartis e IQR
  Q1 <- quantile(valores, 0.25, na.rm = TRUE)
  Q3 <- quantile(valores, 0.75, na.rm = TRUE)
  IQR_valor <- Q3 - Q1

  # Define limites da boxplot
  limite_inferior <- Q1 - 1.5 * IQR_valor
  limite_superior <- Q3 + 1.5 * IQR_valor

  # Aplica tratamento conforme método escolhido
  if (metodo == "mediana") {
    mediana <- median(valores, na.rm = TRUE)
    dados[[col]] <- ifelse(valores < limite_inferior | valores > limite_superior,
                           mediana,
                           valores)
  } else if (metodo == "recorte") {
    dados[[col]] <- pmax(pmin(valores, limite_superior), limite_inferior)
  }

  return(dados)
}

# ==== PRÉ-PROCESSAMENTO SINASC (NASCIMENTOS) ====
sinasc <- read_csv(file.path(dir_dados_originais, "SINASC_2024_06_DF_selecionadas.csv"),
                   show_col_types = FALSE)

# ==== Identificar e tratar valores perdidos (NA) ====
prop_na <- colSums(is.na(sinasc)) / nrow(sinasc) * 100

# Se houver campos com mais de 50% de NA, remove a coluna
# Caso contrário, imputa com mediana (variáveis numéricas) ou moda (categóricas)
sinasc <- sinasc %>%
  select(-all_of(names(prop_na[prop_na > 50])))

# Imputa valores perdidos em variáveis numéricas com a mediana
for (col in colnames(sinasc)) {
  if (is.numeric(sinasc[[col]]) && any(is.na(sinasc[[col]]))) {
    mediana <- median(sinasc[[col]], na.rm = TRUE)
    sinasc[[col]][is.na(sinasc[[col]])] <- mediana
  }
}

# Imputa valores perdidos em variáveis categóricas com a moda (valor mais frequente)
for (col in colnames(sinasc)) {
  if (is.character(sinasc[[col]]) && any(is.na(sinasc[[col]]))) {
    moda <- names(sort(-table(sinasc[[col]], useNA = "no")))[1]
    sinasc[[col]][is.na(sinasc[[col]])] <- moda
  }
}

# ==== Tratar outliers em variáveis numéricas ====
variáveis_numéricas_sinasc <- colnames(sinasc)[sapply(sinasc, is.numeric)]
for (col in variáveis_numéricas_sinasc) {
  Q1 <- quantile(sinasc[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(sinasc[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  outliers <- sum(sinasc[[col]] < Q1 - 1.5*IQR_val | sinasc[[col]] > Q3 + 1.5*IQR_val)
  if (outliers > 0) sinasc <- tratar_outliers_iqr(sinasc, col, metodo = "mediana")
}

write_csv(sinasc,
          file.path(dir_dados_processados, "SINASC_2024_06_DF_intermediario.csv"))

# ==== PRÉ-PROCESSAMENTO SIM (MORTALIDADE) ====
sim <- read_csv(file.path(dir_dados_originais, "SIM_2024_06_DF_selecionadas.csv"),
                show_col_types = FALSE)

# Identifica valores perdidos
prop_na_sim <- colSums(is.na(sim)) / nrow(sim) * 100

# Remove colunas com >50% de NA
sim <- sim %>%
  select(-all_of(names(prop_na_sim[prop_na_sim > 50])))

# Imputa variáveis numéricas com mediana
for (col in colnames(sim)) {
  if (is.numeric(sim[[col]]) && any(is.na(sim[[col]]))) {
    mediana <- median(sim[[col]], na.rm = TRUE)
    sim[[col]][is.na(sim[[col]])] <- mediana
  }
}

# Imputa variáveis categóricas com moda
for (col in colnames(sim)) {
  if (is.character(sim[[col]]) && any(is.na(sim[[col]]))) {
    moda <- names(sort(-table(sim[[col]], useNA = "no")))[1]
    sim[[col]][is.na(sim[[col]])] <- moda
  }
}

# Trata outliers
variáveis_numéricas_sim <- colnames(sim)[sapply(sim, is.numeric)]
for (col in variáveis_numéricas_sim) {
  Q1 <- quantile(sim[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(sim[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  outliers <- sum(sim[[col]] < Q1 - 1.5*IQR_val | sim[[col]] > Q3 + 1.5*IQR_val)

  if (outliers > 0) {
    sim <- tratar_outliers_iqr(sim, col, metodo = "mediana")
  }
}

write_csv(sim,
          file.path(dir_dados_processados, "SIM_2024_06_DF_intermediario.csv"))

# ==== PRÉ-PROCESSAMENTO SINAN (AGRAVOS) ====

sinan <- read_csv(file.path(dir_dados_originais, "SINAN_2024_06_DF_selecionadas.csv"),
                  show_col_types = FALSE)

prop_na_sinan <- colSums(is.na(sinan)) / nrow(sinan) * 100

sinan <- sinan %>%
  select(-all_of(names(prop_na_sinan[prop_na_sinan > 50])))

for (col in colnames(sinan)) {
  if (is.numeric(sinan[[col]]) && any(is.na(sinan[[col]]))) {
    mediana <- median(sinan[[col]], na.rm = TRUE)
    sinan[[col]][is.na(sinan[[col]])] <- mediana
  }
}

for (col in colnames(sinan)) {
  if (is.character(sinan[[col]]) && any(is.na(sinan[[col]]))) {
    moda <- names(sort(-table(sinan[[col]], useNA = "no")))[1]
    sinan[[col]][is.na(sinan[[col]])] <- moda
  }
}

variáveis_numéricas_sinan <- colnames(sinan)[sapply(sinan, is.numeric)]
for (col in variáveis_numéricas_sinan) {
  Q1 <- quantile(sinan[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(sinan[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  outliers <- sum(sinan[[col]] < Q1 - 1.5*IQR_val | sinan[[col]] > Q3 + 1.5*IQR_val)

  if (outliers > 0) {
    sinan <- tratar_outliers_iqr(sinan, col, metodo = "mediana")
  }
}

write_csv(sinan,
          file.path(dir_dados_processados, "SINAN_2024_06_DF_intermediario.csv"))

# ==== PRÉ-PROCESSAMENTO SIA (PRODUÇÃO AMBULATORIAL) ====

sia <- read_csv(file.path(dir_dados_originais, "SIA_2024_06_DF_selecionadas.csv"),
                show_col_types = FALSE)

prop_na_sia <- colSums(is.na(sia)) / nrow(sia) * 100

sia <- sia %>%
  select(-all_of(names(prop_na_sia[prop_na_sia > 50])))

for (col in colnames(sia)) {
  if (is.numeric(sia[[col]]) && any(is.na(sia[[col]]))) {
    mediana <- median(sia[[col]], na.rm = TRUE)
    sia[[col]][is.na(sia[[col]])] <- mediana
  }
}

for (col in colnames(sia)) {
  if (is.character(sia[[col]]) && any(is.na(sia[[col]]))) {
    moda <- names(sort(-table(sia[[col]], useNA = "no")))[1]
    sia[[col]][is.na(sia[[col]])] <- moda
  }
}

variáveis_numéricas_sia <- colnames(sia)[sapply(sia, is.numeric)]
for (col in variáveis_numéricas_sia) {
  Q1 <- quantile(sia[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(sia[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  outliers <- sum(sia[[col]] < Q1 - 1.5*IQR_val | sia[[col]] > Q3 + 1.5*IQR_val)

  if (outliers > 0) {
    sia <- tratar_outliers_iqr(sia, col, metodo = "mediana")
  }
}

write_csv(sia,
          file.path(dir_dados_processados, "SIA_2024_06_DF_intermediario.csv"))

# ==== PRÉ-PROCESSAMENTO SIH (INTERNAÇÕES) ====

sih <- read_csv(file.path(dir_dados_originais, "SIH_2024_06_DF_selecionadas.csv"),
                show_col_types = FALSE)

prop_na_sih <- colSums(is.na(sih)) / nrow(sih) * 100

sih <- sih %>%
  select(-all_of(names(prop_na_sih[prop_na_sih > 50])))

for (col in colnames(sih)) {
  if (is.numeric(sih[[col]]) && any(is.na(sih[[col]]))) {
    mediana <- median(sih[[col]], na.rm = TRUE)
    sih[[col]][is.na(sih[[col]])] <- mediana
  }
}

for (col in colnames(sih)) {
  if (is.character(sih[[col]]) && any(is.na(sih[[col]]))) {
    moda <- names(sort(-table(sih[[col]], useNA = "no")))[1]
    sih[[col]][is.na(sih[[col]])] <- moda
  }
}

variáveis_numéricas_sih <- colnames(sih)[sapply(sih, is.numeric)]
for (col in variáveis_numéricas_sih) {
  Q1 <- quantile(sih[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(sih[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  outliers <- sum(sih[[col]] < Q1 - 1.5*IQR_val | sih[[col]] > Q3 + 1.5*IQR_val)

  if (outliers > 0) {
    sih <- tratar_outliers_iqr(sih, col, metodo = "mediana")
  }
}

write_csv(sih,
          file.path(dir_dados_processados, "SIH_2024_06_DF_intermediario.csv"))

# ==== RESUMO FINAL ====
