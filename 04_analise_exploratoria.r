# ==============================================================================
# AULA 4: ANÁLISE EXPLORATÓRIA DE DADOS (EDA)
# ==============================================================================
# Autor: Cientista de Dados Sênior
# Data de Criação: 26/03/2026
# Objetivo: Análise de frequência, tendência central, dispersão e correlação
#
# ENTRADA: Arquivos CSV brutos da Aula 1 (para variáveis não processadas)
#   - dados_originais_datasus/SINASC_2024_06_DF_selecionadas.csv
#   - dados_originais_datasus/SIM_2024_06_DF_selecionadas.csv
#   - dados_originais_datasus/SINAN_2024_06_DF_selecionadas.csv
#   - dados_originais_datasus/SIA_2024_06_DF_selecionadas.csv
#   - dados_originais_datasus/SIH_2024_06_DF_selecionadas.csv
#
# SAÍDA: Gráficos e sumários estatísticos (visualizações interativas)
# DEPENDÊNCIAS: tidyverse, ggplot2, corrplot, skimr
# ==============================================================================

# ==== CARREGAMENTO DE BIBLIOTECAS ====
library(tidyverse)  # Manipulação e visualização
library(ggplot2)    # Visualização avançada
library(corrplot)   # Visualização de matrizes de correlação
library(skimr)      # Sumários rápidos de dados

# ==== CONFIGURAÇÃO DE DIRETÓRIOS ====
dir_dados_originais <- "dados_originais_datasus"

# ==== ANÁLISE EXPLORATÓRIA SINASC ====

sinasc <- read_csv(file.path(dir_dados_originais, "SINASC_2024_06_DF_selecionadas.csv"),
                   show_col_types = FALSE)

# Sumário rápido de todas as variáveis
skim(sinasc)

# ==== Análise de Frequência (Variáveis Categóricas) ====

colunas_categoricas <- colnames(sinasc)[sapply(sinasc, is.character)]

for (col in colunas_categoricas) {
  freq_abs <- table(sinasc[[col]])
  freq_rel <- prop.table(freq_abs) * 100

  df_freq <- tibble(
    categoria = names(freq_abs),
    frequencia_absoluta = as.integer(freq_abs),
    frequencia_relativa = as.numeric(freq_rel)
  )

}

# ==== Medidas de Tendência Central e Dispersão ====

colunas_numéricas <- colnames(sinasc)[sapply(sinasc, is.numeric)]

estatisticas <- tibble(
  variavel = colunas_numéricas,
  media = sapply(sinasc[colunas_numéricas], mean, na.rm = TRUE),
  mediana = sapply(sinasc[colunas_numéricas], median, na.rm = TRUE),
  desvio_padrao = sapply(sinasc[colunas_numéricas], sd, na.rm = TRUE),
  minimo = sapply(sinasc[colunas_numéricas], min, na.rm = TRUE),
  maximo = sapply(sinasc[colunas_numéricas], max, na.rm = TRUE),
  q1 = sapply(sinasc[colunas_numéricas], quantile, 0.25, na.rm = TRUE),
  q3 = sapply(sinasc[colunas_numéricas], quantile, 0.75, na.rm = TRUE)
)


# Cálculo de coeficiente de variação (CV)
for (col in colunas_numéricas) {
  cv <- (sd(sinasc[[col]], na.rm = TRUE) / mean(sinasc[[col]], na.rm = TRUE)) * 100
}

# ==== Análise de Correlação ====

# Correlação de Pearson (variáveis numéricas contínuas)
if (length(colunas_numéricas) > 1) {
  matriz_pearson <- cor(sinasc[colunas_numéricas], use = "complete.obs", method = "pearson")

  # Visualiza matriz de correlação
  png("eda_sinasc_correlacao_pearson.png", width = 800, height = 800)
  corrplot(matriz_pearson,
           method = "circle",
           type = "upper",
           title = "Correlação de Pearson - SINASC",
           tl.col = "black")
  dev.off()
}

# Correlação de Spearman (ranking, não-paramétrica)
if (length(colunas_numéricas) > 1) {
  matriz_spearman <- cor(sinasc[colunas_numéricas], use = "complete.obs", method = "spearman")
}

# ==== Visualizações EDA ====
# Histogramas para variáveis numéricas

for (col in colunas_numéricas) {
  p <- ggplot(sinasc, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Distribuição de", col),
         x = col,
         y = "Frequência")

  filename <- paste0("eda_sinasc_hist_", col, ".png")
  ggsave(filename, p, width = 8, height = 6)
}

# Box plots para identificar outliers

for (col in colunas_numéricas) {
  p <- ggplot(sinasc, aes(y = .data[[col]])) +
    geom_boxplot(fill = "lightgreen", color = "black") +
    theme_minimal() +
    labs(title = paste("Box plot de", col),
         y = col)

  filename <- paste0("eda_sinasc_boxplot_", col, ".png")
  ggsave(filename, p, width = 6, height = 8)
}

# ==== ANÁLISE EXPLORATÓRIA SIM (MORTALIDADE) ====

sim <- read_csv(file.path(dir_dados_originais, "SIM_2024_06_DF_selecionadas.csv"),
                show_col_types = FALSE)

skim(sim)

# Frequência de categorias importantes
if ("causa_basica" %in% colnames(sim)) {
  freq_causa <- sim %>%
    count(causa_basica, name = "frequencia") %>%
    mutate(percentual = frequencia / sum(frequencia) * 100) %>%
    arrange(desc(frequencia))
}

# Medidas de tendência central
colunas_numéricas_sim <- colnames(sim)[sapply(sim, is.numeric)]
if (length(colunas_numéricas_sim) > 0) {
  if ("idade_obito" %in% colnames(sim)) {
  }
}

# ==== ANÁLISE EXPLORATÓRIA SINAN ====

sinan <- read_csv(file.path(dir_dados_originais, "SINAN_2024_06_DF_selecionadas.csv"),
                  show_col_types = FALSE)

skim(sinan)

# Distribuição de resultados
if ("resultado" %in% colnames(sinan)) {
  freq_resultado <- sinan %>%
    count(resultado, name = "frequencia") %>%
    mutate(percentual = frequencia / sum(frequencia) * 100)
}

# Distribuição por agravo
if ("agravo" %in% colnames(sinan)) {
  freq_agravo <- sinan %>%
    count(agravo, name = "frequencia") %>%
    mutate(percentual = frequencia / sum(frequencia) * 100)
}

# ==== ANÁLISE EXPLORATÓRIA SIA ====

sia <- read_csv(file.path(dir_dados_originais, "SIA_2024_06_DF_selecionadas.csv"),
                show_col_types = FALSE)

skim(sia)

# Distribuição por tipo de procedimento
if ("tipo_procedimento" %in% colnames(sia)) {
  freq_proc <- sia %>%
    count(tipo_procedimento, name = "frequencia") %>%
    mutate(percentual = frequencia / sum(frequencia) * 100) %>%
    arrange(desc(frequencia))
}

# Distribuição por complexidade
if ("complexidade" %in% colnames(sia)) {
  freq_compl <- sia %>%
    count(complexidade, name = "frequencia") %>%
    mutate(percentual = frequencia / sum(frequencia) * 100)
}

# Estatísticas de valor procedimento
if ("valor_procedimento" %in% colnames(sia)) {
}

# ==== ANÁLISE EXPLORATÓRIA SIH ====

sih <- read_csv(file.path(dir_dados_originais, "SIH_2024_06_DF_selecionadas.csv"),
                show_col_types = FALSE)

skim(sih)

# Distribuição de diagnósticos
if ("diagnostico" %in% colnames(sih)) {
  freq_diag <- sih %>%
    count(diagnostico, name = "frequencia") %>%
    mutate(percentual = frequencia / sum(frequencia) * 100) %>%
    arrange(desc(frequencia))
}

# Estatísticas de tempo de permanência
if ("tempo_permanencia" %in% colnames(sih)) {

  # Quartis
}

# Estatísticas de valor de internação
if ("valor_internacao" %in% colnames(sih)) {
}

# ==== RESUMO FINAL ====
