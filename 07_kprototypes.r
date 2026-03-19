# ==============================================================================
# AULA 7: MACHINE LEARNING – CLUSTERING NÃO-SUPERVISIONADO PARTE 2
# ==============================================================================
# Autor: Cientista de Dados Sênior
# Data de Criação: 31/03/2026
# Objetivo: K-Prototypes com SINASC (Dados Mistos: numéricos e categóricos)
#
# ENTRADA: Arquivo CSV processado da Aula 3
#   - dados_processados_datasus/SINASC_2024_06_DF_processado.csv
#
# SAÍDA: Modelos treinados, visualizações, métricas de avaliação
# DEPENDÊNCIAS: tidyverse, clustMixType, cluster, factoextra
#
# OBS: Dados foram padronizados (Z-score) e codificados (one-hot) na Aula 3
# SINASC contém dados mistos: peso (contínuo), idade mãe (contínuo), sexo (0/1), etc.
#
# FLUXO DE ML (4 PASSOS OBRIGATÓRIOS):
#   1. Carregar dados processados (CSV)
#   2. Modelo simples (baseline/default)
#   3. Modelo customizado (tuning de hiperparâmetros)
#   4. Avaliação e comparação de desempenho
# ==============================================================================

# ==== CARREGAMENTO DE BIBLIOTECAS ====
library(tidyverse)       # Manipulação de dados
library(cluster)         # Funções de clustering
library(factoextra)      # Visualização e avaliação
library(clustMixType)    # K-Prototypes para dados mistos

# ==== CONFIGURAÇÃO DE DIRETÓRIOS ====
dir_dados_processados <- "dados_processados_datasus"

# ==== PASSO 1: CARREGAMENTO DOS DADOS ====


# Carrega dados processados do SINASC (já padronizados e codificados)
sinasc <- read_csv(file.path(dir_dados_processados, "SINASC_2024_06_DF_processado.csv"),
                   show_col_types = FALSE)


# ==== FUNÇÃO AUXILIAR: K-Prototypes ====
# K-Prototypes é otimizado para dados com variáveis numéricas e categóricas
# Neste caso, como todos os dados foram já codificados em 0/1, usamos kproto como tabela numérica
# Para demonstrar o conceito, separaremos colunas "categóricas" (binárias 0/1)

# Identifica colunas que parecem ser binárias (0/1) - provavelmente categóricas originalmente
identificar_binarias <- function(dados) {
  binarias <- c()
  for (col in colnames(dados)) {
    valores_unicos <- unique(dados[[col]])
    if (length(valores_unicos) == 2 && all(valores_unicos %in% c(0, 1))) {
      binarias <- c(binarias, col)
    }
  }
  return(binarias)
}

colunas_binarias <- identificar_binarias(sinasc)
if (length(colunas_binarias) > 0) {
}

# Colunas contínuas (não binárias)
colunas_continuas <- colnames(sinasc)[!(colnames(sinasc) %in% colunas_binarias)]
if (length(colunas_continuas) > 0) {
}

# ==== PASSO 2: MODELO SIMPLES (BASELINE) ====


set.seed(123)
kproto_simples <- kproto(sinasc,
                         k = 3,              # 3 protótipos (clusters)
                         iter.max = 10,      # máximo 10 iterações
                         verbose = 0,        # sem saída detalhada
                         drop.levels = FALSE) # mantém todos os níveis




# ==== PASSO 3: MODELO CUSTOMIZADO (TUNING) ====


set.seed(123)
kproto_custom <- kproto(sinasc,
                        k = 4,               # 4 protótipos (tuned)
                        iter.max = 100,      # 100 iterações máximas
                        verbose = 0,
                        drop.levels = FALSE,
                        lambda = 0.5)        # Balanced weighting




# ==== PASSO 4: AVALIAÇÃO E COMPARAÇÃO ====

# Cálculo de Silhueta para k-prototypes (usando distância calculada)

# Calcula matriz de distância para silhueta
# Usa distância Euclidiana após normalização
dist_matrix <- dist(sinasc, method = "euclidean")

# Silhueta K-Prototypes simples
silhueta_kproto_simples <- silhouette(kproto_simples$cluster, dist_matrix)
silhueta_media_simples <- mean(silhueta_kproto_simples[, "sil_width"])


# Silhueta K-Prototypes customizado
silhueta_kproto_custom <- silhouette(kproto_custom$cluster, dist_matrix)
silhueta_media_custom <- mean(silhueta_kproto_custom[, "sil_width"])


# ==== Análise de Estabilidade ====

wcss_kproto <- numeric(7)
silhueta_media_vec <- numeric(7)

for (k in 2:8) {
  set.seed(123)
  km <- kproto(sinasc, k = k, iter.max = 50, verbose = 0, drop.levels = FALSE)
  wcss_kproto[k-1] <- km$tot.withinss

  # Calcula silhueta
  sil <- silhouette(km$cluster, dist_matrix)
  silhueta_media_vec[k-1] <- mean(sil[, "sil_width"])
}

for (k in 2:8) {
      " | Silhueta=", format(round(silhueta_media_vec[k-1], 4), width=8), "\n", sep = "")
}

# Identifica melhor k baseado em silhueta
melhor_k <- which.max(silhueta_media_vec) + 1

# ==== Análise de Pureza e Coesão ====

# Um cluster é mais "puro" quanto menor a variância interna
variancia_clust_simples <- numeric(3)
variancia_clust_custom <- numeric(4)

for (i in 1:3) {
  membros <- which(kproto_simples$cluster == i)
  if (length(membros) > 1) {
    dist_interna <- dist(sinasc[membros, ])
    variancia_clust_simples[i] <- mean(dist_interna)
  }
}

for (i in 1:4) {
  membros <- which(kproto_custom$cluster == i)
  if (length(membros) > 1) {
    dist_interna <- dist(sinasc[membros, ])
    variancia_clust_custom[i] <- mean(dist_interna)
  }
}

for (i in 1:3) {
}

for (i in 1:4) {
}

# ==== Análise de Separação ====

# Distribuição de observações por cluster
distrib_simples <- table(kproto_simples$cluster)
for (i in 1:3) {
  pct <- distrib_simples[i] / sum(distrib_simples) * 100
}

distrib_custom <- table(kproto_custom$cluster)
for (i in 1:4) {
  pct <- distrib_custom[i] / sum(distrib_custom) * 100
}

# ==== COMPARAÇÃO FINAL ====

comparacao_final <- tibble(
  Métrica = c(
    "Silhueta Média",
    "Within-Cluster SS",
    "Número de Clusters",
    "Iterações Máximas",
    "Lambda (ponderação)",
    "Clusters bem definidos (sil>0)"
  ),
  "K-Prototypes Simples" = c(
    round(silhueta_media_simples, 4),
    round(kproto_simples$tot.withinss, 2),
    3,
    10,
    1.0,
    paste0(
      round(sum(silhueta_kproto_simples[, "sil_width"] > 0) / nrow(sinasc) * 100, 1),
      "%"
    )
  ),
  "K-Prototypes Customizado" = c(
    round(silhueta_media_custom, 4),
    round(kproto_custom$tot.withinss, 2),
    4,
    100,
    0.5,
    paste0(
      round(sum(silhueta_kproto_custom[, "sil_width"] > 0) / nrow(sinasc) * 100, 1),
      "%"
    )
  )
)
