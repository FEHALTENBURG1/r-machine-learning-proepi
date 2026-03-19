# ==============================================================================
# AULA 6: MACHINE LEARNING – CLUSTERING NÃO-SUPERVISIONADO PARTE 1
# ==============================================================================
# Autor: Cientista de Dados Sênior
# Data de Criação: 30/03/2026
# Objetivo: K-Means e Clustering Hierárquico com SIA (Produção Ambulatorial)
#
# ENTRADA: Arquivo CSV processado da Aula 3
#   - dados_processados_datasus/SIA_2024_06_DF_processado.csv
#
# SAÍDA: Modelos treinados, visualizações, métricas de avaliação
# DEPENDÊNCIAS: tidyverse, cluster, factoextra, dendextend
#
# FLUXO DE ML (4 PASSOS OBRIGATÓRIOS):
#   1. Carregar dados processados (CSV)
#   2. Modelo simples (baseline/default)
#   3. Modelo customizado (tuning de hiperparâmetros)
#   4. Avaliação e comparação de desempenho
# ==============================================================================

# ==== CARREGAMENTO DE BIBLIOTECAS ====
library(tidyverse)     # Manipulação de dados
library(cluster)       # Funções de clustering
library(factoextra)    # Visualização e avaliação de clusters
library(dendextend)    # Visualização de dendrogramas

# ==== CONFIGURAÇÃO DE DIRETÓRIOS ====
dir_dados_processados <- "dados_processados_datasus"

# ==== PASSO 1: CARREGAMENTO DOS DADOS ====


# Carrega dados processados do SIA
sia <- read_csv(file.path(dir_dados_processados, "SIA_2024_06_DF_processado.csv"),
                show_col_types = FALSE)


# ==== PASSO 2: MODELO SIMPLES (BASELINE) ====

# K-MEANS SIMPLES (padrão do R)

set.seed(123)
kmeans_simples <- kmeans(sia,
                         centers = 3,          # 3 clusters (padrão)
                         iter.max = 10,        # máximo 10 iterações
                         nstart = 1)           # apenas 1 tentativa


# CLUSTERING HIERÁRQUICO SIMPLES (método Ward)

# Calcula matriz de distância euclidiana
dist_euclidiana <- dist(sia, method = "euclidean")

# Realiza clustering hierárquico com método Ward
hierarquico_simples <- hclust(dist_euclidiana, method = "ward.D2")

# Corta o dendrograma para obter 3 clusters (para comparação com K-Means)
clusters_hierarquico_simples <- cutree(hierarquico_simples, k = 3)


# ==== PASSO 3: MODELO CUSTOMIZADO (TUNING) ====

# K-MEANS CUSTOMIZADO (otimizado)

set.seed(123)
kmeans_custom <- kmeans(sia,
                        centers = 4,           # 4 clusters (tuned)
                        iter.max = 100,        # 100 iterações máximas
                        nstart = 25,           # 25 tentativas diferentes
                        algorithm = "Lloyd")   # Algoritmo Lloyd (padrão robusto)


# CLUSTERING HIERÁRQUICO CUSTOMIZADO (múltiplos métodos)

# Método Complete Linkage (mais conservador)
hierarquico_custom <- hclust(dist_euclidiana, method = "complete")
clusters_hierarquico_custom <- cutree(hierarquico_custom, k = 4)


# ==== PASSO 4: AVALIAÇÃO E COMPARAÇÃO ====

# Silhueta: Mede quão bem cada ponto foi classificado
# Varia de -1 a 1 (valores próximos a 1 indicam boa classificação)


# Silhueta K-Means simples
silhueta_kmeans_simples <- silhouette(kmeans_simples$cluster, dist_euclidiana)
silhueta_media_kmeans_simples <- mean(silhueta_kmeans_simples[, "sil_width"])

# Silhueta K-Means customizado
silhueta_kmeans_custom <- silhouette(kmeans_custom$cluster, dist_euclidiana)
silhueta_media_kmeans_custom <- mean(silhueta_kmeans_custom[, "sil_width"])

# Silhueta Hierárquico simples
silhueta_hier_simples <- silhouette(clusters_hierarquico_simples, dist_euclidiana)
silhueta_media_hier_simples <- mean(silhueta_hier_simples[, "sil_width"])

# Silhueta Hierárquico customizado
silhueta_hier_custom <- silhouette(clusters_hierarquico_custom, dist_euclidiana)
silhueta_media_hier_custom <- mean(silhueta_hier_custom[, "sil_width"])

# ==== Teste de Estabilidade: Elbow Method ====

wcss_values <- numeric(10)
for (k in 1:10) {
  set.seed(123)
  km <- kmeans(sia, centers = k, iter.max = 100, nstart = 10)
  wcss_values[k] <- km$tot.withinss
}

for (k in 1:10) {
}

# Detecta ponto de cotovelo (redução percentual)
reducoes <- diff(wcss_values)
reducoes_pct <- (reducoes / wcss_values[-length(wcss_values)]) * 100
for (i in 1:length(reducoes_pct)) {
}

# ==== Métricas Compactness e Separação ====

# Compactness: Quanto menor, melhor (pontos próximos ao centroide)
# Separação: Quanto maior, melhor (clusters bem separados)

# K-Means Simples
compactness_kmeans_simples <- kmeans_simples$tot.withinss / nrow(sia)

# K-Means Customizado
compactness_kmeans_custom <- kmeans_custom$tot.withinss / nrow(sia)

# ==== COMPARAÇÃO FINAL ====

comparacao <- tibble(
  Métrica = c(
    "Silhueta Média",
    "Total WCSS",
    "Between SS",
    "Número de Clusters",
    "Iterações",
    "Tentativas (nstart)"
  ),
  "K-Means Simples" = c(
    round(silhueta_media_kmeans_simples, 4),
    round(kmeans_simples$tot.withinss, 2),
    round(kmeans_simples$betweenss, 2),
    3,
    10,
    1
  ),
  "K-Means Custom" = c(
    round(silhueta_media_kmeans_custom, 4),
    round(kmeans_custom$tot.withinss, 2),
    round(kmeans_custom$betweenss, 2),
    4,
    100,
    25
  ),
  "Hier Simples" = c(
    round(silhueta_media_hier_simples, 4),
    "N/A",
    "N/A",
    3,
    "N/A",
    "Dendrograma"
  ),
  "Hier Custom" = c(
    round(silhueta_media_hier_custom, 4),
    "N/A",
    "N/A",
    4,
    "N/A",
    "Dendrograma"
  )
)


if (silhueta_media_kmeans_custom > silhueta_media_kmeans_simples) {
} else {
}

if (silhueta_media_hier_custom > silhueta_media_kmeans_custom) {
} else {
}

