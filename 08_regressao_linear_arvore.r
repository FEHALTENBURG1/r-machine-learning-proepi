# ==============================================================================
# AULA 8: MACHINE LEARNING – REGRESSÃO (VARIÁVEIS CONTÍNUAS)
# ==============================================================================
# Autor: Cientista de Dados Sênior
# Data de Criação: 01/04/2026
# Objetivo: Regressão Linear e Árvore de Decisão para Regressão com SIH
#
# ENTRADA: Arquivo CSV processado da Aula 3
#   - dados_processados_datasus/SIH_2024_06_DF_processado.csv
#
# SAÍDA: Modelos treinados, predições, métricas de avaliação
# DEPENDÊNCIAS: tidyverse, caret, rpart, glmnet
#
# OBS IMPORTANTE: AMBOS OS MODELOS USAM OS MESMOS DADOS (permitindo comparação direta)
# Objetivo: Prever valor de internação (variável contínua) baseado em outras features
#
# FLUXO DE ML (4 PASSOS OBRIGATÓRIOS):
#   1. Carregar dados processados + divisão treino/teste
#   2. Modelo simples (baseline/default)
#   3. Modelo customizado (tuning de hiperparâmetros)
#   4. Avaliação e comparação de desempenho
# ==============================================================================

# ==== CARREGAMENTO DE BIBLIOTECAS ====
library(tidyverse)      # Manipulação de dados
library(caret)          # Treinamento de modelos
library(rpart)          # Árvore de decisão
library(glmnet)         # Regressão Linear com penalização
# ==== CONFIGURAÇÃO DE DIRETÓRIOS ====
dir_dados_processados <- "dados_processados_datasus"

# ==== PASSO 1: CARREGAMENTO E PRÉ-PROCESSAMENTO DOS DADOS ====


# Carrega dados processados do SIH
sih <- read_csv(file.path(dir_dados_processados, "SIH_2024_06_DF_processado.csv"),
                show_col_types = FALSE)


# Identifica variável alvo
# Procura por coluna com "valor" no nome (valor de internação)
col_alvo_possivel <- colnames(sih)[grep("valor", colnames(sih), ignore.case = TRUE)]

# Se encontrou, extrai; senão, cria uma artificial para demonstração
if (length(col_alvo_possivel) > 0) {
  col_alvo <- col_alvo_possivel[1]
} else {
  # Cria uma target synthetic para demonstração (combinação linear de features + ruído)
  set.seed(123)
  sih$valor_internacao_predicao <- rowSums(sih[, 1:min(5, ncol(sih))]) * 100 + rnorm(nrow(sih), 0, 50)
  col_alvo <- "valor_internacao_predicao"
}

# Extrai variável alvo e features
y <- sih[[col_alvo]]
X <- sih %>% select(-all_of(col_alvo))


# Divisão Treino (70%) e Teste (30%)
set.seed(123)
indice_treino <- createDataPartition(y, p = 0.7, list = FALSE)

X_treino <- X[indice_treino, ]
X_teste <- X[-indice_treino, ]
y_treino <- y[indice_treino]
y_teste <- y[-indice_treino]


# ==== PASSO 2: MODELO SIMPLES (BASELINE) ====

# REGRESSÃO LINEAR SIMPLES

# Treina Regressão Linear (usando lm ou glm)
modelo_regressao_simples <- lm(y_treino ~ ., data = X_treino)

# Predições
pred_treino_linreg_simples <- predict(modelo_regressao_simples, X_treino)
pred_teste_linreg_simples <- predict(modelo_regressao_simples, X_teste)

# Métricas
rmse_treino_linreg_simples <- sqrt(mean((y_treino - pred_treino_linreg_simples)^2))
rmse_teste_linreg_simples <- sqrt(mean((y_teste - pred_teste_linreg_simples)^2))
r2_treino_linreg_simples <- 1 - (sum((y_treino - pred_treino_linreg_simples)^2) / sum((y_treino - mean(y_treino))^2))
r2_teste_linreg_simples <- 1 - (sum((y_teste - pred_teste_linreg_simples)^2) / sum((y_teste - mean(y_teste))^2))


# ÁRVORE DE REGRESSÃO SIMPLES

# Cria dataframe com y e X para rpart
dados_treino_tree <- cbind(y = y_treino, X_treino)

modelo_arvore_simples <- rpart(y ~ .,
                               data = dados_treino_tree,
                               method = "anova",  # "anova" para regressão
                               cp = 0.01,         # complexity parameter (default)
                               minSplit = 20,     # mínimo para split
                               minBucket = 7)     # mínimo por bucket

# Predições
pred_treino_arvore_simples <- predict(modelo_arvore_simples, X_treino)
pred_teste_arvore_simples <- predict(modelo_arvore_simples, X_teste)

# Métricas
rmse_treino_arvore_simples <- sqrt(mean((y_treino - pred_treino_arvore_simples)^2))
rmse_teste_arvore_simples <- sqrt(mean((y_teste - pred_teste_arvore_simples)^2))
r2_treino_arvore_simples <- 1 - (sum((y_treino - pred_treino_arvore_simples)^2) / sum((y_treino - mean(y_treino))^2))
r2_teste_arvore_simples <- 1 - (sum((y_teste - pred_teste_arvore_simples)^2) / sum((y_teste - mean(y_teste))^2))


# ==== PASSO 3: MODELO CUSTOMIZADO (TUNING) ====

# REGRESSÃO LINEAR CUSTOMIZADA (Elastic Net)

# Prepara dados em formato matriz para glmnet
X_treino_matrix <- as.matrix(X_treino)
X_teste_matrix <- as.matrix(X_teste)

# Valdiação cruzada para encontrar melhor lambda
cv_elastic <- cv.glmnet(X_treino_matrix,
                         y_treino,
                         alpha = 0.5,       # Elastic Net (alfa=0.5)
                         nfolds = 5,        # 5-fold cross-validation
                         type.measure = "mse")

lambda_otimo <- cv_elastic$lambda.1se  # Lambda com erro 1 desvio padrão acima do mínimo


# Treina modelo com lambda ótimo
modelo_regressao_custom <- glmnet(X_treino_matrix,
                                  y_treino,
                                  alpha = 0.5,
                                  lambda = lambda_otimo)

# Predições
pred_treino_linreg_custom <- predict(modelo_regressao_custom, X_treino_matrix, s = lambda_otimo)[,1]
pred_teste_linreg_custom <- predict(modelo_regressao_custom, X_teste_matrix, s = lambda_otimo)[,1]

# Métricas
rmse_treino_linreg_custom <- sqrt(mean((y_treino - pred_treino_linreg_custom)^2))
rmse_teste_linreg_custom <- sqrt(mean((y_teste - pred_teste_linreg_custom)^2))
r2_treino_linreg_custom <- 1 - (sum((y_treino - pred_treino_linreg_custom)^2) / sum((y_treino - mean(y_treino))^2))
r2_teste_linreg_custom <- 1 - (sum((y_teste - pred_teste_linreg_custom)^2) / sum((y_teste - mean(y_teste))^2))


# ÁRVORE DE REGRESSÃO CUSTOMIZADA

# Treina árvore maior e depois faz pruning
modelo_arvore_completo <- rpart(y ~ .,
                                data = dados_treino_tree,
                                method = "anova",
                                cp = 0.001,        # Muito pequeno para não podar
                                minSplit = 5,      # Mais permissivo
                                minBucket = 2)     # Mais permissivo

# Validação cruzada integrada
plotcp_arvore <- printcp(modelo_arvore_completo)

# Encontra CP com menor erro de cross-validation
melhor_cp_idx <- which.min(modelo_arvore_completo$cptable[, "xerror"])
melhor_cp <- modelo_arvore_completo$cptable[melhor_cp_idx, "CP"]


# Prune da árvore
modelo_arvore_custom <- prune(modelo_arvore_completo, cp = melhor_cp)

# Predições
pred_treino_arvore_custom <- predict(modelo_arvore_custom, X_treino)
pred_teste_arvore_custom <- predict(modelo_arvore_custom, X_teste)

# Métricas
rmse_treino_arvore_custom <- sqrt(mean((y_treino - pred_treino_arvore_custom)^2))
rmse_teste_arvore_custom <- sqrt(mean((y_teste - pred_teste_arvore_custom)^2))
r2_treino_arvore_custom <- 1 - (sum((y_treino - pred_treino_arvore_custom)^2) / sum((y_treino - mean(y_treino))^2))
r2_teste_arvore_custom <- 1 - (sum((y_teste - pred_teste_arvore_custom)^2) / sum((y_teste - mean(y_teste))^2))


# ==== PASSO 4: AVALIAÇÃO E COMPARAÇÃO ====

# ==== Comparação de Métricas ====

comparacao_regressao <- tibble(
  Modelo = c(
    "Regressão Linear Simples",
    "Regressão Linear Customizada",
    "Árvore Simples",
    "Árvore Customizada"
  ),
  RMSE_Treino = c(
    round(rmse_treino_linreg_simples, 2),
    round(rmse_treino_linreg_custom, 2),
    round(rmse_treino_arvore_simples, 2),
    round(rmse_treino_arvore_custom, 2)
  ),
  RMSE_Teste = c(
    round(rmse_teste_linreg_simples, 2),
    round(rmse_teste_linreg_custom, 2),
    round(rmse_teste_arvore_simples, 2),
    round(rmse_teste_arvore_custom, 2)
  ),
  R2_Treino = c(
    round(r2_treino_linreg_simples, 4),
    round(r2_treino_linreg_custom, 4),
    round(r2_treino_arvore_simples, 4),
    round(r2_treino_arvore_custom, 4)
  ),
  R2_Teste = c(
    round(r2_teste_linreg_simples, 4),
    round(r2_teste_linreg_custom, 4),
    round(r2_teste_arvore_simples, 4),
    round(r2_teste_arvore_custom, 4)
  )
)


# ==== Análise de Overfitting ====





# ==== Análise de Resíduos ====

residuos_linreg <- y_teste - pred_teste_linreg_custom
residuos_arvore <- y_teste - pred_teste_arvore_custom



# ==== COMPARAÇÃO FINAL ====

# Define melhor modelo baseado em RMSE de teste
modelos_rmse <- c(
  rmse_teste_linreg_simples,
  rmse_teste_linreg_custom,
  rmse_teste_arvore_simples,
  rmse_teste_arvore_custom
)
nomes_modelos <- c(
  "Regressão Linear Simples",
  "Regressão Linear Customizada",
  "Árvore Simples",
  "Árvore Customizada"
)
melhor_modelo_idx <- which.min(modelos_rmse)


if (r2_teste_linreg_custom > r2_teste_arvore_custom) {
} else {
}

if (rmse_teste_linreg_custom > rmse_treino_linreg_custom * 1.2 || rmse_teste_arvore_custom > rmse_treino_arvore_custom * 1.2) {
}

