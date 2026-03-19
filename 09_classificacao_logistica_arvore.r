# ==============================================================================
# AULA 9: MACHINE LEARNING – CLASSIFICAÇÃO (VARIÁVEIS BINÁRIAS)
# ==============================================================================
# Autor: Cientista de Dados Sênior
# Data de Criação: 02/04/2026
# Objetivo: Regressão Logística e Árvore de Classificação com SINAN
#
# ENTRADA: Arquivo CSV processado da Aula 3
#   - dados_processados_datasus/SINAN_2024_06_DF_processado.csv
#
# SAÍDA: Modelos treinados, probabilidades, métricas de avaliação
# DEPENDÊNCIAS: tidyverse, caret, rpart, glmnet, pROC
#
# Objetivo: Prever resultado binário (Curado=0 vs Óbito/Complicação=1)
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
library(glmnet)         # Regressão Logística
library(pROC)           # Análise ROC/AUC

# ==== CONFIGURAÇÃO DE DIRETÓRIOS ====
dir_dados_processados <- "dados_processados_datasus"
dir_dados_originais <- "dados_originais_datasus"

# ==== PASSO 1: CARREGAMENTO E PRÉ-PROCESSAMENTO DOS DADOS ====


# Carrega dados processados do SINAN
sinan <- read_csv(file.path(dir_dados_processados, "SINAN_2024_06_DF_processado.csv"),
                  show_col_types = FALSE)

# Também lê dados originais para extrair variável alvo
sinan_original <- read_csv(file.path(dir_dados_originais, "SINAN_2024_06_DF_selecionadas.csv"),
                           show_col_types = FALSE)


# Cria variável alvo binária (Curado=0 vs Desfecho Negativo=1)
# SINAN usa "evolu_cas": 1=Curado, 2+=Óbito/Sequela/etc.
col_evolucao <- intersect(c("resultado", "evolu_cas", "evolucao"), colnames(sinan_original))[1]
if (!is.na(col_evolucao)) {
  y_original <- sinan_original[[col_evolucao]]
  # Binariza: Curado/1=0, Óbito/Sequela/resto=1
  y <- ifelse(y_original %in% c("Curado", "1", 1), 0, 1)
  y <- as.factor(y)
  levels(y) <- c("Curado", "Desfecho_Negativo")

  props <- prop.table(table(y))
} else {
  set.seed(123)
  y <- as.factor(sample(0:1, nrow(sinan), replace = TRUE))
  levels(y) <- c("Curado", "Desfecho_Negativo")
}

X <- sinan


# Divisão Treino (70%) e Teste (30%)
set.seed(123)
indice_treino <- createDataPartition(y, p = 0.7, list = FALSE)

X_treino <- X[indice_treino, ]
X_teste <- X[-indice_treino, ]
y_treino <- y[indice_treino]
y_teste <- y[-indice_treino]


# ==== PASSO 2: MODELO SIMPLES (BASELINE) ====

# REGRESSÃO LOGÍSTICA SIMPLES

# Treina Regressão Logística
dados_treino_log <- cbind(y = y_treino, X_treino)
modelo_logistica_simples <- glm(y ~ .,
                                data = dados_treino_log,
                                family = binomial(link = "logit"))

# Predições (probabilidades)
prob_treino_logistica_simples <- predict(modelo_logistica_simples, X_treino, type = "response")
prob_teste_logistica_simples <- predict(modelo_logistica_simples, X_teste, type = "response")

# Converte para classes (threshold 0.5)
pred_treino_logistica_simples <- as.factor(ifelse(prob_treino_logistica_simples > 0.5, 1, 0))
levels(pred_treino_logistica_simples) <- c("Curado", "Desfecho_Negativo")
pred_teste_logistica_simples <- as.factor(ifelse(prob_teste_logistica_simples > 0.5, 1, 0))
levels(pred_teste_logistica_simples) <- c("Curado", "Desfecho_Negativo")

# Métricas
cm_treino_log_simples <- confusionMatrix(pred_treino_logistica_simples, y_treino)
cm_teste_log_simples <- confusionMatrix(pred_teste_logistica_simples, y_teste)
acc_treino_log_simples <- cm_treino_log_simples$overall["Accuracy"]
acc_teste_log_simples <- cm_teste_log_simples$overall["Accuracy"]

# AUC-ROC
auc_treino_log_simples <- auc(as.numeric(y_treino) - 1, prob_treino_logistica_simples)
auc_teste_log_simples <- auc(as.numeric(y_teste) - 1, prob_teste_logistica_simples)


# ÁRVORE DE CLASSIFICAÇÃO SIMPLES

modelo_arvore_class_simples <- rpart(y ~ .,
                                     data = cbind(y = y_treino, X_treino),
                                     method = "class",  # "class" para classificação
                                     cp = 0.01,
                                     minSplit = 20,
                                     minBucket = 7)

# Predições
pred_treino_arvore_class_simples <- predict(modelo_arvore_class_simples, X_treino, type = "class")
pred_teste_arvore_class_simples <- predict(modelo_arvore_class_simples, X_teste, type = "class")

# Probabilidades (para AUC)
prob_treino_arvore_class_simples <- predict(modelo_arvore_class_simples, X_treino, type = "prob")[, 2]
prob_teste_arvore_class_simples <- predict(modelo_arvore_class_simples, X_teste, type = "prob")[, 2]

# Métricas
cm_treino_arvore_simples <- confusionMatrix(pred_treino_arvore_class_simples, y_treino)
cm_teste_arvore_simples <- confusionMatrix(pred_teste_arvore_class_simples, y_teste)
acc_treino_arvore_simples <- cm_treino_arvore_simples$overall["Accuracy"]
acc_teste_arvore_simples <- cm_teste_arvore_simples$overall["Accuracy"]

auc_treino_arvore_simples <- auc(as.numeric(y_treino) - 1, prob_treino_arvore_class_simples)
auc_teste_arvore_simples <- auc(as.numeric(y_teste) - 1, prob_teste_arvore_class_simples)


# ==== PASSO 3: MODELO CUSTOMIZADO (TUNING) ====

# REGRESSÃO LOGÍSTICA CUSTOMIZADA (com regularização)

# Prepara dados em formato matriz
X_treino_matrix <- as.matrix(X_treino)
X_teste_matrix <- as.matrix(X_teste)
y_treino_numeric <- as.numeric(y_treino) - 1

# Validação cruzada para encontrar melhor lambda
cv_logistica <- cv.glmnet(X_treino_matrix,
                           y_treino_numeric,
                           alpha = 0.5,
                           family = "binomial",
                           nfolds = 5,
                           type.measure = "auc")

lambda_otimo_log <- cv_logistica$lambda.1se


# Treina modelo com lambda ótimo
modelo_logistica_custom <- glmnet(X_treino_matrix,
                                  y_treino_numeric,
                                  alpha = 0.5,
                                  family = "binomial",
                                  lambda = lambda_otimo_log)

# Predições
prob_treino_logistica_custom <- predict(modelo_logistica_custom, X_treino_matrix,
                                        s = lambda_otimo_log, type = "response")[,1]
prob_teste_logistica_custom <- predict(modelo_logistica_custom, X_teste_matrix,
                                       s = lambda_otimo_log, type = "response")[,1]

# Converte para classes
pred_treino_logistica_custom <- as.factor(ifelse(prob_treino_logistica_custom > 0.5, 1, 0))
levels(pred_treino_logistica_custom) <- c("Curado", "Desfecho_Negativo")
pred_teste_logistica_custom <- as.factor(ifelse(prob_teste_logistica_custom > 0.5, 1, 0))
levels(pred_teste_logistica_custom) <- c("Curado", "Desfecho_Negativo")

# Métricas
cm_treino_log_custom <- confusionMatrix(pred_treino_logistica_custom, y_treino)
cm_teste_log_custom <- confusionMatrix(pred_teste_logistica_custom, y_teste)
acc_treino_log_custom <- cm_treino_log_custom$overall["Accuracy"]
acc_teste_log_custom <- cm_teste_log_custom$overall["Accuracy"]

auc_treino_log_custom <- auc(as.numeric(y_treino) - 1, prob_treino_logistica_custom)
auc_teste_log_custom <- auc(as.numeric(y_teste) - 1, prob_teste_logistica_custom)


# ÁRVORE DE CLASSIFICAÇÃO CUSTOMIZADA

# Treina árvore maior
modelo_arvore_completo_class <- rpart(y ~ .,
                                      data = cbind(y = y_treino, X_treino),
                                      method = "class",
                                      cp = 0.001,
                                      minSplit = 5,
                                      minBucket = 2)

# Encontra CP ótimo
melhor_cp_idx_class <- which.min(modelo_arvore_completo_class$cptable[, "xerror"])
melhor_cp_class <- modelo_arvore_completo_class$cptable[melhor_cp_idx_class, "CP"]


# Prune da árvore
modelo_arvore_class_custom <- prune(modelo_arvore_completo_class, cp = melhor_cp_class)

# Predições
pred_treino_arvore_class_custom <- predict(modelo_arvore_class_custom, X_treino, type = "class")
pred_teste_arvore_class_custom <- predict(modelo_arvore_class_custom, X_teste, type = "class")

prob_treino_arvore_class_custom <- predict(modelo_arvore_class_custom, X_treino, type = "prob")[, 2]
prob_teste_arvore_class_custom <- predict(modelo_arvore_class_custom, X_teste, type = "prob")[, 2]

# Métricas
cm_treino_arvore_custom <- confusionMatrix(pred_treino_arvore_class_custom, y_treino)
cm_teste_arvore_custom <- confusionMatrix(pred_teste_arvore_class_custom, y_teste)
acc_treino_arvore_custom <- cm_treino_arvore_custom$overall["Accuracy"]
acc_teste_arvore_custom <- cm_teste_arvore_custom$overall["Accuracy"]

auc_treino_arvore_custom <- auc(as.numeric(y_treino) - 1, prob_treino_arvore_class_custom)
auc_teste_arvore_custom <- auc(as.numeric(y_teste) - 1, prob_teste_arvore_class_custom)


# ==== PASSO 4: AVALIAÇÃO E COMPARAÇÃO ====

# ==== Comparação Geral ====

comparacao_classificacao <- tibble(
  Modelo = c(
    "Logística Simples",
    "Logística Customizada",
    "Árvore Simples",
    "Árvore Customizada"
  ),
  Acurácia_Treino = c(
    round(acc_treino_log_simples, 4),
    round(acc_treino_log_custom, 4),
    round(acc_treino_arvore_simples, 4),
    round(acc_treino_arvore_custom, 4)
  ),
  Acurácia_Teste = c(
    round(acc_teste_log_simples, 4),
    round(acc_teste_log_custom, 4),
    round(acc_teste_arvore_simples, 4),
    round(acc_teste_arvore_custom, 4)
  ),
  AUC_Treino = c(
    round(auc_treino_log_simples, 4),
    round(auc_treino_log_custom, 4),
    round(auc_treino_arvore_simples, 4),
    round(auc_treino_arvore_custom, 4)
  ),
  AUC_Teste = c(
    round(auc_teste_log_simples, 4),
    round(auc_teste_log_custom, 4),
    round(auc_teste_arvore_simples, 4),
    round(auc_teste_arvore_custom, 4)
  )
)


# ==== Matriz de Confusão Melhor Modelo ====

# Identifica melhor modelo por AUC teste
auc_valores <- c(
  auc_teste_log_simples,
  auc_teste_log_custom,
  auc_teste_arvore_simples,
  auc_teste_arvore_custom
)
melhor_modelo_idx <- which.max(auc_valores)
nomes_modelos_class <- c(
  "Logística Simples",
  "Logística Customizada",
  "Árvore Simples",
  "Árvore Customizada"
)


# Exibe matriz confusão do melhor
if (melhor_modelo_idx == 1) {
} else if (melhor_modelo_idx == 2) {
} else if (melhor_modelo_idx == 3) {
} else {
}

# ==== Métricas Detalhadas ====

if (melhor_modelo_idx == 1) {
  metricas <- cm_teste_log_simples$byClass
} else if (melhor_modelo_idx == 2) {
  metricas <- cm_teste_log_custom$byClass
} else if (melhor_modelo_idx == 3) {
  metricas <- cm_teste_arvore_simples$byClass
} else {
  metricas <- cm_teste_arvore_custom$byClass
}


# ==== Análise de Overfitting ====





# ==== RESUMO FINAL ====


if (melhor_modelo_idx %in% c(1, 2)) {
} else {
}

if (max((comparacao_classificacao$Acurácia_Treino - comparacao_classificacao$Acurácia_Teste)) > 0.15) {
}

