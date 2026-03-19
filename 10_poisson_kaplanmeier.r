# ==============================================================================
# AULA 10: MACHINE LEARNING – MODELOS ESPECIAIS
# ==============================================================================
# Objetivo: Regressão de Poisson (SIM) e Kaplan-Meier (glifage SIA 2024)
#
# ENTRADA:
#   - dados_processados_datasus/SIM_2024_06_DF_processado.csv (Poisson)
#   - dados_originais_datasus/SIA_glifage_2024_DF.csv (Kaplan-Meier - OBRIGATÓRIO)
#
# Regra: Poisson usa SIM; Kaplan-Meier usa EXCLUSIVAMENTE dados glifage (Aula 1)
# ==============================================================================

# ==== CARREGAMENTO DE BIBLIOTECAS ====
library(tidyverse)      # Manipulação de dados
library(caret)          # Treinamento de modelos
library(glmnet)         # Regressão generalizada (Poisson)
library(survival)       # Análise de sobrevida
library(survminer)      # Visualização de sobrevida

# ==== CONFIGURAÇÃO DE DIRETÓRIOS ====
dir_dados_processados <- "dados_processados_datasus"
dir_dados_originais <- "dados_originais_datasus"

# ==== PASSO 1: CARREGAMENTO E PRÉ-PROCESSAMENTO DOS DADOS ====


# Carrega dados processados do SIM (para Regressão de Poisson)
sim_processado <- read_csv(file.path(dir_dados_processados, "SIM_2024_06_DF_processado.csv"),
                           show_col_types = FALSE)

# Carrega dados originais do SIM para extrair variáveis de contagem
sim_original <- read_csv(file.path(dir_dados_originais, "SIM_2024_06_DF_selecionadas.csv"),
                         show_col_types = FALSE)


# ========= PARTE 1: REGRESSÃO DE POISSON =========


# Cria variável de contagem agregada
# Agrupa por características e conta óbitos (usa colunas reais do SIM/DATASUS)
col_sexo <- intersect(c("sexo", "SEXO"), colnames(sim_original))[1]
col_causa <- grep("causa|causabas", names(sim_original), ignore.case = TRUE, value = TRUE)[1]
if (!is.na(col_sexo) && !is.na(col_causa)) {
  sim_contagem <- sim_original %>%
    mutate(
      sexo_cat = ifelse(is.na(.[[col_sexo]]), "Desconhecido", as.character(.[[col_sexo]])),
      causa_cat = ifelse(is.na(.[[col_causa]]), "Outras", as.character(.[[col_causa]]))
    ) %>%
    group_by(sexo_cat, causa_cat) %>%
    summarise(
      n_obitos = n(),
      .groups = "drop"
    ) %>%
    filter(n_obitos > 0)

} else {
  # Se não houver informações de sexo, cria dados sintéticos
  set.seed(123)
  sim_contagem <- tibble(
    sexo_cat = rep(c("M", "F"), 15),
    causa_cat = rep(c("Cardio", "Respiratória", "Neoplasia", "Diabetes", "Acidente"), 6),
    n_obitos = rpois(30, lambda = 15)
  )
}

# Prepara features para regressão Poisson
X_poisson <- sim_contagem %>% select(-n_obitos)
y_poisson <- sim_contagem$n_obitos

# Codifica variáveis categóricas
X_poisson_encoded <- data.frame(
  sexo_M = ifelse(X_poisson$sexo_cat == "M", 1, 0),
  causa_Respiratoria = ifelse(X_poisson$causa_cat == "Respiratória", 1, 0),
  causa_Neoplasia = ifelse(X_poisson$causa_cat == "Neoplasia", 1, 0),
  causa_Diabetes = ifelse(X_poisson$causa_cat == "Diabetes", 1, 0),
  causa_Acidente = ifelse(X_poisson$causa_cat == "Acidente", 1, 0)
)


# ==== PASSO 2: MODELO POISSON SIMPLES ====


# Treina GLM Poisson
dados_poisson <- cbind(y = y_poisson, X_poisson_encoded)
modelo_poisson_simples <- glm(y ~ .,
                              data = dados_poisson,
                              family = poisson(link = "log"))

# Predições
pred_poisson_simples <- predict(modelo_poisson_simples, X_poisson_encoded, type = "response")

# Métricas (Deviance, AIC)
deviance_poisson_simples <- modelo_poisson_simples$deviance
aic_poisson_simples <- AIC(modelo_poisson_simples)
rmse_poisson_simples <- sqrt(mean((y_poisson - pred_poisson_simples)^2))


# ==== PASSO 3: MODELO POISSON CUSTOMIZADO ====


# Converte para matriz
X_poisson_matrix <- as.matrix(X_poisson_encoded)

# Validação cruzada para Poisson
cv_poisson <- cv.glmnet(X_poisson_matrix,
                        y_poisson,
                        alpha = 0.7,
                        family = "poisson",
                        nfolds = 5)

lambda_otimo_poisson <- cv_poisson$lambda.1se


# Treina modelo Poisson com regularização
modelo_poisson_custom <- glmnet(X_poisson_matrix,
                                y_poisson,
                                alpha = 0.7,
                                family = "poisson",
                                lambda = lambda_otimo_poisson)

# Predições
pred_poisson_custom <- predict(modelo_poisson_custom, X_poisson_matrix,
                               s = lambda_otimo_poisson, type = "response")[,1]

# Métricas
rmse_poisson_custom <- sqrt(mean((y_poisson - pred_poisson_custom)^2))


# ==== COMPARAÇÃO POISSON ====

if (rmse_poisson_custom < rmse_poisson_simples) {
} else {
}

# ========= PARTE 2: ANÁLISE DE SOBREVIDA KAPLAN-MEIER =========




# CARREGA DADOS GLIFAGE (SIA-SUS 2024) - OBRIGATÓRIO para Kaplan-Meier
glifage <- read_csv(file.path(dir_dados_originais, "SIA_glifage_2024_DF.csv"),
                   show_col_types = FALSE)

# Constrói variáveis de sobrevida a partir dos dados glifage
# tempo = dias desde início de 2024 até a data do procedimento (ou mês)
# evento = proxy para descontinuação: 1 se poucas dispensações, 0 se manteve tratamento
col_data <- grep("pa_data|data|pa_mes|mes", names(glifage), ignore.case = TRUE, value = TRUE)[1]
col_sexo_gl <- grep("sexo|pa_sexo", names(glifage), ignore.case = TRUE, value = TRUE)[1]
col_idade <- grep("idade|pa_idade|idade_anos", names(glifage), ignore.case = TRUE, value = TRUE)[1]
col_qt <- grep("pa_qtprod|qtprod|quantidade", names(glifage), ignore.case = TRUE, value = TRUE)[1]

set.seed(123)
n_glif <- min(nrow(glifage), 1500)
glif_sub <- glifage %>% slice_sample(n = n_glif, replace = FALSE)

if (!is.na(col_data)) {
  datas <- tryCatch(as.Date(glif_sub[[col_data]], format = "%Y-%m-%d"),
                    error = function(e) as.Date(paste0("2024-", sprintf("%02d", pmin(12, pmax(1, as.numeric(glif_sub[[col_data]])))), "-15")))
  tempo_km <- as.numeric(difftime(datas, as.Date("2024-01-01"), units = "days"))
  tempo_km <- pmax(1, pmin(365, tempo_km))  # Limita 1-365 dias
} else {
  tempo_km <- sample(1:365, n_glif, replace = TRUE)
}

# evento: 1 = descontinuação (1 dispensação), 0 = censurado/continuou
if (!is.na(col_qt)) {
  evento_km <- as.integer(glif_sub[[col_qt]] <= 1)
} else {
  evento_km <- rbinom(n_glif, 1, 0.25)
}

# Sexo para estratificação
if (!is.na(col_sexo_gl)) {
  sexo_km <- as.character(glif_sub[[col_sexo_gl]])
  sexo_km <- ifelse(sexo_km %in% c("1", "M", " masculino"), "M", "F")
} else {
  sexo_km <- sample(c("M", "F"), n_glif, replace = TRUE)
}

dados_sobrevida <- tibble(
  tempo = tempo_km,
  evento = evento_km,
  sexo = sexo_km
)


# Cria objeto Surv (survfit input)
survfit_simples <- survfit(Surv(tempo, evento) ~ 1,
                           data = dados_sobrevida)

# Sumário


# ==== PASSO 3: MODELO KAPLAN-MEIER CUSTOMIZADO ====


# Stratified Kaplan-Meier
survfit_custom <- survfit(Surv(tempo, evento) ~ sexo,
                          data = dados_sobrevida)

# Sumário por grupo

# Teste Log-Rank para comparação entre grupos
logrank_test <- survdiff(Surv(tempo, evento) ~ sexo,
                         data = dados_sobrevida)


if (1 - pchisq(logrank_test$chisq, 1) < 0.05) {
} else {
}

# ==== PASSO 4: AVALIAÇÃO E COMPARAÇÃO ====

# Métricas de Sobrevida

# Funão para extrair survival probabilities em tempos específicos
extract_survival_probs <- function(survfit_obj, times) {
  idx <- findInterval(times, survfit_obj$time)
  probs <- c()
  for (i in seq_along(times)) {
    if (idx[i] == 0) {
      probs[i] <- 1.0  # No evento ainda
    } else {
      probs[i] <- survfit_obj$surv[idx[i]]
    }
  }
  return(probs)
}

# Tempos específicos (dias)
tempos_interesse <- c(30, 60, 90, 180)

probs_simples <- extract_survival_probs(survfit_simples, tempos_interesse)
for (i in seq_along(tempos_interesse)) {
}

if (length(survfit_custom$strata) > 1) {
  # Extrai probabilidades para cada estrato

  # Dados para sexo M
  dados_M <- dados_sobrevida %>% filter(sexo == "M")
  survfit_M <- survfit(Surv(tempo, evento) ~ 1, data = dados_M)
  probs_M <- extract_survival_probs(survfit_M, tempos_interesse)

  # Dados para sexo F
  dados_F <- dados_sobrevida %>% filter(sexo == "F")
  survfit_F <- survfit(Surv(tempo, evento) ~ 1, data = dados_F)
  probs_F <- extract_survival_probs(survfit_F, tempos_interesse)

  for (i in seq_along(tempos_interesse)) {
  }

  for (i in seq_along(tempos_interesse)) {
  }

  # Diferenças
  for (i in seq_along(tempos_interesse)) {
    diff <- (probs_M[i] - probs_F[i]) * 100
  }
}

# ==== RESUMO FINAL ====






