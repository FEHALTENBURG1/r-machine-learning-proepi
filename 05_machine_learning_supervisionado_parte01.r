# ==============================================================================
# AULA 5 - MACHINE LEARNING SUPERVISIONADO PARTE 1 - BRASIL
# Curso: Analise de Dados e Machine Learning com R - ProEpi 2026
# ------------------------------------------------------------------------------
# Objetivo geral da aula:
# - Construir modelos supervisionados para prever VAL_TOT no SIH.
# - Comparar Regressao Linear (com selecao stepwise) e Arvore de Decisao.
# - Interpretar o modelo por importancia de variaveis e SHAP.
# ==============================================================================

# ------------------------------------------------------------------------------
# SESSAO 1 - TODOS OS IMPORTS DE PACOTES E DADOS
# Objetivo da sessao:
# - Carregar pacotes de manipulacao, modelagem, avaliacao e explicabilidade.
# - Importar a base tratada do SIH gerada na aula 2.
# - Preparar variaveis para modelagem preditiva supervisionada.
# ------------------------------------------------------------------------------
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("rsample", quietly = TRUE)) install.packages("rsample")
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")
if (!requireNamespace("rpart", quietly = TRUE)) install.packages("rpart")
if (!requireNamespace("rpart.plot", quietly = TRUE)) install.packages("rpart.plot")
if (!requireNamespace("fastshap", quietly = TRUE)) install.packages("fastshap")
if (!requireNamespace("shapviz", quietly = TRUE)) install.packages("shapviz")

library(tidyverse)   # Manipulacao, transformacao e visualizacao de dados com sintaxe tidy.
library(lubridate)   # Funcoes para trabalhar com datas (idade, diferencas e parsing).
library(rsample)     # Divisao treino/teste com reprodutibilidade e boas praticas.
library(MASS)        # Funcoes estatisticas classicas, incluindo Box-Cox.
library(rpart)       # Treinamento de arvore de decisao para regressao.
library(rpart.plot)  # Visualizacao didatica da arvore de decisao.
library(fastshap)    # Calculo de SHAP values por aproximacao via simulacao.
library(shapviz)     # Graficos de interpretabilidade com SHAP (beeswarm e barras).

raiz_dados_processados <- "dados_processados_datasus"
load(file.path(raiz_dados_processados, "sih_modelagem.RData"))

# Define pasta para salvar modelos, metricas e graficos da aula 5.
pasta_saida_modelos <- file.path("modelos_machine_learning", "05_ml_supervisionado_parte01")
dir.create(pasta_saida_modelos, recursive = TRUE, showWarnings = FALSE)

# Funcao auxiliar para salvar graficos com padrao unico de qualidade.
salvar_grafico <- function(grafico, nome_arquivo, largura = 10, altura = 6) {
  ggplot2::ggsave(
    filename = file.path(pasta_saida_modelos, nome_arquivo),
    plot = grafico,
    width = largura,
    height = altura,
    dpi = 300
  )
}

dim(df_sih_modelagem)
glimpse(df_sih_modelagem)

# ------------------------------------------------------------------------------
# SESSAO 2 - PREPARACAO DOS DADOS PARA MODELAGEM
# Objetivo da sessao:
# - Criar a variavel IDADE com base em NASC e DT_INTER (ou data atual).
# - Definir target e retirar variaveis de identificacao da etapa de modelagem.
# - Manter apenas colunas necessarias para os modelos supervisionados.
# ------------------------------------------------------------------------------

# Define variaveis que serao ignoradas na modelagem (somente identificacao/contexto).
variaveis_id <- c("ANO_CMPT", "MES_CMPT", "DT_INTER", "DT_SAIDA", "DIAG_PRINC")
# Define variavel alvo (target) continua para regressao.
target_modelagem <- "VAL_TOT"

# Cria idade usando DT_INTER quando disponivel; caso contrario, usa data atual.
df_sih_modelagem <- df_sih_modelagem %>%
  mutate(
    NASC = as.Date(NASC),
    DT_INTER = as.Date(DT_INTER),
    data_referencia_idade = if_else(!is.na(DT_INTER), DT_INTER, as.Date(Sys.Date())),
    IDADE = as.integer(floor(as.numeric(data_referencia_idade - NASC) / 365.25))
  ) %>%
  dplyr::select(-data_referencia_idade)

# Gera base de trabalho removendo IDs e mantendo colunas de interesse.
df_sih_supervisionado <- df_sih_modelagem %>%
  dplyr::select(-any_of(variaveis_id)) %>%
  mutate(
    SEXO = as.factor(SEXO),
    MORTE = as.factor(MORTE)
  )

# Remove linhas com valores ausentes nas variaveis fundamentais para os modelos.
df_sih_supervisionado <- df_sih_supervisionado %>%
  filter(
    !is.na(VAL_TOT),
    !is.na(QT_DIARIAS),
    !is.na(IDADE),
    !is.na(VAL_SH),
    !is.na(VAL_SP),
    !is.na(SEXO),
    !is.na(MORTE)
  )

dim(df_sih_supervisionado)
glimpse(df_sih_supervisionado)

# ------------------------------------------------------------------------------
# SESSAO 3 - DIVISAO TREINO E TESTE
# Objetivo da sessao:
# - Separar os dados em treino e teste com reprodutibilidade.
# - Garantir boa pratica para avaliar desempenho fora da amostra.
# ------------------------------------------------------------------------------
set.seed(2026)
divisao_treino_teste <- initial_split(df_sih_supervisionado, prop = 0.80, strata = VAL_TOT)
dados_treino <- training(divisao_treino_teste)
dados_teste <- testing(divisao_treino_teste)

dim(dados_treino)
dim(dados_teste)

# ------------------------------------------------------------------------------
# SESSAO 4 - MODELO 1: REGRESSAO LINEAR COM SELECAO STEPWISE
# Objetivo da sessao:
# - Explorar distribuicao do target no treino e testar transformacoes.
# - Selecionar a primeira transformacao que aproximar normalidade (Shapiro-Wilk).
# - Ajustar regressao linear inicial e aplicar selecao stepwise por AIC.
# - Avaliar residuos do modelo final com QQ-plot e teste KS.
# ------------------------------------------------------------------------------

# Preditores iniciais definidos para regressao linear.
preditores_regressao <- c("QT_DIARIAS", "IDADE", "VAL_SH", "VAL_SP")

# Histograma do target original no treino para avaliar assimetria e caudas.
grafico_hist_target_original_treino <- ggplot(dados_treino, aes(x = VAL_TOT)) +
  geom_histogram(aes(y = after_stat(count)), bins = 30, fill = "#2C7FB8", color = "white") +
  theme_minimal(base_size = 11) +
  labs(
    title = "SIH (Treino) - Histograma de VAL_TOT (original)",
    x = "VAL_TOT",
    y = "Frequencia"
  )
print(grafico_hist_target_original_treino)
salvar_grafico(grafico_hist_target_original_treino, "01_hist_target_original_treino.png")

# Boxplot do target no treino para identificar dispersao e possiveis outliers.
grafico_box_target_original_treino <- ggplot(dados_treino, aes(y = VAL_TOT)) +
  geom_boxplot(fill = "#41AB5D", alpha = 0.8) +
  coord_flip() +
  theme_minimal(base_size = 11) +
  labs(
    title = "SIH (Treino) - Boxplot de VAL_TOT",
    x = "VAL_TOT",
    y = ""
  )
print(grafico_box_target_original_treino)
salvar_grafico(grafico_box_target_original_treino, "02_box_target_original_treino.png")

# Funcao auxiliar para aplicar Shapiro-Wilk com regra de amostragem para N > 5000.
calcular_shapiro <- function(vetor_numerico) {
  vetor_numerico <- vetor_numerico[is.finite(vetor_numerico)]
  vetor_numerico <- vetor_numerico[!is.na(vetor_numerico)]
  if (length(vetor_numerico) < 3) return(list(p_value = NA_real_, n = length(vetor_numerico)))
  if (length(vetor_numerico) > 5000) {
    set.seed(2026)
    vetor_numerico <- sample(vetor_numerico, size = 5000)
  }
  teste <- shapiro.test(vetor_numerico)
  list(p_value = as.numeric(teste$p.value), n = length(vetor_numerico))
}

# Define funcoes de transformacao e inversa para uso em treino/teste.
transformacao_log <- function(y, parametros) log(y + parametros$shift_log)
inversa_log <- function(y_t, parametros) pmax(exp(y_t) - parametros$shift_log, 0)

transformacao_exp <- function(y, parametros) exp((y - parametros$media_exp) / parametros$dp_exp)
inversa_exp <- function(y_t, parametros) (log(pmax(y_t, .Machine$double.eps)) * parametros$dp_exp) + parametros$media_exp

transformacao_boxcox <- function(y, parametros) {
  y_pos <- y + parametros$shift_box
  if (abs(parametros$lambda_box) < 1e-8) log(y_pos) else (y_pos^parametros$lambda_box - 1) / parametros$lambda_box
}
inversa_boxcox <- function(y_t, parametros) {
  y_pos <- if (abs(parametros$lambda_box) < 1e-8) {
    exp(y_t)
  } else {
    (parametros$lambda_box * y_t + 1)^(1 / parametros$lambda_box)
  }
  pmax(y_pos - parametros$shift_box, 0)
}

# Parametros das transformacoes estimados no treino.
target_treino_original <- dados_treino$VAL_TOT
parametros_transformacao <- list(
  shift_log = ifelse(min(target_treino_original, na.rm = TRUE) <= 0, abs(min(target_treino_original, na.rm = TRUE)) + 1, 0),
  media_exp = mean(target_treino_original, na.rm = TRUE),
  dp_exp = sd(target_treino_original, na.rm = TRUE),
  shift_box = ifelse(min(target_treino_original, na.rm = TRUE) <= 0, abs(min(target_treino_original, na.rm = TRUE)) + 1, 0),
  lambda_box = NA_real_
)
if (!is.finite(parametros_transformacao$dp_exp) || parametros_transformacao$dp_exp == 0) parametros_transformacao$dp_exp <- 1

# Estima lambda da Box-Cox no treino (sobre target positiva).
target_treino_pos_box <- target_treino_original + parametros_transformacao$shift_box
ajuste_boxcox <- MASS::boxcox(lm(target_treino_pos_box ~ 1), plotit = FALSE, lambda = seq(-2, 2, by = 0.1))
parametros_transformacao$lambda_box <- ajuste_boxcox$x[which.max(ajuste_boxcox$y)]

# Gera targets transformadas no treino.
target_treino_log <- transformacao_log(target_treino_original, parametros_transformacao)
target_treino_exp <- transformacao_exp(target_treino_original, parametros_transformacao)
target_treino_box <- transformacao_boxcox(target_treino_original, parametros_transformacao)

# Histograma das transformacoes solicitadas.
grafico_hist_target_log <- tibble(valor = target_treino_log) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(aes(y = after_stat(count)), bins = 30, fill = "#41AB5D", color = "white") +
  theme_minimal(base_size = 11) +
  labs(title = "SIH (Treino) - Histograma da transformacao logaritmica", x = "log(VAL_TOT + c)", y = "Frequencia")
print(grafico_hist_target_log)
salvar_grafico(grafico_hist_target_log, "03_hist_target_transformacao_log.png")

grafico_hist_target_exp <- tibble(valor = target_treino_exp) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(aes(y = after_stat(count)), bins = 30, fill = "#7570B3", color = "white") +
  theme_minimal(base_size = 11) +
  labs(title = "SIH (Treino) - Histograma da transformacao exponencial", x = "exp(z-score de VAL_TOT)", y = "Frequencia")
print(grafico_hist_target_exp)
salvar_grafico(grafico_hist_target_exp, "04_hist_target_transformacao_exponencial.png")

grafico_hist_target_boxcox <- tibble(valor = target_treino_box) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(aes(y = after_stat(count)), bins = 30, fill = "#D95F02", color = "white") +
  theme_minimal(base_size = 11) +
  labs(title = "SIH (Treino) - Histograma da transformacao Box-Cox", x = "Box-Cox(VAL_TOT + c)", y = "Frequencia")
print(grafico_hist_target_boxcox)
salvar_grafico(grafico_hist_target_boxcox, "05_hist_target_transformacao_boxcox.png")

# Calcula e imprime Shapiro para cada opcao (original + transformacoes).
resultado_shapiro <- tibble(
  transformacao = c("original", "logaritmica", "exponencial", "boxcox"),
  p_value = c(
    calcular_shapiro(target_treino_original)$p_value,
    calcular_shapiro(target_treino_log)$p_value,
    calcular_shapiro(target_treino_exp)$p_value,
    calcular_shapiro(target_treino_box)$p_value
  )
)
cat("\nShapiro-Wilk para target no treino (p-value)\n")
print(resultado_shapiro)

# Seleciona a primeira transformacao com p-value > 0.05 (ordem: log, exp, boxcox).
transformacao_escolhida <- "original"
if (!is.na(resultado_shapiro$p_value[resultado_shapiro$transformacao == "logaritmica"]) &&
    resultado_shapiro$p_value[resultado_shapiro$transformacao == "logaritmica"] > 0.05) {
  transformacao_escolhida <- "logaritmica"
} else if (!is.na(resultado_shapiro$p_value[resultado_shapiro$transformacao == "exponencial"]) &&
           resultado_shapiro$p_value[resultado_shapiro$transformacao == "exponencial"] > 0.05) {
  transformacao_escolhida <- "exponencial"
} else if (!is.na(resultado_shapiro$p_value[resultado_shapiro$transformacao == "boxcox"]) &&
           resultado_shapiro$p_value[resultado_shapiro$transformacao == "boxcox"] > 0.05) {
  transformacao_escolhida <- "boxcox"
}
cat("\nTransformacao selecionada para modelagem:", transformacao_escolhida, "\n")

# Aplica a transformacao escolhida em treino e teste para a variavel alvo do modelo.
aplicar_transformacao <- function(y, tipo, parametros) {
  if (tipo == "logaritmica") return(transformacao_log(y, parametros))
  if (tipo == "exponencial") return(transformacao_exp(y, parametros))
  if (tipo == "boxcox") return(transformacao_boxcox(y, parametros))
  y
}
destransformar_predicao <- function(y_t, tipo, parametros) {
  if (tipo == "logaritmica") return(inversa_log(y_t, parametros))
  if (tipo == "exponencial") return(inversa_exp(y_t, parametros))
  if (tipo == "boxcox") return(inversa_boxcox(y_t, parametros))
  y_t
}

dados_treino <- dados_treino %>% mutate(VAL_TOT_TRANSF = aplicar_transformacao(VAL_TOT, transformacao_escolhida, parametros_transformacao))
dados_teste <- dados_teste %>% mutate(VAL_TOT_TRANSF = aplicar_transformacao(VAL_TOT, transformacao_escolhida, parametros_transformacao))

# Ajusta modelo linear inicial com todos os preditores definidos na target transformada.
target_modelagem_transformada <- "VAL_TOT_TRANSF"
formula_lm_inicial <- as.formula(
  paste(target_modelagem_transformada, "~", paste(preditores_regressao, collapse = " + "))
)

modelo_lm_inicial <- lm(formula_lm_inicial, data = dados_treino)
summary(modelo_lm_inicial)

# Selecao stepwise por AIC para encontrar modelo mais parcimonioso.
modelo_lm_step <- step(modelo_lm_inicial, direction = "both", trace = 0)
summary(modelo_lm_step)

# Salva os preditores selecionados no stepwise (base para arvore na proxima sessao).
preditores_stepwise <- attr(terms(modelo_lm_step), "term.labels")
preditores_stepwise

# Analise visual de residuos: QQ-plot para comparar com normal teorica.
residuos_lm_treino <- residuals(modelo_lm_step)
grafico_qq_residuos_lm <- ggplot(tibble(residuos = residuos_lm_treino), aes(sample = residuos)) +
  stat_qq(color = "#2C7FB8", alpha = 0.65) +
  stat_qq_line(color = "#B2182B", linewidth = 0.8) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Regressao Linear (Stepwise) - QQ-plot dos residuos (treino)",
    x = "Quantis teoricos",
    y = "Quantis observados"
  )
print(grafico_qq_residuos_lm)
salvar_grafico(grafico_qq_residuos_lm, "06_qqplot_residuos_regressao_linear_stepwise.png")

# KS para normalidade dos residuos: padroniza para media 0 e desvio 1.
residuos_lm_padronizados <- as.numeric(scale(residuos_lm_treino))
teste_ks_residuos_lm <- ks.test(residuos_lm_padronizados, "pnorm")
cat("\nTeste KS para normalidade dos residuos da regressao linear (stepwise)\n")
print(teste_ks_residuos_lm)

# ------------------------------------------------------------------------------
# SESSAO 5 - MODELO 2: ARVORE DE DECISAO PARA REGRESSAO
# Objetivo da sessao:
# - Construir arvore com variaveis do stepwise + SEXO e MORTE.
# - Plotar arvore de forma didatica para leitura das regras.
# ------------------------------------------------------------------------------

# Define preditores da arvore: selecionados pelo stepwise e acrescidos de SEXO/MORTE.
preditores_arvore <- union(preditores_stepwise, c("SEXO", "MORTE"))
preditores_arvore <- preditores_arvore[preditores_arvore %in% names(dados_treino)]
preditores_arvore

# Formula final da arvore de decisao para regressao.
formula_arvore <- as.formula(
  paste(target_modelagem_transformada, "~", paste(preditores_arvore, collapse = " + "))
)

# Ajusta arvore de regressao com criterio ANOVA.
modelo_arvore <- rpart(
  formula_arvore,
  data = dados_treino,
  method = "anova",
  control = rpart.control(cp = 0.001, minsplit = 20, maxdepth = 6)
)

# Plota arvore para facilitar interpretacao das regras de decisao.
png(
  filename = file.path(pasta_saida_modelos, "07_arvore_regressao_estrutura.png"),
  width = 1800,
  height = 1200,
  res = 180
)
rpart.plot(
  modelo_arvore,
  type = 2,
  extra = 101,
  fallen.leaves = TRUE,
  tweak = 1.1,
  main = "Arvore de Decisao para Regressao - Predicao de VAL_TOT"
)
dev.off()

# ------------------------------------------------------------------------------
# SESSAO 6 - COMPARACAO DE MODELOS E AVALIACAO NO TESTE
# Objetivo da sessao:
# - Gerar predicoes e residuos para Regressao Linear e Arvore.
# - Comparar distribuicao de residuos entre os modelos.
# - Avaliar RMSE, MAE e R2 para decidir o melhor desempenho.
# ------------------------------------------------------------------------------

# Predicoes no conjunto de teste para regressao linear stepwise.
pred_lm_teste_escala_modelo <- as.numeric(predict(modelo_lm_step, newdata = dados_teste))
# Predicoes no conjunto de teste para arvore de regressao.
pred_arvore_teste_escala_modelo <- as.numeric(predict(modelo_arvore, newdata = dados_teste))

# IC95% para regressao linear na escala transformada.
ic_lm_teste_escala_modelo <- predict(modelo_lm_step, newdata = dados_teste, interval = "confidence", level = 0.95)
# IC95% aproximado para arvore usando desvio dos residuos no treino.
residuos_arvore_treino <- dados_treino$VAL_TOT_TRANSF - as.numeric(predict(modelo_arvore, newdata = dados_treino))
erro_padrao_aprox_arvore <- sd(residuos_arvore_treino, na.rm = TRUE)
if (!is.finite(erro_padrao_aprox_arvore) || erro_padrao_aprox_arvore == 0) erro_padrao_aprox_arvore <- 1e-6
ic_arvore_lwr_escala_modelo <- pred_arvore_teste_escala_modelo - 1.96 * erro_padrao_aprox_arvore
ic_arvore_upr_escala_modelo <- pred_arvore_teste_escala_modelo + 1.96 * erro_padrao_aprox_arvore

# Destransforma preditos para escala original de VAL_TOT antes de comparar com observado.
pred_lm_teste <- destransformar_predicao(pred_lm_teste_escala_modelo, transformacao_escolhida, parametros_transformacao)
pred_arvore_teste <- destransformar_predicao(pred_arvore_teste_escala_modelo, transformacao_escolhida, parametros_transformacao)
ic_lm_lwr_teste <- destransformar_predicao(ic_lm_teste_escala_modelo[, "lwr"], transformacao_escolhida, parametros_transformacao)
ic_lm_upr_teste <- destransformar_predicao(ic_lm_teste_escala_modelo[, "upr"], transformacao_escolhida, parametros_transformacao)
ic_arvore_lwr_teste <- destransformar_predicao(ic_arvore_lwr_escala_modelo, transformacao_escolhida, parametros_transformacao)
ic_arvore_upr_teste <- destransformar_predicao(ic_arvore_upr_escala_modelo, transformacao_escolhida, parametros_transformacao)

# Tabela com valores observados, predicoes e residuos de cada modelo.
resultados_teste <- dados_teste %>%
  transmute(
    VAL_TOT_observado = VAL_TOT,
    pred_lm = pred_lm_teste,
    pred_arvore = pred_arvore_teste,
    ic_lm_lwr = ic_lm_lwr_teste,
    ic_lm_upr = ic_lm_upr_teste,
    ic_arvore_lwr = ic_arvore_lwr_teste,
    ic_arvore_upr = ic_arvore_upr_teste,
    residuo_lm = VAL_TOT_observado - pred_lm,
    residuo_arvore = VAL_TOT_observado - pred_arvore
  )

# Mostra resumo da precisao dos residuos em cada modelo.
resumo_residuos <- resultados_teste %>%
  summarise(
    media_residuo_lm = mean(residuo_lm, na.rm = TRUE),
    dp_residuo_lm = sd(residuo_lm, na.rm = TRUE),
    media_abs_residuo_lm = mean(abs(residuo_lm), na.rm = TRUE),
    media_residuo_arvore = mean(residuo_arvore, na.rm = TRUE),
    dp_residuo_arvore = sd(residuo_arvore, na.rm = TRUE),
    media_abs_residuo_arvore = mean(abs(residuo_arvore), na.rm = TRUE)
  )
cat("\nResumo da precisao dos residuos no teste\n")
print(resumo_residuos)

# Compara residuos dos dois modelos com boxplot.
grafico_box_residuos_modelos <- resultados_teste %>%
  dplyr::select(residuo_lm, residuo_arvore) %>%
  pivot_longer(cols = everything(), names_to = "modelo", values_to = "residuo") %>%
  ggplot(aes(x = modelo, y = residuo, fill = modelo)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Comparacao de residuos no conjunto de teste",
    x = "Modelo",
    y = "Residuo (observado - previsto)",
    fill = "Modelo"
  )
print(grafico_box_residuos_modelos)
salvar_grafico(grafico_box_residuos_modelos, "08_boxplot_residuos_modelos_teste.png")

# Compara residuos com densidade para observar concentracao em torno de zero.
grafico_densidade_residuos_modelos <- resultados_teste %>%
  dplyr::select(residuo_lm, residuo_arvore) %>%
  pivot_longer(cols = everything(), names_to = "modelo", values_to = "residuo") %>%
  ggplot(aes(x = residuo, color = modelo, fill = modelo)) +
  geom_density(alpha = 0.15) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Densidade dos residuos no teste",
    x = "Residuo",
    y = "Densidade",
    color = "Modelo",
    fill = "Modelo"
  )
print(grafico_densidade_residuos_modelos)
salvar_grafico(grafico_densidade_residuos_modelos, "09_densidade_residuos_modelos_teste.png")

# Grafico de linhas da target original separando treino e teste.#CORRIGIR
dados_linha_target <- bind_rows(
  dados_treino %>% transmute(indice = row_number(), conjunto = "Treino", target = VAL_TOT),
  dados_teste %>% transmute(indice = row_number(), conjunto = "Teste", target = VAL_TOT)
)
grafico_linha_target_treino_teste <- ggplot(dados_linha_target, aes(x = indice, y = target, color = conjunto)) +
  geom_line(alpha = 0.75, linewidth = 0.5) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Target original VAL_TOT dividida em treino e teste",
    x = "Indice da observacao em cada conjunto",
    y = "VAL_TOT",
    color = "Conjunto"
  )
print(grafico_linha_target_treino_teste)
salvar_grafico(grafico_linha_target_treino_teste, "10_linha_target_original_treino_teste.png")

# Grafico de linhas no teste com observado, estimado e IC95% para cada modelo.
dados_linha_teste_modelos <- bind_rows(
  resultados_teste %>%
    transmute(
      indice_teste = row_number(),
      modelo = "Regressao_Linear_Stepwise",
      observado = VAL_TOT_observado,
      estimado = pred_lm,
      ic_lwr = ic_lm_lwr,
      ic_upr = ic_lm_upr
    ),
  resultados_teste %>%
    transmute(
      indice_teste = row_number(),
      modelo = "Arvore_Regressao",
      observado = VAL_TOT_observado,
      estimado = pred_arvore,
      ic_lwr = ic_arvore_lwr,
      ic_upr = ic_arvore_upr
    )
)
grafico_linha_teste_modelos_ic95 <- ggplot(dados_linha_teste_modelos, aes(x = indice_teste)) +
  geom_ribbon(aes(ymin = ic_lwr, ymax = ic_upr), fill = "#9E9AC8", alpha = 0.25) +
  geom_line(aes(y = observado, color = "Observado"), linewidth = 0.5, alpha = 0.9) +
  geom_line(aes(y = estimado, color = "Estimado"), linewidth = 0.5, alpha = 0.9) +
  facet_wrap(~modelo, scales = "free_y") +
  scale_color_manual(values = c(Observado = "#2C7FB8", Estimado = "#B2182B")) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Teste - Observado vs Estimado com IC95% por modelo",
    subtitle = "Faixa sombreada representa intervalo de confianca de 95% da estimativa",
    x = "Indice da observacao no conjunto de teste",
    y = "VAL_TOT (escala original)",
    color = "Serie"
  )
print(grafico_linha_teste_modelos_ic95)
salvar_grafico(grafico_linha_teste_modelos_ic95, "11_linha_teste_observado_estimado_ic95_por_modelo.png")

# Funcao de metricas para regressao (RMSE, MAE e R2).
calcular_metricas_regressao <- function(observado, previsto) {
  rmse <- sqrt(mean((observado - previsto)^2, na.rm = TRUE))
  mae <- mean(abs(observado - previsto), na.rm = TRUE)
  sse <- sum((observado - previsto)^2, na.rm = TRUE)
  sst <- sum((observado - mean(observado, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- ifelse(sst > 0, 1 - (sse / sst), NA_real_)
  tibble::tibble(RMSE = rmse, MAE = mae, R2 = r2)
}

# Gera tabela final de desempenho por modelo no conjunto de teste.
metricas_modelos <- bind_rows(
  calcular_metricas_regressao(resultados_teste$VAL_TOT_observado, resultados_teste$pred_lm) %>%
    mutate(modelo = "Regressao_Linear_Stepwise"),
  calcular_metricas_regressao(resultados_teste$VAL_TOT_observado, resultados_teste$pred_arvore) %>%
    mutate(modelo = "Arvore_Regressao")
) %>%
  dplyr::select(modelo, RMSE, MAE, R2) %>%
  arrange(RMSE)

cat("\nMetricas no conjunto de teste (menor RMSE/MAE e maior R2 sao melhores)\n")
print(metricas_modelos)
write_csv(metricas_modelos, file.path(pasta_saida_modelos, "metricas_modelos_teste.csv"))
write_csv(resultados_teste, file.path(pasta_saida_modelos, "predicoes_e_residuos_teste.csv"))

# Identifica modelo vencedor com base no menor RMSE.
modelo_vencedor <- metricas_modelos %>%
  slice(1) %>%
  dplyr::pull(modelo)
cat("\nModelo com melhor desempenho (criterio: menor RMSE):", modelo_vencedor, "\n")

# Exibe summary do melhor modelo para apoiar interpretacao final.
if (modelo_vencedor == "Regressao_Linear_Stepwise") {
  cat("\nSummary do melhor modelo: Regressao Linear Stepwise\n")
  print(summary(modelo_lm_step))
} else {
  cat("\nSummary do melhor modelo: Arvore de Regressao\n")
  print(summary(modelo_arvore))
}

# Salva summaries e metricas em markdown elegante, um arquivo por modelo.
sumario_lm_md <- capture.output(summary(modelo_lm_step))
sumario_arvore_md <- capture.output(summary(modelo_arvore))

metricas_lm_md <- metricas_modelos %>% filter(modelo == "Regressao_Linear_Stepwise")
metricas_arvore_md <- metricas_modelos %>% filter(modelo == "Arvore_Regressao")

conteudo_md_lm <- c(
  "# Modelo - Regressao Linear Stepwise",
  "",
  paste0("- Transformacao da target usada: `", transformacao_escolhida, "`"),
  paste0("- Modelo vencedor global (menor RMSE): `", modelo_vencedor, "`"),
  "",
  "## Metricas no teste",
  "",
  "| Modelo | RMSE | MAE | R2 |",
  "|---|---:|---:|---:|",
  paste0(
    "| ", metricas_lm_md$modelo,
    " | ", formatC(metricas_lm_md$RMSE, digits = 4, format = "f"),
    " | ", formatC(metricas_lm_md$MAE, digits = 4, format = "f"),
    " | ", formatC(metricas_lm_md$R2, digits = 4, format = "f"),
    " |"
  ),
  "",
  "## Summary do modelo",
  "",
  "```r",
  sumario_lm_md,
  "```"
)
writeLines(conteudo_md_lm, file.path(pasta_saida_modelos, "modelo_regressao_linear_stepwise.md"))

conteudo_md_arvore <- c(
  "# Modelo - Arvore de Regressao",
  "",
  paste0("- Transformacao da target usada: `", transformacao_escolhida, "`"),
  paste0("- Modelo vencedor global (menor RMSE): `", modelo_vencedor, "`"),
  "",
  "## Metricas no teste",
  "",
  "| Modelo | RMSE | MAE | R2 |",
  "|---|---:|---:|---:|",
  paste0(
    "| ", metricas_arvore_md$modelo,
    " | ", formatC(metricas_arvore_md$RMSE, digits = 4, format = "f"),
    " | ", formatC(metricas_arvore_md$MAE, digits = 4, format = "f"),
    " | ", formatC(metricas_arvore_md$R2, digits = 4, format = "f"),
    " |"
  ),
  "",
  "## Summary do modelo",
  "",
  "```r",
  sumario_arvore_md,
  "```"
)
writeLines(conteudo_md_arvore, file.path(pasta_saida_modelos, "modelo_arvore_regressao.md"))

# ------------------------------------------------------------------------------
# SESSAO 7 - INTERPRETABILIDADE: FEATURE IMPORTANCE E SHAP (ARVORE)
# Objetivo da sessao:
# - Medir importancia de variaveis da arvore treinada.
# - Explicar contribuicoes locais e globais com SHAP.
# ------------------------------------------------------------------------------

# Importancia de variaveis da arvore (reducao de erro por divisao).
importancia_arvore <- modelo_arvore$variable.importance

if (!is.null(importancia_arvore) && length(importancia_arvore) > 0) {
  tabela_importancia <- tibble(
    variavel = names(importancia_arvore),
    importancia = as.numeric(importancia_arvore)
  ) %>%
    arrange(desc(importancia))

  print(tabela_importancia)
  write_csv(tabela_importancia, file.path(pasta_saida_modelos, "importancia_variaveis_arvore.csv"))

  # Grafico de importancia para leitura rapida das variaveis mais influentes.
  grafico_importancia_arvore <- ggplot(tabela_importancia, aes(x = reorder(variavel, importancia), y = importancia)) +
    geom_col(fill = "#2C7FB8") +
    coord_flip() +
    theme_minimal(base_size = 11) +
    labs(
      title = "Arvore de Regressao - Importancia das variaveis",
      x = "Variavel",
      y = "Importancia"
    )
  print(grafico_importancia_arvore)
  salvar_grafico(grafico_importancia_arvore, "12_importancia_variaveis_arvore.png")
}

# Dados de entrada para SHAP com as variaveis preditoras da arvore.
dados_treino_shap <- dados_treino %>% dplyr::select(all_of(preditores_arvore))

# Funcao de predicao para fastshap.
pred_wrapper_arvore <- function(object, newdata) {
  as.numeric(predict(object, newdata = newdata))
}

# Calcula valores SHAP por simulacao de contribuicoes marginais.
set.seed(2026)
valores_shap <- fastshap::explain(
  object = modelo_arvore,
  X = dados_treino_shap,
  pred_wrapper = pred_wrapper_arvore,
  nsim = 200,
  adjust = TRUE
)

# Cria objeto shapviz e gera grafico global de impacto das variaveis.
obj_shapviz <- shapviz(valores_shap, X = dados_treino_shap)
grafico_shap_beeswarm <- sv_importance(obj_shapviz, kind = "beeswarm")
print(grafico_shap_beeswarm)
salvar_grafico(grafico_shap_beeswarm, "13_shap_beeswarm_arvore.png", largura = 11, altura = 7)

# Grafico complementar em barras (impacto medio absoluto dos SHAP values).
grafico_shap_bar <- sv_importance(obj_shapviz, kind = "bar")
print(grafico_shap_bar)
salvar_grafico(grafico_shap_bar, "14_shap_bar_arvore.png", largura = 10, altura = 6)