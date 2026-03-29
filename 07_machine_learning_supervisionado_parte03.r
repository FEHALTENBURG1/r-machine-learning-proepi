# ==============================================================================
# AULA 7 - MACHINE LEARNING SUPERVISIONADO PARTE 3 - BRASIL
# Curso: Analise de Dados e Machine Learning com R - ProEpi 2026
# ------------------------------------------------------------------------------
# Objetivo geral da aula:
# - Modelar contagens diarias de internacoes com Poisson e Binomial Negativa.
# - Modelar tempo ate evento de tratamento com Analise de Sobrevida.
# - Salvar modelos e graficos no diretorio padrao de resultados da aula.
# ==============================================================================

# ------------------------------------------------------------------------------
# SESSAO 1 - TODOS OS IMPORTS DE PACOTES E CONFIGURACAO DE SAIDA
# Objetivo da sessao:
# - Carregar pacotes necessarios para series de contagem e sobrevida.
# - Criar pasta de resultados da aula para salvar summaries e graficos.
# - Definir funcao auxiliar unica para exportacao de figuras.
# ------------------------------------------------------------------------------
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("rsample", quietly = TRUE)) install.packages("rsample")
if (!requireNamespace("AER", quietly = TRUE)) install.packages("AER")
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")
if (!requireNamespace("survival", quietly = TRUE)) install.packages("survival")
if (!requireNamespace("survminer", quietly = TRUE)) install.packages("survminer")

library(tidyverse)   # Manipulacao de dados, tabelas e graficos.
library(lubridate)   # Extracao de componentes de data e calculo de tempo.
library(rsample)     # Split treino/teste com boas praticas.
library(AER)         # Teste de overdispersao para modelo Poisson.
library(MASS)        # Modelo binomial negativa (glm.nb) para comparacao.
library(survival)    # Objetos Surv e modelo de Cox.
library(survminer)   # Curvas Kaplan-Meier com visual didatica.

# Define diretorio de saida da aula 7.
pasta_saida_modelos <- file.path("modelos_machine_learning", "07_ml_supervisionado_parte03")
dir.create(pasta_saida_modelos, recursive = TRUE, showWarnings = FALSE)

# Funcao auxiliar para salvar graficos.
salvar_grafico <- function(grafico, nome_arquivo, largura = 10, altura = 6) {
  ggplot2::ggsave(
    filename = file.path(pasta_saida_modelos, nome_arquivo),
    plot = grafico,
    width = largura,
    height = altura,
    dpi = 300
  )
}

# ------------------------------------------------------------------------------
# SESSAO 2 - MODELO 1: REGRESSAO DE POISSON (CONTAGEM DE INTERNACOES)
# Objetivo da sessao:
# - Construir base diaria de internacoes e avaliar pressupostos da Poisson.
# - Treinar modelo Poisson e testar formalmente overdispersao.
# - Com overdispersao, adotar Binomial Negativa como modelo oficial.
# ------------------------------------------------------------------------------

# Importa base tratada do SIH.
raiz_dados_processados <- "dados_processados_datasus"
load(file.path(raiz_dados_processados, "sih_modelagem.RData"))

dim(df_sih_modelagem)
glimpse(df_sih_modelagem)

# Engenharia de features:
# - Agrupa por data de internacao para obter contagem diaria.
# - Extrai dia da semana, mes e ano como preditores administrativos/temporais.
base_diaria_internacoes <- df_sih_modelagem %>%
  filter(!is.na(DT_INTER)) %>%
  mutate(DT_INTER = as.Date(DT_INTER)) %>%
  count(DT_INTER, name = "N_INTERNACOES") %>%
  arrange(DT_INTER) %>%
  mutate(
    DIA_SEMANA = as.integer(wday(DT_INTER, week_start = 1)),
    MES = as.integer(month(DT_INTER)),
    ANO = as.integer(year(DT_INTER))
  )

dim(base_diaria_internacoes)
glimpse(base_diaria_internacoes)

# Histograma da target discreta de contagem.
grafico_hist_poisson <- ggplot(base_diaria_internacoes, aes(x = N_INTERNACOES)) +
  geom_histogram(binwidth = 1, fill = "#2C7FB8", color = "white") +
  theme_minimal(base_size = 11) +
  labs(
    title = "SIH - Histograma da contagem diaria de internacoes",
    x = "N_INTERNACOES por dia",
    y = "Frequencia"
  )
print(grafico_hist_poisson)
salvar_grafico(grafico_hist_poisson, "01_histograma_contagem_diaria_poisson.png")

# Media e variancia para discutir pressuposto Poisson (media ~= variancia).
media_contagem <- mean(base_diaria_internacoes$N_INTERNACOES, na.rm = TRUE)
variancia_contagem <- var(base_diaria_internacoes$N_INTERNACOES, na.rm = TRUE)
tabela_media_variancia <- tibble::tibble(
  medida = c("Media", "Variancia"),
  valor = c(media_contagem, variancia_contagem)
)
print(tabela_media_variancia)

cat("\nInterpretacao didatica (Poisson):\n")
cat("- Se media e variancia forem proximas, a Poisson e plausivel.\n")
cat("- Se variancia for muito maior que media, pode haver overdispersao.\n")

# Divisao treino/teste cronologica (70/30):
# - Em series temporais administrativas, split cronologico evita vazamento de futuro.
n_total_dias <- nrow(base_diaria_internacoes)
ponto_corte <- floor(0.70 * n_total_dias)
dados_treino_poisson <- base_diaria_internacoes %>% slice(1:ponto_corte)
dados_teste_poisson <- base_diaria_internacoes %>% slice((ponto_corte + 1):n_total_dias)

dim(dados_treino_poisson)
dim(dados_teste_poisson)

# Define formula de contagem de forma robusta:
# - Se um preditor nao variar no treino, ele e removido automaticamente.
preditores_contagem <- c("DIA_SEMANA", "MES", "ANO")
preditores_validos_contagem <- preditores_contagem[
  sapply(
    dados_treino_poisson[preditores_contagem],
    function(x) dplyr::n_distinct(x[!is.na(x)]) > 1
  )
]
if (length(preditores_validos_contagem) == 0) {
  formula_contagem <- N_INTERNACOES ~ 1
} else {
  formula_contagem <- as.formula(
    paste("N_INTERNACOES ~", paste(preditores_validos_contagem, collapse = " + "))
  )
}
cat("\nPreditores validos no treino para modelos de contagem:\n")
print(preditores_validos_contagem)

# Modelo Poisson base.
modelo_poisson <- glm(
  formula_contagem,
  data = dados_treino_poisson,
  family = "poisson"
)
summary(modelo_poisson)

# Teste formal de overdispersao.
teste_overdispersao <- AER::dispersiontest(modelo_poisson)
cat("\nTeste de overdispersao (Poisson):\n")
print(teste_overdispersao)

# Treina modelos alternativos para comparacao didatica.
modelo_quasipoisson <- glm(
  formula_contagem,
  data = dados_treino_poisson,
  family = "quasipoisson"
)
summary(modelo_quasipoisson)

modelo_binomial_negativa <- MASS::glm.nb(
  formula_contagem,
  data = dados_treino_poisson
)
summary(modelo_binomial_negativa)

# Predicoes no teste e comparacao por RMSE.
pred_poisson <- as.numeric(predict(modelo_poisson, newdata = dados_teste_poisson, type = "response"))
pred_quasipoisson <- as.numeric(predict(modelo_quasipoisson, newdata = dados_teste_poisson, type = "response"))
pred_binomial_negativa <- as.numeric(predict(modelo_binomial_negativa, newdata = dados_teste_poisson, type = "response"))

calcular_rmse <- function(real, previsto) sqrt(mean((real - previsto)^2, na.rm = TRUE))

rmse_poisson <- calcular_rmse(dados_teste_poisson$N_INTERNACOES, pred_poisson)
rmse_quasipoisson <- calcular_rmse(dados_teste_poisson$N_INTERNACOES, pred_quasipoisson)
rmse_binomial_negativa <- calcular_rmse(dados_teste_poisson$N_INTERNACOES, pred_binomial_negativa)

tabela_rmse_poisson <- tibble::tibble(
  modelo = c("Poisson", "Quase-Poisson", "Binomial_Negativa"),
  RMSE = c(rmse_poisson, rmse_quasipoisson, rmse_binomial_negativa)
) %>%
  arrange(RMSE)
print(tabela_rmse_poisson)

# Define modelo final de contagem:
# - Se houver overdispersao, adotamos Binomial Negativa como oficial.
# - Caso nao haja, mantemos selecao por menor RMSE.
overdispersao_detectada <- isTRUE(!is.null(teste_overdispersao$p.value) && is.finite(teste_overdispersao$p.value) && teste_overdispersao$p.value < 0.05)
if (overdispersao_detectada) {
  nome_modelo_final_contagem <- "Binomial_Negativa"
  modelo_final_contagem <- modelo_binomial_negativa
  cat("\nOverdispersao detectada: Binomial Negativa definida como modelo oficial.\n")
} else {
  nome_modelo_final_contagem <- tabela_rmse_poisson$modelo[1]
  modelo_final_contagem <- if (nome_modelo_final_contagem == "Poisson") {
    modelo_poisson
  } else if (nome_modelo_final_contagem == "Quase-Poisson") {
    modelo_quasipoisson
  } else {
    modelo_binomial_negativa
  }
}
cat("\nModelo final de contagem:", nome_modelo_final_contagem, "\n")
cat("Interpretacao administrativa: o modelo final descreve variacao diaria de demanda hospitalar com robustez para dispersao observada.\n")

# Grafico real vs predito no teste (modelo final).
pred_final_teste <- if (nome_modelo_final_contagem == "Poisson") {
  pred_poisson
} else if (nome_modelo_final_contagem == "Quase-Poisson") {
  pred_quasipoisson
} else {
  pred_binomial_negativa
}

tabela_real_predito_poisson <- dados_teste_poisson %>%
  transmute(
    DT_INTER,
    REAL = N_INTERNACOES,
    PREDITO = pred_final_teste
  )

grafico_real_vs_predito <- tabela_real_predito_poisson %>%
  pivot_longer(cols = c("REAL", "PREDITO"), names_to = "serie", values_to = "contagem") %>%
  ggplot(aes(x = contagem, fill = serie, color = serie)) +
  geom_histogram(
    position = "identity",
    alpha = 0.45,
    bins = 20,
    linewidth = 0.4
  ) +
  theme_minimal(base_size = 11) +
  labs(
    title = paste("Teste - Histogramas sobrepostos: real vs predito (", nome_modelo_final_contagem, ")", sep = ""),
    x = "N_INTERNACOES",
    y = "Frequencia",
    fill = "Serie",
    color = "Serie"
  )
print(grafico_real_vs_predito)
salvar_grafico(grafico_real_vs_predito, "03_real_vs_predito_contagem_teste.png", largura = 11, altura = 6)

# ------------------------------------------------------------------------------
# SESSAO 3 - MODELO 2: ANALISE DE SOBREVIDA (TEMPO ATE O EVENTO)
# Objetivo da sessao:
# - Construir tempo de tratamento por paciente e status de evento.
# - Gerar curvas Kaplan-Meier por sexo, raca/cor e faixa etaria.
# - Ajustar modelo de Cox e interpretar Hazard Ratios.
# ------------------------------------------------------------------------------

# Importa base tratada do SIA.
load(file.path(raiz_dados_processados, "sia_modelagem.RData"))

dim(df_sia_modelagem)
glimpse(df_sia_modelagem)

# Prepara base de sobrevida por paciente:
# - TEMPO_TRAT_DIAS: tempo entre primeira e ultima AP_MVM.
# - EVENTO_TRAT: 1 se encerrou sem obito; 0 se censurado.
#   Regra de negocio: obito e evento concorrente, portanto censura.
# - Faixa etaria derivada de AP_NUIDADE.
base_sobrevida <- df_sia_modelagem %>%
  mutate(
    AP_MVM = as.Date(AP_MVM),
    AP_OBITO_chr = as.character(AP_OBITO),
    AP_ENCERR_chr = as.character(AP_ENCERR)
  ) %>%
  filter(!is.na(AP_CNSPCN), !is.na(AP_MVM)) %>%
  group_by(AP_CNSPCN) %>%
  summarise(
    DATA_INICIO = min(AP_MVM, na.rm = TRUE),
    DATA_FIM = max(AP_MVM, na.rm = TRUE),
    TEMPO_TRAT_DIAS = as.integer(DATA_FIM - DATA_INICIO),
    AP_NUIDADE = suppressWarnings(median(AP_NUIDADE, na.rm = TRUE)),
    AP_SEXO = first(na.omit(AP_SEXO)),
    AP_RACACOR = first(na.omit(AP_RACACOR)),
    AM_TRANSPL = first(na.omit(AM_TRANSPL)),
    AP_CIDPRI = first(na.omit(AP_CIDPRI)),
    ALGUM_ENCERR_SIM = any(AP_ENCERR_chr %in% c("Sim", "1", 1), na.rm = TRUE),
    ALGUM_OBITO_SIM = any(AP_OBITO_chr %in% c("Sim", "1", 1), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TEMPO_TRAT_DIAS = if_else(is.na(TEMPO_TRAT_DIAS), 0L, TEMPO_TRAT_DIAS),
    TEMPO_TRAT_DIAS = pmax(TEMPO_TRAT_DIAS, 1L), # garante tempo positivo para Surv.
    EVENTO_TRAT = if_else(ALGUM_ENCERR_SIM & !ALGUM_OBITO_SIM, 1L, 0L),
    FAIXA_ETARIA = cut(
      AP_NUIDADE,
      breaks = c(-Inf, 20, 40, 60, Inf),
      labels = c("<20", "20-40", "40-60", "60+"),
      right = FALSE
    ),
    AP_SEXO = as.factor(AP_SEXO),
    AP_RACACOR = as.factor(AP_RACACOR),
    AM_TRANSPL = as.factor(AM_TRANSPL),
    AP_CIDPRI = as.factor(AP_CIDPRI),
    FAIXA_ETARIA = as.factor(FAIXA_ETARIA)
  ) %>%
  filter(
    !is.na(FAIXA_ETARIA),
    !is.na(AP_SEXO),
    !is.na(AP_RACACOR),
    !is.na(AM_TRANSPL),
    !is.na(AP_CIDPRI),
    !is.na(EVENTO_TRAT)
  )

dim(base_sobrevida)
glimpse(base_sobrevida)

cat("\nInterpretacao biologica/administrativa (Sobrevida):\n")
cat("- EVENTO_TRAT = 1 representa encerramento sem obito (fim do tratamento).\n")
cat("- EVENTO_TRAT = 0 representa censura (inclui obito como evento concorrente).\n")
cat("- O tempo modelado reflete permanencia em tratamento ao longo das AP_MVM.\n")

# Objeto de sobrevida.
obj_surv <- Surv(time = base_sobrevida$TEMPO_TRAT_DIAS, event = base_sobrevida$EVENTO_TRAT)

# Curvas Kaplan-Meier por SEXO.
km_sexo <- survfit(obj_surv ~ AP_SEXO, data = base_sobrevida)
grafico_km_sexo <- ggsurvplot(
  km_sexo,
  data = base_sobrevida,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  ggtheme = theme_minimal(base_size = 11),
  title = "Kaplan-Meier por Sexo",
  xlab = "Tempo de tratamento (dias)",
  ylab = "Probabilidade de permanencia no tratamento"
)
print(grafico_km_sexo)
salvar_grafico(grafico_km_sexo$plot, "05_km_por_sexo.png", largura = 10, altura = 6)

# Curvas Kaplan-Meier por Raca/Cor.
km_racacor <- survfit(obj_surv ~ AP_RACACOR, data = base_sobrevida)
grafico_km_racacor <- ggsurvplot(
  km_racacor,
  data = base_sobrevida,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  ggtheme = theme_minimal(base_size = 11),
  title = "Kaplan-Meier por Raca/Cor",
  xlab = "Tempo de tratamento (dias)",
  ylab = "Probabilidade de permanencia no tratamento"
)
print(grafico_km_racacor)
salvar_grafico(grafico_km_racacor$plot, "06_km_por_racacor.png", largura = 10, altura = 6)

# Curvas Kaplan-Meier por Faixa Etaria.
km_faixa_etaria <- survfit(obj_surv ~ FAIXA_ETARIA, data = base_sobrevida)
grafico_km_faixa_etaria <- ggsurvplot(
  km_faixa_etaria,
  data = base_sobrevida,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  ggtheme = theme_minimal(base_size = 11),
  title = "Kaplan-Meier por Faixa Etaria",
  xlab = "Tempo de tratamento (dias)",
  ylab = "Probabilidade de permanencia no tratamento"
)
print(grafico_km_faixa_etaria)
salvar_grafico(grafico_km_faixa_etaria$plot, "07_km_por_faixa_etaria.png", largura = 10, altura = 6)

# ------------------------------------------------------------------------------
# SESSAO 4 - MODELO COX PROPORTIONAL HAZARDS
# Objetivo da sessao:
# - Ajustar modelo de Cox para inferencia de risco relativo.
# - Testar pressuposto de riscos proporcionais com cox.zph.
# - Interpretar HR e C-index para utilidade clinico-administrativa.
# ------------------------------------------------------------------------------

# Escolha didatica:
# - Usamos a base completa para foco em inferencia dos Hazard Ratios.
# - Em cenarios de predicao operacional, treino/teste pode ser adotado.
modelo_cox <- coxph(
  Surv(TEMPO_TRAT_DIAS, EVENTO_TRAT) ~ FAIXA_ETARIA + AP_SEXO + AP_RACACOR + AM_TRANSPL + AP_CIDPRI,
  data = base_sobrevida
)
summary_modelo_cox <- summary(modelo_cox)
print(summary_modelo_cox)

# Teste de Schoenfeld para pressuposto de riscos proporcionais.
teste_schoenfeld <- cox.zph(modelo_cox)
print(teste_schoenfeld)

png(
  filename = file.path(pasta_saida_modelos, "08_plot_schoenfeld_cox.png"),
  width = 1800,
  height = 1400,
  res = 180
)
plot(teste_schoenfeld)
dev.off()

# C-index como medida de discriminacao do modelo de Cox.
c_index <- as.numeric(summary_modelo_cox$concordance[1])
tabela_c_index <- tibble::tibble(metrica = "C_index", valor = c_index)
print(tabela_c_index)

# Tabela de Hazard Ratios (HR) com IC95%.
tabela_hr <- tibble::tibble(
  variavel = rownames(summary_modelo_cox$coefficients),
  HR = summary_modelo_cox$coefficients[, "exp(coef)"],
  IC95_inf = summary_modelo_cox$conf.int[, "lower .95"],
  IC95_sup = summary_modelo_cox$conf.int[, "upper .95"],
  p_valor = summary_modelo_cox$coefficients[, "Pr(>|z|)"]
)
print(tabela_hr)

cat("\nInterpretacao biologica/administrativa (Cox):\n")
cat("- HR > 1: maior taxa de encerramento do tratamento ao longo do tempo para o grupo.\n")
cat("- HR < 1: menor taxa de encerramento, sugerindo maior permanencia no tratamento.\n")
cat("- Exemplo: HR > 1 para faixa etaria pode indicar desfecho mais rapido do acompanhamento.\n")
cat("- C-index proximo de 1 indica boa discriminacao; proximo de 0.5 indica discriminacao fraca.\n")

# ------------------------------------------------------------------------------
# SESSAO 5 - RESUMO FINAL E SALVAMENTO DE RELATORIOS
# Objetivo da sessao:
# - Consolidar resultados de contagem e sobrevida em markdown.
# - Garantir reprodutibilidade com summaries em markdown e metricas.
# ------------------------------------------------------------------------------

# Salva summaries dos modelos em .md (sem uso de .rds).
sumario_poisson_md <- capture.output(summary(modelo_poisson))
sumario_quasipoisson_md <- capture.output(summary(modelo_quasipoisson))
sumario_binomial_negativa_md <- capture.output(summary(modelo_binomial_negativa))
sumario_cox_md <- capture.output(summary_modelo_cox)
sumario_km_sexo_md <- capture.output(summary(km_sexo))
sumario_km_racacor_md <- capture.output(summary(km_racacor))
sumario_km_faixa_etaria_md <- capture.output(summary(km_faixa_etaria))

conteudo_md_summaries <- c(
  "# Aula 7 - Summaries dos Modelos",
  "",
  "## Regressao de Poisson",
  "```r",
  sumario_poisson_md,
  "```",
  "",
  "## Regressao Quase-Poisson",
  "```r",
  sumario_quasipoisson_md,
  "```",
  "",
  "## Regressao Binomial Negativa (oficial com overdispersao)",
  "```r",
  sumario_binomial_negativa_md,
  "```",
  "",
  "## Kaplan-Meier por Sexo",
  "```r",
  sumario_km_sexo_md,
  "```",
  "",
  "## Kaplan-Meier por Raca/Cor",
  "```r",
  sumario_km_racacor_md,
  "```",
  "",
  "## Kaplan-Meier por Faixa Etaria",
  "```r",
  sumario_km_faixa_etaria_md,
  "```",
  "",
  "## Cox Proportional Hazards",
  "```r",
  sumario_cox_md,
  "```"
)
writeLines(conteudo_md_summaries, file.path(pasta_saida_modelos, "summaries_modelos_aula7.md"))