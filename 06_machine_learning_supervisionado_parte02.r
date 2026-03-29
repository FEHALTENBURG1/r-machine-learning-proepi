# ==============================================================================
# AULA 6 - MACHINE LEARNING SUPERVISIONADO PARTE 2 - BRASIL
# Curso: Analise de Dados e Machine Learning com R - ProEpi 2026
# ------------------------------------------------------------------------------
# Objetivo geral da aula:
# - Construir modelos de classificacao para prever probabilidade de obito por I219.
# - Comparar Regressao Logistica e Arvore de Decisao em base de teste.
# - Explicar os modelos com importancia de variaveis, odds ratios e ROC-AUC.
# ==============================================================================

# ------------------------------------------------------------------------------
# SESSAO 1 - TODOS OS IMPORTS DE PACOTES E DADOS
# Objetivo da sessao:
# - Carregar pacotes necessarios para preparacao, amostragem, modelagem e avaliacao.
# - Importar a base tratada do SIM gerada na aula 2.
# - Preparar objetos para analise de classificacao supervisionada.
# ------------------------------------------------------------------------------
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("rsample", quietly = TRUE)) install.packages("rsample")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("rpart", quietly = TRUE)) install.packages("rpart")
if (!requireNamespace("rpart.plot", quietly = TRUE)) install.packages("rpart.plot")
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")

library(tidyverse)   # Manipulacao, transformacao e visualizacao de dados.
library(rsample)     # Divisao treino/teste com estratificacao e reproducibilidade.
library(caret)       # Tecnicas de sampling: upSample e downSample.
library(rpart)       # Arvore de decisao para classificacao.
library(rpart.plot)  # Visualizacao didatica da arvore de decisao.
library(pROC)        # Curva ROC e AUC para avaliacao de classificacao.

raiz_dados_processados <- "dados_processados_datasus"
load(file.path(raiz_dados_processados, "sim_modelagem.RData"))

# Define pasta para salvar resultados da aula 6.
pasta_saida_modelos <- file.path("modelos_machine_learning", "06_ml_supervisionado_parte02")
dir.create(pasta_saida_modelos, recursive = TRUE, showWarnings = FALSE)

# Funcao auxiliar para salvar graficos com padrao unico.
salvar_grafico <- function(grafico, nome_arquivo, largura = 10, altura = 6) {
  ggplot2::ggsave(
    filename = file.path(pasta_saida_modelos, nome_arquivo),
    plot = grafico,
    width = largura,
    height = altura,
    dpi = 300
  )
}

dim(df_sim_modelagem)
glimpse(df_sim_modelagem)

# ------------------------------------------------------------------------------
# SESSAO 2 - IMPORTACAO, LIMPEZA E CRIACAO DO TARGET
# Objetivo da sessao:
# - Filtrar a base para TIPOBITO nao fetal.
# - Remover variaveis nao utilizadas na modelagem.
# - Criar target dummy para classe de interesse (I219).
# ------------------------------------------------------------------------------

# Filtra somente nao fetal (com e sem acento para robustez de string).
df_sim_supervisionado <- df_sim_modelagem %>%
  filter(TIPOBITO %in% c("Não Fetal", "Nao Fetal")) %>%
  dplyr::select(-any_of(c("ORIGEM", "DTNASC", "IDADE")))

# Cria target binaria para evento de interesse.
# Classe 1: CAUSABAS == I219 (infarto agudo do miocardio).
# Classe 0: qualquer outra causa basica.
df_sim_supervisionado <- df_sim_supervisionado %>%
  mutate(
    RACACOR = as.factor(RACACOR),
    TARGET_I219 = if_else(CAUSABAS == "I219", 1L, 0L),
    TARGET_FATOR = factor(TARGET_I219, levels = c(0L, 1L), labels = c("0", "1"))
  ) %>%
  dplyr::select(RACACOR, IDADEanos, TARGET_I219, TARGET_FATOR)

# Remove registros com missing nas variaveis usadas no modelo.
df_sim_supervisionado <- df_sim_supervisionado %>%
  filter(!is.na(RACACOR), !is.na(IDADEanos), !is.na(TARGET_I219))

dim(df_sim_supervisionado)
glimpse(df_sim_supervisionado)

# Distribuicao do novo target (contagem e proporcao).
tabela_target <- df_sim_supervisionado %>%
  count(TARGET_I219, name = "frequencia") %>%
  mutate(percentual = 100 * frequencia / sum(frequencia))
print(tabela_target)
write_csv(tabela_target, file.path(pasta_saida_modelos, "01_distribuicao_target_dummy.csv"))

grafico_distribuicao_target <- ggplot(tabela_target, aes(x = factor(TARGET_I219), y = frequencia, fill = factor(TARGET_I219))) +
  geom_col(alpha = 0.85) +
  theme_minimal(base_size = 11) +
  labs(
    title = "SIM - Distribuicao da variavel target dummy (0 e 1)",
    x = "Classe target (0 = nao I219, 1 = I219)",
    y = "Frequencia",
    fill = "Classe"
  )
print(grafico_distribuicao_target)
salvar_grafico(grafico_distribuicao_target, "01_distribuicao_target_dummy.png")

# ------------------------------------------------------------------------------
# SESSAO 3 - DIVISAO TREINO E TESTE
# Objetivo da sessao:
# - Separar dados em treino e teste (70/30) com estratificacao da classe alvo.
# - Preservar proporcao de classe para avaliacao mais confiavel.
# ------------------------------------------------------------------------------
set.seed(2026)
divisao_treino_teste <- initial_split(df_sim_supervisionado, prop = 0.70, strata = TARGET_FATOR)
dados_treino <- training(divisao_treino_teste)
dados_teste <- testing(divisao_treino_teste)

dim(dados_treino)
dim(dados_teste)

# Confere proporcao da classe no treino e no teste.
tabela_prop_treino <- dados_treino %>%
  count(TARGET_FATOR, name = "frequencia") %>%
  mutate(percentual = 100 * frequencia / sum(frequencia), conjunto = "Treino")
tabela_prop_teste <- dados_teste %>%
  count(TARGET_FATOR, name = "frequencia") %>%
  mutate(percentual = 100 * frequencia / sum(frequencia), conjunto = "Teste")
tabela_proporcao_split <- bind_rows(tabela_prop_treino, tabela_prop_teste)
print(tabela_proporcao_split)
write_csv(tabela_proporcao_split, file.path(pasta_saida_modelos, "02_proporcao_target_treino_teste.csv"))

# ------------------------------------------------------------------------------
# SESSAO 4 - BALANCEAMENTO E TECNICAS DE SAMPLING
# Objetivo da sessao:
# - Diagnosticar desbalanceamento de classe no treino.
# - Ensinar undersample (downSample) e oversample (upSample) de forma explicita.
# ------------------------------------------------------------------------------

# Comentario didatico:
# - Em dados de eventos raros, a classe 1 pode ser minoritaria.
# - Modelos treinados em base muito desbalanceada podem favorecer a classe 0.
# - Downsample reduz a classe majoritaria.
# - Upsample replica observacoes da classe minoritaria para equilibrar classes.

cat("\nDistribuicao de classe no treino original:\n")
print(prop.table(table(dados_treino$TARGET_FATOR)))
tabela_balanceamento_treino_original <- dados_treino %>%
  count(TARGET_FATOR, name = "frequencia") %>%
  mutate(percentual = 100 * frequencia / sum(frequencia), tipo_base = "Treino_Original")

# Base de treino para sampling com preditores e classe fator.
x_treino <- dados_treino %>% dplyr::select(RACACOR, IDADEanos)
y_treino <- dados_treino$TARGET_FATOR

# Undersample (subamostragem da classe majoritaria).
dados_treino_undersample <- downSample(x = x_treino, y = y_treino, yname = "TARGET_FATOR")
cat("\nDistribuicao apos undersample:\n")
print(prop.table(table(dados_treino_undersample$TARGET_FATOR)))
tabela_balanceamento_undersample <- dados_treino_undersample %>%
  count(TARGET_FATOR, name = "frequencia") %>%
  mutate(percentual = 100 * frequencia / sum(frequencia), tipo_base = "Treino_Undersample")

# Uppersample/Oversample (superamostragem da classe minoritaria).
dados_treino_uppersample <- upSample(x = x_treino, y = y_treino, yname = "TARGET_FATOR")
cat("\nDistribuicao apos uppersample:\n")
print(prop.table(table(dados_treino_uppersample$TARGET_FATOR)))
tabela_balanceamento_uppersample <- dados_treino_uppersample %>%
  count(TARGET_FATOR, name = "frequencia") %>%
  mutate(percentual = 100 * frequencia / sum(frequencia), tipo_base = "Treino_Uppersample")

# Consolida e salva tabelas de balanceamento.
tabela_balanceamento_sampling <- bind_rows(
  tabela_balanceamento_treino_original,
  tabela_balanceamento_undersample,
  tabela_balanceamento_uppersample
)
write_csv(tabela_balanceamento_sampling, file.path(pasta_saida_modelos, "03_balanceamento_sampling.csv"))

# Converte classe fator em dummy numerica para glm binomial.
dados_treino_uppersample <- dados_treino_uppersample %>%
  mutate(TARGET_I219 = if_else(TARGET_FATOR == "1", 1L, 0L))

# ------------------------------------------------------------------------------
# SESSAO 5 - MODELO 1: REGRESSAO LOGISTICA
# Objetivo da sessao:
# - Ajustar regressao logistica completa usando somente base de uppersample.
# - Ajustar versao stepwise (modelo oficial) para comparacao didatica.
# - Mostrar summary dos modelos e interpretar efeitos via Odds Ratios.
# ------------------------------------------------------------------------------

# Pressupostos principais da Regressao Logistica:
# - Observacoes independentes entre si.
# - Ausencia de multicolinearidade forte entre preditores.
# - Relacao aproximadamente linear dos preditores continuos com o log-odds.
# - Variavel resposta binaria (evento vs nao evento).
# - Tamanho amostral adequado para estimativas estaveis dos coeficientes.

# REGRA ESTRITA: modelos logisticos treinados somente na base de uppersample.
modelo_logistico_completo <- glm(
  TARGET_I219 ~ RACACOR + IDADEanos,
  data = dados_treino_uppersample,
  family = "binomial"
)

modelo_logistico_stepwise <- step(modelo_logistico_completo, direction = "both", trace = 0)

cat("\nSummary - Regressao Logistica Completa\n")
summary(modelo_logistico_completo)
cat("\nSummary - Regressao Logistica Stepwise (MODELO OFICIAL)\n")
summary(modelo_logistico_stepwise)

# Funcao auxiliar para tabela de Odds Ratios e IC95%.
gerar_tabela_or <- function(modelo_glm) {
  coeficientes_logit <- summary(modelo_glm)$coefficients
  tabela <- tibble::tibble(
    variavel = rownames(coeficientes_logit),
    beta = coeficientes_logit[, "Estimate"],
    erro_padrao = coeficientes_logit[, "Std. Error"]
  )
  tabela$OR <- exp(tabela$beta)
  tabela$IC95_inf <- exp(tabela$beta - 1.96 * tabela$erro_padrao)
  tabela$IC95_sup <- exp(tabela$beta + 1.96 * tabela$erro_padrao)
  tabela[tabela$variavel != "(Intercept)", , drop = FALSE]
}

# Explicabilidade por Odds Ratios e IC95% (aproximacao de Wald).
tabela_or_logistica_completa <- gerar_tabela_or(modelo_logistico_completo)
tabela_or_logistica_stepwise <- gerar_tabela_or(modelo_logistico_stepwise)
print(tabela_or_logistica_completa)
print(tabela_or_logistica_stepwise)
write_csv(tabela_or_logistica_completa, file.path(pasta_saida_modelos, "04a_odds_ratios_regressao_logistica_completa.csv"))
write_csv(tabela_or_logistica_stepwise, file.path(pasta_saida_modelos, "04b_odds_ratios_regressao_logistica_stepwise_oficial.csv"))

grafico_odds_ratio_logistica_completa <- ggplot(tabela_or_logistica_completa, aes(x = reorder(variavel, OR), y = OR)) +
  geom_point(color = "#2C7FB8", size = 2) +
  geom_errorbar(aes(ymin = IC95_inf, ymax = IC95_sup), width = 0.2, color = "#2C7FB8") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#B2182B") +
  coord_flip() +
  theme_minimal(base_size = 11) +
  labs(
    title = "Regressao Logistica Completa - Odds Ratios (IC95%)",
    x = "Variavel",
    y = "Odds Ratio (OR)"
  )
print(grafico_odds_ratio_logistica_completa)
salvar_grafico(grafico_odds_ratio_logistica_completa, "04a_odds_ratios_regressao_logistica_completa.png")

grafico_odds_ratio_logistica_stepwise <- ggplot(tabela_or_logistica_stepwise, aes(x = reorder(variavel, OR), y = OR)) +
  geom_point(color = "#1B9E77", size = 2) +
  geom_errorbar(aes(ymin = IC95_inf, ymax = IC95_sup), width = 0.2, color = "#1B9E77") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#B2182B") +
  coord_flip() +
  theme_minimal(base_size = 11) +
  labs(
    title = "Regressao Logistica Stepwise (Oficial) - Odds Ratios (IC95%)",
    x = "Variavel",
    y = "Odds Ratio (OR)"
  )
print(grafico_odds_ratio_logistica_stepwise)
salvar_grafico(grafico_odds_ratio_logistica_stepwise, "04b_odds_ratios_regressao_logistica_stepwise_oficial.png")

# ------------------------------------------------------------------------------
# SESSAO 6 - MODELO 2: ARVORE DE DECISAO PARA CLASSIFICACAO
# Objetivo da sessao:
# - Ajustar arvore na base de treino original (desbalanceada).
# - Mostrar summary, plot da arvore e explicabilidade por importancia.
# ------------------------------------------------------------------------------

# Nota didatica:
# - Aqui usamos treino original para comparar com a regressao treinada em uppersample.
modelo_arvore_classificacao <- rpart(
  TARGET_FATOR ~ RACACOR + IDADEanos,
  data = dados_treino,
  method = "class",
  parms = list(split = "gini"),
  control = rpart.control(cp = 0.001, minsplit = 20, maxdepth = 6)
)

summary(modelo_arvore_classificacao)

png(
  filename = file.path(pasta_saida_modelos, "05_arvore_classificacao.png"),
  width = 1800,
  height = 1200,
  res = 180
)
rpart.plot(
  modelo_arvore_classificacao,
  type = 2,
  extra = 106,
  fallen.leaves = TRUE,
  tweak = 1.05,
  main = "Arvore de Decisao - Predicao de obito por I219"
)
dev.off()

# Feature Importance classica da arvore.
importancia_arvore <- modelo_arvore_classificacao$variable.importance
if (!is.null(importancia_arvore) && length(importancia_arvore) > 0) {
  tabela_importancia_arvore <- tibble(
    variavel = names(importancia_arvore),
    importancia = as.numeric(importancia_arvore)
  ) %>%
    arrange(desc(importancia))
  print(tabela_importancia_arvore)
  write_csv(tabela_importancia_arvore, file.path(pasta_saida_modelos, "05_importancia_variaveis_arvore.csv"))

  grafico_importancia_arvore <- ggplot(tabela_importancia_arvore, aes(x = reorder(variavel, importancia), y = importancia)) +
    geom_col(fill = "#41AB5D") +
    coord_flip() +
    theme_minimal(base_size = 11) +
    labs(
      title = "Arvore de Classificacao - Importancia das variaveis",
      x = "Variavel",
      y = "Importancia"
    )
  print(grafico_importancia_arvore)
  salvar_grafico(grafico_importancia_arvore, "06_importancia_variaveis_arvore.png")
}

# ------------------------------------------------------------------------------
# SESSAO 7 - AVALIACAO E COMPARACAO DE MODELOS NO TESTE
# Objetivo da sessao:
# - Gerar probabilidades preditas na base de teste original.
# - Comparar curvas ROC no mesmo grafico e calcular AUC de cada modelo.
# - Discutir impacto do uppersample na regressao vs arvore em dado desbalanceado.
# ------------------------------------------------------------------------------

# Probabilidade prevista de classe I219 para cada modelo.
prob_logistica_completa_teste <- predict(modelo_logistico_completo, newdata = dados_teste, type = "response")
prob_logistica_stepwise_teste <- predict(modelo_logistico_stepwise, newdata = dados_teste, type = "response")
prob_arvore_teste <- as.numeric(predict(modelo_arvore_classificacao, newdata = dados_teste, type = "prob")[, "1"])

# Curvas ROC e AUC.
roc_logistica_completa <- pROC::roc(
  response = dados_teste$TARGET_I219,
  predictor = prob_logistica_completa_teste,
  levels = c(0, 1),
  direction = "<"
)
roc_logistica_stepwise <- pROC::roc(
  response = dados_teste$TARGET_I219,
  predictor = prob_logistica_stepwise_teste,
  levels = c(0, 1),
  direction = "<"
)
roc_arvore <- pROC::roc(
  response = dados_teste$TARGET_I219,
  predictor = prob_arvore_teste,
  levels = c(0, 1),
  direction = "<"
)

auc_logistica_completa <- as.numeric(pROC::auc(roc_logistica_completa))
auc_logistica_stepwise <- as.numeric(pROC::auc(roc_logistica_stepwise))
auc_arvore <- as.numeric(pROC::auc(roc_arvore))

cat("\nAUC - Regressao Logistica Completa:", round(auc_logistica_completa, 4), "\n")
cat("AUC - Regressao Logistica Stepwise (Oficial):", round(auc_logistica_stepwise, 4), "\n")
cat("AUC - Arvore de Decisao:", round(auc_arvore, 4), "\n")

# Curvas ROC no mesmo grafico para comparacao visual direta.
png(
  filename = file.path(pasta_saida_modelos, "09_curva_roc_comparacao_modelos.png"),
  width = 1800,
  height = 1200,
  res = 180
)
plot(roc_logistica_completa, col = "#2C7FB8", lwd = 2, legacy.axes = TRUE, main = "Curvas ROC - Logistica (Completa/Stepwise) vs Arvore")
lines(roc_logistica_stepwise, col = "#1B9E77", lwd = 2)
lines(roc_arvore, col = "#41AB5D", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray40")
legend(
  "bottomright",
  legend = c(
    paste("Logistica Completa (AUC =", round(auc_logistica_completa, 4), ")"),
    paste("Logistica Stepwise Oficial (AUC =", round(auc_logistica_stepwise, 4), ")"),
    paste("Arvore de Decisao (AUC =", round(auc_arvore, 4), ")")
  ),
  col = c("#2C7FB8", "#1B9E77", "#41AB5D"),
  lwd = 2,
  bty = "n"
)
dev.off()

# Metricas adicionais com limiar 0.50 para apoio didatico.
calcular_metricas_classificacao <- function(observado_binario, prob_predita, limiar = 0.50) {
  classe_predita <- dplyr::if_else(prob_predita >= limiar, 1L, 0L)
  vp <- sum(classe_predita == 1 & observado_binario == 1, na.rm = TRUE)
  vn <- sum(classe_predita == 0 & observado_binario == 0, na.rm = TRUE)
  fp <- sum(classe_predita == 1 & observado_binario == 0, na.rm = TRUE)
  fn <- sum(classe_predita == 0 & observado_binario == 1, na.rm = TRUE)

  acuracia <- (vp + vn) / (vp + vn + fp + fn)
  sensibilidade <- ifelse((vp + fn) > 0, vp / (vp + fn), NA_real_)
  especificidade <- ifelse((vn + fp) > 0, vn / (vn + fp), NA_real_)
  precisao <- ifelse((vp + fp) > 0, vp / (vp + fp), NA_real_)
  f1 <- ifelse(!is.na(precisao) && !is.na(sensibilidade) && (precisao + sensibilidade) > 0, 2 * (precisao * sensibilidade) / (precisao + sensibilidade), NA_real_)

  tibble::tibble(
    Acuracia = acuracia,
    Sensibilidade = sensibilidade,
    Especificidade = especificidade,
    Precisao = precisao,
    F1 = f1
  )
}

metricas_logistica_completa <- calcular_metricas_classificacao(dados_teste$TARGET_I219, prob_logistica_completa_teste) %>%
  mutate(Modelo = "Regressao_Logistica_Completa", AUC = auc_logistica_completa)
metricas_logistica_stepwise <- calcular_metricas_classificacao(dados_teste$TARGET_I219, prob_logistica_stepwise_teste) %>%
  mutate(Modelo = "Regressao_Logistica_Stepwise_Oficial", AUC = auc_logistica_stepwise)
metricas_arvore <- calcular_metricas_classificacao(dados_teste$TARGET_I219, prob_arvore_teste) %>%
  mutate(Modelo = "Arvore_Decisao_Treino_Original", AUC = auc_arvore)

metricas_finais <- bind_rows(metricas_logistica_completa, metricas_logistica_stepwise, metricas_arvore) %>%
  dplyr::select(Modelo, AUC, Acuracia, Sensibilidade, Especificidade, Precisao, F1) %>%
  arrange(desc(AUC))
cat("\nTabela final de metricas no teste:\n")
print(metricas_finais)
write_csv(metricas_finais, file.path(pasta_saida_modelos, "10_metricas_finais_teste.csv"))

# Summary especifico solicitado: Precision, Recall e Accuracy no teste.
summary_pra_teste <- metricas_finais %>%
  transmute(
    Modelo,
    Precision = Precisao,
    Recall = Sensibilidade,
    Accuracy = Acuracia
  )
cat("\nSummary de Precision, Recall e Accuracy no teste:\n")
print(summary_pra_teste)
write_csv(summary_pra_teste, file.path(pasta_saida_modelos, "10b_summary_precision_recall_accuracy_teste.csv"))

# Salva probabilidades previstas no teste para reproducao e auditoria.
predicoes_teste <- dados_teste %>%
  mutate(
    prob_logistica_completa = prob_logistica_completa_teste,
    prob_logistica_stepwise = prob_logistica_stepwise_teste,
    prob_arvore = prob_arvore_teste
  ) %>%
  dplyr::select(RACACOR, IDADEanos, TARGET_I219, TARGET_FATOR, prob_logistica_completa, prob_logistica_stepwise, prob_arvore)
write_csv(predicoes_teste, file.path(pasta_saida_modelos, "11_probabilidades_preditas_teste.csv"))

# Comentario automatico para discutir desempenho relativo.
modelo_melhor_auc <- metricas_finais %>% slice(1) %>% dplyr::pull(Modelo)
if (modelo_melhor_auc == "Regressao_Logistica_Stepwise_Oficial") {
  cat("\nInterpretacao didatica:\n")
  cat("- A Regressao Logistica Stepwise (modelo oficial) teve maior AUC no teste.\n")
  cat("- O stepwise reduziu o modelo mantendo apenas termos mais relevantes por AIC.\n")
  cat("- O uso de uppersample pode ter ajudado na classe minoritaria (I219).\n")
} else if (modelo_melhor_auc == "Regressao_Logistica_Completa") {
  cat("\nInterpretacao didatica:\n")
  cat("- A Regressao Logistica Completa teve maior AUC no teste.\n")
  cat("- Neste caso, o stepwise nao superou a versao completa.\n")
  cat("- A comparacao mostra a importancia de validar modelos no teste.\n")
} else if (modelo_melhor_auc == "Arvore_Decisao_Treino_Original") {
  cat("\nInterpretacao didatica:\n")
  cat("- A Arvore de Decisao teve maior AUC no teste.\n")
  cat("- Mesmo com treino desbalanceado, a arvore capturou regras nao lineares uteis.\n")
  cat("- O uppersample na logistica nem sempre garante melhor AUC.\n")
} else {
  cat("\nInterpretacao didatica:\n")
  cat("- Os modelos apresentaram desempenho semelhante no teste.\n")
  cat("- A comparacao entre completo, stepwise e arvore permanece essencial.\n")
}

# ------------------------------------------------------------------------------
# SESSAO 8 - SALVAMENTO DE SUMMARIES DOS MODELOS EM MARKDOWN
# Objetivo da sessao:
# - Registrar os resultados completos dos modelos em arquivos .md.
# - Facilitar compartilhamento e documentacao da aula.
# ------------------------------------------------------------------------------
sumario_logistico_completo_md <- capture.output(summary(modelo_logistico_completo))
sumario_logistico_stepwise_md <- capture.output(summary(modelo_logistico_stepwise))
sumario_arvore_md <- capture.output(summary(modelo_arvore_classificacao))

conteudo_md_logistico_completo <- c(
  "# Regressao Logistica Completa (Uppersample)",
  "",
  "## Summary de Precision, Recall e Accuracy (teste)",
  "",
  "| Modelo | Precision | Recall | Accuracy |",
  "|---|---:|---:|---:|",
  paste0(
    "| ", metricas_logistica_completa$Modelo,
    " | ", formatC(metricas_logistica_completa$Precisao, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_completa$Sensibilidade, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_completa$Acuracia, digits = 4, format = "f"),
    " |"
  ),
  "",
  "## Metricas de classificacao no teste",
  "",
  "| Modelo | AUC | Acuracia | Sensibilidade | Especificidade | Precisao | F1 |",
  "|---|---:|---:|---:|---:|---:|---:|",
  paste0(
    "| ", metricas_logistica_completa$Modelo,
    " | ", formatC(metricas_logistica_completa$AUC, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_completa$Acuracia, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_completa$Sensibilidade, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_completa$Especificidade, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_completa$Precisao, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_completa$F1, digits = 4, format = "f"),
    " |"
  ),
  "",
  "## Summary do modelo",
  "",
  "```r",
  sumario_logistico_completo_md,
  "```"
)
writeLines(conteudo_md_logistico_completo, file.path(pasta_saida_modelos, "modelo_regressao_logistica_completa.md"))

conteudo_md_logistico_stepwise <- c(
  "# Regressao Logistica Stepwise (MODELO OFICIAL)",
  "",
  "## Summary de Precision, Recall e Accuracy (teste)",
  "",
  "| Modelo | Precision | Recall | Accuracy |",
  "|---|---:|---:|---:|",
  paste0(
    "| ", metricas_logistica_stepwise$Modelo,
    " | ", formatC(metricas_logistica_stepwise$Precisao, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_stepwise$Sensibilidade, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_stepwise$Acuracia, digits = 4, format = "f"),
    " |"
  ),
  "",
  "## Metricas de classificacao no teste",
  "",
  "| Modelo | AUC | Acuracia | Sensibilidade | Especificidade | Precisao | F1 |",
  "|---|---:|---:|---:|---:|---:|---:|",
  paste0(
    "| ", metricas_logistica_stepwise$Modelo,
    " | ", formatC(metricas_logistica_stepwise$AUC, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_stepwise$Acuracia, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_stepwise$Sensibilidade, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_stepwise$Especificidade, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_stepwise$Precisao, digits = 4, format = "f"),
    " | ", formatC(metricas_logistica_stepwise$F1, digits = 4, format = "f"),
    " |"
  ),
  "",
  "## Summary do modelo",
  "",
  "```r",
  sumario_logistico_stepwise_md,
  "```"
)
writeLines(conteudo_md_logistico_stepwise, file.path(pasta_saida_modelos, "modelo_regressao_logistica_stepwise_oficial.md"))

conteudo_md_arvore <- c(
  "# Arvore de Decisao (Treino Original)",
  "",
  "## Summary de Precision, Recall e Accuracy (teste)",
  "",
  "| Modelo | Precision | Recall | Accuracy |",
  "|---|---:|---:|---:|",
  paste0(
    "| ", metricas_arvore$Modelo,
    " | ", formatC(metricas_arvore$Precisao, digits = 4, format = "f"),
    " | ", formatC(metricas_arvore$Sensibilidade, digits = 4, format = "f"),
    " | ", formatC(metricas_arvore$Acuracia, digits = 4, format = "f"),
    " |"
  ),
  "",
  "## Metricas de classificacao no teste",
  "",
  "| Modelo | AUC | Acuracia | Sensibilidade | Especificidade | Precisao | F1 |",
  "|---|---:|---:|---:|---:|---:|---:|",
  paste0(
    "| ", metricas_arvore$Modelo,
    " | ", formatC(metricas_arvore$AUC, digits = 4, format = "f"),
    " | ", formatC(metricas_arvore$Acuracia, digits = 4, format = "f"),
    " | ", formatC(metricas_arvore$Sensibilidade, digits = 4, format = "f"),
    " | ", formatC(metricas_arvore$Especificidade, digits = 4, format = "f"),
    " | ", formatC(metricas_arvore$Precisao, digits = 4, format = "f"),
    " | ", formatC(metricas_arvore$F1, digits = 4, format = "f"),
    " |"
  ),
  "",
  "## Summary do modelo",
  "",
  "```r",
  sumario_arvore_md,
  "```"
)
writeLines(conteudo_md_arvore, file.path(pasta_saida_modelos, "modelo_arvore_decisao.md"))

# Salva um markdown consolidado apenas com o summary solicitado (ambos os modelos).
conteudo_md_pra <- c(
  "# Summary de Precision, Recall e Accuracy (Teste)",
  "",
  "| Modelo | Precision | Recall | Accuracy |",
  "|---|---:|---:|---:|",
  paste0(
    "| ", summary_pra_teste$Modelo,
    " | ", formatC(summary_pra_teste$Precision, digits = 4, format = "f"),
    " | ", formatC(summary_pra_teste$Recall, digits = 4, format = "f"),
    " | ", formatC(summary_pra_teste$Accuracy, digits = 4, format = "f"),
    " |"
  )
)
writeLines(conteudo_md_pra, file.path(pasta_saida_modelos, "summary_precision_recall_accuracy_teste.md"))
