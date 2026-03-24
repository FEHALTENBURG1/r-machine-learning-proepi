# ==============================================================================
# AULA 4 - INTRODUCAO A TESTES DE HIPOTESE
# Curso: Analise de Dados e Machine Learning com R - ProEpi 2026
# ------------------------------------------------------------------------------
# Objetivo geral da aula:
# - Introduzir testes de hipotese para comparacao de proporcoes, medias, medianas
#   e distribuicoes.
# - Relacionar cada teste com sua distribuicao teorica por meio de graficos.
# - Aplicar os testes nos dados tratados da aula 2 e interpretar os resultados.
# ==============================================================================

# ------------------------------------------------------------------------------
# SESSAO 1 - TODOS OS IMPORTS DE PACOTES E DADOS
# Objetivo da sessao:
# - Carregar pacotes necessarios para testes, tabelas e visualizacoes.
# - Importar os arquivos .RData gerados na aula 2.
# - Preparar bases auxiliares para os testes da aula 4.
# ------------------------------------------------------------------------------
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")

library(tidyverse)

raiz_dados_processados <- "dados_processados_datasus"
load(file.path(raiz_dados_processados, "sinasc_modelagem.RData"))
load(file.path(raiz_dados_processados, "sim_modelagem.RData"))
load(file.path(raiz_dados_processados, "sia_modelagem.RData"))
load(file.path(raiz_dados_processados, "sih_modelagem.RData"))
load(file.path(raiz_dados_processados, "sinan_modelagem.RData"))

dim(df_sinasc_modelagem)
dim(df_sim_modelagem)
dim(df_sia_modelagem)
dim(df_sih_modelagem)
dim(df_sinan_modelagem)

# ------------------------------------------------------------------------------
# SESSAO 2 - TESTES PARA DISTRIBUICOES DISCRETAS/PROPORCOES
# SISTEMAS: SISTEMA DE INFORMACOES HOSPITALARES (SIH) E
#           SISTEMA DE INFORMACOES AMBULATORIAIS (SIA)
# Objetivo da sessao:
# - Aplicar Qui-quadrado e Exato de Fisher em tabelas de contingencia.
# - Visualizar a distribuicao teorica de cada teste.
# - Comparar observado vs esperado e mapear residuos do Qui-quadrado.
# ------------------------------------------------------------------------------

# Explicacao curta:
# - Qui-quadrado testa se duas variaveis categoricas sao independentes.
# - Exato de Fisher testa associacao em tabelas de contingencia sem depender de
#   aproximacoes assintoticas (especialmente util em amostras pequenas).
# Quando usar:
# - Qui-quadrado: quando as duas variaveis sao categoricas e as frequencias
#   esperadas da tabela nao sao muito pequenas.
# - Fisher: quando ha celulas com contagens baixas, amostra pequena ou quando
#   se deseja um teste mais conservador para tabela de contingencia.
# Pressupostos principais (Qui-quadrado/Fisher):
# - Observacoes independentes (um registro nao deve influenciar outro).
# - Categorias bem definidas e mutuamente exclusivas.
# - Para Qui-quadrado, idealmente frequencias esperadas >= 5 na maior parte das
#   celulas (se isso falhar, Fisher costuma ser preferivel).

# Grafico da distribuicao teorica do Qui-quadrado (simulada).
df_teorico_quiquadrado <- tibble(estatistica = rchisq(10000, df = 4))
ggplot(df_teorico_quiquadrado, aes(x = estatistica)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "#2C7FB8", color = "white") +
  stat_function(fun = dchisq, args = list(df = 4), color = "#B2182B", linewidth = 1) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Distribuicao teorica do Qui-quadrado (df = 4)",
    x = "Estatistica qui-quadrado",
    y = "Densidade"
  )

# Tabela SIH: SEXO x grupo de CID principal.
# Como nao houve obitos no periodo, substituimos MORTE por um desfecho
# categorico derivado de DIAG_PRINC (CID principal mais frequente vs demais).
dados_sih_quiquadrado <- df_sih_modelagem %>%
  filter(!is.na(SEXO), !is.na(DIAG_PRINC)) %>%
  mutate(
    SEXO = as.factor(SEXO),
    DIAG_PRINC = as.character(DIAG_PRINC)
  )
cid_principal_mais_frequente <- dados_sih_quiquadrado %>%
  count(DIAG_PRINC, sort = TRUE) %>%
  slice(1) %>%
  pull(DIAG_PRINC)
dados_sih_quiquadrado <- dados_sih_quiquadrado %>%
  mutate(
    DIAG_PRINC_GRUPO = factor(
      if_else(
        DIAG_PRINC == cid_principal_mais_frequente,
        paste("CID principal =", cid_principal_mais_frequente),
        "Outros CID principais"
      )
    )
  )
tabela_sih_sexo_diag <- table(dados_sih_quiquadrado$SEXO, dados_sih_quiquadrado$DIAG_PRINC_GRUPO)
tabela_sih_sexo_diag

# Grafico de comparacao da tabela observada (SEXO x grupo de CID principal).
as.data.frame(tabela_sih_sexo_diag) %>%
  ggplot(aes(x = Var1, y = Freq, fill = Var2)) +
  geom_col(position = "dodge") +
  theme_minimal(base_size = 11) +
  labs(
    title = "SIH - Contagens observadas: SEXO x grupo de CID principal",
    x = "SEXO",
    y = "Frequencia",
    fill = "Grupo CID principal"
  )

# Testes Qui-quadrado e Exato de Fisher para SIH.
teste_quiquadrado_sih <- chisq.test(tabela_sih_sexo_diag)
teste_fisher_sih <- fisher.test(tabela_sih_sexo_diag)

# Prints dos resultados (sumarios principais).
teste_quiquadrado_sih
teste_fisher_sih

# Mapa de calor dos residuos padronizados do Qui-quadrado (SIH).
as_tibble(as.data.frame(as.table(teste_quiquadrado_sih$stdres))) %>%
  rename(SEXO = Var1, GRUPO_CID = Var2, residuo_padronizado = Freq) %>%
  ggplot(aes(x = GRUPO_CID, y = SEXO, fill = residuo_padronizado)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(residuo_padronizado, 2)), size = 4) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0) +
  theme_minimal(base_size = 11) +
  labs(
    title = "SIH - Mapa de calor dos residuos (Qui-quadrado)",
    x = "Grupo CID principal",
    y = "SEXO",
    fill = "Residuo"
  )

# Grafico observado vs esperado para Qui-quadrado (SIH).
bind_rows(
  as_tibble(as.data.frame(as.table(teste_quiquadrado_sih$observed))) %>% mutate(tipo = "Observado"),
  as_tibble(as.data.frame(as.table(teste_quiquadrado_sih$expected))) %>% mutate(tipo = "Esperado")
) %>%
  rename(SEXO = Var1, GRUPO_CID = Var2, frequencia = Freq) %>%
  ggplot(aes(x = interaction(SEXO, GRUPO_CID, sep = " x "), y = frequencia, fill = tipo)) +
  geom_col(position = "dodge") +
  theme_minimal(base_size = 11) +
  labs(
    title = "SIH - Qui-quadrado: observado vs esperado",
    x = "Combinacao SEXO x grupo CID principal",
    y = "Frequencia",
    fill = "Tipo"
  )

# Grafico da distribuicao teorica relacionada ao Exato de Fisher (hipergeometrica).
# A simulacao abaixo usa as margens da tabela SEXO x grupo CID principal para
# ilustrar a distribuicao da celula [1,1] sob hipotese nula.
sim_fisher_sih <- rhyper(
  nn = 10000,
  m = sum(tabela_sih_sexo_diag[, 1]),
  n = sum(tabela_sih_sexo_diag[, 2]),
  k = sum(tabela_sih_sexo_diag[1, ])
)
tibble(valor = sim_fisher_sih) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#41AB5D", color = "white") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Distribuicao teorica (simulada) para Exato de Fisher",
    subtitle = "Celula [1,1] sob distribuicao hipergeometrica",
    x = "Contagem simulada da celula",
    y = "Densidade"
  )

# Tabela SIA: AP_RACACOR x AP_SEXO.
tabela_sia_racacor_sexo <- table(df_sia_modelagem$AP_RACACOR, df_sia_modelagem$AP_SEXO)
tabela_sia_racacor_sexo

# Grafico da tabela observada (AP_RACACOR x AP_SEXO).
as.data.frame(tabela_sia_racacor_sexo) %>%
  ggplot(aes(x = Var1, y = Freq, fill = Var2)) +
  geom_col(position = "dodge") +
  theme_minimal(base_size = 11) +
  labs(
    title = "SIA - Contagens observadas: AP_RACACOR x AP_SEXO",
    x = "AP_RACACOR",
    y = "Frequencia",
    fill = "AP_SEXO"
  )

# Testes Qui-quadrado e Exato de Fisher para SIA.
teste_quiquadrado_sia <- chisq.test(tabela_sia_racacor_sexo)
teste_fisher_sia <- fisher.test(tabela_sia_racacor_sexo, simulate.p.value = TRUE, B = 10000)

# Prints dos resultados (sumarios principais).
teste_quiquadrado_sia
teste_fisher_sia

# Mapa de calor dos residuos padronizados do Qui-quadrado (SIA).
as_tibble(as.data.frame(as.table(teste_quiquadrado_sia$stdres))) %>%
  rename(AP_RACACOR = Var1, AP_SEXO = Var2, residuo_padronizado = Freq) %>%
  ggplot(aes(x = AP_SEXO, y = AP_RACACOR, fill = residuo_padronizado)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(residuo_padronizado, 2)), size = 4) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0) +
  theme_minimal(base_size = 11) +
  labs(
    title = "SIA - Mapa de calor dos residuos (Qui-quadrado)",
    x = "AP_SEXO",
    y = "AP_RACACOR",
    fill = "Residuo"
  )

# Grafico observado vs esperado para Qui-quadrado (SIA).
bind_rows(
  as_tibble(as.data.frame(as.table(teste_quiquadrado_sia$observed))) %>% mutate(tipo = "Observado"),
  as_tibble(as.data.frame(as.table(teste_quiquadrado_sia$expected))) %>% mutate(tipo = "Esperado")
) %>%
  rename(AP_RACACOR = Var1, AP_SEXO = Var2, frequencia = Freq) %>%
  ggplot(aes(x = interaction(AP_RACACOR, AP_SEXO, sep = " x "), y = frequencia, fill = tipo)) +
  geom_col(position = "dodge") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "SIA - Qui-quadrado: observado vs esperado",
    x = "Combinacao AP_RACACOR x AP_SEXO",
    y = "Frequencia",
    fill = "Tipo"
  )

# ------------------------------------------------------------------------------
# SESSAO 3 - TESTE t-STUDENT E ANOVA
# SISTEMA: SISTEMA DE INFORMACOES HOSPITALARES (SIH)
# Objetivo da sessao:
# - Comparar VAL_TOT entre grupos de SEXO.
# - Verificar normalidade por grupo (Shapiro) antes do t-Student.
# - Aplicar t-Student e ANOVA e interpretar os resultados.
# ------------------------------------------------------------------------------

# Explicacao curta:
# - t-Student compara medias entre dois grupos.
# - ANOVA compara medias entre dois ou mais grupos por variancia entre grupos.
# - O t-Student pressupoe normalidade aproximada em cada grupo; sem isso, o
#   resultado pode ser enganoso.
# Quando usar:
# - t-Student: comparar media de uma variavel numerica entre dois grupos
#   independentes (ex.: VAL_TOT por SEXO).
# - ANOVA: comparar media de uma variavel numerica entre dois ou mais grupos.
# Pressupostos principais dos testes parametricos (t e ANOVA):
# - Variavel resposta numerica continua.
# - Independencia entre observacoes.
# - Normalidade aproximada dos residuos (ou da variavel por grupo, de forma
#   pratica/didatica nesta aula).
# - Homogeneidade de variancias entre grupos (variancias semelhantes).
# Observacao didatica:
# - Com amostras grandes, pequenas violacoes de normalidade tendem a ter menor
#   impacto; com amostras pequenas, os pressupostos ficam mais criticos.

# Distribuicao teorica do teste t (simulada).
tibble(estatistica = rt(10000, df = 30)) %>%
  ggplot(aes(x = estatistica)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "#2C7FB8", color = "white") +
  stat_function(fun = dt, args = list(df = 30), color = "#B2182B", linewidth = 1) +
  theme_minimal(base_size = 11) +
  labs(title = "Distribuicao teorica t-Student (df = 30)", x = "Estatistica t", y = "Densidade")

# Distribuicao teorica do teste F (ANOVA) simulada.
tibble(estatistica = rf(10000, df1 = 2, df2 = 60)) %>%
  ggplot(aes(x = estatistica)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "#41AB5D", color = "white") +
  stat_function(fun = df, args = list(df1 = 2, df2 = 60), color = "#B2182B", linewidth = 1) +
  theme_minimal(base_size = 11) +
  labs(title = "Distribuicao teorica F (ANOVA)", x = "Estatistica F", y = "Densidade")

# Dados para comparacao de VAL_TOT por SEXO.
dados_valtot <- df_sih_modelagem %>%
  filter(!is.na(SEXO), !is.na(VAL_TOT)) %>%
  mutate(SEXO = droplevels(SEXO))

# Histograma de VAL_TOT por SEXO.
for (nivel_sexo in levels(dados_valtot$SEXO)) {
  dados_plot <- dados_valtot %>% filter(SEXO == nivel_sexo)
  print(
    ggplot(dados_plot, aes(x = VAL_TOT)) +
      geom_histogram(aes(y = after_stat(count)), bins = 30, fill = "#7570B3", color = "white") +
      theme_minimal(base_size = 11) +
      labs(
        title = paste("SIH - Histograma de VAL_TOT para SEXO =", nivel_sexo),
        x = "VAL_TOT",
        y = "Frequencia"
      )
  )
}

# Teste de Shapiro por grupo de SEXO para checar normalidade.
for (nivel_sexo in levels(dados_valtot$SEXO)) {
  dados_plot <- dados_valtot %>% filter(SEXO == nivel_sexo)
  if (nrow(dados_plot) >= 3 && nrow(dados_plot) <= 5000) {
    cat("\nShapiro para VAL_TOT no grupo SEXO =", nivel_sexo, "\n")
    print(shapiro.test(dados_plot$VAL_TOT))
  }
}

# Grafico comparando cada grupo com distribuicao normal teorica (QQ plot).
for (nivel_sexo in levels(dados_valtot$SEXO)) {
  dados_plot <- dados_valtot %>% filter(SEXO == nivel_sexo)
  print(
    ggplot(dados_plot, aes(sample = VAL_TOT)) +
      stat_qq(color = "#2C7FB8", alpha = 0.6) +
      stat_qq_line(color = "#B2182B", linewidth = 0.8) +
      theme_minimal(base_size = 11) +
      labs(
        title = paste("SIH - QQ plot de VAL_TOT para SEXO =", nivel_sexo),
        subtitle = "Comparacao da amostra com distribuicao normal teorica",
        x = "Quantis teoricos (normal)",
        y = "Quantis observados"
      )
  )
}

# t-Student para comparar medias de VAL_TOT entre grupos de SEXO.
teste_t_valtot <- t.test(VAL_TOT ~ SEXO, data = dados_valtot)
cat("\nResultado do teste t-Student (VAL_TOT ~ SEXO)\n")
print(teste_t_valtot)
#Nota didatica: o t-Student pressupoe normalidade aproximada nos grupos; sem normalidade, o p-valor pode ser enganoso.

# ANOVA para comparar medias de VAL_TOT entre grupos de SEXO.
modelo_anova_valtot <- aov(VAL_TOT ~ SEXO, data = dados_valtot)
cat("\nResultado da ANOVA (VAL_TOT ~ SEXO)\n")
print(summary(modelo_anova_valtot))

# Boxplot para visualizar diferencas entre grupos (apoio ao t e ANOVA).
ggplot(dados_valtot, aes(x = SEXO, y = VAL_TOT, fill = SEXO)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal(base_size = 11) +
  labs(
    title = "SIH - Comparacao de VAL_TOT por SEXO",
    x = "SEXO",
    y = "VAL_TOT",
    fill = "SEXO"
  )

# ------------------------------------------------------------------------------
# SESSAO 4 - TESTE DE WILCOXON-MANN-WHITNEY (NAO PARAMETRICO)
# SISTEMA: SISTEMA DE INFORMACOES HOSPITALARES (SIH)
# Objetivo da sessao:
# - Comparar QT_DIARIAS entre grupos de SEXO sem supor normalidade.
# - Mostrar a distribuicao teorica (simulada) da estatistica W.
# - Relacionar resultado numerico com graficos de distribuicao dos grupos.
# ------------------------------------------------------------------------------

# Explicacao curta:
# - O teste de Wilcoxon-Mann-Whitney compara a posicao (tendencia central/ranks)
#   de dois grupos independentes sem exigir normalidade dos dados.
# Quando usar:
# - Alternativa ao t-Student quando a normalidade e questionavel, ha assimetria
#   forte ou presenca de outliers relevantes.
# Pressupostos principais:
# - Dois grupos independentes.
# - Variavel ao menos ordinal (ou numerica).
# - Interpretacao mais direta quando as distribuicoes dos grupos tem formato
#   semelhante (mudando principalmente a localizacao).

# Dados para comparacao de QT_DIARIAS por SEXO.
dados_qt_diarias <- df_sih_modelagem %>%
  filter(!is.na(SEXO), !is.na(QT_DIARIAS)) %>%
  mutate(SEXO = droplevels(SEXO))

# Histograma de QT_DIARIAS por SEXO.
for (nivel_sexo in levels(dados_qt_diarias$SEXO)) {
  dados_plot <- dados_qt_diarias %>% filter(SEXO == nivel_sexo)
  print(
    ggplot(dados_plot, aes(x = QT_DIARIAS)) +
      geom_histogram(aes(y = after_stat(count)), bins = 30, fill = "#7570B3", color = "white") +
      theme_minimal(base_size = 11) +
      labs(
        title = paste("SIH - Histograma de QT_DIARIAS para SEXO =", nivel_sexo),
        x = "QT_DIARIAS",
        y = "Frequencia"
      )
  )
}

# Teste de Shapiro por grupo para mostrar que normalidade pode nao ser atendida.
for (nivel_sexo in levels(dados_qt_diarias$SEXO)) {
  dados_plot <- dados_qt_diarias %>% filter(SEXO == nivel_sexo)
  if (nrow(dados_plot) >= 3 && nrow(dados_plot) <= 5000) {
    cat("\nShapiro para QT_DIARIAS no grupo SEXO =", nivel_sexo, "\n")
    print(shapiro.test(dados_plot$QT_DIARIAS))
  }
}

# Distribuicao teorica (simulada sob H0) da estatistica W.
n_grupo_1 <- sum(dados_qt_diarias$SEXO == levels(dados_qt_diarias$SEXO)[1])
n_grupo_2 <- sum(dados_qt_diarias$SEXO == levels(dados_qt_diarias$SEXO)[2])
sim_w <- replicate(
  3000,
  wilcox.test(rnorm(n_grupo_1), rnorm(n_grupo_2), exact = FALSE)$statistic[[1]]
)
tibble(estatistica_w = sim_w) %>%
  ggplot(aes(x = estatistica_w)) +
  geom_histogram(aes(y = after_stat(count)), bins = 35, fill = "#2C7FB8", color = "white") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Distribuicao teorica (simulada) da estatistica W",
    subtitle = "Teste de Wilcoxon-Mann-Whitney sob H0",
    x = "Estatistica W",
    y = "Frequencia"
  )

# Teste de Wilcoxon-Mann-Whitney (aproximacao normal).
teste_wilcox_aprox <- wilcox.test(QT_DIARIAS ~ SEXO, data = dados_qt_diarias, exact = FALSE)
cat("\nResultado do Wilcoxon-Mann-Whitney (aproximacao)\n")
print(teste_wilcox_aprox)

# Teste de Wilcoxon-Mann-Whitney (p-valor exato, quando possivel).
# Comentario didatico:
# Este comando e um exercicio para tentar o p-valor exato (exact = TRUE).
# O warning "cannot compute exact p-value with ties" significa que existem
# empates (valores repetidos de QT_DIARIAS) entre as observacoes, e por isso o
# R nao consegue calcular o p-valor exato do Wilcoxon; nesse caso, ele usa uma
# aproximacao para o p-valor.
teste_wilcox_exato <- wilcox.test(QT_DIARIAS ~ SEXO, data = dados_qt_diarias, exact = TRUE)
cat("\nResultado do Wilcoxon-Mann-Whitney (exato)\n")
print(teste_wilcox_exato)

# Comparacao da estatistica observada com distribuicao teorica simulada.
tibble(estatistica_w = sim_w) %>%
  ggplot(aes(x = estatistica_w)) +
  geom_histogram(aes(y = after_stat(count)), bins = 35, fill = "#41AB5D", color = "white") +
  geom_vline(xintercept = teste_wilcox_aprox$statistic[[1]], color = "#B2182B", linewidth = 1) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Wilcoxon - Estatistica observada vs distribuicao teorica",
    subtitle = "Linha vermelha = W observado nos dados SIH",
    x = "Estatistica W",
    y = "Frequencia"
  )

# Boxplot para comparar grupos (apoio visual ao Wilcoxon).
ggplot(dados_qt_diarias, aes(x = SEXO, y = QT_DIARIAS, fill = SEXO)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal(base_size = 11) +
  labs(
    title = "SIH - QT_DIARIAS por SEXO",
    x = "SEXO",
    y = "QT_DIARIAS",
    fill = "SEXO"
  )

# ------------------------------------------------------------------------------
# SESSAO 5 - TESTE DE KOLMOGOROV-SMIRNOV (COMPARACAO DE DISTRIBUICOES)
# SISTEMA: SISTEMA DE INFORMACOES HOSPITALARES (SIH)
# Objetivo da sessao:
# - Comparar variaveis continuas com distribuicoes teoricas.
# - Aplicar KS contra normal padrao e contra outra distribuicao conhecida.
# - Usar graficos para comparar dados observados e funcoes teoricas.
# ------------------------------------------------------------------------------

# Explicacao curta:
# - O teste KS mede a maior distancia entre a CDF empirica dos dados e a CDF
#   teorica escolhida.
# Quando usar:
# - Para comparar uma amostra com uma distribuicao teorica especifica
#   (ex.: normal, exponencial) e avaliar aderencia global.
# - Util como triagem didatica de distribuicao, junto com histograma e curva
#   teorica sobreposta.
# Pressupostos e cuidados:
# - Dados devem ser independentes.
# - Para KS de uma amostra, a distribuicao teorica precisa estar bem definida.
# - Quando parametros da distribuicao sao estimados da propria amostra, o
#   p-valor do KS pode ficar otimista (interpretar com cautela).
# - Em amostras muito grandes, o KS detecta diferencas pequenas; por isso,
#   sempre combinar com avaliacao visual e contexto pratico.

# Distribuicao teorica (simulada) da estatistica D do KS sob H0.
sim_d_ks <- replicate(
  4000,
  ks.test(rnorm(120), "pnorm")$statistic[[1]]
)
tibble(estatistica_d = sim_d_ks) %>%
  ggplot(aes(x = estatistica_d)) +
  geom_histogram(aes(y = after_stat(count)), bins = 35, fill = "#2C7FB8", color = "white") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Distribuicao teorica (simulada) da estatistica D - KS",
    x = "Estatistica D",
    y = "Frequencia"
  )

# Variaveis solicitadas para comparacao de distribuicoes no SIH.
variaveis_ks <- c("VAL_SH", "VAL_SP", "VAL_TOT")
variaveis_ks <- variaveis_ks[variaveis_ks %in% names(df_sih_modelagem)]

for (var in variaveis_ks) {
  x <- df_sih_modelagem[[var]]
  x <- x[is.finite(x)]
  x <- x[!is.na(x)]

  # Comparacao com normal padrao: padroniza para media 0 e desvio 1.
  z <- as.numeric(scale(x))
  teste_ks_normal <- ks.test(z, "pnorm")

  cat("\nKS para", var, "vs Normal Padrao\n")
  print(teste_ks_normal)

  # Grafico do dado padronizado vs curva normal teorica.
  print(
    tibble(valor = z) %>%
      ggplot(aes(x = valor)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#7570B3", color = "white") +
      stat_function(fun = dnorm, color = "#B2182B", linewidth = 1) +
      theme_minimal(base_size = 11) +
      labs(
        title = paste("SIH -", var, "padronizada vs Normal padrao"),
        subtitle = paste("KS p-valor =", signif(teste_ks_normal$p.value, 4)),
        x = paste(var, "(padronizada)"),
        y = "Densidade"
      )
  )

  # Comparacao com outra distribuicao famosa: Exponencial.
  # Ajusta para valores estritamente positivos antes de aplicar Exponencial.
  x_pos <- x - min(x, na.rm = TRUE) + 1e-6
  taxa_exp <- 1 / mean(x_pos, na.rm = TRUE)
  teste_ks_exponencial <- ks.test(x_pos, "pexp", rate = taxa_exp)

  cat("\nKS para", var, "vs Exponencial\n")
  print(teste_ks_exponencial)

  # Grafico da variavel positiva vs curva exponencial teorica.
  print(
    tibble(valor = x_pos) %>%
      ggplot(aes(x = valor)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#41AB5D", color = "white") +
      stat_function(fun = dexp, args = list(rate = taxa_exp), color = "#B2182B", linewidth = 1) +
      theme_minimal(base_size = 11) +
      labs(
        title = paste("SIH -", var, "ajustada vs Exponencial"),
        subtitle = paste("KS p-valor =", signif(teste_ks_exponencial$p.value, 4)),
        x = paste(var, "(ajustada para positivo)"),
        y = "Densidade"
      )
  )
}
