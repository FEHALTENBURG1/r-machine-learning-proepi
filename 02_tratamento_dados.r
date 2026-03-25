# ==============================================================================
# AULA 2 - TRATAMENTO DE DADOS
# Curso: Analise de Dados e Machine Learning com R - ProEpi 2026
# ------------------------------------------------------------------------------
# Objetivo geral da aula:
# - Padronizar os dados dos sistemas de informacao em tipos adequados para analise.
# - Aplicar tratamento de outliers e valores ausentes de forma didatica.
# - Salvar uma base tratada por sistema para uso na aula de analise exploratoria.
# ==============================================================================

# Instala tidyverse se necessario para leitura, transformacao e graficos.
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
# Carrega tidyverse.
library(tidyverse)
# Carrega lubridate para trabalhar com datas.
library(lubridate)
# Instala patchwork se necessario para juntar graficos.
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
# Carrega patchwork para combinar histogramas e boxplots na mesma imagem.
library(patchwork)

# Define a pasta de saida dos arquivos tratados.
raiz_dados_processados <- "dados_processados_datasus"
# Cria a pasta de saida caso nao exista.
dir.create(raiz_dados_processados, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# SESSAO 1 - SISTEMA DE INFORMACOES AMBULATORIAIS (SIA)
# Objetivo da sessao:
# - Importar e padronizar tipos de variaveis do SIA.
# - Tratar outliers e valores ausentes com tecnicas didaticas.
# - Salvar o arquivo final do SIA para modelagem.
# ------------------------------------------------------------------------------

# Importa o arquivo do SIA.
caminho_sia <- file.path("dados_originais_datasus", "sia", "sia_DF_2023_2024_variaveis_e10.csv")
# Le o arquivo do SIA com codificacao Latin1.
df_sia_variaveis <- read_csv(caminho_sia, locale = locale(encoding = "Latin1"), show_col_types = FALSE)
# Mostra estrutura inicial.
glimpse(df_sia_variaveis)

# Calcula largura maxima dos IDs para completar com zero a esquerda.
max_digitos_ap_autoriz <- max(nchar(as.character(df_sia_variaveis$AP_AUTORIZ)), na.rm = TRUE)
# Calcula largura maxima dos IDs de unidade.
max_digitos_ap_coduni <- max(nchar(as.character(df_sia_variaveis$AP_CODUNI)), na.rm = TRUE)

# Converte e padroniza tipos principais do SIA.
df_sia_variaveis <- df_sia_variaveis %>%
  mutate(
    AP_AUTORIZ = str_pad(as.character(AP_AUTORIZ), width = max_digitos_ap_autoriz, side = "left", pad = "0"), # ID em texto com zero a esquerda.
    AP_CODUNI = str_pad(as.character(AP_CODUNI), width = max_digitos_ap_coduni, side = "left", pad = "0"), # ID em texto com zero a esquerda.
    AP_CNSPCN = as.character(AP_CNSPCN), # Mantem CNS como texto.
    AP_MVM = ymd(paste0(str_pad(as.character(AP_MVM), width = 6, side = "left", pad = "0"), "01")), # Converte YYYYMM para Date (dia 01).
    AP_NUIDADE = as.integer(AP_NUIDADE), # Converte para inteiro.
    AP_SEXO = factor(case_when(AP_SEXO %in% c("M", "1", 1) ~ 1L, AP_SEXO %in% c("F", "2", 2) ~ 2L, TRUE ~ NA_integer_), levels = c(1L, 2L), labels = c("Masculino", "Feminino")), # Recodifica sexo para fator nominal.
    AP_RACACOR = as.factor(AP_RACACOR), # Converte raca/cor para fator nominal.
    AP_OBITO = factor(as.integer(AP_OBITO), levels = c(0L, 1L), labels = c("Nao", "Sim")), # Converte obito para fator nominal binario.
    AP_ENCERR = factor(as.integer(AP_ENCERR), levels = c(0L, 1L), labels = c("Nao", "Sim")), # Converte encerramento para fator nominal binario.
    AM_TRANSPL = factor(case_when(AM_TRANSPL == "S" ~ 1L, AM_TRANSPL == "N" ~ 0L, TRUE ~ NA_integer_), levels = c(0L, 1L), labels = c("Nao", "Sim")), # Recodifica transplante para fator nominal binario.
    AP_CIDPRI = as.factor(AP_CIDPRI) # Converte CID principal para fator nominal.
  )

# Missing 1: remove colunas 100% NA.
df_sia_variaveis <- df_sia_variaveis %>% select(where(~ !all(is.na(.x))))
# Missing 2: remove colunas com menos de 90% de preenchimento.
completude_sia <- colMeans(!is.na(df_sia_variaveis))
# Mantem somente colunas com pelo menos 90%.
df_sia_variaveis <- df_sia_variaveis[, completude_sia >= 0.90, drop = FALSE]
# Missing 3: imputa mediana para colunas numericas.
# Observacao: media ou moda tambem podem ser usadas conforme o contexto.
numericas_sia <- names(df_sia_variaveis)[sapply(df_sia_variaveis, is.numeric)]
for (coluna in numericas_sia) df_sia_variaveis[[coluna]][is.na(df_sia_variaveis[[coluna]])] <- median(df_sia_variaveis[[coluna]], na.rm = TRUE)

# Define objeto final do SIA.
df_sia_modelagem <- df_sia_variaveis
# Salva SIA em RData.
save(df_sia_modelagem, file = file.path(raiz_dados_processados, "sia_modelagem.RData"))

# ------------------------------------------------------------------------------
# SESSAO 2 - SISTEMA DE INFORMACOES HOSPITALARES (SIH)
# Objetivo da sessao:
# - Importar e padronizar tipos de variaveis do SIH.
# - Comparar e aplicar tecnicas de tratamento de outliers e missing values.
# - Salvar o arquivo final do SIH para modelagem.
# ------------------------------------------------------------------------------

# Importa o arquivo do SIH.
caminho_sih <- file.path("dados_originais_datasus", "sih", "sih_DF_2024_06_variaveis_j18.csv")
# Le o arquivo do SIH.
df_sih_variaveis <- read_csv(caminho_sih, show_col_types = FALSE)
# Mostra estrutura inicial.
glimpse(df_sih_variaveis)

# Converte e padroniza tipos principais do SIH.
df_sih_variaveis <- df_sih_variaveis %>%
  mutate(
    ANO_CMPT = as.integer(ANO_CMPT), # Converte para inteiro.
    MES_CMPT = as.integer(MES_CMPT), # Converte para inteiro.
    DT_INTER = as.Date(DT_INTER), # Converte para Date.
    DT_SAIDA = as.Date(DT_SAIDA), # Converte para Date.
    DIAG_PRINC = as.factor(DIAG_PRINC), # Converte para fator nominal.
    QT_DIARIAS = as.numeric(QT_DIARIAS), # Converte para float.
    NASC = as.Date(NASC), # Converte para Date.
    SEXO = factor(case_when(SEXO %in% c("Masculino", "M", "1", 1) ~ "Masculino", SEXO %in% c("Feminino", "F", "2", 2) ~ "Feminino", TRUE ~ NA_character_), levels = c("Masculino", "Feminino")), # Recodifica sexo para fator nominal.
    MORTE = factor(case_when(MORTE %in% c("Sim", "1", 1) ~ 1L, MORTE %in% c("Nao", "Não", "0", 0) ~ 0L, TRUE ~ NA_integer_), levels = c(0L, 1L), labels = c("Nao", "Sim")), # Recodifica obito para fator nominal binario.
    VAL_SH = parse_number(as.character(VAL_SH), locale = locale(decimal_mark = ".")), # Converte para float.
    VAL_SP = parse_number(as.character(VAL_SP), locale = locale(decimal_mark = ".")), # Converte para float.
    VAL_TOT = parse_number(as.character(VAL_TOT), locale = locale(decimal_mark = ".")) # Converte para float.
  )

# Outliers: identifica variaveis numericas.
numericas_sih <- names(df_sih_variaveis)[sapply(df_sih_variaveis, is.numeric)]
# Calcula percentis para limites de outliers.
p05_sih <- sapply(df_sih_variaveis[numericas_sih], function(x) as.numeric(quantile(x, probs = 0.05, na.rm = TRUE)))
# Calcula percentis superiores.
p95_sih <- sapply(df_sih_variaveis[numericas_sih], function(x) as.numeric(quantile(x, probs = 0.95, na.rm = TRUE)))
# Exibe limites.
tibble(variavel = numericas_sih, p05 = p05_sih, p95 = p95_sih)

# Gera histograma e boxplot para cada variavel numerica.
for (coluna in numericas_sih) {
  # Remove NAs para calcular a largura de classe apenas com valores observados.
  dados_validos <- df_sih_variaveis[[coluna]][!is.na(df_sih_variaveis[[coluna]])]
  # Conta observacoes validas da variavel atual.
  n_validos <- length(dados_validos)
  # Calcula IQR (amplitude interquartil), base do metodo de Freedman-Diaconis.
  iqr_coluna <- IQR(dados_validos, na.rm = TRUE)
  # Calcula a amplitude total da variavel para o fallback de Sturges.
  amplitude_coluna <- diff(range(dados_validos, na.rm = TRUE))
  # Freedman-Diaconis: 2 * IQR / n^(1/3), bom para distribuicoes assimetricas e com outliers.
  largura_fd <- if (n_validos > 1 && iqr_coluna > 0) 2 * iqr_coluna / (n_validos^(1/3)) else NA_real_
  # Sturges como fallback quando FD nao for calculavel (ex.: pouca variacao ou poucos dados).
  largura_sturges <- if (n_validos > 1 && is.finite(amplitude_coluna) && amplitude_coluna > 0) amplitude_coluna / (1 + log2(n_validos)) else 1
  # Prioriza FD; se nao for valido, usa Sturges.
  largura_bin <- if (is.finite(largura_fd) && largura_fd > 0) largura_fd else largura_sturges
  # Garante largura positiva para evitar falha no histograma.
  if (!is.finite(largura_bin) || largura_bin <= 0) largura_bin <- 1
  nome_coluna_wrap <- stringr::str_wrap(coluna, width = 28)
  grafico_hist <- ggplot(df_sih_variaveis, aes(x = .data[[coluna]])) +
    geom_histogram(aes(y = after_stat(count)), binwidth = largura_bin, fill = "#1B9E77") +
    theme_minimal() +
    theme(aspect.ratio = 0.45, plot.title = element_text(size = 10), plot.margin = margin(8, 8, 8, 8)) +
    labs(title = paste("Histograma -", nome_coluna_wrap), x = nome_coluna_wrap, y = "Frequencia")
  grafico_box <- ggplot(df_sih_variaveis, aes(y = .data[[coluna]])) +
    geom_boxplot(fill = "#1B9E77") +
    coord_flip() +
    theme_minimal() +
    theme(aspect.ratio = 0.45, plot.title = element_text(size = 10), plot.margin = margin(8, 8, 8, 8)) +
    labs(title = paste("Boxplot (horizontal) -", nome_coluna_wrap), x = nome_coluna_wrap, y = "")
  print(
    (grafico_hist + grafico_box + plot_layout(ncol = 2, widths = c(2, 1))) +
      plot_annotation(
        title = paste("SIH - Comparacao para:", nome_coluna_wrap),
        subtitle = paste("Binwidth (FD/Sturges):", round(largura_bin, 2))
      )
  )
}

# Tecnica 1: remocao de outliers.
keep_sih <- rep(TRUE, nrow(df_sih_variaveis))
for (coluna in numericas_sih) keep_sih <- keep_sih & (is.na(df_sih_variaveis[[coluna]]) | (df_sih_variaveis[[coluna]] >= p05_sih[coluna] & df_sih_variaveis[[coluna]] <= p95_sih[coluna]))
df_sih_sem_outlier_remocao <- df_sih_variaveis[keep_sih, ]

# Tecnica 2: validacao por outra coluna (idade calculada por data).
# Calcula idade usando data de nascimento e data de internacao.
if (all(c("IDADE", "NASC", "DT_INTER") %in% names(df_sih_variaveis))) {
  df_sih_variaveis <- df_sih_variaveis %>%
    mutate(
      IDADE_calculada = as.integer(floor(as.numeric(DT_INTER - NASC) / 365.25)),
      IDADE = if_else(!is.na(IDADE) & !is.na(IDADE_calculada) & IDADE != IDADE_calculada, IDADE_calculada, IDADE)
    )
}

# Tecnica 3: imputacao pela media.
# Observacao: mediana ou moda tambem podem ser usadas.
for (coluna in numericas_sih) {
  media_coluna <- mean(df_sih_variaveis[[coluna]], na.rm = TRUE)
  df_sih_variaveis[[coluna]] <- ifelse(df_sih_variaveis[[coluna]] < p05_sih[coluna] | df_sih_variaveis[[coluna]] > p95_sih[coluna], media_coluna, df_sih_variaveis[[coluna]])
}

# Missing 1: remove colunas 100% NA.
df_sih_variaveis <- df_sih_variaveis %>% select(where(~ !all(is.na(.x))))
# Missing 2: remove colunas com menos de 90% de preenchimento.
completude_sih <- colMeans(!is.na(df_sih_variaveis))
df_sih_variaveis <- df_sih_variaveis[, completude_sih >= 0.90, drop = FALSE]
# Missing 3: imputacao da mediana.
# Observacao: media ou moda tambem podem ser usadas.
numericas_sih <- names(df_sih_variaveis)[sapply(df_sih_variaveis, is.numeric)]
for (coluna in numericas_sih) df_sih_variaveis[[coluna]][is.na(df_sih_variaveis[[coluna]])] <- median(df_sih_variaveis[[coluna]], na.rm = TRUE)

# Define objeto final do SIH.
df_sih_modelagem <- df_sih_variaveis
# Salva SIH em RData.
save(df_sih_modelagem, file = file.path(raiz_dados_processados, "sih_modelagem.RData"))

# ------------------------------------------------------------------------------
# SESSAO 3 - SISTEMA DE INFORMACOES SOBRE MORTALIDADE (SIM)
# Objetivo da sessao:
# - Importar e padronizar tipos de variaveis do SIM.
# - Validar variaveis de idade e aplicar tratamento de outliers/missing.
# - Salvar o arquivo final do SIM para modelagem.
# ------------------------------------------------------------------------------

# Importa o arquivo do SIM.
caminho_sim <- file.path("dados_originais_datasus", "sim", "sim_DF_2024_variaveis.csv")
# Le o arquivo do SIM.
df_sim_variaveis <- read_csv(caminho_sim, show_col_types = FALSE)
# Mostra estrutura inicial.
glimpse(df_sim_variaveis)

# Converte e padroniza tipos principais do SIM.
df_sim_variaveis <- df_sim_variaveis %>%
  mutate(
    ORIGEM = as.factor(ORIGEM), # Converte para fator nominal.
    TIPOBITO = as.factor(TIPOBITO), # Converte para fator nominal.
    DTNASC = as.Date(DTNASC), # Converte para Date.
    IDADE = as.integer(IDADE), # Converte para inteiro.
    RACACOR = factor(case_when(RACACOR == "Branca" ~ 1L, RACACOR == "Preta" ~ 2L, RACACOR == "Amarela" ~ 3L, RACACOR == "Parda" ~ 4L, RACACOR == "Indígena" ~ 5L, TRUE ~ NA_integer_), levels = c(1L, 2L, 3L, 4L, 5L), labels = c("Branca", "Preta", "Amarela", "Parda", "Indigena")), # Recodifica raca/cor para fator nominal.
    ESCMAE = factor(ESCMAE, levels = c("Nenhuma", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "12 anos ou mais"), ordered = TRUE), # Fator ordinal.
    PARTO = factor(PARTO, levels = c("Vaginal", "Cesáreo")), # Fator nominal.
    GRAVIDEZ = factor(GRAVIDEZ, levels = c("única", "Dupla", "Tríplice e mais")), # Fator nominal.
    SEMAGESTAC = as.integer(SEMAGESTAC), # Mantem semanas de gestacao como inteiro.
    IDADEanos = as.integer(IDADEanos) # Converte para inteiro.
  )

# Outliers: identifica variaveis numericas.
numericas_sim <- names(df_sim_variaveis)[sapply(df_sim_variaveis, is.numeric)]
# Calcula limites de outliers.
p05_sim <- sapply(df_sim_variaveis[numericas_sim], function(x) as.numeric(quantile(x, probs = 0.05, na.rm = TRUE)))
p95_sim <- sapply(df_sim_variaveis[numericas_sim], function(x) as.numeric(quantile(x, probs = 0.95, na.rm = TRUE)))
tibble(variavel = numericas_sim, p05 = p05_sim, p95 = p95_sim)

# Gera histograma e boxplot para cada variavel numerica.
for (coluna in numericas_sim) {
  # Remove NAs para calcular a largura de classe apenas com valores observados.
  dados_validos <- df_sim_variaveis[[coluna]][!is.na(df_sim_variaveis[[coluna]])]
  # Conta observacoes validas da variavel atual.
  n_validos <- length(dados_validos)
  # Calcula IQR (amplitude interquartil), base do metodo de Freedman-Diaconis.
  iqr_coluna <- IQR(dados_validos, na.rm = TRUE)
  # Calcula a amplitude total da variavel para o fallback de Sturges.
  amplitude_coluna <- diff(range(dados_validos, na.rm = TRUE))
  # Freedman-Diaconis: 2 * IQR / n^(1/3), bom para distribuicoes assimetricas e com outliers.
  largura_fd <- if (n_validos > 1 && iqr_coluna > 0) 2 * iqr_coluna / (n_validos^(1/3)) else NA_real_
  # Sturges como fallback quando FD nao for calculavel (ex.: pouca variacao ou poucos dados).
  largura_sturges <- if (n_validos > 1 && is.finite(amplitude_coluna) && amplitude_coluna > 0) amplitude_coluna / (1 + log2(n_validos)) else 1
  # Prioriza FD; se nao for valido, usa Sturges.
  largura_bin <- if (is.finite(largura_fd) && largura_fd > 0) largura_fd else largura_sturges
  # Garante largura positiva para evitar falha no histograma.
  if (!is.finite(largura_bin) || largura_bin <= 0) largura_bin <- 1
  nome_coluna_wrap <- stringr::str_wrap(coluna, width = 28)
  grafico_hist <- ggplot(df_sim_variaveis, aes(x = .data[[coluna]])) +
    geom_histogram(aes(y = after_stat(count)), binwidth = largura_bin, fill = "#D95F02") +
    theme_minimal() +
    theme(aspect.ratio = 0.45, plot.title = element_text(size = 10), plot.margin = margin(8, 8, 8, 8)) +
    labs(title = paste("Histograma -", nome_coluna_wrap), x = nome_coluna_wrap, y = "Frequencia")
  grafico_box <- ggplot(df_sim_variaveis, aes(y = .data[[coluna]])) +
    geom_boxplot(fill = "#D95F02") +
    coord_flip() +
    theme_minimal() +
    theme(aspect.ratio = 0.45, plot.title = element_text(size = 10), plot.margin = margin(8, 8, 8, 8)) +
    labs(title = paste("Boxplot (horizontal) -", nome_coluna_wrap), x = nome_coluna_wrap, y = "")
  print(
    (grafico_hist + grafico_box + plot_layout(ncol = 2, widths = c(2, 1))) +
      plot_annotation(
        title = paste("SIM - Comparacao para:", nome_coluna_wrap),
        subtitle = paste("Binwidth (FD/Sturges):", round(largura_bin, 2))
      )
  )
}

# Tecnica 1: remocao de outliers.
keep_sim <- rep(TRUE, nrow(df_sim_variaveis))
for (coluna in numericas_sim) keep_sim <- keep_sim & (is.na(df_sim_variaveis[[coluna]]) | (df_sim_variaveis[[coluna]] >= p05_sim[coluna] & df_sim_variaveis[[coluna]] <= p95_sim[coluna]))
df_sim_sem_outlier_remocao <- df_sim_variaveis[keep_sim, ]

# Tecnica 2: validacao por outra coluna (idade calculada por data).
# Define a data de evento priorizando obito, depois notificacao, depois internacao.
if ("DTNASC" %in% names(df_sim_variaveis)) {
  data_evento_sim <- rep(as.Date(NA), nrow(df_sim_variaveis))
  if ("DTOBITO" %in% names(df_sim_variaveis)) data_evento_sim <- coalesce(data_evento_sim, as.Date(df_sim_variaveis$DTOBITO))
  if ("DT_OBITO" %in% names(df_sim_variaveis)) data_evento_sim <- coalesce(data_evento_sim, as.Date(df_sim_variaveis$DT_OBITO))
  if ("DT_NOTIFIC" %in% names(df_sim_variaveis)) data_evento_sim <- coalesce(data_evento_sim, as.Date(df_sim_variaveis$DT_NOTIFIC))
  if ("DTNOTIFIC" %in% names(df_sim_variaveis)) data_evento_sim <- coalesce(data_evento_sim, as.Date(df_sim_variaveis$DTNOTIFIC))
  if ("DT_INTER" %in% names(df_sim_variaveis)) data_evento_sim <- coalesce(data_evento_sim, as.Date(df_sim_variaveis$DT_INTER))
  if ("IDADE" %in% names(df_sim_variaveis)) {
    df_sim_variaveis <- df_sim_variaveis %>%
      mutate(
        IDADE = suppressWarnings(as.integer(readr::parse_number(as.character(IDADE)))), # Garante IDADE como inteiro para evitar conflito de tipo.
        IDADE_calculada = as.integer(floor(as.numeric(data_evento_sim - DTNASC) / 365.25)),
        IDADE = if_else(!is.na(IDADE) & !is.na(IDADE_calculada) & IDADE != IDADE_calculada, IDADE_calculada, IDADE)
      )
  }
}

# Tecnica 3: imputacao pela media.
# Observacao: mediana ou moda tambem podem ser usadas.
for (coluna in numericas_sim) {
  media_coluna <- mean(df_sim_variaveis[[coluna]], na.rm = TRUE)
  df_sim_variaveis[[coluna]] <- ifelse(df_sim_variaveis[[coluna]] < p05_sim[coluna] | df_sim_variaveis[[coluna]] > p95_sim[coluna], media_coluna, df_sim_variaveis[[coluna]])
}

# Missing 1: remove colunas 100% NA.
df_sim_variaveis <- df_sim_variaveis %>% select(where(~ !all(is.na(.x))))
# Missing 2: remove colunas com menos de 90% de preenchimento.
completude_sim <- colMeans(!is.na(df_sim_variaveis))
df_sim_variaveis <- df_sim_variaveis[, completude_sim >= 0.90, drop = FALSE]
# Missing 3: imputacao da mediana.
# Observacao: media ou moda tambem podem ser usadas.
numericas_sim <- names(df_sim_variaveis)[sapply(df_sim_variaveis, is.numeric)]
for (coluna in numericas_sim) df_sim_variaveis[[coluna]][is.na(df_sim_variaveis[[coluna]])] <- median(df_sim_variaveis[[coluna]], na.rm = TRUE)

# Define objeto final do SIM.
df_sim_modelagem <- df_sim_variaveis
# Salva SIM em RData.
save(df_sim_modelagem, file = file.path(raiz_dados_processados, "sim_modelagem.RData"))

# ------------------------------------------------------------------------------
# SESSAO 4 - SISTEMA DE INFORMACAO DE AGRAVOS DE NOTIFICACAO (SINAN)
# Objetivo da sessao:
# - Importar e padronizar tipos de variaveis do SINAN.
# - Tratar outliers, incluindo validacao de idade calculada por data.
# - Salvar o arquivo final do SINAN para modelagem.
# ------------------------------------------------------------------------------

# Importa o arquivo do SINAN.
caminho_sinan <- file.path("dados_originais_datasus", "sinan", "sinan_DF_2024_variaveis.csv")
# Le o arquivo do SINAN.
df_sinan_variaveis <- read_csv(caminho_sinan, show_col_types = FALSE)
# Mostra estrutura inicial.
glimpse(df_sinan_variaveis)

# Converte e padroniza tipos principais do SINAN.
df_sinan_variaveis <- df_sinan_variaveis %>%
  mutate(
    TP_NOT = as.character(TP_NOT), # Mantem tipo de notificacao como texto.
    ID_AGRAVO = as.character(ID_AGRAVO), # Mantem identificador como texto.
    DT_NOTIFIC = as.Date(DT_NOTIFIC), # Converte para Date.
    ANO_NASC = as.integer(ANO_NASC) # Converte para inteiro.
  )

# Outliers: identifica variaveis numericas.
numericas_sinan <- names(df_sinan_variaveis)[sapply(df_sinan_variaveis, is.numeric)]
# Calcula limites de outliers.
p05_sinan <- sapply(df_sinan_variaveis[numericas_sinan], function(x) as.numeric(quantile(x, probs = 0.05, na.rm = TRUE)))
p95_sinan <- sapply(df_sinan_variaveis[numericas_sinan], function(x) as.numeric(quantile(x, probs = 0.95, na.rm = TRUE)))
tibble(variavel = numericas_sinan, p05 = p05_sinan, p95 = p95_sinan)

# Gera histograma e boxplot para cada variavel numerica.
for (coluna in numericas_sinan) {
  # Remove NAs para calcular a largura de classe apenas com valores observados.
  dados_validos <- df_sinan_variaveis[[coluna]][!is.na(df_sinan_variaveis[[coluna]])]
  # Conta observacoes validas da variavel atual.
  n_validos <- length(dados_validos)
  # Calcula IQR (amplitude interquartil), base do metodo de Freedman-Diaconis.
  iqr_coluna <- IQR(dados_validos, na.rm = TRUE)
  # Calcula a amplitude total da variavel para o fallback de Sturges.
  amplitude_coluna <- diff(range(dados_validos, na.rm = TRUE))
  # Freedman-Diaconis: 2 * IQR / n^(1/3), bom para distribuicoes assimetricas e com outliers.
  largura_fd <- if (n_validos > 1 && iqr_coluna > 0) 2 * iqr_coluna / (n_validos^(1/3)) else NA_real_
  # Sturges como fallback quando FD nao for calculavel (ex.: pouca variacao ou poucos dados).
  largura_sturges <- if (n_validos > 1 && is.finite(amplitude_coluna) && amplitude_coluna > 0) amplitude_coluna / (1 + log2(n_validos)) else 1
  # Prioriza FD; se nao for valido, usa Sturges.
  largura_bin <- if (is.finite(largura_fd) && largura_fd > 0) largura_fd else largura_sturges
  # Garante largura positiva para evitar falha no histograma.
  if (!is.finite(largura_bin) || largura_bin <= 0) largura_bin <- 1
  nome_coluna_wrap <- stringr::str_wrap(coluna, width = 28)
  grafico_hist <- ggplot(df_sinan_variaveis, aes(x = .data[[coluna]])) +
    geom_histogram(aes(y = after_stat(count)), binwidth = largura_bin, fill = "#7570B3") +
    theme_minimal() +
    theme(aspect.ratio = 0.45, plot.title = element_text(size = 10), plot.margin = margin(8, 8, 8, 8)) +
    labs(title = paste("Histograma -", nome_coluna_wrap), x = nome_coluna_wrap, y = "Frequencia")
  grafico_box <- ggplot(df_sinan_variaveis, aes(y = .data[[coluna]])) +
    geom_boxplot(fill = "#7570B3") +
    coord_flip() +
    theme_minimal() +
    theme(aspect.ratio = 0.45, plot.title = element_text(size = 10), plot.margin = margin(8, 8, 8, 8)) +
    labs(title = paste("Boxplot (horizontal) -", nome_coluna_wrap), x = nome_coluna_wrap, y = "")
  print(
    (grafico_hist + grafico_box + plot_layout(ncol = 2, widths = c(2, 1))) +
      plot_annotation(
        title = paste("SINAN - Comparacao para:", nome_coluna_wrap),
        subtitle = paste("Binwidth (FD/Sturges):", round(largura_bin, 2))
      )
  )
}

# Tecnica 1: remocao de outliers.
keep_sinan <- rep(TRUE, nrow(df_sinan_variaveis))
for (coluna in numericas_sinan) keep_sinan <- keep_sinan & (is.na(df_sinan_variaveis[[coluna]]) | (df_sinan_variaveis[[coluna]] >= p05_sinan[coluna] & df_sinan_variaveis[[coluna]] <= p95_sinan[coluna]))
df_sinan_sem_outlier_remocao <- df_sinan_variaveis[keep_sinan, ]

# Tecnica 2: validacao por outra coluna (idade calculada por data).
# Cria data de nascimento aproximada com ANO_NASC (01/07 para reduzir vies de aniversario).
if (all(c("ANO_NASC", "DT_NOTIFIC") %in% names(df_sinan_variaveis))) {
  df_sinan_variaveis <- df_sinan_variaveis %>%
    mutate(
      DTNASC_APROX = as.Date(paste0(ANO_NASC, "-07-01")),
      IDADE_calculada = as.integer(floor(as.numeric(DT_NOTIFIC - DTNASC_APROX) / 365.25))
    )
  if ("IDADE" %in% names(df_sinan_variaveis)) {
    df_sinan_variaveis <- df_sinan_variaveis %>%
      mutate(IDADE = if_else(!is.na(IDADE) & !is.na(IDADE_calculada) & IDADE != IDADE_calculada, IDADE_calculada, IDADE))
  }
}

# Tecnica 3: imputacao pela media.
# Observacao: mediana ou moda tambem podem ser usadas.
for (coluna in numericas_sinan) {
  media_coluna <- mean(df_sinan_variaveis[[coluna]], na.rm = TRUE)
  df_sinan_variaveis[[coluna]] <- ifelse(df_sinan_variaveis[[coluna]] < p05_sinan[coluna] | df_sinan_variaveis[[coluna]] > p95_sinan[coluna], media_coluna, df_sinan_variaveis[[coluna]])
}

# Missing 1: remove colunas 100% NA.
df_sinan_variaveis <- df_sinan_variaveis %>% select(where(~ !all(is.na(.x))))
# Missing 2: remove colunas com menos de 90% de preenchimento.
completude_sinan <- colMeans(!is.na(df_sinan_variaveis))
df_sinan_variaveis <- df_sinan_variaveis[, completude_sinan >= 0.90, drop = FALSE]
# Missing 3: imputacao da mediana.
# Observacao: media ou moda tambem podem ser usadas.
numericas_sinan <- names(df_sinan_variaveis)[sapply(df_sinan_variaveis, is.numeric)]
for (coluna in numericas_sinan) df_sinan_variaveis[[coluna]][is.na(df_sinan_variaveis[[coluna]])] <- median(df_sinan_variaveis[[coluna]], na.rm = TRUE)

# Define objeto final do SINAN.
df_sinan_modelagem <- df_sinan_variaveis
# Salva SINAN em RData.
save(df_sinan_modelagem, file = file.path(raiz_dados_processados, "sinan_modelagem.RData"))

# ------------------------------------------------------------------------------
# SESSAO 5 - SISTEMA DE INFORMACOES SOBRE NASCIDOS VIVOS (SINASC)
# Objetivo da sessao:
# - Importar e padronizar tipos de variaveis do SINASC.
# - Tratar outliers e valores ausentes mantendo consistencia dos dados.
# - Salvar o arquivo final do SINASC para modelagem.
# ------------------------------------------------------------------------------

# Importa o arquivo do SINASC.
caminho_sinasc <- file.path("dados_originais_datasus", "sinasc", "sinasc_DF_2024_06_variaveis.csv")
# Le o arquivo do SINASC.
df_sinasc_variaveis <- read_csv(caminho_sinasc, show_col_types = FALSE)
# Mostra estrutura inicial.
glimpse(df_sinasc_variaveis)

# Calcula largura maxima do codigo de municipio para padding.
max_digitos_codmunnasc <- max(nchar(as.character(df_sinasc_variaveis$CODMUNNASC)), na.rm = TRUE)

# Converte e padroniza tipos principais do SINASC.
df_sinasc_variaveis <- df_sinasc_variaveis %>%
  mutate(
    CODMUNNASC = str_pad(as.character(CODMUNNASC), width = max_digitos_codmunnasc, side = "left", pad = "0"), # ID em texto com zero a esquerda.
    IDADEMAE = as.integer(IDADEMAE), # Converte para inteiro.
    PARTO = factor(case_when(PARTO == "Vaginal" ~ 1L, PARTO == "Cesáreo" ~ 2L, TRUE ~ NA_integer_), levels = c(1L, 2L), labels = c("Vaginal", "Cesareo")), # Recodifica parto para fator nominal binario.
    ESCMAE = factor(ESCMAE, levels = c("Nenhum", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "12 anos ou mais"), ordered = TRUE), # Fator ordinal.
    QTDFILVIVO = as.integer(QTDFILVIVO), # Converte para inteiro.
    CONSULTAS = factor(CONSULTAS, levels = c("Nenhuma", "1 a 3 vezes", "4 a 6 vezes", "7 ou mais vezes"), ordered = TRUE), # Fator ordinal.
    SERIESCMAE = factor(as.integer(SERIESCMAE), levels = sort(unique(as.integer(SERIESCMAE))), ordered = TRUE), # Fator ordinal.
    RACACORMAE = factor(case_when(RACACORMAE == "Branca" ~ 1L, RACACORMAE == "Preta" ~ 2L, RACACORMAE == "Amarela" ~ 3L, RACACORMAE == "Parda" ~ 4L, RACACORMAE == "Indígena" ~ 5L, TRUE ~ NA_integer_), levels = c(1L, 2L, 3L, 4L, 5L), labels = c("Branca", "Preta", "Amarela", "Parda", "Indigena")), # Recodifica raca/cor para fator nominal.
    ESTCIVMAE = as.factor(ESTCIVMAE), # Fator nominal.
    APGAR1 = factor(as.integer(APGAR1), levels = 0:10, ordered = TRUE), # Fator ordinal.
    APGAR5 = factor(as.integer(APGAR5), levels = 0:10, ordered = TRUE) # Fator ordinal.
  )

# Outliers: identifica variaveis numericas.
numericas_sinasc <- names(df_sinasc_variaveis)[sapply(df_sinasc_variaveis, is.numeric)]
# Calcula limites de outliers.
p05_sinasc <- sapply(df_sinasc_variaveis[numericas_sinasc], function(x) as.numeric(quantile(x, probs = 0.05, na.rm = TRUE)))
p95_sinasc <- sapply(df_sinasc_variaveis[numericas_sinasc], function(x) as.numeric(quantile(x, probs = 0.95, na.rm = TRUE)))
tibble(variavel = numericas_sinasc, p05 = p05_sinasc, p95 = p95_sinasc)

# Gera histograma e boxplot para cada variavel numerica.
for (coluna in numericas_sinasc) {
  # Remove NAs para calcular a largura de classe apenas com valores observados.
  dados_validos <- df_sinasc_variaveis[[coluna]][!is.na(df_sinasc_variaveis[[coluna]])]
  # Conta observacoes validas da variavel atual.
  n_validos <- length(dados_validos)
  # Calcula IQR (amplitude interquartil), base do metodo de Freedman-Diaconis.
  iqr_coluna <- IQR(dados_validos, na.rm = TRUE)
  # Calcula a amplitude total da variavel para o fallback de Sturges.
  amplitude_coluna <- diff(range(dados_validos, na.rm = TRUE))
  # Freedman-Diaconis: 2 * IQR / n^(1/3), bom para distribuicoes assimetricas e com outliers.
  largura_fd <- if (n_validos > 1 && iqr_coluna > 0) 2 * iqr_coluna / (n_validos^(1/3)) else NA_real_
  # Sturges como fallback quando FD nao for calculavel (ex.: pouca variacao ou poucos dados).
  largura_sturges <- if (n_validos > 1 && is.finite(amplitude_coluna) && amplitude_coluna > 0) amplitude_coluna / (1 + log2(n_validos)) else 1
  # Prioriza FD; se nao for valido, usa Sturges.
  largura_bin <- if (is.finite(largura_fd) && largura_fd > 0) largura_fd else largura_sturges
  # Garante largura positiva para evitar falha no histograma.
  if (!is.finite(largura_bin) || largura_bin <= 0) largura_bin <- 1
  nome_coluna_wrap <- stringr::str_wrap(coluna, width = 28)
  grafico_hist <- ggplot(df_sinasc_variaveis, aes(x = .data[[coluna]])) +
    geom_histogram(aes(y = after_stat(count)), binwidth = largura_bin, fill = "#E7298A") +
    theme_minimal() +
    theme(aspect.ratio = 0.45, plot.title = element_text(size = 10), plot.margin = margin(8, 8, 8, 8)) +
    labs(title = paste("Histograma -", nome_coluna_wrap), x = nome_coluna_wrap, y = "Frequencia")
  grafico_box <- ggplot(df_sinasc_variaveis, aes(y = .data[[coluna]])) +
    geom_boxplot(fill = "#E7298A") +
    coord_flip() +
    theme_minimal() +
    theme(aspect.ratio = 0.45, plot.title = element_text(size = 10), plot.margin = margin(8, 8, 8, 8)) +
    labs(title = paste("Boxplot (horizontal) -", nome_coluna_wrap), x = nome_coluna_wrap, y = "")
  print(
    (grafico_hist + grafico_box + plot_layout(ncol = 2, widths = c(2, 1))) +
      plot_annotation(
        title = paste("SINASC - Comparacao para:", nome_coluna_wrap),
        subtitle = paste("Binwidth (FD/Sturges):", round(largura_bin, 2))
      )
  )
}

# Tecnica 1: remocao de outliers.
keep_sinasc <- rep(TRUE, nrow(df_sinasc_variaveis))
for (coluna in numericas_sinasc) keep_sinasc <- keep_sinasc & (is.na(df_sinasc_variaveis[[coluna]]) | (df_sinasc_variaveis[[coluna]] >= p05_sinasc[coluna] & df_sinasc_variaveis[[coluna]] <= p95_sinasc[coluna]))
df_sinasc_sem_outlier_remocao <- df_sinasc_variaveis[keep_sinasc, ]

# Tecnica 2: validacao por outra coluna (idade calculada por data).
# Calcula idade da mae com data de nascimento da mae e data do nascimento.
if ("IDADEMAE" %in% names(df_sinasc_variaveis)) {
  data_nasc_mae <- rep(as.Date(NA), nrow(df_sinasc_variaveis))
  if ("DTNASCMAE" %in% names(df_sinasc_variaveis)) data_nasc_mae <- coalesce(data_nasc_mae, as.Date(df_sinasc_variaveis$DTNASCMAE))
  if ("DT_NASC_MAE" %in% names(df_sinasc_variaveis)) data_nasc_mae <- coalesce(data_nasc_mae, as.Date(df_sinasc_variaveis$DT_NASC_MAE))
  if ("NASCMAE" %in% names(df_sinasc_variaveis)) data_nasc_mae <- coalesce(data_nasc_mae, as.Date(df_sinasc_variaveis$NASCMAE))

  data_evento_sinasc <- rep(as.Date(NA), nrow(df_sinasc_variaveis))
  if ("DTNASC" %in% names(df_sinasc_variaveis)) data_evento_sinasc <- coalesce(data_evento_sinasc, as.Date(df_sinasc_variaveis$DTNASC))
  if ("DT_NASC" %in% names(df_sinasc_variaveis)) data_evento_sinasc <- coalesce(data_evento_sinasc, as.Date(df_sinasc_variaveis$DT_NASC))

  df_sinasc_variaveis <- df_sinasc_variaveis %>%
    mutate(
      IDADEMAE_calculada = as.integer(floor(as.numeric(data_evento_sinasc - data_nasc_mae) / 365.25)),
      IDADEMAE = if_else(!is.na(IDADEMAE) & !is.na(IDADEMAE_calculada) & IDADEMAE != IDADEMAE_calculada, IDADEMAE_calculada, IDADEMAE)
    )
}

# Tecnica 3: imputacao pela media.
# Observacao: mediana ou moda tambem podem ser usadas.
for (coluna in numericas_sinasc) {
  media_coluna <- mean(df_sinasc_variaveis[[coluna]], na.rm = TRUE)
  df_sinasc_variaveis[[coluna]] <- ifelse(df_sinasc_variaveis[[coluna]] < p05_sinasc[coluna] | df_sinasc_variaveis[[coluna]] > p95_sinasc[coluna], media_coluna, df_sinasc_variaveis[[coluna]])
}

# Missing 1: remove colunas 100% NA.
df_sinasc_variaveis <- df_sinasc_variaveis %>% select(where(~ !all(is.na(.x))))
# Missing 2: remove colunas com menos de 90% de preenchimento.
completude_sinasc <- colMeans(!is.na(df_sinasc_variaveis))
df_sinasc_variaveis <- df_sinasc_variaveis[, completude_sinasc >= 0.90, drop = FALSE]
# Missing 3: imputacao da mediana.
# Observacao: media ou moda tambem podem ser usadas.
numericas_sinasc <- names(df_sinasc_variaveis)[sapply(df_sinasc_variaveis, is.numeric)]
for (coluna in numericas_sinasc) df_sinasc_variaveis[[coluna]][is.na(df_sinasc_variaveis[[coluna]])] <- median(df_sinasc_variaveis[[coluna]], na.rm = TRUE)

# Define objeto final do SINASC.
df_sinasc_modelagem <- df_sinasc_variaveis
# Salva SINASC em RData.
save(df_sinasc_modelagem, file = file.path(raiz_dados_processados, "sinasc_modelagem.RData"))
