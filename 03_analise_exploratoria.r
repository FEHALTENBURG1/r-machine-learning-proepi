# ==============================================================================
# AULA 3 - ANALISE EXPLORATORIA
# Curso: Analise de Dados e Machine Learning com R - ProEpi 2026
# ------------------------------------------------------------------------------
# Objetivo geral da aula:
# - Explorar os dados tratados com tabelas e graficos didaticos por sistema.
# - Descrever variaveis categoricas e numericas com medidas estatisticas principais.
# - Avaliar correlacoes e relacoes bivariadas para apoiar interpretacao dos dados.
# ==============================================================================

# ------------------------------------------------------------------------------
# SESSAO 1 - TODOS OS IMPORTS DE PACOTES E DADOS
# Objetivo da sessao:
# - Carregar pacotes e bases tratadas da aula 2.
# - Conferir dimensoes dos dados antes das analises.
# ------------------------------------------------------------------------------
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")

library(tidyverse)
library(patchwork)

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
# ------------------------------------------------------------------------------
# SESSAO 2 - SISTEMA DE INFORMACOES SOBRE NASCIDOS VIVOS (SINASC)
# Objetivo da sessao:
# - Realizar analise de frequencias de variaveis categoricas.
# - Avaliar distribuicoes numericas com medidas de tendencia e dispersao.
# - Investigar correlacoes e diagramas de dispersao entre variaveis numericas.
# ------------------------------------------------------------------------------

# 1) Analise de frequencia absoluta e relativa para variaveis categoricas.
categoricas_sinasc <- names(df_sinasc_modelagem)[sapply(df_sinasc_modelagem, function(x) is.factor(x) || is.character(x))]
categoricas_sinasc <- categoricas_sinasc[!str_detect(str_to_lower(categoricas_sinasc), padrao_excluir_cid_causa)]

if (length(categoricas_sinasc) > 0) {
  tabela_freq_abs_sinasc <- df_sinasc_modelagem %>%
    select(all_of(categoricas_sinasc)) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(id_linha = row_number()) %>%
    pivot_longer(cols = -id_linha, names_to = "variavel", values_to = "categoria") %>%
    mutate(categoria = forcats::fct_na_value_to_level(as.factor(categoria), level = "(NA)")) %>%
    count(variavel, categoria, name = "frequencia")
  print(tabela_freq_abs_sinasc)

  for (var_cat in unique(tabela_freq_abs_sinasc$variavel)) {
    dados_plot <- tabela_freq_abs_sinasc %>% filter(variavel == var_cat)
    grafico_freq_abs_sinasc <- ggplot(dados_plot, aes(x = categoria, y = frequencia)) +
      geom_col(fill = "#2C7FB8") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold")) +
      labs(title = paste("SINASC - Frequencia absoluta:", var_cat), x = var_cat, y = "Frequencia")
    print(grafico_freq_abs_sinasc)
  }

  tabela_freq_rel_sinasc <- tabela_freq_abs_sinasc %>%
    group_by(variavel) %>%
    mutate(percentual = 100 * frequencia / sum(frequencia)) %>%
    ungroup()
  print(tabela_freq_rel_sinasc)

  for (var_cat in unique(tabela_freq_rel_sinasc$variavel)) {
    dados_plot <- tabela_freq_rel_sinasc %>% filter(variavel == var_cat)
    grafico_freq_rel_sinasc <- ggplot(dados_plot, aes(x = categoria, y = percentual)) +
      geom_col(fill = "#41AB5D") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold")) +
      labs(title = paste("SINASC - Frequencia relativa (%):", var_cat), x = var_cat, y = "Percentual")
    print(grafico_freq_rel_sinasc)
  }
}

# 2) Medidas de tendencia central e dispersao para variaveis numericas.
numericas_sinasc <- names(df_sinasc_modelagem)[sapply(df_sinasc_modelagem, is.numeric)]

if (length(numericas_sinasc) > 0) {
  tabela_medidas_sinasc <- df_sinasc_modelagem %>%
    select(all_of(numericas_sinasc)) %>%
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") %>%
    group_by(variavel) %>%
    summarise(
      media = mean(valor, na.rm = TRUE),
      mediana = median(valor, na.rm = TRUE),
      moda = {
        x <- na.omit(valor)
        if (length(x) == 0) NA_real_ else as.numeric(names(sort(table(round(x, 2)), decreasing = TRUE)[1]))
      },
      variancia = var(valor, na.rm = TRUE),
      desvio_padrao = sd(valor, na.rm = TRUE),
      coef_var_percent = if_else(abs(media) > 0, 100 * desvio_padrao / abs(media), NA_real_),
      .groups = "drop"
    )
  print(tabela_medidas_sinasc)

  cat("\nSINASC - Coeficiente de variacao (CV): usar para comparar variabilidade relativa entre variaveis com escalas ou medias diferentes.\n\n")
  tabela_cv_sinasc <- tabela_medidas_sinasc %>%
    select(variavel, media, desvio_padrao, coef_var_percent) %>%
    arrange(desc(coef_var_percent))
  print(tabela_cv_sinasc)

  dados_hist_sinasc <- df_sinasc_modelagem %>%
    select(all_of(numericas_sinasc)) %>%
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor")

  linhas_hist_sinasc <- tabela_medidas_sinasc %>%
    select(variavel, media, mediana, moda, desvio_padrao) %>%
    mutate(sd_menos = media - desvio_padrao, sd_mais = media + desvio_padrao) %>%
    pivot_longer(cols = c(media, mediana, moda, sd_menos, sd_mais), names_to = "estatistica", values_to = "valor") %>%
    filter(!is.na(valor))
  print(linhas_hist_sinasc)

  tabela_bins_hist_sinasc <- dados_hist_sinasc %>%
    filter(!is.na(valor), is.finite(valor)) %>%
    group_by(variavel) %>%
    group_modify(~{
      h <- hist(.x$valor, breaks = 30, plot = FALSE)
      tibble(
        limite_inferior = h$breaks[-length(h$breaks)],
        limite_superior = h$breaks[-1],
        frequencia = h$counts
      )
    }) %>%
    ungroup()
  print(tabela_bins_hist_sinasc)

  for (var_num in numericas_sinasc) {
    dados_plot <- dados_hist_sinasc %>% filter(variavel == var_num)
    linhas_plot <- linhas_hist_sinasc %>% filter(variavel == var_num)
    grafico_hist_sinasc <- ggplot(dados_plot, aes(x = valor)) +
      geom_histogram(aes(y = after_stat(count)), bins = 30, fill = "#9E9AC8", color = "white") +
      geom_vline(data = linhas_plot, aes(xintercept = valor, color = estatistica), linewidth = 0.6) +
      scale_color_manual(
        values = c(media = "#E41A1C", mediana = "#377EB8", moda = "#4DAF4A", sd_menos = "#FF7F00", sd_mais = "#FF7F00"),
        labels = c(media = "Media", mediana = "Mediana", moda = "Moda", sd_menos = "Media - DP", sd_mais = "Media + DP")
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
      labs(title = paste("SINASC - Histograma:", var_num), x = var_num, y = "Frequencia", color = "Linhas")
    print(grafico_hist_sinasc)
  }
}

# 3) Correlacao para cada par de variaveis numericas: Pearson, Spearman e dispersao.
if (length(numericas_sinasc) >= 2) {
  numericas_sinasc_cor <- numericas_sinasc[sapply(df_sinasc_modelagem[numericas_sinasc], function(x) sd(x, na.rm = TRUE) > 0)]
  if (length(numericas_sinasc_cor) < 2) {
    cat("\nSINASC - Sem variaveis numericas com variacao suficiente para correlacao.\n")
  } else {
    matriz_pearson_sinasc <- cor(df_sinasc_modelagem %>% select(all_of(numericas_sinasc_cor)), use = "pairwise.complete.obs", method = "pearson")
    matriz_spearman_sinasc <- cor(df_sinasc_modelagem %>% select(all_of(numericas_sinasc_cor)), use = "pairwise.complete.obs", method = "spearman")

  tabela_cor_pearson_sinasc <- as_tibble(as.data.frame(as.table(matriz_pearson_sinasc))) %>%
    rename(var1 = Var1, var2 = Var2, cor_pearson = Freq)
  tabela_cor_spearman_sinasc <- as_tibble(as.data.frame(as.table(matriz_spearman_sinasc))) %>%
    rename(var1 = Var1, var2 = Var2, cor_spearman = Freq)

  print(tabela_cor_pearson_sinasc)
  print(tabela_cor_spearman_sinasc)

  pares_cor_sinasc <- tabela_cor_pearson_sinasc %>%
    filter(as.character(var1) < as.character(var2)) %>%
    left_join(
      tabela_cor_spearman_sinasc %>% filter(as.character(var1) < as.character(var2)),
      by = c("var1", "var2")
    )
  print(pares_cor_sinasc)

  tabela_disp_sinasc <- pares_cor_sinasc %>%
    mutate(
      n_pares_validos = map2_int(var1, var2, ~sum(complete.cases(df_sinasc_modelagem[[.x]], df_sinasc_modelagem[[.y]]))),
      min_x = map2_dbl(var1, var2, ~min(df_sinasc_modelagem[[.x]], na.rm = TRUE)),
      max_x = map2_dbl(var1, var2, ~max(df_sinasc_modelagem[[.x]], na.rm = TRUE)),
      min_y = map2_dbl(var1, var2, ~min(df_sinasc_modelagem[[.y]], na.rm = TRUE)),
      max_y = map2_dbl(var1, var2, ~max(df_sinasc_modelagem[[.y]], na.rm = TRUE))
    )
  print(tabela_disp_sinasc)

    for (i in seq_len(nrow(pares_cor_sinasc))) {
    var_x <- as.character(pares_cor_sinasc$var1[i])
    var_y <- as.character(pares_cor_sinasc$var2[i])
    r_p <- round(pares_cor_sinasc$cor_pearson[i], 3)
    r_s <- round(pares_cor_sinasc$cor_spearman[i], 3)
    dados_disp <- df_sinasc_modelagem %>%
      transmute(x = .data[[var_x]], y = .data[[var_y]]) %>%
      filter(complete.cases(x, y))

    if (nrow(dados_disp) > 1 && n_distinct(dados_disp$x) > 1 && n_distinct(dados_disp$y) > 1) {
      grafico_disp <- ggplot(dados_disp, aes(x = x, y = y)) +
        geom_point(alpha = 0.45, color = "#2C7FB8") +
        geom_smooth(method = "lm", se = FALSE, color = "#B2182B", linewidth = 0.6) +
        theme_minimal(base_size = 10) +
        labs(
          title = paste("SINASC - Dispersao:", var_x, "x", var_y),
          subtitle = paste("Pearson =", r_p, "| Spearman =", r_s),
          x = var_x,
          y = var_y
        )
      print(grafico_disp)
    }
    }
  }
}

# ------------------------------------------------------------------------------
# SESSAO 3 - SISTEMA DE INFORMACOES SOBRE MORTALIDADE (SIM)
# Objetivo da sessao:
# - Realizar analise de frequencias de variaveis categoricas.
# - Avaliar distribuicoes numericas com medidas de tendencia e dispersao.
# - Investigar correlacoes e diagramas de dispersao entre variaveis numericas.
# ------------------------------------------------------------------------------

# 1) Analise de frequencia absoluta e relativa para variaveis categoricas.
categoricas_sim <- names(df_sim_modelagem)[sapply(df_sim_modelagem, function(x) is.factor(x) || is.character(x))]

if (length(categoricas_sim) > 0) {
  tabela_freq_abs_sim <- df_sim_modelagem %>%
    select(all_of(categoricas_sim)) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(id_linha = row_number()) %>%
    pivot_longer(cols = -id_linha, names_to = "variavel", values_to = "categoria") %>%
    mutate(categoria = forcats::fct_na_value_to_level(as.factor(categoria), level = "(NA)")) %>%
    count(variavel, categoria, name = "frequencia")
  print(tabela_freq_abs_sim)

  for (var_cat in unique(tabela_freq_abs_sim$variavel)) {
    dados_plot <- tabela_freq_abs_sim %>% filter(variavel == var_cat)
    grafico_freq_abs_sim <- ggplot(dados_plot, aes(x = categoria, y = frequencia)) +
      geom_col(fill = "#2C7FB8") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold")) +
      labs(title = paste("SIM - Frequencia absoluta:", var_cat), x = var_cat, y = "Frequencia")
    print(grafico_freq_abs_sim)
  }

  tabela_freq_rel_sim <- tabela_freq_abs_sim %>%
    group_by(variavel) %>%
    mutate(percentual = 100 * frequencia / sum(frequencia)) %>%
    ungroup()
  print(tabela_freq_rel_sim)

  for (var_cat in unique(tabela_freq_rel_sim$variavel)) {
    dados_plot <- tabela_freq_rel_sim %>% filter(variavel == var_cat)
    grafico_freq_rel_sim <- ggplot(dados_plot, aes(x = categoria, y = percentual)) +
      geom_col(fill = "#41AB5D") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold")) +
      labs(title = paste("SIM - Frequencia relativa (%):", var_cat), x = var_cat, y = "Percentual")
    print(grafico_freq_rel_sim)
  }
}

# 2) Medidas de tendencia central e dispersao para variaveis numericas.
numericas_sim <- names(df_sim_modelagem)[sapply(df_sim_modelagem, is.numeric)]

if (length(numericas_sim) > 0) {
  tabela_medidas_sim <- df_sim_modelagem %>%
    select(all_of(numericas_sim)) %>%
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") %>%
    group_by(variavel) %>%
    summarise(
      media = mean(valor, na.rm = TRUE),
      mediana = median(valor, na.rm = TRUE),
      moda = {
        x <- na.omit(valor)
        if (length(x) == 0) NA_real_ else as.numeric(names(sort(table(round(x, 2)), decreasing = TRUE)[1]))
      },
      variancia = var(valor, na.rm = TRUE),
      desvio_padrao = sd(valor, na.rm = TRUE),
      coef_var_percent = if_else(abs(media) > 0, 100 * desvio_padrao / abs(media), NA_real_),
      .groups = "drop"
    )
  print(tabela_medidas_sim)

  cat("\nSIM - Coeficiente de variacao (CV): usar para comparar variabilidade relativa entre variaveis com escalas ou medias diferentes.\n\n")
  tabela_cv_sim <- tabela_medidas_sim %>%
    select(variavel, media, desvio_padrao, coef_var_percent) %>%
    arrange(desc(coef_var_percent))
  print(tabela_cv_sim)

  dados_hist_sim <- df_sim_modelagem %>%
    select(all_of(numericas_sim)) %>%
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor")

  linhas_hist_sim <- tabela_medidas_sim %>%
    select(variavel, media, mediana, moda, desvio_padrao) %>%
    mutate(sd_menos = media - desvio_padrao, sd_mais = media + desvio_padrao) %>%
    pivot_longer(cols = c(media, mediana, moda, sd_menos, sd_mais), names_to = "estatistica", values_to = "valor") %>%
    filter(!is.na(valor))
  print(linhas_hist_sim)

  tabela_bins_hist_sim <- dados_hist_sim %>%
    filter(!is.na(valor), is.finite(valor)) %>%
    group_by(variavel) %>%
    group_modify(~{
      h <- hist(.x$valor, breaks = 30, plot = FALSE)
      tibble(
        limite_inferior = h$breaks[-length(h$breaks)],
        limite_superior = h$breaks[-1],
        frequencia = h$counts
      )
    }) %>%
    ungroup()
  print(tabela_bins_hist_sim)

  for (var_num in numericas_sim) {
    dados_plot <- dados_hist_sim %>% filter(variavel == var_num)
    linhas_plot <- linhas_hist_sim %>% filter(variavel == var_num)
    grafico_hist_sim <- ggplot(dados_plot, aes(x = valor)) +
      geom_histogram(aes(y = after_stat(count)), bins = 30, fill = "#9E9AC8", color = "white") +
      geom_vline(data = linhas_plot, aes(xintercept = valor, color = estatistica), linewidth = 0.6) +
      scale_color_manual(
        values = c(media = "#E41A1C", mediana = "#377EB8", moda = "#4DAF4A", sd_menos = "#FF7F00", sd_mais = "#FF7F00"),
        labels = c(media = "Media", mediana = "Mediana", moda = "Moda", sd_menos = "Media - DP", sd_mais = "Media + DP")
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
      labs(title = paste("SIM - Histograma:", var_num), x = var_num, y = "Frequencia", color = "Linhas")
    print(grafico_hist_sim)
  }
}

# 3) Correlacao para cada par de variaveis numericas: Pearson, Spearman e dispersao.
if (length(numericas_sim) >= 2) {
  numericas_sim_cor <- numericas_sim[sapply(df_sim_modelagem[numericas_sim], function(x) sd(x, na.rm = TRUE) > 0)]
  if (length(numericas_sim_cor) < 2) {
    cat("\nSIM - Sem variaveis numericas com variacao suficiente para correlacao.\n")
  } else {
    matriz_pearson_sim <- cor(df_sim_modelagem %>% select(all_of(numericas_sim_cor)), use = "pairwise.complete.obs", method = "pearson")
    matriz_spearman_sim <- cor(df_sim_modelagem %>% select(all_of(numericas_sim_cor)), use = "pairwise.complete.obs", method = "spearman")

  tabela_cor_pearson_sim <- as_tibble(as.data.frame(as.table(matriz_pearson_sim))) %>%
    rename(var1 = Var1, var2 = Var2, cor_pearson = Freq)
  tabela_cor_spearman_sim <- as_tibble(as.data.frame(as.table(matriz_spearman_sim))) %>%
    rename(var1 = Var1, var2 = Var2, cor_spearman = Freq)

  print(tabela_cor_pearson_sim)
  print(tabela_cor_spearman_sim)

  pares_cor_sim <- tabela_cor_pearson_sim %>%
    filter(as.character(var1) < as.character(var2)) %>%
    left_join(
      tabela_cor_spearman_sim %>% filter(as.character(var1) < as.character(var2)),
      by = c("var1", "var2")
    )
  print(pares_cor_sim)

  tabela_disp_sim <- pares_cor_sim %>%
    mutate(
      n_pares_validos = map2_int(var1, var2, ~sum(complete.cases(df_sim_modelagem[[.x]], df_sim_modelagem[[.y]]))),
      min_x = map2_dbl(var1, var2, ~min(df_sim_modelagem[[.x]], na.rm = TRUE)),
      max_x = map2_dbl(var1, var2, ~max(df_sim_modelagem[[.x]], na.rm = TRUE)),
      min_y = map2_dbl(var1, var2, ~min(df_sim_modelagem[[.y]], na.rm = TRUE)),
      max_y = map2_dbl(var1, var2, ~max(df_sim_modelagem[[.y]], na.rm = TRUE))
    )
  print(tabela_disp_sim)

    for (i in seq_len(nrow(pares_cor_sim))) {
    var_x <- as.character(pares_cor_sim$var1[i])
    var_y <- as.character(pares_cor_sim$var2[i])
    r_p <- round(pares_cor_sim$cor_pearson[i], 3)
    r_s <- round(pares_cor_sim$cor_spearman[i], 3)

    dados_disp <- df_sim_modelagem %>%
      transmute(x = .data[[var_x]], y = .data[[var_y]]) %>%
      filter(complete.cases(x, y))

    if (nrow(dados_disp) > 1 && n_distinct(dados_disp$x) > 1 && n_distinct(dados_disp$y) > 1) {
      grafico_disp <- ggplot(dados_disp, aes(x = x, y = y)) +
        geom_point(alpha = 0.45, color = "#2C7FB8") +
        geom_smooth(method = "lm", se = FALSE, color = "#B2182B", linewidth = 0.6) +
        theme_minimal(base_size = 10) +
        labs(
          title = paste("SIM - Dispersao:", var_x, "x", var_y),
          subtitle = paste("Pearson =", r_p, "| Spearman =", r_s),
          x = var_x,
          y = var_y
        )
      print(grafico_disp)
    }
    }
  }
}

# ------------------------------------------------------------------------------
# SESSAO 4 - SISTEMA DE INFORMACOES AMBULATORIAIS (SIA)
# Objetivo da sessao:
# - Realizar analise de frequencias de variaveis categoricas.
# - Avaliar distribuicoes numericas com medidas de tendencia e dispersao.
# - Investigar correlacoes e diagramas de dispersao entre variaveis numericas.
# ------------------------------------------------------------------------------

# 1) Analise de frequencia absoluta e relativa para variaveis categoricas.
categoricas_sia <- names(df_sia_modelagem)[sapply(df_sia_modelagem, function(x) is.factor(x) || is.character(x))]

if (length(categoricas_sia) > 0) {
  tabela_freq_abs_sia <- df_sia_modelagem %>%
    select(all_of(categoricas_sia)) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(id_linha = row_number()) %>%
    pivot_longer(cols = -id_linha, names_to = "variavel", values_to = "categoria") %>%
    mutate(categoria = forcats::fct_na_value_to_level(as.factor(categoria), level = "(NA)")) %>%
    count(variavel, categoria, name = "frequencia")
  print(tabela_freq_abs_sia)

  for (var_cat in unique(tabela_freq_abs_sia$variavel)) {
    dados_plot <- tabela_freq_abs_sia %>% filter(variavel == var_cat)
    grafico_freq_abs_sia <- ggplot(dados_plot, aes(x = categoria, y = frequencia)) +
      geom_col(fill = "#2C7FB8") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold")) +
      labs(title = paste("SIA - Frequencia absoluta:", var_cat), x = var_cat, y = "Frequencia")
    print(grafico_freq_abs_sia)
  }

  tabela_freq_rel_sia <- tabela_freq_abs_sia %>%
    group_by(variavel) %>%
    mutate(percentual = 100 * frequencia / sum(frequencia)) %>%
    ungroup()
  print(tabela_freq_rel_sia)

  for (var_cat in unique(tabela_freq_rel_sia$variavel)) {
    dados_plot <- tabela_freq_rel_sia %>% filter(variavel == var_cat)
    grafico_freq_rel_sia <- ggplot(dados_plot, aes(x = categoria, y = percentual)) +
      geom_col(fill = "#41AB5D") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold")) +
      labs(title = paste("SIA - Frequencia relativa (%):", var_cat), x = var_cat, y = "Percentual")
    print(grafico_freq_rel_sia)
  }
}

# 2) Medidas de tendencia central e dispersao para variaveis numericas.
numericas_sia <- names(df_sia_modelagem)[sapply(df_sia_modelagem, is.numeric)]

if (length(numericas_sia) > 0) {
  tabela_medidas_sia <- df_sia_modelagem %>%
    select(all_of(numericas_sia)) %>%
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") %>%
    group_by(variavel) %>%
    summarise(
      media = mean(valor, na.rm = TRUE),
      mediana = median(valor, na.rm = TRUE),
      moda = {
        x <- na.omit(valor)
        if (length(x) == 0) NA_real_ else as.numeric(names(sort(table(round(x, 2)), decreasing = TRUE)[1]))
      },
      variancia = var(valor, na.rm = TRUE),
      desvio_padrao = sd(valor, na.rm = TRUE),
      coef_var_percent = if_else(abs(media) > 0, 100 * desvio_padrao / abs(media), NA_real_),
      .groups = "drop"
    )
  print(tabela_medidas_sia)

  cat("\nSIA - Coeficiente de variacao (CV): usar para comparar variabilidade relativa entre variaveis com escalas ou medias diferentes.\n\n")
  tabela_cv_sia <- tabela_medidas_sia %>%
    select(variavel, media, desvio_padrao, coef_var_percent) %>%
    arrange(desc(coef_var_percent))
  print(tabela_cv_sia)

  dados_hist_sia <- df_sia_modelagem %>%
    select(all_of(numericas_sia)) %>%
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor")

  linhas_hist_sia <- tabela_medidas_sia %>%
    select(variavel, media, mediana, moda, desvio_padrao) %>%
    mutate(sd_menos = media - desvio_padrao, sd_mais = media + desvio_padrao) %>%
    pivot_longer(cols = c(media, mediana, moda, sd_menos, sd_mais), names_to = "estatistica", values_to = "valor") %>%
    filter(!is.na(valor))
  print(linhas_hist_sia)

  tabela_bins_hist_sia <- dados_hist_sia %>%
    filter(!is.na(valor), is.finite(valor)) %>%
    group_by(variavel) %>%
    group_modify(~{
      h <- hist(.x$valor, breaks = 30, plot = FALSE)
      tibble(
        limite_inferior = h$breaks[-length(h$breaks)],
        limite_superior = h$breaks[-1],
        frequencia = h$counts
      )
    }) %>%
    ungroup()
  print(tabela_bins_hist_sia)

  for (var_num in numericas_sia) {
    dados_plot <- dados_hist_sia %>% filter(variavel == var_num)
    linhas_plot <- linhas_hist_sia %>% filter(variavel == var_num)
    grafico_hist_sia <- ggplot(dados_plot, aes(x = valor)) +
      geom_histogram(aes(y = after_stat(count)), bins = 30, fill = "#9E9AC8", color = "white") +
      geom_vline(data = linhas_plot, aes(xintercept = valor, color = estatistica), linewidth = 0.6) +
      scale_color_manual(
        values = c(media = "#E41A1C", mediana = "#377EB8", moda = "#4DAF4A", sd_menos = "#FF7F00", sd_mais = "#FF7F00"),
        labels = c(media = "Media", mediana = "Mediana", moda = "Moda", sd_menos = "Media - DP", sd_mais = "Media + DP")
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
      labs(title = paste("SIA - Histograma:", var_num), x = var_num, y = "Frequencia", color = "Linhas")
    print(grafico_hist_sia)
  }
}

# 3) Correlacao para cada par de variaveis numericas: Pearson, Spearman e dispersao.
if (length(numericas_sia) >= 2) {
  numericas_sia_cor <- numericas_sia[sapply(df_sia_modelagem[numericas_sia], function(x) sd(x, na.rm = TRUE) > 0)]
  if (length(numericas_sia_cor) < 2) {
    cat("\nSIA - Sem variaveis numericas com variacao suficiente para correlacao.\n")
  } else {
    matriz_pearson_sia <- cor(df_sia_modelagem %>% select(all_of(numericas_sia_cor)), use = "pairwise.complete.obs", method = "pearson")
    matriz_spearman_sia <- cor(df_sia_modelagem %>% select(all_of(numericas_sia_cor)), use = "pairwise.complete.obs", method = "spearman")

  tabela_cor_pearson_sia <- as_tibble(as.data.frame(as.table(matriz_pearson_sia))) %>%
    rename(var1 = Var1, var2 = Var2, cor_pearson = Freq)
  tabela_cor_spearman_sia <- as_tibble(as.data.frame(as.table(matriz_spearman_sia))) %>%
    rename(var1 = Var1, var2 = Var2, cor_spearman = Freq)

  print(tabela_cor_pearson_sia)
  print(tabela_cor_spearman_sia)

  pares_cor_sia <- tabela_cor_pearson_sia %>%
    filter(as.character(var1) < as.character(var2)) %>%
    left_join(
      tabela_cor_spearman_sia %>% filter(as.character(var1) < as.character(var2)),
      by = c("var1", "var2")
    )
  print(pares_cor_sia)

  tabela_disp_sia <- pares_cor_sia %>%
    mutate(
      n_pares_validos = map2_int(var1, var2, ~sum(complete.cases(df_sia_modelagem[[.x]], df_sia_modelagem[[.y]]))),
      min_x = map2_dbl(var1, var2, ~min(df_sia_modelagem[[.x]], na.rm = TRUE)),
      max_x = map2_dbl(var1, var2, ~max(df_sia_modelagem[[.x]], na.rm = TRUE)),
      min_y = map2_dbl(var1, var2, ~min(df_sia_modelagem[[.y]], na.rm = TRUE)),
      max_y = map2_dbl(var1, var2, ~max(df_sia_modelagem[[.y]], na.rm = TRUE))
    )
  print(tabela_disp_sia)

    for (i in seq_len(nrow(pares_cor_sia))) {
    var_x <- as.character(pares_cor_sia$var1[i])
    var_y <- as.character(pares_cor_sia$var2[i])
    r_p <- round(pares_cor_sia$cor_pearson[i], 3)
    r_s <- round(pares_cor_sia$cor_spearman[i], 3)

    dados_disp <- df_sia_modelagem %>%
      transmute(x = .data[[var_x]], y = .data[[var_y]]) %>%
      filter(complete.cases(x, y))

    if (nrow(dados_disp) > 1 && n_distinct(dados_disp$x) > 1 && n_distinct(dados_disp$y) > 1) {
      grafico_disp <- ggplot(dados_disp, aes(x = x, y = y)) +
        geom_point(alpha = 0.45, color = "#2C7FB8") +
        geom_smooth(method = "lm", se = FALSE, color = "#B2182B", linewidth = 0.6) +
        theme_minimal(base_size = 10) +
        labs(
          title = paste("SIA - Dispersao:", var_x, "x", var_y),
          subtitle = paste("Pearson =", r_p, "| Spearman =", r_s),
          x = var_x,
          y = var_y
        )
      print(grafico_disp)
    }
    }
  }
}

# ------------------------------------------------------------------------------
# SESSAO 5 - SISTEMA DE INFORMACOES HOSPITALARES (SIH)
# Objetivo da sessao:
# - Realizar analise de frequencias de variaveis categoricas.
# - Avaliar distribuicoes numericas com medidas de tendencia e dispersao.
# - Investigar correlacoes e diagramas de dispersao entre variaveis numericas.
# ------------------------------------------------------------------------------

# 1) Analise de frequencia absoluta e relativa para variaveis categoricas.
categoricas_sih <- names(df_sih_modelagem)[sapply(df_sih_modelagem, function(x) is.factor(x) || is.character(x))]

if (length(categoricas_sih) > 0) {
  tabela_freq_abs_sih <- df_sih_modelagem %>%
    select(all_of(categoricas_sih)) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(id_linha = row_number()) %>%
    pivot_longer(cols = -id_linha, names_to = "variavel", values_to = "categoria") %>%
    mutate(categoria = forcats::fct_na_value_to_level(as.factor(categoria), level = "(NA)")) %>%
    count(variavel, categoria, name = "frequencia")
  print(tabela_freq_abs_sih)

  for (var_cat in unique(tabela_freq_abs_sih$variavel)) {
    dados_plot <- tabela_freq_abs_sih %>% filter(variavel == var_cat)
    grafico_freq_abs_sih <- ggplot(dados_plot, aes(x = categoria, y = frequencia)) +
      geom_col(fill = "#2C7FB8") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold")) +
      labs(title = paste("SIH - Frequencia absoluta:", var_cat), x = var_cat, y = "Frequencia")
    print(grafico_freq_abs_sih)
  }

  tabela_freq_rel_sih <- tabela_freq_abs_sih %>%
    group_by(variavel) %>%
    mutate(percentual = 100 * frequencia / sum(frequencia)) %>%
    ungroup()
  print(tabela_freq_rel_sih)

  for (var_cat in unique(tabela_freq_rel_sih$variavel)) {
    dados_plot <- tabela_freq_rel_sih %>% filter(variavel == var_cat)
    grafico_freq_rel_sih <- ggplot(dados_plot, aes(x = categoria, y = percentual)) +
      geom_col(fill = "#41AB5D") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold")) +
      labs(title = paste("SIH - Frequencia relativa (%):", var_cat), x = var_cat, y = "Percentual")
    print(grafico_freq_rel_sih)
  }
}

# 2) Medidas de tendencia central e dispersao para variaveis numericas.
numericas_sih <- names(df_sih_modelagem)[sapply(df_sih_modelagem, is.numeric)]

if (length(numericas_sih) > 0) {
  tabela_medidas_sih <- df_sih_modelagem %>%
    select(all_of(numericas_sih)) %>%
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") %>%
    group_by(variavel) %>%
    summarise(
      media = mean(valor, na.rm = TRUE),
      mediana = median(valor, na.rm = TRUE),
      moda = {
        x <- na.omit(valor)
        if (length(x) == 0) NA_real_ else as.numeric(names(sort(table(round(x, 2)), decreasing = TRUE)[1]))
      },
      variancia = var(valor, na.rm = TRUE),
      desvio_padrao = sd(valor, na.rm = TRUE),
      coef_var_percent = if_else(abs(media) > 0, 100 * desvio_padrao / abs(media), NA_real_),
      .groups = "drop"
    )
  print(tabela_medidas_sih)

  cat("\nSIH - Coeficiente de variacao (CV): usar para comparar variabilidade relativa entre variaveis com escalas ou medias diferentes.\n\n")
  tabela_cv_sih <- tabela_medidas_sih %>%
    select(variavel, media, desvio_padrao, coef_var_percent) %>%
    arrange(desc(coef_var_percent))
  print(tabela_cv_sih)

  dados_hist_sih <- df_sih_modelagem %>%
    select(all_of(numericas_sih)) %>%
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor")

  linhas_hist_sih <- tabela_medidas_sih %>%
    select(variavel, media, mediana, moda, desvio_padrao) %>%
    mutate(sd_menos = media - desvio_padrao, sd_mais = media + desvio_padrao) %>%
    pivot_longer(cols = c(media, mediana, moda, sd_menos, sd_mais), names_to = "estatistica", values_to = "valor") %>%
    filter(!is.na(valor))
  print(linhas_hist_sih)

  tabela_bins_hist_sih <- dados_hist_sih %>%
    filter(!is.na(valor), is.finite(valor)) %>%
    group_by(variavel) %>%
    group_modify(~{
      h <- hist(.x$valor, breaks = 30, plot = FALSE)
      tibble(
        limite_inferior = h$breaks[-length(h$breaks)],
        limite_superior = h$breaks[-1],
        frequencia = h$counts
      )
    }) %>%
    ungroup()
  print(tabela_bins_hist_sih)

  for (var_num in numericas_sih) {
    dados_plot <- dados_hist_sih %>% filter(variavel == var_num)
    linhas_plot <- linhas_hist_sih %>% filter(variavel == var_num)
    grafico_hist_sih <- ggplot(dados_plot, aes(x = valor)) +
      geom_histogram(aes(y = after_stat(count)), bins = 30, fill = "#9E9AC8", color = "white") +
      geom_vline(data = linhas_plot, aes(xintercept = valor, color = estatistica), linewidth = 0.6) +
      scale_color_manual(
        values = c(media = "#E41A1C", mediana = "#377EB8", moda = "#4DAF4A", sd_menos = "#FF7F00", sd_mais = "#FF7F00"),
        labels = c(media = "Media", mediana = "Mediana", moda = "Moda", sd_menos = "Media - DP", sd_mais = "Media + DP")
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
      labs(title = paste("SIH - Histograma:", var_num), x = var_num, y = "Frequencia", color = "Linhas")
    print(grafico_hist_sih)
  }
}

# 3) Correlacao para cada par de variaveis numericas: Pearson, Spearman e dispersao.
if (length(numericas_sih) >= 2) {
  numericas_sih_cor <- numericas_sih[sapply(df_sih_modelagem[numericas_sih], function(x) sd(x, na.rm = TRUE) > 0)]
  if (length(numericas_sih_cor) < 2) {
    cat("\nSIH - Sem variaveis numericas com variacao suficiente para correlacao.\n")
  } else {
    matriz_pearson_sih <- cor(df_sih_modelagem %>% select(all_of(numericas_sih_cor)), use = "pairwise.complete.obs", method = "pearson")
    matriz_spearman_sih <- cor(df_sih_modelagem %>% select(all_of(numericas_sih_cor)), use = "pairwise.complete.obs", method = "spearman")

  tabela_cor_pearson_sih <- as_tibble(as.data.frame(as.table(matriz_pearson_sih))) %>%
    rename(var1 = Var1, var2 = Var2, cor_pearson = Freq)
  tabela_cor_spearman_sih <- as_tibble(as.data.frame(as.table(matriz_spearman_sih))) %>%
    rename(var1 = Var1, var2 = Var2, cor_spearman = Freq)

  print(tabela_cor_pearson_sih)
  print(tabela_cor_spearman_sih)

  pares_cor_sih <- tabela_cor_pearson_sih %>%
    filter(as.character(var1) < as.character(var2)) %>%
    left_join(
      tabela_cor_spearman_sih %>% filter(as.character(var1) < as.character(var2)),
      by = c("var1", "var2")
    )
  print(pares_cor_sih)

  tabela_disp_sih <- pares_cor_sih %>%
    mutate(
      n_pares_validos = map2_int(var1, var2, ~sum(complete.cases(df_sih_modelagem[[.x]], df_sih_modelagem[[.y]]))),
      min_x = map2_dbl(var1, var2, ~min(df_sih_modelagem[[.x]], na.rm = TRUE)),
      max_x = map2_dbl(var1, var2, ~max(df_sih_modelagem[[.x]], na.rm = TRUE)),
      min_y = map2_dbl(var1, var2, ~min(df_sih_modelagem[[.y]], na.rm = TRUE)),
      max_y = map2_dbl(var1, var2, ~max(df_sih_modelagem[[.y]], na.rm = TRUE))
    )
  print(tabela_disp_sih)

    for (i in seq_len(nrow(pares_cor_sih))) {
    var_x <- as.character(pares_cor_sih$var1[i])
    var_y <- as.character(pares_cor_sih$var2[i])
    r_p <- round(pares_cor_sih$cor_pearson[i], 3)
    r_s <- round(pares_cor_sih$cor_spearman[i], 3)

    dados_disp <- df_sih_modelagem %>%
      transmute(x = .data[[var_x]], y = .data[[var_y]]) %>%
      filter(complete.cases(x, y))

    if (nrow(dados_disp) > 1 && n_distinct(dados_disp$x) > 1 && n_distinct(dados_disp$y) > 1) {
      grafico_disp <- ggplot(dados_disp, aes(x = x, y = y)) +
        geom_point(alpha = 0.45, color = "#2C7FB8") +
        geom_smooth(method = "lm", se = FALSE, color = "#B2182B", linewidth = 0.6) +
        theme_minimal(base_size = 10) +
        labs(
          title = paste("SIH - Dispersao:", var_x, "x", var_y),
          subtitle = paste("Pearson =", r_p, "| Spearman =", r_s),
          x = var_x,
          y = var_y
        )
      print(grafico_disp)
    }
    }
  }
}

# ------------------------------------------------------------------------------
# SESSAO 6 - SISTEMA DE INFORMACAO DE AGRAVOS DE NOTIFICACAO (SINAN)
# Objetivo da sessao:
# - Realizar analise de frequencias de variaveis categoricas.
# - Avaliar distribuicoes numericas com medidas de tendencia e dispersao.
# - Investigar correlacoes e diagramas de dispersao entre variaveis numericas.
# ------------------------------------------------------------------------------

# 1) Analise de frequencia absoluta e relativa para variaveis categoricas.
categoricas_sinan <- names(df_sinan_modelagem)[sapply(df_sinan_modelagem, function(x) is.factor(x) || is.character(x))]

if (length(categoricas_sinan) > 0) {
  tabela_freq_abs_sinan <- df_sinan_modelagem %>%
    select(all_of(categoricas_sinan)) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(id_linha = row_number()) %>%
    pivot_longer(cols = -id_linha, names_to = "variavel", values_to = "categoria") %>%
    mutate(categoria = forcats::fct_na_value_to_level(as.factor(categoria), level = "(NA)")) %>%
    count(variavel, categoria, name = "frequencia")
  print(tabela_freq_abs_sinan)

  for (var_cat in unique(tabela_freq_abs_sinan$variavel)) {
    dados_plot <- tabela_freq_abs_sinan %>% filter(variavel == var_cat)
    grafico_freq_abs_sinan <- ggplot(dados_plot, aes(x = categoria, y = frequencia)) +
      geom_col(fill = "#2C7FB8") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold")) +
      labs(title = paste("SINAN - Frequencia absoluta:", var_cat), x = var_cat, y = "Frequencia")
    print(grafico_freq_abs_sinan)
  }

  tabela_freq_rel_sinan <- tabela_freq_abs_sinan %>%
    group_by(variavel) %>%
    mutate(percentual = 100 * frequencia / sum(frequencia)) %>%
    ungroup()
  print(tabela_freq_rel_sinan)

  for (var_cat in unique(tabela_freq_rel_sinan$variavel)) {
    dados_plot <- tabela_freq_rel_sinan %>% filter(variavel == var_cat)
    grafico_freq_rel_sinan <- ggplot(dados_plot, aes(x = categoria, y = percentual)) +
      geom_col(fill = "#41AB5D") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold")) +
      labs(title = paste("SINAN - Frequencia relativa (%):", var_cat), x = var_cat, y = "Percentual")
    print(grafico_freq_rel_sinan)
  }
}

# 2) Medidas de tendencia central e dispersao para variaveis numericas.
numericas_sinan <- names(df_sinan_modelagem)[sapply(df_sinan_modelagem, is.numeric)]

if (length(numericas_sinan) > 0) {
  tabela_medidas_sinan <- df_sinan_modelagem %>%
    select(all_of(numericas_sinan)) %>%
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") %>%
    group_by(variavel) %>%
    summarise(
      media = mean(valor, na.rm = TRUE),
      mediana = median(valor, na.rm = TRUE),
      moda = {
        x <- na.omit(valor)
        if (length(x) == 0) NA_real_ else as.numeric(names(sort(table(round(x, 2)), decreasing = TRUE)[1]))
      },
      variancia = var(valor, na.rm = TRUE),
      desvio_padrao = sd(valor, na.rm = TRUE),
      coef_var_percent = if_else(abs(media) > 0, 100 * desvio_padrao / abs(media), NA_real_),
      .groups = "drop"
    )
  print(tabela_medidas_sinan)

  cat("\nSINAN - Coeficiente de variacao (CV): usar para comparar variabilidade relativa entre variaveis com escalas ou medias diferentes.\n\n")
  tabela_cv_sinan <- tabela_medidas_sinan %>%
    select(variavel, media, desvio_padrao, coef_var_percent) %>%
    arrange(desc(coef_var_percent))
  print(tabela_cv_sinan)

  dados_hist_sinan <- df_sinan_modelagem %>%
    select(all_of(numericas_sinan)) %>%
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor")

  linhas_hist_sinan <- tabela_medidas_sinan %>%
    select(variavel, media, mediana, moda, desvio_padrao) %>%
    mutate(sd_menos = media - desvio_padrao, sd_mais = media + desvio_padrao) %>%
    pivot_longer(cols = c(media, mediana, moda, sd_menos, sd_mais), names_to = "estatistica", values_to = "valor") %>%
    filter(!is.na(valor))
  print(linhas_hist_sinan)

  tabela_bins_hist_sinan <- dados_hist_sinan %>%
    filter(!is.na(valor), is.finite(valor)) %>%
    group_by(variavel) %>%
    group_modify(~{
      h <- hist(.x$valor, breaks = 30, plot = FALSE)
      tibble(
        limite_inferior = h$breaks[-length(h$breaks)],
        limite_superior = h$breaks[-1],
        frequencia = h$counts
      )
    }) %>%
    ungroup()
  print(tabela_bins_hist_sinan)

  for (var_num in numericas_sinan) {
    dados_plot <- dados_hist_sinan %>% filter(variavel == var_num)
    linhas_plot <- linhas_hist_sinan %>% filter(variavel == var_num)
    grafico_hist_sinan <- ggplot(dados_plot, aes(x = valor)) +
      geom_histogram(aes(y = after_stat(count)), bins = 30, fill = "#9E9AC8", color = "white") +
      geom_vline(data = linhas_plot, aes(xintercept = valor, color = estatistica), linewidth = 0.6) +
      scale_color_manual(
        values = c(media = "#E41A1C", mediana = "#377EB8", moda = "#4DAF4A", sd_menos = "#FF7F00", sd_mais = "#FF7F00"),
        labels = c(media = "Media", mediana = "Mediana", moda = "Moda", sd_menos = "Media - DP", sd_mais = "Media + DP")
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
      labs(title = paste("SINAN - Histograma:", var_num), x = var_num, y = "Frequencia", color = "Linhas")
    print(grafico_hist_sinan)
  }
}

# 3) Correlacao para cada par de variaveis numericas: Pearson, Spearman e dispersao.
if (length(numericas_sinan) >= 2) {
  numericas_sinan_cor <- numericas_sinan[sapply(df_sinan_modelagem[numericas_sinan], function(x) sd(x, na.rm = TRUE) > 0)]
  if (length(numericas_sinan_cor) < 2) {
    cat("\nSINAN - Sem variaveis numericas com variacao suficiente para correlacao.\n")
  } else {
    matriz_pearson_sinan <- cor(df_sinan_modelagem %>% select(all_of(numericas_sinan_cor)), use = "pairwise.complete.obs", method = "pearson")
    matriz_spearman_sinan <- cor(df_sinan_modelagem %>% select(all_of(numericas_sinan_cor)), use = "pairwise.complete.obs", method = "spearman")

  tabela_cor_pearson_sinan <- as_tibble(as.data.frame(as.table(matriz_pearson_sinan))) %>%
    rename(var1 = Var1, var2 = Var2, cor_pearson = Freq)
  tabela_cor_spearman_sinan <- as_tibble(as.data.frame(as.table(matriz_spearman_sinan))) %>%
    rename(var1 = Var1, var2 = Var2, cor_spearman = Freq)

  print(tabela_cor_pearson_sinan)
  print(tabela_cor_spearman_sinan)

  pares_cor_sinan <- tabela_cor_pearson_sinan %>%
    filter(as.character(var1) < as.character(var2)) %>%
    left_join(
      tabela_cor_spearman_sinan %>% filter(as.character(var1) < as.character(var2)),
      by = c("var1", "var2")
    )
  print(pares_cor_sinan)

  tabela_disp_sinan <- pares_cor_sinan %>%
    mutate(
      n_pares_validos = map2_int(var1, var2, ~sum(complete.cases(df_sinan_modelagem[[.x]], df_sinan_modelagem[[.y]]))),
      min_x = map2_dbl(var1, var2, ~min(df_sinan_modelagem[[.x]], na.rm = TRUE)),
      max_x = map2_dbl(var1, var2, ~max(df_sinan_modelagem[[.x]], na.rm = TRUE)),
      min_y = map2_dbl(var1, var2, ~min(df_sinan_modelagem[[.y]], na.rm = TRUE)),
      max_y = map2_dbl(var1, var2, ~max(df_sinan_modelagem[[.y]], na.rm = TRUE))
    )
  print(tabela_disp_sinan)

    for (i in seq_len(nrow(pares_cor_sinan))) {
    var_x <- as.character(pares_cor_sinan$var1[i])
    var_y <- as.character(pares_cor_sinan$var2[i])
    r_p <- round(pares_cor_sinan$cor_pearson[i], 3)
    r_s <- round(pares_cor_sinan$cor_spearman[i], 3)

    dados_disp <- df_sinan_modelagem %>%
      transmute(x = .data[[var_x]], y = .data[[var_y]]) %>%
      filter(complete.cases(x, y))

    if (nrow(dados_disp) > 1 && n_distinct(dados_disp$x) > 1 && n_distinct(dados_disp$y) > 1) {
      grafico_disp <- ggplot(dados_disp, aes(x = x, y = y)) +
        geom_point(alpha = 0.45, color = "#2C7FB8") +
        geom_smooth(method = "lm", se = FALSE, color = "#B2182B", linewidth = 0.6) +
        theme_minimal(base_size = 10) +
        labs(
          title = paste("SINAN - Dispersao:", var_x, "x", var_y),
          subtitle = paste("Pearson =", r_p, "| Spearman =", r_s),
          x = var_x,
          y = var_y
        )
      print(grafico_disp)
    }
    }
  }
}