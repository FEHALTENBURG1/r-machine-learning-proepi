# ==============================================================================
# AULA 5: ESTATÍSTICA INFERENCIAL
# ==============================================================================
# Autor: Cientista de Dados Sênior
# Data de Criação: 27/03/2026
# Objetivo: Testes de hipótese para comparação de médias, medianas e proporções
#
# ENTRADA: Arquivos CSV brutos da Aula 1
#   - dados_originais_datasus/SINASC_2024_06_DF_selecionadas.csv
#   - dados_originais_datasus/SIM_2024_06_DF_selecionadas.csv
#   - dados_originais_datasus/SINAN_2024_06_DF_selecionadas.csv
#   - dados_originais_datasus/SIA_2024_06_DF_selecionadas.csv
#   - dados_originais_datasus/SIH_2024_06_DF_selecionadas.csv
#
# SAÍDA: Resultados de testes estatísticos (console output e sumários)
# DEPENDÊNCIAS: tidyverse
# ==============================================================================

# ==== CARREGAMENTO DE BIBLIOTECAS ====
library(tidyverse)  # Manipulação de dados

# ==== CONFIGURAÇÃO DE DIRETÓRIOS ====
dir_dados_originais <- "dados_originais_datasus"

# ==== FUNÇÃO AUXILIAR: Teste t-Student ====
# Compara médias entre dois grupos
# H0: As médias dos grupos são iguais
# Resultado: p-value < 0.05 rejeita H0 (diferença significativa)

realizar_teste_t <- function(dados, var_num, var_grupo, grupo1, grupo2, alpha = 0.05) {
  g1 <- dados %>% filter(.data[[var_grupo]] == grupo1) %>% pull(.data[[var_num]])
  g2 <- dados %>% filter(.data[[var_grupo]] == grupo2) %>% pull(.data[[var_num]])

  resultado <- t.test(g1, g2, var.equal = FALSE)


  if (resultado$p.value < alpha) {
  } else {
  }

  return(resultado)
}

# ==== FUNÇÃO AUXILIAR: Teste Wilcoxon-Mann-Whitney ====
# Compara medianas entre dois grupos (não-paramétrico)
# Alternativa ao teste t quando dados não são normais

realizar_teste_wilcoxon <- function(dados, var_num, var_grupo, grupo1, grupo2, alpha = 0.05) {
  g1 <- dados %>% filter(.data[[var_grupo]] == grupo1) %>% pull(.data[[var_num]])
  g2 <- dados %>% filter(.data[[var_grupo]] == grupo2) %>% pull(.data[[var_num]])

  resultado <- wilcox.test(g1, g2)


  if (resultado$p.value < alpha) {
  } else {
  }

  return(resultado)
}

# ==== FUNÇÃO AUXILIAR: Teste Kolmogorov-Smirnov ====
# Compara duas distribuições (forma geral)

realizar_teste_ks <- function(dados, var_num, var_grupo, grupo1, grupo2, alpha = 0.05) {
  g1 <- dados %>% filter(.data[[var_grupo]] == grupo1) %>% pull(.data[[var_num]])
  g2 <- dados %>% filter(.data[[var_grupo]] == grupo2) %>% pull(.data[[var_num]])

  resultado <- ks.test(g1, g2)


  if (resultado$p.value < alpha) {
  } else {
  }

  return(resultado)
}

# ==== FUNÇÃO AUXILIAR: Teste Qui-Quadrado ====
# Compara proporções entre categorias

realizar_teste_qui2 <- function(tabela, alpha = 0.05) {
  resultado <- chisq.test(tabela)


  if (resultado$p.value < alpha) {
  } else {
  }

  return(resultado)
}

# ==== FUNÇÃO AUXILIAR: ANOVA (Análise de Variância) ====
# Compara médias entre 3 ou mais grupos

realizar_anova <- function(dados, var_num, var_grupo, alpha = 0.05) {
  formula_str <- paste(var_num, "~", var_grupo)
  modelo <- aov(as.formula(formula_str), data = dados)


  if (anova(modelo)$`Pr(>F)`[1] < alpha) {
  } else {
  }

  return(modelo)
}

# ==== TESTES SINASC ====

sinasc <- read_csv(file.path(dir_dados_originais, "SINASC_2024_06_DF_selecionadas.csv"),
                   show_col_types = FALSE)


# Teste t: Comparação peso ao nascer entre sexos (se houver variação)
if ("peso_nasc" %in% colnames(sinasc) && "sexo" %in% colnames(sinasc)) {
  sexos_unicos <- unique(sinasc$sexo)
  if (length(sexos_unicos) >= 2) {
    teste_sinasc_t <- realizar_teste_t(sinasc, "peso_nasc", "sexo",
                                        sexos_unicos[1], sexos_unicos[2])
  }
}

# Teste Wilcoxon: Comparação Apgar entre sexos
if ("apgar_1" %in% colnames(sinasc) && "sexo" %in% colnames(sinasc)) {
  teste_sinasc_wilcox <- realizar_teste_wilcoxon(sinasc, "apgar_1", "sexo",
                                                  sexos_unicos[1], sexos_unicos[2])
}

# ==== TESTES SIM ====

sim <- read_csv(file.path(dir_dados_originais, "SIM_2024_06_DF_selecionadas.csv"),
                show_col_types = FALSE)


# Teste t: Comparação idade de óbito entre sexos
if ("idade_obito" %in% colnames(sim) && "sexo" %in% colnames(sim)) {
  sexos_sim <- unique(sim$sexo)
  if (length(sexos_sim) >= 2) {
    teste_sim_t <- realizar_teste_t(sim, "idade_obito", "sexo",
                                     sexos_sim[1], sexos_sim[2])
  }
}

# Teste Qui-Quadrado: Associação sexo vs local de óbito
if ("sexo" %in% colnames(sim) && "local_obito" %in% colnames(sim)) {
  tabela_sim <- table(sim$sexo, sim$local_obito)
  teste_sim_qui2 <- realizar_teste_qui2(tabela_sim)
}

# ==== TESTES SINAN ====

sinan <- read_csv(file.path(dir_dados_originais, "SINAN_2024_06_DF_selecionadas.csv"),
                  show_col_types = FALSE)


# Teste t: Comparação idade por resultado
if ("idade" %in% colnames(sinan) && "resultado" %in% colnames(sinan)) {
  resultados_unicos <- unique(sinan$resultado)
  if (length(resultados_unicos) >= 2) {
    teste_sinan_t <- realizar_teste_t(sinan, "idade", "resultado",
                                       resultados_unicos[1], resultados_unicos[2])
  }
}

# ANOVA: Idade por tipo de agravo (múltiplos grupos)
if ("idade" %in% colnames(sinan) && "agravo" %in% colnames(sinan)) {
  anova_sinan <- realizar_anova(sinan, "idade", "agravo")
}

# Teste Qui-Quadrado: Sexo vs Resultado
if ("sexo" %in% colnames(sinan) && "resultado" %in% colnames(sinan)) {
  tabela_sinan <- table(sinan$sexo, sinan$resultado)
  teste_sinan_qui2 <- realizar_teste_qui2(tabela_sinan)
}

# ==== TESTES SIA ====

sia <- read_csv(file.path(dir_dados_originais, "SIA_2024_06_DF_selecionadas.csv"),
                show_col_types = FALSE)


# ANOVA: Valor do procedimento por complexidade
if ("valor_procedimento" %in% colnames(sia) && "complexidade" %in% colnames(sia)) {
  anova_sia <- realizar_anova(sia, "valor_procedimento", "complexidade")
}

# Teste Qui-Quadrado: Sexo vs Tipo de Procedimento
if ("sexo" %in% colnames(sia) && "tipo_procedimento" %in% colnames(sia)) {
  tabela_sia <- table(sia$sexo, sia$tipo_procedimento)
  teste_sia_qui2 <- realizar_teste_qui2(tabela_sia)
}

# ==== TESTES SIH ====

sih <- read_csv(file.path(dir_dados_originais, "SIH_2024_06_DF_selecionadas.csv"),
                show_col_types = FALSE)


# ANOVA: Tempo de permanência por diagnóstico
if ("tempo_permanencia" %in% colnames(sih) && "diagnostico" %in% colnames(sih)) {
  anova_sih <- realizar_anova(sih, "tempo_permanencia", "diagnostico")
}

# Teste Wilcoxon: Comparação valor internação por sexo
if ("valor_internacao" %in% colnames(sih) && "sexo" %in% colnames(sih)) {
  sexos_sih <- unique(sih$sexo)
  if (length(sexos_sih) >= 2) {
    teste_sih_wilcox <- realizar_teste_wilcoxon(sih, "valor_internacao", "sexo",
                                                 sexos_sih[1], sexos_sih[2])
  }
}

# Teste Kolmogorov-Smirnov: Distribuição idade por sexo
if ("idade" %in% colnames(sih) && "sexo" %in% colnames(sih)) {
  sexos_sih <- unique(sih$sexo)
  if (length(sexos_sih) >= 2) {
    teste_sih_ks <- realizar_teste_ks(sih, "idade", "sexo",
                                       sexos_sih[1], sexos_sih[2])
  }
}

# ==== RESUMO FINAL ====
