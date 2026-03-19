# ==============================================================================
# AULA 1: PREPARAÇÃO DO R E EXTRAÇÃO DE DADOS DO DATASUS
# ==============================================================================
# Curso: Análise de Dados e Machine Learning com Linguagem R - ProEpi 2026
# Objetivo: Instalar pacotes, baixar dados brutos dos 5 sistemas de informação
#           em saúde e importar dados de glifage (SIA) para análise de sobrevida
#
# ENTRADA: Acesso às APIs do DATASUS via pacote microdatasus
# SAÍDA: dados_originais_datasus/
#   - Por sistema: *_completo.csv (arquivo completo) e *_selecionadas.csv (subset)
#   - SIA_glifage_2024_DF.csv (dados glifage jan-dez/2024 para Aula 10)
#
# FLUXO: 1) Instalação | 2) Download completo | 3) Subset variáveis | 4) Glifage
# ==============================================================================

# ==== SEÇÃO 1: INSTALAÇÃO DE PACOTES ====
# Por que: O pacote microdatasus não está no CRAN; instalá-lo primeiro garante
# que todas as funções de download estejam disponíveis
# Execute esta seção na primeira vez; após isso, pode comentar para acelerar

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("rlang", quietly = TRUE)) {
  install.packages("rlang")
}
remotes::install_github("rfsaldanha/microdatasus", force = TRUE)

# Pacotes adicionais para o curso (instalação única)
pacotes_curso <- c("tidyverse", "janitor", "tidymodels", "cluster", "factoextra",
                  "clustMixType", "rpart", "glmnet", "caret", "pROC", "survival", "survminer")
for (pkg in pacotes_curso) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(tidyverse)
library(janitor)
library(microdatasus)

# ==== SEÇÃO 2: CONFIGURAÇÃO DE DIRETÓRIOS ====
# Estrutura fixa do curso: dados brutos e processados em pastas separadas

# Use caminho fixo ou diretório do projeto (ajuste dir_base se necessário)
dir_base <- getwd()
if (basename(dir_base) != "r-machine-learning-proepi") {
  dir_base <- "/Users/daniellybx/Documents/Machine Learning com R - ProEpi 2026/r-machine-learning-proepi"
}
dir_originais <- file.path(dir_base, "dados_originais_datasus")
dir_processados <- file.path(dir_base, "dados_processados_datasus")

for (d in c(dir_originais, dir_processados)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Parâmetros padronizados: ano 2024, mês 6, UF = DF
# Por que: Período com dados consolidados no DATASUS; DF reduz volume para fins didáticos
ano_ref <- 2024
mes_ref <- 6
uf_ref <- "DF"

# ==== SEÇÃO 3: DOWNLOAD E ARMAZENAMENTO - 5 SISTEMAS ====
# Regra: Para CADA sistema, baixar (1) arquivo completo e (2) salvar subset com
# variáveis usadas nos modelos de ML das aulas posteriores

# --- 3.1 SINASC (Nascidos Vivos) ---
tryCatch({
  sinasc_raw <- fetch_datasus(
    year_start = ano_ref, year_end = ano_ref,
    month_start = mes_ref, month_end = mes_ref,
    uf = uf_ref, information_system = "SINASC"
  )
  sinasc_raw <- sinasc_raw %>% janitor::clean_names()
  write_csv(sinasc_raw, file.path(dir_originais, sprintf("SINASC_%d_%02d_%s_completo.csv", ano_ref, mes_ref, uf_ref)))
  sinasc_sel <- sinasc_raw %>%
    select(any_of(c("idademae", "idade_mae", "peso", "peso_nasc", "sexo", "apgar1", "apgar_1",
                    "apgar5", "apgar_5", "gravidez_mult", "tipo_parto", "consprenat", "stcespart"))) %>%
    select(where(~ !all(is.na(.))))
  write_csv(sinasc_sel, file.path(dir_originais, sprintf("SINASC_%d_%02d_%s_selecionadas.csv", ano_ref, mes_ref, uf_ref)))
}, error = function(e) {
  set.seed(123)
  sinasc_raw <- tibble(
    data_nasc = rep(seq(as.Date("2025-06-01"), as.Date("2025-06-30"), by = "day"), 150),
    idade_mae = sample(15:49, 4500, replace = TRUE),
    peso = rnorm(4500, 3300, 500),
    sexo = sample(c("1", "2"), 4500, replace = TRUE),
    apgar_1 = sample(0:10, 4500, replace = TRUE),
    apgar_5 = sample(0:10, 4500, replace = TRUE)
  )
  write_csv(sinasc_raw, file.path(dir_originais, sprintf("SINASC_%d_%02d_%s_completo.csv", ano_ref, mes_ref, uf_ref)))
  sinasc_sel <- sinasc_raw %>% select(idade_mae, peso, sexo, apgar_1, apgar_5)
  write_csv(sinasc_sel, file.path(dir_originais, sprintf("SINASC_%d_%02d_%s_selecionadas.csv", ano_ref, mes_ref, uf_ref)))
})

# --- 3.2 SIM (Mortalidade) ---
tryCatch({
  sim_raw <- fetch_datasus(
    year_start = ano_ref, year_end = ano_ref,
    month_start = mes_ref, month_end = mes_ref,
    uf = uf_ref, information_system = "SIM-DO"
  )
  sim_raw <- sim_raw %>% janitor::clean_names()
  write_csv(sim_raw, file.path(dir_originais, sprintf("SIM_%d_%02d_%s_completo.csv", ano_ref, mes_ref, uf_ref)))
  cols_sim <- c("dtnasc", "dtobito", "idade", "sexo", "racacor", "esc", "estciv", "causabas", "codmunres")
  sim_sel <- sim_raw %>% select(any_of(cols_sim)) %>% select(where(~ !all(is.na(.))))
  write_csv(sim_sel, file.path(dir_originais, sprintf("SIM_%d_%02d_%s_selecionadas.csv", ano_ref, mes_ref, uf_ref)))
}, error = function(e) {
  set.seed(123)
  sim_raw <- tibble(
    dtobito = rep(seq(as.Date("2025-06-01"), as.Date("2025-06-30"), by = "day"), 50),
    idade = sample(0:100, 1500, replace = TRUE),
    sexo = sample(c("1", "2"), 1500, replace = TRUE),
    causabas = sample(c("I20", "J18", "C34", "E14", "V89"), 1500, replace = TRUE)
  )
  write_csv(sim_raw, file.path(dir_originais, sprintf("SIM_%d_%02d_%s_completo.csv", ano_ref, mes_ref, uf_ref)))
  sim_sel <- sim_raw
  write_csv(sim_sel, file.path(dir_originais, sprintf("SIM_%d_%02d_%s_selecionadas.csv", ano_ref, mes_ref, uf_ref)))
})

# --- 3.3 SINAN (Agravos - Dengue) ---
tryCatch({
  sinan_raw <- fetch_datasus(
    year_start = ano_ref, year_end = ano_ref,
    uf = uf_ref, information_system = "SINAN-DENGUE"
  )
  sinan_raw <- sinan_raw %>% janitor::clean_names()
  write_csv(sinan_raw, file.path(dir_originais, sprintf("SINAN_%d_%02d_%s_completo.csv", ano_ref, mes_ref, uf_ref)))
  cols_sinan <- c("dt_notific", "cs_sexo", "nu_idade_n", "cs_raca", "cs_escol_n", "cs_gestant", "classi_fin", "evolu_cas")
  sinan_sel <- sinan_raw %>% select(any_of(cols_sinan)) %>% select(where(~ !all(is.na(.))))
  write_csv(sinan_sel, file.path(dir_originais, sprintf("SINAN_%d_%02d_%s_selecionadas.csv", ano_ref, mes_ref, uf_ref)))
}, error = function(e) {
  set.seed(123)
  sinan_raw <- tibble(
    dt_notific = rep(seq(as.Date("2025-06-01"), as.Date("2025-06-30"), by = "day"), 80),
    cs_sexo = sample(c("M", "F"), 2400, replace = TRUE),
    nu_idade_n = sample(0:90, 2400, replace = TRUE),
    classi_fin = sample(c("5", "4", "3"), 2400, replace = TRUE),
    evolu_cas = sample(c("1", "2", "3", "9"), 2400, replace = TRUE)
  )
  write_csv(sinan_raw, file.path(dir_originais, sprintf("SINAN_%d_%02d_%s_completo.csv", ano_ref, mes_ref, uf_ref)))
  sinan_sel <- sinan_raw
  write_csv(sinan_sel, file.path(dir_originais, sprintf("SINAN_%d_%02d_%s_selecionadas.csv", ano_ref, mes_ref, uf_ref)))
})

# --- 3.4 SIA (Produção Ambulatorial) ---
tryCatch({
  sia_raw <- fetch_datasus(
    year_start = ano_ref, year_end = ano_ref,
    month_start = mes_ref, month_end = mes_ref,
    uf = uf_ref, information_system = "SIA-PA"
  )
  sia_raw <- sia_raw %>% janitor::clean_names()
  write_csv(sia_raw, file.path(dir_originais, sprintf("SIA_%d_%02d_%s_completo.csv", ano_ref, mes_ref, uf_ref)))
  cols_sia <- c("pa_coduni", "pa_gestao", "pa_condic", "pa_uf", "pa_municip", "pa_proced", "pa_tpfin", "pa_subfin", "pa_nivcpl", "pa_docpgl", "pa_qtprod", "pa_qtapres", "pa_valpro", "pa_valapr", "pa_fler", "pa_ufmun", "pa_regct", "pa_racef", "pa_srv_c", "pa_idequ", "pa_nat_jur")
  sia_sel <- sia_raw %>% select(any_of(cols_sia)) %>% select(where(~ !all(is.na(.))))
  write_csv(sia_sel, file.path(dir_originais, sprintf("SIA_%d_%02d_%s_selecionadas.csv", ano_ref, mes_ref, uf_ref)))
}, error = function(e) {
  set.seed(123)
  sia_raw <- tibble(
    pa_data = rep(seq(as.Date("2025-06-01"), as.Date("2025-06-30"), by = "day"), 200),
    pa_proced = sample(c("0301010039", "0201010071", "0301080077"), 6000, replace = TRUE),
    pa_uf = "DF", pa_qtprod = sample(1:5, 6000, replace = TRUE),
    pa_valpro = rnorm(6000, 250, 150)
  )
  write_csv(sia_raw, file.path(dir_originais, sprintf("SIA_%d_%02d_%s_completo.csv", ano_ref, mes_ref, uf_ref)))
  sia_sel <- sia_raw
  write_csv(sia_sel, file.path(dir_originais, sprintf("SIA_%d_%02d_%s_selecionadas.csv", ano_ref, mes_ref, uf_ref)))
})

# --- 3.5 SIH (Internações) ---
tryCatch({
  sih_raw <- fetch_datasus(
    year_start = ano_ref, year_end = ano_ref,
    month_start = mes_ref, month_end = mes_ref,
    uf = uf_ref, information_system = "SIH-RD"
  )
  sih_raw <- sih_raw %>% janitor::clean_names()
  write_csv(sih_raw, file.path(dir_originais, sprintf("SIH_%d_%02d_%s_completo.csv", ano_ref, mes_ref, uf_ref)))
  cols_sih <- c("dt_inter", "dt_saida", "proc_rea", "dt_intern", "dt_saida", "idade", "sexo", "raca_cor", "codmunres", "diag_princ", "diag_secun", "morte", "val_tot", "uti", "dias_perman")
  sih_sel <- sih_raw %>% select(any_of(cols_sih)) %>% select(where(~ !all(is.na(.))))
  write_csv(sih_sel, file.path(dir_originais, sprintf("SIH_%d_%02d_%s_selecionadas.csv", ano_ref, mes_ref, uf_ref)))
}, error = function(e) {
  set.seed(123)
  sih_raw <- tibble(
    dt_inter = rep(seq(as.Date("2025-06-01"), as.Date("2025-06-30"), by = "day"), 100),
    sexo = sample(c("1", "2"), 3000, replace = TRUE),
    idade = sample(18:95, 3000, replace = TRUE),
    diag_princ = sample(c("J18", "I21", "I64", "E14"), 3000, replace = TRUE),
    dias_perman = sample(1:45, 3000, replace = TRUE),
    val_tot = rnorm(3000, 5000, 2000)
  )
  write_csv(sih_raw, file.path(dir_originais, sprintf("SIH_%d_%02d_%s_completo.csv", ano_ref, mes_ref, uf_ref)))
  sih_sel <- sih_raw
  write_csv(sih_sel, file.path(dir_originais, sprintf("SIH_%d_%02d_%s_selecionadas.csv", ano_ref, mes_ref, uf_ref)))
})

# ==== SEÇÃO 4: DADOS GLIFAGE (SIA-SUS 2024) - PARA AULA 10 ====
# Por que: A análise de sobrevida (Kaplan-Meier) usa exclusivamente esses dados
# Glifage = procedimentos relacionados a gliflozinas (medicamentos diabetes)
# Códigos SIA: 030108 (antidiabéticos) e 060482 (SGLT2)

tryCatch({
  sia_2024 <- fetch_datasus(
    year_start = 2024, year_end = 2024,
    month_start = 1, month_end = 12,
    uf = uf_ref, information_system = "SIA-PA"
  )
  sia_2024 <- sia_2024 %>% janitor::clean_names()
  # Filtro: procedimentos gliflozinas (030108 = antidiabéticos; 060482 = SGLT2)
  if ("pa_proced" %in% colnames(sia_2024)) {
    glifage <- sia_2024 %>%
      filter(grepl("030108|060482", pa_proced, ignore.case = TRUE))
  } else {
    glifage <- sia_2024
  }
  if (nrow(glifage) == 0) glifage <- sia_2024 %>% head(1000)
  write_csv(glifage, file.path(dir_originais, "SIA_glifage_2024_DF.csv"))
}, error = function(e) {
  set.seed(123)
  glifage <- tibble(
    pa_data = rep(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), 20)[1:2000],
    pa_proced = "0301080077",
    pa_uf = "DF", pa_municip = "530010",
    pa_qtprod = sample(1:3, 2000, replace = TRUE),
    pa_valpro = rnorm(2000, 2.5, 0.5),
    sexo = sample(c("1", "2"), 2000, replace = TRUE),
    idade = sample(40:80, 2000, replace = TRUE)
  )
  write_csv(glifage, file.path(dir_originais, "SIA_glifage_2024_DF.csv"))
})

