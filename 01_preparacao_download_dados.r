# ==============================================================================
# AULA 1 - PREPARACAO DO R E ACESSO INICIAL AOS DADOS DO SUS - BRASIL
# Curso: Analise de Dados e Machine Learning com R - ProEpi 2026
# ------------------------------------------------------------------------------
# Objetivo geral da aula:
# - Instalar e organizar os pacotes essenciais.
# - Instalar o microdatasus para acesso aos sistemas do DATASUS.
# - Realizar downloads completos e com variaveis selecionadas.
# ==============================================================================

# ------------------------------------------------------------------------------
# SESSAO 1 - PREPARACAO DO AMBIENTE
# Objetivo da sessao:
# - Garantir que todos os pacotes usados na aula estejam disponiveis.
# ------------------------------------------------------------------------------
pacotes_necessarios <- c(
  "tidyverse", # Manipulacao, transformacao e visualizacao de dados
  "remotes"    # Instalacao de pacotes a partir do GitHub
)

install.packages(pacotes_necessarios)
library(readr)       # Exportacao/importacao de arquivos tabulares (CSV)
library(remotes)     # Funcao install_github() para instalar pacotes remotos
library(tidyverse)   # Funcoes de dados usadas no script (dplyr/stringr/ggplot2)

install_github("rfsaldanha/microdatasus", force = TRUE)
library(microdatasus)# Download de bases DATASUS com fetch_datasus()

# ------------------------------------------------------------------------------
# CONFIGURACAO PADRAO DE CAMINHOS, PARAMETROS E NOMES
# ------------------------------------------------------------------------------
raiz_dados_datasus <- "dados_originais_datasus"
pastas_sistemas <- list(
  sinasc = file.path(raiz_dados_datasus, "sinasc"),
  sim = file.path(raiz_dados_datasus, "sim"),
  sia = file.path(raiz_dados_datasus, "sia"),
  sih = file.path(raiz_dados_datasus, "sih"),
  sinan = file.path(raiz_dados_datasus, "sinan")
)
invisible(lapply(pastas_sistemas, dir.create, recursive = TRUE, showWarnings = FALSE))

uf_download <- "DF"
ano_download <- 2024
mes_download <- 6

gerar_nome_arquivo <- function(sistema, uf, ano, tipo, mes = NULL) {
  if (is.null(mes)) {
    return(sprintf("%s_%s_%04d_%s.csv", sistema, uf, ano, tipo))
  }
  sprintf("%s_%s_%04d_%02d_%s.csv", sistema, uf, ano, mes, tipo)
}

# ------------------------------------------------------------------------------
# SESSAO 2 - DOWNLOAD SINASC (DF, 2024-06)
# ------------------------------------------------------------------------------
sistema_sinasc <- "sinasc"
codigo_sinasc <- "SINASC"

df_sinasc_completo <- fetch_datasus(
  year_start = ano_download,
  year_end = ano_download,
  month_start = mes_download,
  month_end = mes_download,
  uf = uf_download,
  information_system = codigo_sinasc
)

df_sinasc_completo <- process_sinasc(df_sinasc_completo)

arquivo_sinasc_completo <- gerar_nome_arquivo(
  sistema = sistema_sinasc,
  uf = uf_download,
  ano = ano_download,
  mes = mes_download,
  tipo = "completo"
)
caminho_sinasc_completo <- file.path(pastas_sistemas[[sistema_sinasc]], arquivo_sinasc_completo)
write_csv(df_sinasc_completo, caminho_sinasc_completo)

print(head(df_sinasc_completo))
glimpse(df_sinasc_completo)

variaveis_sinasc <- c(
  "CODMUNNASC",
  "IDADEMAE",
  "PARTO",
  "ESCMAE",
  "QTDFILVIVO",
  "CONSULTAS",
  "SERIESCMAE",
  "RACACORMAE",
  "ESTCIVMAE",
  "APGAR1",
  "APGAR5"
)

df_sinasc_variaveis <- fetch_datasus(
  year_start = ano_download,
  year_end = ano_download,
  month_start = mes_download,
  month_end = mes_download,
  uf = uf_download,
  information_system = codigo_sinasc,
  vars = variaveis_sinasc
)

df_sinasc_variaveis <- process_sinasc(df_sinasc_variaveis)

arquivo_sinasc_variaveis <- gerar_nome_arquivo(
  sistema = sistema_sinasc,
  uf = uf_download,
  ano = ano_download,
  mes = mes_download,
  tipo = "variaveis"
)
caminho_sinasc_variaveis <- file.path(pastas_sistemas[[sistema_sinasc]], arquivo_sinasc_variaveis)
write_csv(df_sinasc_variaveis, caminho_sinasc_variaveis)

print(head(df_sinasc_variaveis))
glimpse(df_sinasc_variaveis)

# ------------------------------------------------------------------------------
# SESSAO 3 - DOWNLOAD SIM (DF, 2024)
# ------------------------------------------------------------------------------
sistema_sim <- "sim"
codigo_sim <- "SIM-DO"

df_sim_completo <- fetch_datasus(
  year_start = ano_download,
  year_end = ano_download,
  uf = uf_download,
  information_system = codigo_sim
)

df_sim_completo <- process_sim(df_sim_completo)

arquivo_sim_completo <- gerar_nome_arquivo(
  sistema = sistema_sim,
  uf = uf_download,
  ano = ano_download,
  mes = NULL,
  tipo = "completo"
)

caminho_sim_completo <- file.path(pastas_sistemas[[sistema_sim]], arquivo_sim_completo)
write_csv(df_sim_completo, caminho_sim_completo)

print(head(df_sim_completo))
glimpse(df_sim_completo)

variaveis_sim <- c(
  "ORIGEM",
  "TIPOBITO",
  "DTNASC",
  "IDADE",
  "RACACOR",
  "ESCMAE",
  "PARTO",
  "GRAVIDEZ",
  "SEMAGESTAC",
  "CAUSABAS"
)

df_sim_variaveis <- fetch_datasus(
  year_start = ano_download,
  year_end = ano_download,
  uf = uf_download,
  information_system = codigo_sim,
  vars = variaveis_sim
)

df_sim_variaveis <- process_sim(df_sim_variaveis)

arquivo_sim_variaveis <- gerar_nome_arquivo(
  sistema = sistema_sim,
  uf = uf_download,
  ano = ano_download,
  mes = NULL,
  tipo = "variaveis"
)
caminho_sim_variaveis <- file.path(pastas_sistemas[[sistema_sim]], arquivo_sim_variaveis)
write_csv(df_sim_variaveis, caminho_sim_variaveis)

print(head(df_sim_variaveis))
glimpse(df_sim_variaveis)

# ------------------------------------------------------------------------------
# SESSAO 4 - DOWNLOAD SIA (DF, 2024-06)
# ------------------------------------------------------------------------------
sistema_sia <- "sia"
codigo_sia <- "SIA-AM"

df_sia_completo <- fetch_datasus(
  year_start = ano_download,
  year_end = ano_download,
  month_start = mes_download,
  month_end = mes_download,
  uf = uf_download,
  information_system = codigo_sia
)

arquivo_sia_completo <- gerar_nome_arquivo(
  sistema = sistema_sia,
  uf = uf_download,
  ano = ano_download,
  mes = mes_download,
  tipo = "completo"
)

caminho_sia_completo <- file.path(pastas_sistemas[[sistema_sia]], arquivo_sia_completo)
write_csv(df_sia_completo, caminho_sia_completo)

print(head(df_sia_completo))
glimpse(df_sia_completo)

# Download de variaveis do SIA-AM para 2023-2024 (todos os meses).
variaveis_sia <- c(
  "AP_MVM",
  "AP_AUTORIZ",
  "AP_CODUNI",
  "AP_CNSPCN",
  "AP_NUIDADE",
  "AP_SEXO",
  "AP_RACACOR",
  "AP_OBITO",
  "AP_ENCERR",
  "AM_TRANSPL",
  "AP_CIDPRI"
)

df_sia_variaveis <- fetch_datasus(
  year_start = 2023,
  year_end = 2024,
  month_start = 1,
  month_end = 12,
  uf = uf_download,
  information_system = codigo_sia,
  vars = variaveis_sia
)

# Mantem somente CID principal iniciando com E10 (diabetes).
df_sia_variaveis <- df_sia_variaveis %>%
  filter(str_starts(AP_CIDPRI, "E10"))

arquivo_sia_variaveis <- sprintf(
  "%s_%s_%04d_%04d_%s.csv",
  sistema_sia,
  uf_download,
  2023,
  2024,
  "variaveis_e10"
)

caminho_sia_variaveis <- file.path(pastas_sistemas[[sistema_sia]], arquivo_sia_variaveis)
write_csv(df_sia_variaveis, caminho_sia_variaveis)

print(head(df_sia_variaveis))
glimpse(df_sia_variaveis)

# ------------------------------------------------------------------------------
# SESSAO 5 - DOWNLOAD SIH (DF, 2024-06)
# ------------------------------------------------------------------------------
sistema_sih <- "sih"
codigo_sih <- "SIH-RD"

df_sih_completo <- fetch_datasus(
  year_start = ano_download,
  year_end = ano_download,
  month_start = mes_download,
  month_end = mes_download,
  uf = uf_download,
  information_system = codigo_sih
)

df_sih_completo <- process_sih(df_sih_completo)

arquivo_sih_completo <- gerar_nome_arquivo(
  sistema = sistema_sih,
  uf = uf_download,
  ano = ano_download,
  mes = mes_download,
  tipo = "completo"
)

caminho_sih_completo <- file.path(pastas_sistemas[[sistema_sih]], arquivo_sih_completo)
write_csv(df_sih_completo, caminho_sih_completo)

print(head(df_sih_completo))
glimpse(df_sih_completo)

variaveis_sih <- c(
  "ANO_CMPT",
  "MES_CMPT",
  "DT_INTER",
  "DT_SAIDA",
  "DIAG_PRINC",
  "QT_DIARIAS",
  "NASC",
  "SEXO",
  "MORTE",
  "VAL_SH",
  "VAL_SP",
  "VAL_TOT"
)

df_sih_variaveis <- fetch_datasus(
  year_start = ano_download,
  year_end = ano_download,
  month_start = mes_download,
  month_end = mes_download,
  uf = uf_download,
  information_system = codigo_sih,
  vars = variaveis_sih
)

df_sih_variaveis <- process_sih(df_sih_variaveis) %>%
  filter(str_starts(DIAG_PRINC, "J18"))

arquivo_sih_variaveis <- sprintf(
  "%s_%s_%04d_%02d_%s.csv",
  sistema_sih,
  uf_download,
  ano_download,
  mes_download,
  "variaveis_j18"
)

caminho_sih_variaveis <- file.path(pastas_sistemas[[sistema_sih]], arquivo_sih_variaveis)
write_csv(df_sih_variaveis, caminho_sih_variaveis)

print(head(df_sih_variaveis))
glimpse(df_sih_variaveis)

# ------------------------------------------------------------------------------
# SESSAO 6 - DOWNLOAD SINAN DENGUE (DF, 2024)
# ------------------------------------------------------------------------------
sistema_sinan <- "sinan"
codigo_sinan <- "SINAN-DENGUE"

df_sinan_completo <- fetch_datasus(
  year_start = ano_download,
  year_end = ano_download,
  information_system = codigo_sinan
)

# Recorte do DF ainda no dado bruto (municipios iniciados por 53).
df_sinan_completo <- df_sinan_completo %>%
  filter(str_starts(as.character(ID_MUNICIP), "53"))

df_sinan_completo <- process_sinan_dengue(df_sinan_completo)

arquivo_sinan_completo <- paste(
  sistema_sinan,
  uf_download,
  ano_download,
  "completo.csv",
  sep = "_"
)

caminho_sinan_completo <- file.path(pastas_sistemas[[sistema_sinan]], arquivo_sinan_completo)
write_csv(df_sinan_completo, caminho_sinan_completo)

print(head(df_sinan_completo))
glimpse(df_sinan_completo)

df_sinan_variaveis <- df_sinan_completo %>%
  select(any_of(c("DT_SIN_PRI", "DT_NOTIFIC", "TP_NOT", "ID_AGRAVO", "DT_NOTIFIC", "ANO_NASC", "IDADEanos")))

arquivo_sinan_variaveis <- paste(
  sistema_sinan,
  uf_download,
  ano_download,
  "variaveis.csv",
  sep = "_"
)

caminho_sinan_variaveis <- file.path(pastas_sistemas[[sistema_sinan]], arquivo_sinan_variaveis)
write_csv(df_sinan_variaveis, caminho_sinan_variaveis)

print(head(df_sinan_variaveis))
glimpse(df_sinan_variaveis)
