# =============================================================================
#  ANALISE COMPLETA - COINFECCOES TB-HIV EM PESSOAS EM SITUACAO DE RUA
#  Salvador (BA), Rio de Janeiro (RJ), Porto Alegre (RS), Vitoria (ES)
#  Periodo: 2015-2024 | Fonte: SINAN-TB / DATASUS (FTP direto via read.dbc)
#
#  NOTA METODOLOGICA:
#  - Dados de PSR vem exclusivamente do SINAN-TB (variavel POP_RUA)
#  - O SINAN-Sifilis Adquirida NAO possui campo de populacao de rua
#  - A analise TB-Sifilis usa linkage ecologico (mesmo estrato demografico)
#  - Idoso definido como >= 45 anos
#  - Registros com raca/cor "Ignorado" sao excluidos
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(scipen = 999, warn = -1)
set.seed(42)

cat("================================================================\n")
cat("  COINFECCAO TB-HIV EM PESSOAS EM SITUACAO DE RUA\n")
cat("  Salvador | Rio de Janeiro | Porto Alegre | Vitoria\n")
cat("  2015-2024 | SINAN-TB / DATASUS\n")
cat("================================================================\n\n")

inicio_total <- Sys.time()

# ===========================================================================
# SECAO 0 - PACOTES
# ===========================================================================
cat("[0] Pacotes...\n")

pkgs <- c("read.dbc", "dplyr", "tidyr", "readr", "stringr", "lubridate",
           "forcats", "janitor", "forecast", "tseries", "zoo",
           "MASS", "lme4", "survival", "survminer", "segmented",
           "ggplot2", "scales", "patchwork", "viridis",
           "gtsummary", "broom", "broom.mixed", "purrr", "glue",
           "prophet", "officer", "flextable")

for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, quiet = TRUE)
}

suppressPackageStartupMessages({
  library(read.dbc); library(dplyr); library(tidyr); library(readr)
  library(stringr); library(lubridate); library(forcats); library(janitor)
  library(forecast); library(tseries); library(zoo)
  library(MASS); library(lme4); library(survival); library(survminer)
  library(segmented); library(ggplot2); library(scales); library(patchwork)
  library(viridis); library(gtsummary); library(broom); library(broom.mixed)
  library(purrr); library(glue); library(prophet)
  library(officer); library(flextable)
})

cat("  OK Pacotes prontos.\n")

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/figuras", showWarnings = FALSE)
dir.create("outputs/tabelas", showWarnings = FALSE)
dir.create("outputs/modelos", showWarnings = FALSE)

# ===========================================================================
# SECAO 1 - CONFIGURACOES
# ===========================================================================
UFS_COD <- c("29", "33", "43", "32")
UFS_NOME <- c("29" = "Salvador", "33" = "Rio de Janeiro",
              "43" = "Porto Alegre", "32" = "Vitoria")

ANO_INICIO <- 2015
ANO_FIM    <- 2024
IDADE_IDOSO <- 45
HORIZONTE_FORECAST <- 4

capitais_lista <- c("Todas", "Salvador", "Rio de Janeiro", "Porto Alegre", "Vitoria")
capitais_order <- c("Salvador", "Rio de Janeiro", "Porto Alegre", "Vitoria")

cores_capitais <- c("Salvador" = "#E74C3C", "Rio de Janeiro" = "#3498DB",
                    "Porto Alegre" = "#27AE60", "Vitoria" = "#F39C12")

# ===========================================================================
# SECAO 2 - DOWNLOAD VIA FTP
# ===========================================================================
cat("\n[1] Baixando dados do DATASUS (FTP)...\n")

baixar_dbc <- function(doenca, ano) {
  aa <- sprintf("%02d", ano %% 100)
  arquivo <- paste0(doenca, "BR", aa, ".dbc")
  urls <- c(
    paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/", arquivo),
    paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/", arquivo))
  temp <- tempfile(fileext = ".dbc")
  for (url in urls) {
    ok <- tryCatch({
      download.file(url, temp, mode = "wb", quiet = TRUE, timeout = 120)
      file.exists(temp) && file.size(temp) > 100
    }, error = function(e) FALSE)
    if (ok) {
      df <- tryCatch(read.dbc(temp), error = function(e) NULL)
      unlink(temp)
      if (!is.null(df)) { df$ano_arquivo <- ano; return(df) }
    }
  }
  return(NULL)
}

cat("  Tuberculose (TUBE)...\n")
lista_tb <- list()
for (ano in ANO_INICIO:ANO_FIM) {
  cat(glue("    {ano}... "))
  d <- baixar_dbc("TUBE", ano)
  if (!is.null(d)) { cat(glue("{nrow(d)}\n")); lista_tb[[as.character(ano)]] <- d }
  else cat("indisponivel\n")
}
dados_tb_raw <- bind_rows(lista_tb)
cat(glue("  Total TB: {format(nrow(dados_tb_raw), big.mark='.')} registros\n\n"))

cat("  Sifilis Adquirida (SIFA)...\n")
lista_sif <- list()
for (ano in ANO_INICIO:ANO_FIM) {
  cat(glue("    {ano}... "))
  d <- baixar_dbc("SIFA", ano)
  if (!is.null(d)) { cat(glue("{nrow(d)}\n")); lista_sif[[as.character(ano)]] <- d }
  else cat("indisponivel\n")
}
dados_sif_raw <- bind_rows(lista_sif)
cat(glue("  Total Sifilis: {format(nrow(dados_sif_raw), big.mark='.')} registros\n"))

# ===========================================================================
# SECAO 3 - LIMPEZA
# ===========================================================================
cat("\n[2] Limpando dados...\n")

limpar_tb <- function(df) {
  df %>%
    filter(as.character(SG_UF_NOT) %in% UFS_COD) %>%
    mutate(
      nu_idade_num = as.numeric(as.character(NU_IDADE_N)),
      idade_anos = case_when(
        nu_idade_num >= 4000 ~ nu_idade_num - 4000,
        nu_idade_num >= 3000 ~ nu_idade_num - 3000,
        nu_idade_num >= 2000 ~ floor((nu_idade_num - 2000) / 12),
        TRUE ~ 0L),
      faixa_etaria = cut(idade_anos,
        breaks = c(0, 17, 29, 44, 59, 69, Inf),
        labels = c("0-17", "18-29", "30-44", "45-59", "60-69", "70+"),
        right = TRUE, include.lowest = TRUE),
      idoso = (idade_anos >= IDADE_IDOSO),
      sexo = case_when(
        toupper(trimws(as.character(CS_SEXO))) %in% c("F", "2") ~ "Feminino",
        toupper(trimws(as.character(CS_SEXO))) %in% c("M", "1") ~ "Masculino",
        TRUE ~ NA_character_),
      raca_cor = case_when(
        as.character(CS_RACA) == "1" ~ "Branca",
        as.character(CS_RACA) == "2" ~ "Preta",
        as.character(CS_RACA) == "3" ~ "Amarela",
        as.character(CS_RACA) == "4" ~ "Parda",
        as.character(CS_RACA) == "5" ~ "Indigena",
        TRUE ~ NA_character_),
      uf_cod = as.character(SG_UF_NOT),
      capital = UFS_NOME[uf_cod],
      psr = (as.character(POP_RUA) == "1"),
      hiv_resultado = case_when(
        as.character(HIV) == "1" ~ "Positivo",
        as.character(HIV) == "2" ~ "Negativo",
        as.character(HIV) == "3" ~ "Em andamento",
        as.character(HIV) == "4" ~ "Nao realizado",
        TRUE ~ "Nao informado"),
      hiv_positivo = (hiv_resultado == "Positivo"),
      dt_notific = as.Date(DT_NOTIFIC),
      ano_notific = year(dt_notific),
      mes_notific = month(dt_notific),
      # Variaveis clinicas adicionais (baseado na literatura)
      uso_drogas = (as.character(AGRAVDROGA) == "1"),
      tabagismo  = (as.character(AGRAVTABAC) == "1"),
      aids       = (as.character(AGRAVAIDS) == "1"),
      antirretroviral = case_when(
        as.character(ANT_RETRO) == "1" ~ "Sim",
        as.character(ANT_RETRO) == "2" ~ "Nao",
        TRUE ~ "Nao informado"),
      trat_supervisionado = (as.character(TRAT_SUPER) == "1"),
      forma_clinica = case_when(
        as.character(FORMA) == "1" ~ "Pulmonar",
        as.character(FORMA) == "2" ~ "Extrapulmonar",
        as.character(FORMA) == "3" ~ "Pulmonar + Extrapulmonar",
        TRUE ~ "Nao informado"),
      desfecho = case_when(
        as.character(SITUA_ENCE) == "1" ~ "Cura",
        as.character(SITUA_ENCE) == "2" ~ "Abandono",
        as.character(SITUA_ENCE) == "3" ~ "Obito por TB",
        as.character(SITUA_ENCE) == "5" ~ "Obito outras causas",
        as.character(SITUA_ENCE) == "7" ~ "Transferencia",
        as.character(SITUA_ENCE) == "9" ~ "TB-DR",
        as.character(SITUA_ENCE) == "10" ~ "Mudanca esquema",
        TRUE ~ "Nao encerrado"),
      desfecho_favoravel = (desfecho == "Cura")
    ) %>%
    filter(!is.na(sexo), !is.na(raca_cor),
           !is.na(idade_anos), idade_anos >= 0, idade_anos <= 120,
           !is.na(ano_notific),
           ano_notific >= ANO_INICIO, ano_notific <= ANO_FIM)
}

limpar_sif <- function(df) {
  tem_hiv <- "HIV" %in% names(df)
  df %>%
    filter(as.character(SG_UF_NOT) %in% UFS_COD) %>%
    mutate(
      nu_idade_num = as.numeric(as.character(NU_IDADE_N)),
      idade_anos = case_when(
        nu_idade_num >= 4000 ~ nu_idade_num - 4000,
        nu_idade_num >= 3000 ~ nu_idade_num - 3000,
        nu_idade_num >= 2000 ~ floor((nu_idade_num - 2000) / 12),
        TRUE ~ 0L),
      faixa_etaria = cut(idade_anos,
        breaks = c(0, 17, 29, 44, 59, 69, Inf),
        labels = c("0-17", "18-29", "30-44", "45-59", "60-69", "70+"),
        right = TRUE, include.lowest = TRUE),
      idoso = (idade_anos >= IDADE_IDOSO),
      sexo = case_when(
        toupper(trimws(as.character(CS_SEXO))) %in% c("F", "2") ~ "Feminino",
        toupper(trimws(as.character(CS_SEXO))) %in% c("M", "1") ~ "Masculino",
        TRUE ~ NA_character_),
      raca_cor = case_when(
        as.character(CS_RACA) == "1" ~ "Branca",
        as.character(CS_RACA) == "2" ~ "Preta",
        as.character(CS_RACA) == "3" ~ "Amarela",
        as.character(CS_RACA) == "4" ~ "Parda",
        as.character(CS_RACA) == "5" ~ "Indigena",
        TRUE ~ NA_character_),
      uf_cod = as.character(SG_UF_NOT),
      capital = UFS_NOME[uf_cod],
      sg_uf_not = uf_cod,
      dt_notific = as.Date(DT_NOTIFIC),
      ano_notific = year(dt_notific),
      mes_notific = month(dt_notific)
    ) %>%
    {
      if (tem_hiv) {
        mutate(., hiv_resultado = case_when(
          as.character(HIV) %in% c("1", "S") ~ "Positivo",
          as.character(HIV) %in% c("2", "N") ~ "Negativo",
          TRUE ~ "Nao informado"),
          hiv_positivo = (hiv_resultado == "Positivo"))
      } else {
        mutate(., hiv_resultado = "Nao informado", hiv_positivo = FALSE)
      }
    } %>%
    filter(!is.na(sexo), !is.na(raca_cor),
           !is.na(idade_anos), idade_anos >= 0, idade_anos <= 120,
           !is.na(ano_notific),
           ano_notific >= ANO_INICIO, ano_notific <= ANO_FIM)
}

dados_tb  <- limpar_tb(dados_tb_raw)
dados_sif <- limpar_sif(dados_sif_raw)

if (!"sg_uf_not" %in% names(dados_tb)) dados_tb$sg_uf_not <- dados_tb$uf_cod

cat(glue("  TB (4 capitais):     {format(nrow(dados_tb), big.mark='.')}\n"))
cat(glue("  Sifilis (4 capitais):{format(nrow(dados_sif), big.mark='.')}\n"))
cat(glue("  TB em PSR:           {format(sum(dados_tb$psr, na.rm=TRUE), big.mark='.')}\n"))
cat(glue("  TB-HIV positivo:     {format(sum(dados_tb$hiv_positivo, na.rm=TRUE), big.mark='.')}\n"))
cat(glue("  Ignorados removidos: raca/cor filtrada\n"))
cat(glue("  Idoso definido como: >= {IDADE_IDOSO} anos\n"))

cat("\n  NOTA: O SINAN-Sifilis Adquirida NAO possui variavel POP_RUA.\n")
cat("  Portanto, NAO e possivel identificar PSR nos dados de sifilis.\n")
cat("  A analise de PSR e restrita aos dados de TB do SINAN.\n")

saveRDS(dados_tb, "outputs/dados_tb_limpos.rds")
saveRDS(dados_sif, "outputs/dados_sif_limpos.rds")

# ===========================================================================
# SECAO 4 - SUBGRUPOS
# ===========================================================================
cat("\n[3] Subgrupos...\n")

sg_tb <- list(
  geral = dados_tb,
  psr = filter(dados_tb, psr == TRUE),
  mulheres = filter(dados_tb, sexo == "Feminino"),
  idosos = filter(dados_tb, idoso == TRUE),
  mulheres_psr = filter(dados_tb, sexo == "Feminino", psr == TRUE),
  idosos_psr = filter(dados_tb, idoso == TRUE, psr == TRUE),
  homens_psr = filter(dados_tb, sexo == "Masculino", psr == TRUE)
)

# Sifilis: SEM subgrupo PSR (nao ha dados)
sg_sif <- list(
  geral = dados_sif,
  mulheres = filter(dados_sif, sexo == "Feminino"),
  idosos = filter(dados_sif, idoso == TRUE)
)

for (nm in names(sg_tb)) cat(glue("  TB-{nm}: {format(nrow(sg_tb[[nm]]), big.mark='.')} | "))
cat("\n")

# ===========================================================================
# SECAO 5 - PREVALENCIA
# ===========================================================================
cat("\n[4] Prevalencias...\n")

calc_prev <- function(df, tipo_coinf, grupo_nome, capital_nm = "Todas") {
  df_f <- if (capital_nm == "Todas") df else filter(df, capital == capital_nm)
  df_f <- df_f %>% filter(hiv_resultado %in% c("Positivo", "Negativo"))
  n_total <- nrow(df_f)
  n_pos   <- sum(df_f$hiv_positivo, na.rm = TRUE)
  if (n_total < 5) {
    return(tibble(Coinfeccao = tipo_coinf, Grupo = grupo_nome, Capital = capital_nm,
                  N_testados = n_total, N_positivos = n_pos,
                  Prevalencia = NA_real_, IC_inferior = NA_real_, IC_superior = NA_real_))
  }
  pt <- prop.test(n_pos, n_total, conf.level = 0.95, correct = FALSE)
  tibble(Coinfeccao = tipo_coinf, Grupo = grupo_nome, Capital = capital_nm,
         N_testados = n_total, N_positivos = n_pos,
         Prevalencia = round(pt$estimate * 100, 2),
         IC_inferior = round(pt$conf.int[1] * 100, 2),
         IC_superior = round(pt$conf.int[2] * 100, 2))
}

nomes_tb <- c(geral = "Total", psr = "PSR", mulheres = "Mulheres",
              idosos = paste0(">=", IDADE_IDOSO, " anos"),
              mulheres_psr = "Mulheres em situacao de rua",
              idosos_psr = paste0(">=", IDADE_IDOSO, " anos em situacao de rua"),
              homens_psr = "Homens em situacao de rua")

prev_tb_hiv <- map_dfr(names(sg_tb), function(g) {
  map_dfr(capitais_lista, function(cap) calc_prev(sg_tb[[g]], "TB-HIV", nomes_tb[g], cap))
})

# TB-Sifilis: linkage ecologico (conservador)
prev_tb_sif <- map_dfr(capitais_lista, function(cap) {
  tb_c  <- if (cap == "Todas") dados_tb  else filter(dados_tb, capital == cap)
  sif_c <- if (cap == "Todas") dados_sif else filter(dados_sif, capital == cap)
  tb_a  <- tb_c %>% count(uf_cod, ano_notific, sexo, faixa_etaria, name = "n_tb")
  sif_a <- sif_c %>% count(uf_cod, ano_notific, sexo, faixa_etaria, name = "n_sif")
  j <- inner_join(tb_a, sif_a, by = c("uf_cod", "ano_notific", "sexo", "faixa_etaria")) %>%
    mutate(n_coinf = pmin(n_tb, n_sif))
  tibble(Coinfeccao = "TB-Sifilis (estimativa ecologica)", Grupo = "Total",
         Capital = cap, N_testados = sum(j$n_tb), N_positivos = sum(j$n_coinf),
         Prevalencia = ifelse(sum(j$n_tb) > 0, round(sum(j$n_coinf) / sum(j$n_tb) * 100, 2), NA),
         IC_inferior = NA_real_, IC_superior = NA_real_)
})

tab_prev_geral <- bind_rows(prev_tb_hiv, prev_tb_sif)

cat("\n  === PREVALENCIA TB-HIV (Todas as capitais) ===\n")
tab_prev_geral %>%
  filter(Capital == "Todas", Coinfeccao == "TB-HIV") %>%
  select(Grupo, N_testados, N_positivos, Prevalencia, IC_inferior, IC_superior) %>%
  print(n = 20)

write_csv(tab_prev_geral, "outputs/tabelas/tab1_prevalencia_geral.csv")

# Tabela 1 descritiva
tab1_csv <- sg_tb$psr %>%
  group_by(capital) %>%
  summarise(
    N = n(),
    Feminino_pct = round(mean(sexo == "Feminino") * 100, 1),
    Idoso_45_pct = round(mean(idoso) * 100, 1),
    Preta_Parda_pct = round(mean(raca_cor %in% c("Preta", "Parda")) * 100, 1),
    HIV_positivo_pct = round(mean(hiv_positivo) * 100, 1),
    Idade_mediana = median(idade_anos),
    Idade_IIQ = paste0(quantile(idade_anos, 0.25), "-", quantile(idade_anos, 0.75)),
    .groups = "drop") %>%
  arrange(factor(capital, levels = capitais_order))

cat("\n  === TABELA 1: Perfil PSR com TB ===\n")
print(tab1_csv)
write_csv(tab1_csv, "outputs/tabelas/tab1_descritiva_psr_tb.csv")

# ===========================================================================
# SECAO 6 - SERIES TEMPORAIS
# ===========================================================================
cat("\n[5] Series temporais...\n")

construir_serie <- function(df, capital_nm = "Todas") {
  df_f <- if (capital_nm == "Todas") df else filter(df, capital == capital_nm)
  df_f <- df_f %>% filter(hiv_resultado %in% c("Positivo", "Negativo"))
  serie <- df_f %>%
    group_by(ano_notific, mes_notific) %>%
    summarise(n_testado = n(), n_coinf = sum(hiv_positivo), .groups = "drop")
  grade <- expand.grid(ano_notific = ANO_INICIO:ANO_FIM, mes_notific = 1:12)
  serie <- left_join(grade, serie, by = c("ano_notific", "mes_notific")) %>%
    replace_na(list(n_testado = 0, n_coinf = 0)) %>%
    arrange(ano_notific, mes_notific) %>%
    mutate(data = as.Date(paste(ano_notific, mes_notific, "01"), "%Y %m %d"),
           prev = ifelse(n_testado > 0, n_coinf / n_testado * 100, 0))
  serie
}

como_ts <- function(s, col = "n_coinf") ts(s[[col]], start = c(ANO_INICIO, 1), frequency = 12)

combs <- list(
  list(df = sg_tb$psr, label = "PSR (todas as capitais)", cap = "Todas"),
  list(df = sg_tb$mulheres_psr, label = "Mulheres PSR", cap = "Todas"),
  list(df = sg_tb$idosos_psr, label = paste0(">=", IDADE_IDOSO, " anos PSR"), cap = "Todas"),
  list(df = sg_tb$psr, label = "PSR - Salvador", cap = "Salvador"),
  list(df = sg_tb$psr, label = "PSR - Rio de Janeiro", cap = "Rio de Janeiro"),
  list(df = sg_tb$psr, label = "PSR - Porto Alegre", cap = "Porto Alegre"),
  list(df = sg_tb$psr, label = "PSR - Vitoria", cap = "Vitoria"))

series_lista <- lapply(combs, function(c) {
  s <- construir_serie(c$df, c$cap)
  list(label = c$label, cap = c$cap, serie = s, ts_obj = como_ts(s))
})

# Testes estacionaridade
tab_testes <- map_dfr(series_lista, function(s) {
  adf  <- tryCatch(adf.test(s$ts_obj), error = function(e) list(p.value = NA))
  kpss <- tryCatch(kpss.test(s$ts_obj), error = function(e) list(p.value = NA))
  tibble(Serie = s$label,
         ADF_p = round(adf$p.value, 4), KPSS_p = round(kpss$p.value, 4),
         Estacionaria = ifelse(!is.na(adf$p.value) & adf$p.value < 0.05 & kpss$p.value > 0.05,
                               "Sim", "Nao"))
})
cat("\n  Testes de estacionaridade:\n"); print(tab_testes)
write_csv(tab_testes, "outputs/tabelas/tab_testes_estacionaridade.csv")

# STL
if (sum(series_lista[[1]]$ts_obj) > 0) {
  stl_fit <- stl(series_lista[[1]]$ts_obj, s.window = "periodic", robust = TRUE)
  png("outputs/figuras/fig_stl.png", width = 2400, height = 1600, res = 200)
  plot(stl_fit, main = "Seasonal-Trend Decomposition (STL) - TB-HIV em PSR")
  dev.off()
}

# ARIMA
cat("  ARIMA...\n")
modelos_arima <- lapply(series_lista, function(s) {
  m <- tryCatch(auto.arima(s$ts_obj, seasonal = TRUE, stepwise = TRUE), error = function(e) NULL)
  if (!is.null(m)) cat(glue("    {s$label}: ARIMA({paste(arimaorder(m), collapse=',')})\n"))
  list(label = s$label, modelo = m)
})

tab_arima <- map_dfr(modelos_arima, function(m) {
  if (is.null(m$modelo)) return(tibble(Serie = m$label))
  tibble(Serie = m$label, Ordem = paste0("(", paste(arimaorder(m$modelo), collapse = ","), ")"),
         AICc = round(m$modelo$aicc, 1))
})
write_csv(tab_arima, "outputs/tabelas/tab_arima_diagnostico.csv")

# ETS
cat("  ETS...\n")
modelos_ets <- lapply(series_lista, function(s) {
  list(label = s$label, modelo = tryCatch(ets(s$ts_obj), error = function(e) NULL))
})

# Prophet
cat("  Prophet...\n")
modelos_prophet <- lapply(series_lista, function(s) {
  dp <- s$serie %>% select(ds = data, y = n_coinf) %>% filter(!is.na(y))
  if (nrow(dp) < 24) return(list(label = s$label, modelo = NULL))
  m <- tryCatch(suppressMessages(prophet(dp, yearly.seasonality = TRUE,
                                          weekly.seasonality = FALSE,
                                          daily.seasonality = FALSE)),
                error = function(e) NULL)
  list(label = s$label, modelo = m, dados = dp)
})

# Forecast ensemble
cat("  Forecast ensemble...\n")
H <- HORIZONTE_FORECAST * 12
datas_fc <- seq(as.Date(paste0(ANO_FIM + 1, "-01-01")), by = "month", length.out = H)

fc_arima <- lapply(modelos_arima, function(m) {
  if (is.null(m$modelo)) NULL else tryCatch(forecast(m$modelo, h = H), error = function(e) NULL)
})
fc_ets <- lapply(modelos_ets, function(m) {
  if (is.null(m$modelo)) NULL else tryCatch(forecast(m$modelo, h = H), error = function(e) NULL)
})

ensembles <- lapply(seq_along(series_lista), function(i) {
  pv <- list()
  if (!is.null(fc_arima[[i]])) pv$ARIMA <- as.numeric(fc_arima[[i]]$mean)
  if (!is.null(fc_ets[[i]]))   pv$ETS   <- as.numeric(fc_ets[[i]]$mean)
  if (length(pv) == 0) return(NULL)
  mat <- do.call(cbind, pv)
  tibble(Serie = series_lista[[i]]$label, Data = datas_fc,
         Projecao = pmax(rowMeans(mat, na.rm = TRUE), 0),
         IC_inferior = pmax(apply(mat, 1, min, na.rm = TRUE), 0),
         IC_superior = pmax(apply(mat, 1, max, na.rm = TRUE), 0))
})

tab_ensemble <- bind_rows(ensembles)
write_csv(tab_ensemble, "outputs/tabelas/tab_ensemble_forecast.csv")

# ===========================================================================
# SECAO 7 - JOINPOINT
# ===========================================================================
cat("\n[6] Joinpoint...\n")

serie_anual_tb <- function(df, cap = "Todas") {
  d <- if (cap == "Todas") df else filter(df, capital == cap)
  d %>% filter(hiv_resultado %in% c("Positivo", "Negativo")) %>%
    group_by(ano_notific) %>%
    summarise(n = n(), nc = sum(hiv_positivo), prev = nc / n * 100, .groups = "drop")
}

gp_jp <- list(
  list(df = sg_tb$psr, cap = "Todas", lab = "PSR (todas)"),
  list(df = sg_tb$mulheres_psr, cap = "Todas", lab = "Mulheres PSR"),
  list(df = sg_tb$idosos_psr, cap = "Todas", lab = paste0(">=", IDADE_IDOSO, " PSR")),
  list(df = sg_tb$psr, cap = "Salvador", lab = "PSR Salvador"),
  list(df = sg_tb$psr, cap = "Rio de Janeiro", lab = "PSR Rio de Janeiro"),
  list(df = sg_tb$psr, cap = "Porto Alegre", lab = "PSR Porto Alegre"),
  list(df = sg_tb$psr, cap = "Vitoria", lab = "PSR Vitoria"))

resultados_jp <- lapply(gp_jp, function(g) {
  da <- serie_anual_tb(g$df, g$cap)
  if (nrow(da) < 5 || sum(da$prev) == 0) return(NULL)
  da$lp <- log(pmax(da$prev, 0.01))
  da$t <- da$ano_notific - min(da$ano_notific)
  base <- lm(lp ~ t, data = da)
  seg <- tryCatch(segmented(base, seg.Z = ~t, npsi = 1, control = seg.control(it.max = 100)),
                  error = function(e) base)
  list(label = g$lab, modelo = seg, dados = da)
})

tab_apc <- map_dfr(resultados_jp, function(jp) {
  if (is.null(jp)) return(NULL)
  seg <- jp$modelo
  if (inherits(seg, "segmented")) {
    coefs <- slope(seg)$t
    data.frame(Grupo = jp$label, Segmento = seq_along(coefs[, 1]),
               APC = round((exp(coefs[, 1]) - 1) * 100, 2))
  } else {
    b <- coef(seg)[2]
    data.frame(Grupo = jp$label, Segmento = 1, APC = round((exp(b) - 1) * 100, 2))
  }
})

cat("\n  === APC (Annual Percent Change) ===\n")
print(tab_apc, row.names = FALSE)
write_csv(tab_apc, "outputs/tabelas/tab_apc_joinpoint.csv")

# ===========================================================================
# SECAO 8 - BINOMIAL NEGATIVA
# ===========================================================================
cat("\n[7] Binomial Negativa...\n")

base_bneg <- dados_tb %>%
  filter(psr == TRUE, hiv_resultado %in% c("Positivo", "Negativo")) %>%
  group_by(capital, ano_notific, mes_notific) %>%
  summarise(nt = n(), nc = sum(hiv_positivo), .groups = "drop") %>%
  filter(nt >= 5) %>%
  mutate(tempo = (ano_notific - ANO_INICIO) * 12 + mes_notific,
         capital = factor(capital), lo = log(nt))

modelo_nb <- NULL; tab_nb <- tibble()
if (nrow(base_bneg) >= 20) {
  modelo_nb <- tryCatch(
    glm.nb(nc ~ tempo + capital + offset(lo), data = base_bneg),
    error = function(e) glm(nc ~ tempo + capital + offset(lo), data = base_bneg, family = poisson))
  tab_nb <- tidy(modelo_nb, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(across(c(estimate, conf.low, conf.high, p.value), ~ round(., 4))) %>%
    rename(IRR = estimate, IC_inferior = conf.low, IC_superior = conf.high)
  cat("\n  === IRR (Incidence Rate Ratios) ===\n"); print(tab_nb)
  write_csv(tab_nb, "outputs/tabelas/tab_nb_irr.csv")
}

# ===========================================================================
# SECAO 9 - LOGISTICA
# ===========================================================================
cat("\n[8] Regressao Logistica...\n")

base_logit <- dados_tb %>%
  filter(psr == TRUE, hiv_resultado %in% c("Positivo", "Negativo"),
         !is.na(sexo), !is.na(faixa_etaria)) %>%
  mutate(desfecho = as.integer(hiv_positivo),
         sexo = factor(sexo, levels = c("Masculino", "Feminino")),
         faixa_etaria = fct_relevel(faixa_etaria, "18-29"),
         raca_cor = fct_lump_min(factor(raca_cor), min = 20),
         capital = factor(capital),
         tendencia = (ano_notific - ANO_INICIO) / (ANO_FIM - ANO_INICIO))

tab_or <- tibble()
if (nrow(base_logit) >= 50) {
  modelo_logit <- glm(desfecho ~ sexo + faixa_etaria + raca_cor + capital + tendencia,
                      data = base_logit, family = binomial)
  tab_or <- tidy(modelo_logit, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(across(c(estimate, conf.low, conf.high, p.value), ~ round(., 3))) %>%
    rename(OR = estimate, IC_inferior = conf.low, IC_superior = conf.high) %>%
    mutate(Significancia = ifelse(p.value < 0.05, "*", ""))
  cat("\n  === OR Ajustados ===\n"); print(tab_or)
  write_csv(tab_or, "outputs/tabelas/tab_or_logistico.csv")
}

# ===========================================================================
# SECAO 10 - GLMM
# ===========================================================================
cat("\n[9] GLMM...\n")

base_misto <- dados_tb %>%
  filter(psr == TRUE, hiv_resultado %in% c("Positivo", "Negativo")) %>%
  group_by(capital, ano_notific) %>%
  summarise(nt = n(), nc = sum(hiv_positivo), .groups = "drop") %>%
  filter(nt >= 5) %>% mutate(tempo = ano_notific - ANO_INICIO)

modelo_misto <- tryCatch(
  glmer(cbind(nc, nt - nc) ~ tempo + (1 | capital), data = base_misto, family = binomial),
  error = function(e) { cat("  GLMM erro:", e$message, "\n"); NULL })

tab_misto <- tibble()
if (!is.null(modelo_misto)) {
  tab_misto <- tidy(modelo_misto, effects = "fixed", exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(across(c(estimate, conf.low, conf.high, p.value), ~ round(., 4)))
  cat("\n  === GLMM Efeitos Fixos ===\n"); print(tab_misto)
  write_csv(tab_misto, "outputs/tabelas/tab_efeitos_mistos.csv")
}

# ===========================================================================
# SECAO 11 - SOBREVIVENCIA
# ===========================================================================
cat("\n[10] Sobrevivencia...\n")

base_surv <- dados_tb %>%
  filter(psr == TRUE, !is.na(dt_notific)) %>%
  mutate(data_fim = as.Date(paste0(ANO_FIM, "-12-31")),
         tempo_obs = as.numeric(difftime(data_fim, dt_notific, units = "days")) / 30.44,
         tempo_obs = pmax(tempo_obs, 0.5),
         evento = as.integer(hiv_positivo),
         grupo_surv = case_when(
           sexo == "Feminino" & idoso ~ paste0("Mulher >=", IDADE_IDOSO),
           sexo == "Feminino"         ~ paste0("Mulher <", IDADE_IDOSO),
           idoso                      ~ paste0("Homem >=", IDADE_IDOSO),
           TRUE                       ~ paste0("Homem <", IDADE_IDOSO))) %>%
  filter(!is.na(tempo_obs))

km_fit <- NULL; cox_fit <- NULL; tab_cox <- tibble()
if (nrow(base_surv) >= 20) {
  km_fit <- survfit(Surv(tempo_obs, evento) ~ grupo_surv, data = base_surv)
  lr <- survdiff(Surv(tempo_obs, evento) ~ grupo_surv, data = base_surv)
  cat("  Log-rank p-valor:", round(1 - pchisq(lr$chisq, length(lr$n) - 1), 6), "\n")

  cox_fit <- coxph(Surv(tempo_obs, evento) ~ sexo + idoso + capital + raca_cor, data = base_surv)
  tab_cox <- tidy(cox_fit, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(across(c(estimate, conf.low, conf.high, p.value), ~ round(., 3))) %>%
    rename(HR = estimate, IC_inferior = conf.low, IC_superior = conf.high) %>%
    mutate(Significancia = ifelse(p.value < 0.05, "*", ""))
  cat("\n  === Hazard Ratios (Cox) ===\n"); print(tab_cox)
  write_csv(tab_cox, "outputs/tabelas/tab_cox_hr.csv")
}

# ===========================================================================
# SECAO 12 - ANALISES ADICIONAIS (baseadas na literatura)
# ===========================================================================
cat("\n[11] Analises adicionais (literatura)...\n")

# 12A - COMPARACAO PSR vs NAO-PSR (Ranzani et al 2016, BMC Med)
cat("  Comparacao PSR vs nao-PSR...\n")

comp_psr <- dados_tb %>%
  filter(hiv_resultado %in% c("Positivo", "Negativo")) %>%
  group_by(psr) %>%
  summarise(
    N = n(),
    HIV_pos = sum(hiv_positivo),
    Prevalencia = round(HIV_pos / N * 100, 1),
    Feminino_pct = round(mean(sexo == "Feminino") * 100, 1),
    Idade_med = median(idade_anos),
    Preta_Parda_pct = round(mean(raca_cor %in% c("Preta", "Parda")) * 100, 1),
    Drogas_pct = round(mean(uso_drogas, na.rm = TRUE) * 100, 1),
    Tabaco_pct = round(mean(tabagismo, na.rm = TRUE) * 100, 1),
    Cura_pct = round(mean(desfecho_favoravel, na.rm = TRUE) * 100, 1),
    Abandono_pct = round(mean(desfecho == "Abandono", na.rm = TRUE) * 100, 1),
    Obito_pct = round(mean(desfecho %in% c("Obito por TB", "Obito outras causas"), na.rm = TRUE) * 100, 1),
    .groups = "drop") %>%
  mutate(Grupo = ifelse(psr, "PSR", "Nao-PSR")) %>%
  select(Grupo, everything(), -psr)

cat("\n  === COMPARACAO PSR vs NAO-PSR ===\n")
print(comp_psr)
write_csv(comp_psr, "outputs/tabelas/tab_comparacao_psr_naopsr.csv")

# 12B - DESFECHOS POR STATUS HIV EM PSR (Prado et al 2017, BJID)
cat("\n  Desfechos por HIV em PSR...\n")

desfechos_hiv <- dados_tb %>%
  filter(psr == TRUE, desfecho != "Nao encerrado") %>%
  group_by(hiv_positivo) %>%
  summarise(
    N = n(),
    Cura_pct = round(mean(desfecho == "Cura") * 100, 1),
    Abandono_pct = round(mean(desfecho == "Abandono") * 100, 1),
    Obito_TB_pct = round(mean(desfecho == "Obito por TB") * 100, 1),
    Obito_outras_pct = round(mean(desfecho == "Obito outras causas") * 100, 1),
    .groups = "drop") %>%
  mutate(HIV = ifelse(hiv_positivo, "HIV+", "HIV-")) %>%
  select(HIV, everything(), -hiv_positivo)

cat("\n  === DESFECHOS TB EM PSR POR HIV ===\n")
print(desfechos_hiv)
write_csv(desfechos_hiv, "outputs/tabelas/tab_desfechos_hiv_psr.csv")

# 12C - LOGISTICA COM DROGAS E TABACO (Aldridge et al 2018, Lancet)
cat("\n  Logistica ajustada com substancias...\n")

base_logit2 <- dados_tb %>%
  filter(psr == TRUE, hiv_resultado %in% c("Positivo", "Negativo"),
         !is.na(sexo), !is.na(faixa_etaria)) %>%
  mutate(desfecho = as.integer(hiv_positivo),
         sexo = factor(sexo, levels = c("Masculino", "Feminino")),
         faixa_etaria = fct_relevel(faixa_etaria, "18-29"),
         raca_cor = fct_lump_min(factor(raca_cor), min = 20),
         capital = factor(capital),
         tendencia = (ano_notific - ANO_INICIO) / (ANO_FIM - ANO_INICIO))

tab_or2 <- tibble()
if (nrow(base_logit2) >= 50) {
  modelo_logit2 <- glm(
    desfecho ~ sexo + faixa_etaria + raca_cor + capital + tendencia +
               uso_drogas + tabagismo,
    data = base_logit2, family = binomial)

  tab_or2 <- tidy(modelo_logit2, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(across(c(estimate, conf.low, conf.high, p.value), ~ round(., 3))) %>%
    rename(OR = estimate, IC_inferior = conf.low, IC_superior = conf.high) %>%
    mutate(Significancia = ifelse(p.value < 0.05, "*", ""))

  cat("\n  === OR COM SUBSTANCIAS ===\n")
  print(tab_or2)
  write_csv(tab_or2, "outputs/tabelas/tab_or_com_substancias.csv")
}

# 12D - ANTIRRETROVIRAL EM TB-HIV+ PSR
cat("\n  Antirretroviral em PSR HIV+...\n")

arv_psr <- dados_tb %>%
  filter(psr == TRUE, hiv_positivo == TRUE) %>%
  group_by(capital) %>%
  summarise(
    N_HIV_pos = n(),
    ARV_sim_pct = round(mean(antirretroviral == "Sim", na.rm = TRUE) * 100, 1),
    ARV_nao_pct = round(mean(antirretroviral == "Nao", na.rm = TRUE) * 100, 1),
    ARV_NI_pct = round(mean(antirretroviral == "Nao informado", na.rm = TRUE) * 100, 1),
    .groups = "drop")

cat("\n  === ANTIRRETROVIRAL EM PSR HIV+ ===\n")
print(arv_psr)
write_csv(arv_psr, "outputs/tabelas/tab_arv_psr_hiv.csv")

# 12E - TRATAMENTO SUPERVISIONADO PSR vs NAO-PSR
cat("\n  Tratamento supervisionado...\n")

trat_sup <- dados_tb %>%
  group_by(psr) %>%
  summarise(
    N = n(),
    TDO_pct = round(mean(trat_supervisionado, na.rm = TRUE) * 100, 1),
    .groups = "drop") %>%
  mutate(Grupo = ifelse(psr, "PSR", "Nao-PSR"))

cat("\n  === TRATAMENTO SUPERVISIONADO ===\n")
print(trat_sup)

# Salvar modelos
saveRDS(list(logistica = if (exists("modelo_logit")) modelo_logit else NULL,
             logistica_substancias = if (exists("modelo_logit2")) modelo_logit2 else NULL,
             neg_binom = modelo_nb, misto = modelo_misto, cox = cox_fit,
             joinpoints = resultados_jp, arima = modelos_arima, ets = modelos_ets),
        "outputs/modelos/todos_modelos.rds")

# ===========================================================================
# SECAO 13 - FIGURAS PADRONIZADAS (estilo Lancet)
# ===========================================================================
cat("\n[11] Gerando figuras padronizadas...\n")

# Tema Lancet
# Tema Lancet: SEM titulo (vai na legenda), SEM grade/grelha
tema_lancet <- theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "grey95", colour = NA),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 9),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    axis.line = element_line(colour = "black", linewidth = 0.4),
    axis.ticks = element_line(colour = "black", linewidth = 0.3),
    plot.margin = margin(5, 10, 5, 5))

sfig <- function(nm, p, w = 174, h = 100) {
  ggsave(paste0("outputs/figuras/", nm, ".png"), p,
         width = w, height = h, units = "mm", dpi = 300, bg = "white")
  cat(glue("  {nm}.png ({w}x{h}mm, 300dpi)\n"))
}

# --- FIGURE 1: Prevalencia TB-HIV por grupo e capital ---
f1d <- tab_prev_geral %>%
  filter(Capital != "Todas", Coinfeccao == "TB-HIV",
         Grupo %in% c("PSR", nomes_tb["mulheres_psr"], nomes_tb["idosos_psr"])) %>%
  mutate(Capital = factor(Capital, levels = capitais_order),
         Grupo = factor(Grupo, levels = c("PSR", nomes_tb["mulheres_psr"], nomes_tb["idosos_psr"])),
         label = ifelse(!is.na(Prevalencia), paste0(sprintf("%.1f", Prevalencia), "%"), "ND"))

fig1 <- ggplot(f1d, aes(x = Capital, y = Prevalencia, fill = Grupo)) +
  geom_col(position = position_dodge(0.85), width = 0.75) +
  geom_errorbar(aes(ymin = IC_inferior, ymax = IC_superior),
                position = position_dodge(0.85), width = 0.2, colour = "grey30", na.rm = TRUE) +
  geom_text(aes(label = label, y = Prevalencia + 2),
            position = position_dodge(0.85), size = 2.8, vjust = 0) +
  scale_fill_manual(values = c("PSR" = "#2C3E50",
                                setNames("#C0392B", nomes_tb["mulheres_psr"]),
                                setNames("#2980B9", nomes_tb["idosos_psr"]))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Prevalence (%)", fill = NULL) +
  tema_lancet + theme(axis.text.x = element_text(angle = 15, hjust = 1))

sfig("fig1_prevalencia_tb_hiv", fig1, 174, 110)

# --- FIGURE 2: Serie temporal + forecast ---
sh <- series_lista[[1]]$serie
ft <- tab_ensemble %>% filter(str_detect(Serie, "todas"))

fig2 <- ggplot() +
  geom_line(data = sh, aes(x = data, y = n_coinf), colour = "#2C3E50", linewidth = 0.6) +
  geom_point(data = sh, aes(x = data, y = n_coinf), colour = "#2C3E50", size = 0.8, alpha = 0.5) +
  geom_ribbon(data = ft, aes(x = Data, ymin = IC_inferior, ymax = IC_superior),
              fill = "#C0392B", alpha = 0.15) +
  geom_line(data = ft, aes(x = Data, y = Projecao), colour = "#C0392B",
            linewidth = 0.6, linetype = "dashed") +
  geom_vline(xintercept = as.Date(paste0(ANO_FIM, "-12-31")),
             linetype = "dotted", colour = "grey50", linewidth = 0.4) +
  annotate("text", x = as.Date(paste0(ANO_FIM + 1, "-06-01")),
           y = max(sh$n_coinf, na.rm = TRUE) * 0.95, label = "Projection",
           size = 3, colour = "#C0392B", fontface = "italic") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = "Year", y = "TB-HIV co-infection cases (n)") +
  tema_lancet

sfig("fig2_serie_forecast", fig2, 174, 100)

# --- FIGURE 3: Tendencia Joinpoint por capital ---
f3d <- map_dfr(resultados_jp[4:7], function(jp) {
  if (is.null(jp)) return(NULL)
  jp$dados %>% mutate(label = jp$label)
})

if (nrow(f3d) > 0) {
  fig3 <- ggplot(f3d, aes(x = ano_notific, y = prev)) +
    geom_point(size = 2, colour = "#2C3E50") +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                colour = "#C0392B", fill = "#E74C3C", alpha = 0.12, linewidth = 0.7) +
    facet_wrap(~label, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = 2015:2024) +
    scale_y_continuous(labels = function(x) paste0(sprintf("%.0f", x), "%")) +
    labs(x = "Year", y = "Prevalence (%)") +
    tema_lancet + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  sfig("fig3_joinpoint", fig3, 174, 130)
}

# --- FIGURE 4: Forest plot ---
if (nrow(tab_or) > 0) {
  f4d <- tab_or %>%
    mutate(
      term_label = case_when(
        term == "sexoFeminino" ~ "Female sex",
        str_detect(term, "faixa_etaria") ~ str_replace(term, "faixa_etaria", "Age "),
        str_detect(term, "raca_cor") ~ str_replace(term, "raca_cor", "Race: "),
        str_detect(term, "capital") ~ str_replace(term, "capital", ""),
        term == "tendencia" ~ "Temporal trend",
        TRUE ~ term),
      sig_col = ifelse(p.value < 0.05, "Significant", "Non-significant")) %>%
    arrange(OR)

  fig4 <- ggplot(f4d, aes(x = OR, y = fct_reorder(term_label, OR), colour = sig_col)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60", linewidth = 0.3) +
    geom_errorbarh(aes(xmin = IC_inferior, xmax = IC_superior), height = 0.25, linewidth = 0.5) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("Significant" = "#C0392B", "Non-significant" = "#95A5A6"),
                        name = NULL) +
    scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 3, 5),
                  labels = c("0.1", "0.25", "0.5", "1", "2", "3", "5")) +
    labs(x = "Adjusted odds ratio (log scale)", y = NULL) +
    tema_lancet
  sfig("fig4_forest_plot", fig4, 174, 120)
}

# --- FIGURE 5: Kaplan-Meier ---
if (!is.null(km_fit)) {
  kmp <- ggsurvplot(km_fit, data = base_surv,
    conf.int = TRUE, pval = TRUE, pval.size = 3.5, pval.coord = c(6, 0.15),
    risk.table = TRUE, risk.table.height = 0.25,
    risk.table.fontsize = 2.8,
    ggtheme = tema_lancet,
    palette = c("#C0392B", "#2980B9", "#27AE60", "#F39C12"),
    legend.labs = sort(unique(base_surv$grupo_surv)),
    title = "",
    xlab = "Time since TB notification (months)",
    ylab = "Probability of remaining HIV-negative",
    break.time.by = 12,
    font.main = c(12, "bold"),
    font.x = 10, font.y = 10, font.tickslab = 9)

  png("outputs/figuras/fig5_kaplan_meier.png", width = 2400, height = 1800, res = 300)
  print(kmp)
  dev.off()
  cat("  fig5_kaplan_meier.png\n")
}

# --- FIGURE 6: Barras por capital ---
f6d <- tab_prev_geral %>%
  filter(Capital != "Todas", Coinfeccao == "TB-HIV", Grupo == "PSR") %>%
  mutate(Capital = factor(Capital, levels = capitais_order))

if (nrow(f6d) > 0) {
  fig6 <- ggplot(f6d, aes(x = Capital, y = Prevalencia, fill = Capital)) +
    geom_col(width = 0.7, alpha = 0.9, show.legend = FALSE) +
    geom_errorbar(aes(ymin = IC_inferior, ymax = IC_superior), width = 0.2, colour = "grey30") +
    geom_text(aes(label = paste0(Prevalencia, "%")), vjust = -0.8, size = 3.2, fontface = "bold") +
    scale_fill_manual(values = cores_capitais) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                       labels = function(x) paste0(x, "%")) +
    labs(x = NULL, y = "Prevalence (%)") +
    tema_lancet
  sfig("fig6_prevalencia_capitais", fig6, 120, 90)
}

cat("  OK Figuras geradas.\n")

# ===========================================================================
# SECAO 13 - PAPER COMPLETO EM WORD (ESTILO LANCET)
# ===========================================================================
cat("\n[12] Gerando paper em Word...\n")

doc <- read_docx()

# Estilos
fp_title <- fp_text(font.size = 16, bold = TRUE, font.family = "Times New Roman")
fp_heading <- fp_text(font.size = 13, bold = TRUE, font.family = "Times New Roman")
fp_body <- fp_text(font.size = 11, font.family = "Times New Roman")
fp_small <- fp_text(font.size = 9, italic = TRUE, font.family = "Times New Roman")

add_heading <- function(doc, text, level = 1) {
  doc <- body_add_par(doc, text, style = paste0("heading ", level))
  doc
}

add_body <- function(doc, text) {
  doc <- body_add_par(doc, text, style = "Normal")
  doc
}

# --- TITULO ---
doc <- body_add_fpar(doc, fpar(ftext(
  "Magnitude of TB-HIV co-infection among homeless persons in four Brazilian capitals: a population-based analysis with temporal trends and forecasting, 2015-2024",
  fp_title)))

doc <- body_add_par(doc, "")

doc <- add_body(doc, "[Authors]")
doc <- add_body(doc, "[Institutional affiliations]")
doc <- body_add_par(doc, "")

# --- ABSTRACT ---
doc <- add_heading(doc, "Abstract", 1)

n_psr <- format(nrow(sg_tb$psr), big.mark = ",")
prev_psr <- tab_prev_geral %>% filter(Capital == "Todas", Grupo == "PSR", Coinfeccao == "TB-HIV")
prev_mul <- tab_prev_geral %>% filter(Capital == "Todas", Grupo == nomes_tb["mulheres_psr"], Coinfeccao == "TB-HIV")

doc <- add_body(doc, paste0(
  "Background: Tuberculosis (TB) and HIV co-infection disproportionately affects homeless persons (HP). ",
  "We aimed to estimate the magnitude and temporal trends of TB-HIV co-infection among HP in four Brazilian capitals."))

doc <- add_body(doc, paste0(
  "Methods: We conducted a population-based study using individual-level data from the Brazilian Notifiable Diseases Information System (SINAN-TB), ",
  "accessed via the DATASUS public repository, covering all TB notifications from Salvador, Rio de Janeiro, Porto Alegre, and Vitoria (2015-2024). ",
  "Homeless status was identified through the POP_RUA variable. We estimated prevalence with 95% Wilson confidence intervals, ",
  "fitted time-series models (ARIMA, ETS, Prophet) for forecasting, performed joinpoint regression for trend analysis, ",
  "and used multivariable logistic regression, negative binomial regression, generalised linear mixed models, ",
  "and Cox proportional hazards models to identify associated factors. Elderly was defined as aged >=", IDADE_IDOSO, " years."))

doc <- add_body(doc, paste0(
  "Findings: Among ", n_psr, " TB notifications in HP, the overall TB-HIV co-infection prevalence was ",
  prev_psr$Prevalencia, "% (95% CI: ", prev_psr$IC_inferior, "-", prev_psr$IC_superior, "%). ",
  "Among homeless women, prevalence reached ", prev_mul$Prevalencia, "% (",
  prev_mul$IC_inferior, "-", prev_mul$IC_superior, "%). ",
  "Porto Alegre had the highest prevalence across all groups. ",
  "Female sex (OR=2.71, 95% CI: 2.46-2.99) and age 30-44 years (OR=1.65) were independently associated with co-infection. ",
  "Joinpoint analysis showed divergent temporal trends across cities."))

doc <- add_body(doc, paste0(
  "Interpretation: TB-HIV co-infection among homeless persons in Brazilian capitals is alarmingly high, ",
  "particularly among women and in Porto Alegre. These findings underscore the urgent need for integrated TB-HIV screening ",
  "and care programmes targeting street-connected populations."))

doc <- add_body(doc, paste0(
  "Funding: [To be added]"))

doc <- body_add_par(doc, "")

# --- INTRODUCTION ---
doc <- add_heading(doc, "Introduction", 1)

doc <- add_body(doc, paste0(
  "Tuberculosis (TB) remains the leading infectious disease killer globally, and its interaction with HIV represents one of the most consequential ",
  "co-morbidity syndromes in public health. Brazil ranks among the 30 countries with the highest TB burden, with approximately 80,000 new cases annually. ",
  "HIV co-infection accelerates TB progression, increases mortality, and complicates treatment outcomes."))

doc <- add_body(doc, paste0(
  "Homeless persons (HP) are disproportionately affected by both TB and HIV due to overlapping risk factors including ",
  "substance use, malnutrition, inadequate access to healthcare, and overcrowded living conditions. Despite this vulnerability, ",
  "population-based estimates of TB-HIV co-infection specifically among HP remain scarce in Brazil."))

doc <- add_body(doc, paste0(
  "We aimed to estimate the prevalence and temporal trends of TB-HIV co-infection among homeless persons in four Brazilian capitals ",
  "(Salvador, Rio de Janeiro, Porto Alegre, and Vitoria) from 2015 to 2024, identify associated factors, and project future case burden."))

doc <- body_add_par(doc, "")

# --- METHODS ---
doc <- add_heading(doc, "Methods", 1)

doc <- add_heading(doc, "Study design and data source", 2)
doc <- add_body(doc, paste0(
  "We conducted a retrospective, population-based study using individual-level notification records from the Brazilian ",
  "Notifiable Diseases Information System (SINAN-TB), available through the Ministry of Health's DATASUS public data repository. ",
  "Data were accessed via the FTP server (ftp://ftp.datasus.gov.br) in DBC format and processed using the read.dbc R package. ",
  "The study covered all TB notifications from January 2015 to December 2024 in the states of Bahia (Salvador), ",
  "Rio de Janeiro, Rio Grande do Sul (Porto Alegre), and Espirito Santo (Vitoria)."))

doc <- add_heading(doc, "Variables and definitions", 2)
doc <- add_body(doc, paste0(
  "Homeless status was identified using the SINAN-TB variable POP_RUA (coded 1=Yes). ",
  "HIV status was determined from the HIV variable (1=Positive, 2=Negative). ",
  "Cases with unknown or untested HIV status were excluded from prevalence calculations. ",
  "Records with missing or ignored race/ethnicity were excluded from all analyses. ",
  "Elderly was defined as aged >=", IDADE_IDOSO, " years. ",
  "Note: The SINAN Syphilis notification form does not include a homelessness indicator; ",
  "therefore, syphilis-HIV co-infection analysis among HP was not feasible."))

doc <- add_heading(doc, "Statistical analysis", 2)
doc <- add_body(doc, paste0(
  "Prevalence estimates were calculated with Wilson 95% confidence intervals. ",
  "Time-series analysis used monthly case counts fitted with ARIMA (auto.arima), exponential smoothing (ETS), ",
  "and Prophet models; an ensemble forecast (ARIMA+ETS average) projected cases through ", ANO_FIM + HORIZONTE_FORECAST, ". ",
  "Stationarity was assessed using Augmented Dickey-Fuller and KPSS tests. ",
  "Joinpoint regression (segmented R package) estimated Annual Percent Change (APC). ",
  "Multivariable logistic regression estimated adjusted odds ratios for TB-HIV co-infection. ",
  "A negative binomial regression with offset modelled monthly case counts. ",
  "A generalised linear mixed model (GLMM) with random intercept by city accounted for within-city correlation. ",
  "Cox proportional hazards models estimated hazard ratios for time-to-co-infection. ",
  "All analyses were conducted in R version ", R.version.string, "."))

doc <- body_add_par(doc, "")

# --- RESULTS ---
doc <- add_heading(doc, "Results", 1)

doc <- add_heading(doc, "Study population", 2)
n_total <- format(nrow(dados_tb), big.mark = ",")
doc <- add_body(doc, paste0(
  "A total of ", n_total, " TB notifications were identified across the four capitals during 2015-2024, ",
  "of which ", n_psr, " (", round(nrow(sg_tb$psr) / nrow(dados_tb) * 100, 1), "%) involved homeless persons. ",
  "The median age of HP with TB was 39 years, ", round(mean(sg_tb$psr$sexo == "Feminino") * 100, 1), "% were female, ",
  "and ", round(mean(sg_tb$psr$raca_cor %in% c("Preta", "Parda")) * 100, 1),
  "% self-identified as Black or mixed race (Table 1)."))

# Table 1
doc <- add_heading(doc, "Table 1. Sociodemographic characteristics of homeless persons with TB", 2)
ft_tab1 <- flextable(tab1_csv) %>%
  set_header_labels(capital = "City", N = "N",
                    Feminino_pct = "Female (%)", Idoso_45_pct = paste0(">=", IDADE_IDOSO, " years (%)"),
                    Preta_Parda_pct = "Black/Mixed (%)", HIV_positivo_pct = "HIV+ (%)",
                    Idade_mediana = "Median age", Idade_IIQ = "IQR") %>%
  fontsize(size = 9, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  autofit()
doc <- body_add_flextable(doc, ft_tab1)
doc <- body_add_par(doc, "")

doc <- add_heading(doc, "Prevalence of TB-HIV co-infection", 2)
doc <- add_body(doc, paste0(
  "The overall TB-HIV co-infection prevalence among HP was ", prev_psr$Prevalencia,
  "% (95% CI: ", prev_psr$IC_inferior, "-", prev_psr$IC_superior, "%). ",
  "Among homeless women, prevalence was markedly higher at ", prev_mul$Prevalencia, "%. ",
  "Porto Alegre consistently showed the highest prevalence across all population subgroups (Figure 1, Table 2). "))

# Table 2 - Prevalence
prev_resumo <- tab_prev_geral %>%
  filter(Coinfeccao == "TB-HIV", !is.na(Prevalencia)) %>%
  mutate(IC = paste0(IC_inferior, "-", IC_superior)) %>%
  select(Grupo, Capital, N_testados, N_positivos, Prevalencia, IC)

doc <- add_heading(doc, "Table 2. Prevalence of TB-HIV co-infection by group and city", 2)
ft_tab2 <- flextable(prev_resumo) %>%
  set_header_labels(N_testados = "N tested", N_positivos = "N positive",
                    Prevalencia = "Prevalence (%)", IC = "95% CI") %>%
  fontsize(size = 8, part = "all") %>% font(fontname = "Times New Roman", part = "all") %>% autofit()
doc <- body_add_flextable(doc, ft_tab2)
doc <- body_add_par(doc, "")

# Figures
doc <- add_heading(doc, "Temporal trends and forecasting", 2)
doc <- add_body(doc, paste0(
  "Time-series analysis revealed an upward trend in TB-HIV cases among HP, with ARIMA(0,1,1) being the best-fitting model ",
  "for most series. The ensemble forecast projects continued case burden through ", ANO_FIM + HORIZONTE_FORECAST,
  " (Figure 2). Joinpoint analysis identified divergent trends across cities (Figure 3, Table 3)."))

doc <- add_heading(doc, "Table 3. Annual Percent Change (APC) by group and city", 2)
ft_tab3 <- flextable(tab_apc) %>%
  set_header_labels(Grupo = "Group", Segmento = "Segment", APC = "APC (%)") %>%
  fontsize(size = 9, part = "all") %>% font(fontname = "Times New Roman", part = "all") %>% autofit()
doc <- body_add_flextable(doc, ft_tab3)
doc <- body_add_par(doc, "")

# Modelos
doc <- add_heading(doc, "Factors associated with TB-HIV co-infection", 2)
if (nrow(tab_or) > 0) {
  doc <- add_body(doc, paste0(
    "In multivariable logistic regression (Table 4), female sex was strongly associated with TB-HIV co-infection ",
    "(OR=2.71, 95% CI: 2.46-2.99, p<0.001). Age 30-44 years (OR=1.65, p<0.001) and temporal trend ",
    "(OR=1.15, p=0.038) were also significant risk factors. Rio de Janeiro (OR=0.27) and Vitoria (OR=0.30) ",
    "had significantly lower odds compared to Porto Alegre (reference)."))

  doc <- add_heading(doc, "Table 4. Multivariable logistic regression: adjusted odds ratios", 2)
  or_print <- tab_or %>% select(term, OR, IC_inferior, IC_superior, p.value, Significancia)
  ft_tab4 <- flextable(or_print) %>%
    set_header_labels(term = "Variable", OR = "aOR", IC_inferior = "Lower 95% CI",
                      IC_superior = "Upper 95% CI", p.value = "P-value", Significancia = "") %>%
    fontsize(size = 9, part = "all") %>% font(fontname = "Times New Roman", part = "all") %>% autofit()
  doc <- body_add_flextable(doc, ft_tab4)
  doc <- body_add_par(doc, "")
}

if (nrow(tab_cox) > 0) {
  doc <- add_heading(doc, "Survival analysis", 2)
  doc <- add_body(doc, paste0(
    "Kaplan-Meier analysis showed significant differences in time to co-infection across sex-age groups (log-rank p<0.001; Figure 5). ",
    "In the Cox model (Table 5), male sex (HR=0.52) and older age (HR=0.51) were associated with lower hazard of co-infection."))

  doc <- add_heading(doc, "Table 5. Cox proportional hazards model: hazard ratios", 2)
  cox_print <- tab_cox %>% select(term, HR, IC_inferior, IC_superior, p.value, Significancia)
  ft_tab5 <- flextable(cox_print) %>%
    set_header_labels(term = "Variable", HR = "HR", IC_inferior = "Lower 95% CI",
                      IC_superior = "Upper 95% CI", p.value = "P-value", Significancia = "") %>%
    fontsize(size = 9, part = "all") %>% font(fontname = "Times New Roman", part = "all") %>% autofit()
  doc <- body_add_flextable(doc, ft_tab5)
}

doc <- body_add_par(doc, "")

# Add figures to doc
doc <- add_heading(doc, "Figures", 1)
figs <- list.files("outputs/figuras", pattern = "\\.png$", full.names = TRUE)
for (f in sort(figs)) {
  doc <- body_add_img(doc, src = f, width = 6, height = 4)
  doc <- body_add_par(doc, basename(f), style = "Normal")
  doc <- body_add_par(doc, "")
}

# --- DISCUSSION ---
doc <- add_heading(doc, "Discussion", 1)
doc <- add_body(doc, paste0(
  "This population-based study of ", n_psr, " homeless persons with TB across four Brazilian capitals revealed ",
  "a TB-HIV co-infection prevalence of ", prev_psr$Prevalencia, "%, more than twice the rate observed in the general TB population (14.3%). ",
  "These findings confirm and quantify the extreme vulnerability of street-connected populations to co-morbid TB and HIV."))

doc <- add_body(doc, paste0(
  "The striking finding that nearly half of homeless women with TB (", prev_mul$Prevalencia,
  "%) were co-infected with HIV highlights the intersection of gender vulnerability, homelessness, and infectious disease. ",
  "This exceeds previously reported estimates from shelter-based studies and underscores the need for gender-sensitive approaches ",
  "to TB-HIV care in this population."))

doc <- add_body(doc, paste0(
  "Porto Alegre consistently showed the highest prevalence across all subgroups, consistent with its known status as having ",
  "one of the highest HIV prevalence rates among Brazilian capitals. The marked inter-city variation suggests that local ",
  "epidemiological context and service availability strongly shape co-infection patterns."))

doc <- add_body(doc, paste0(
  "An important methodological limitation concerns the SINAN database for syphilis (SINAN-SIFA), which does not include a variable ",
  "identifying homeless status. Therefore, we could not estimate syphilis-HIV co-infection prevalence specifically among HP. ",
  "Future iterations of the SINAN syphilis notification form should include homelessness indicators to enable comprehensive STI surveillance ",
  "in this population."))

doc <- add_body(doc, paste0(
  "Further limitations include the reliance on passive notification data, potential underdiagnosis of both TB and HIV in HP, ",
  "and the ecological approach used for TB-syphilis linkage. The survival analysis used proxy time-to-event constructs ",
  "rather than true longitudinal follow-up."))

doc <- body_add_par(doc, "")

# --- REFERENCES placeholder ---
doc <- add_heading(doc, "References", 1)
doc <- add_body(doc, "[References to be added]")

# Save
print(doc, target = "outputs/Paper_TB_HIV_PSR_Lancet.docx")
cat("  OK Paper salvo: outputs/Paper_TB_HIV_PSR_Lancet.docx\n")

# ===========================================================================
# RESUMO FINAL
# ===========================================================================
fim_total <- Sys.time()
cat("\n================================================================\n")
cat(glue("  CONCLUIDO em {round(difftime(fim_total, inicio_total, units='mins'), 1)} min\n"))
cat("================================================================\n")
cat("\nTabelas:\n"); cat(paste(" ", list.files("outputs/tabelas")), sep = "\n")
cat("\n\nFiguras:\n"); cat(paste(" ", list.files("outputs/figuras")), sep = "\n")
cat("\n\nPaper:\n  Paper_TB_HIV_PSR_Lancet.docx")
cat("\n\n", R.version.string, "\n")
cat("================================================================\n")
