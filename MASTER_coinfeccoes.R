# =============================================================================
#  SCRIPT MESTRE - COINFECCOES TB-HIV / TB-SIFILIS / SIFILIS-HIV
#  Pessoas em Situacao de Rua | Mulheres | Idosos
#  Quatro Capitais: Salvador, Rio de Janeiro, Porto Alegre, Vitoria
#  Periodo: 2015-2025 + Forecast 2026-2029
# =============================================================================
#  Como usar:
#    1. Abra este arquivo no RStudio
#    2. Execute com: source("MASTER_coinfeccoes.R")
#    3. Os resultados serao salvos na pasta outputs/
#
#  Estrutura do projeto:
#    MASTER_coinfeccoes.R         <- este arquivo (executa tudo)
#    parte1_setup_dados.R         <- instalacao, download, limpeza
#    parte2_descritiva.R          <- subgrupos, prevalencia, Tabela 1
#    parte3_series_temporais.R    <- ARIMA, ETS, TBATS, Prophet, Forecast
#    parte4_modelos.R             <- Joinpoint, NB, Logistico, GLMM, Cox
#    parte5_visualizacoes.R       <- todas as 7 figuras + painel
#
#  Outputs gerados:
#    outputs/tabelas/             <- 10+ tabelas CSV
#    outputs/figuras/             <- 8 figuras PNG (300 dpi)
#    outputs/modelos/             <- objetos R dos modelos (.rds)
# =============================================================================

cat("================================================================\n")
cat("  ANALISE DE COINFECCOES EM PSR - QUATRO CAPITAIS BRASILEIRAS\n")
cat("  Salvador | Rio de Janeiro | Porto Alegre | Vitoria\n")
cat("  TB-HIV | TB-Sifilis | Sifilis-HIV | 2015-2025\n")
cat("================================================================\n\n")

inicio <- Sys.time()

# ── Executar partes sequencialmente ──────────────────────────────────────────
cat("[1/5] Setup, instalacao e download dos dados...\n")
source("parte1_setup_dados.R")

cat("\n[2/5] Subgrupos e analise descritiva...\n")
source("parte2_descritiva.R")

cat("\n[3/5] Series temporais e forecast...\n")
source("parte3_series_temporais.R")

cat("\n[4/5] Modelos estatisticos (joinpoint, NB, logistico, GLMM, Cox)...\n")
source("parte4_modelos.R")

cat("\n[5/5] Gerando todas as figuras do artigo...\n")
source("parte5_visualizacoes.R")

fim   <- Sys.time()
tempo <- round(difftime(fim, inicio, units = "mins"), 1)

# ── Resumo final ─────────────────────────────────────────────────────────────
cat("\n")
cat("================================================================\n")
cat("  ANALISE CONCLUIDA COM SUCESSO\n")
cat(sprintf("  Tempo total: %.1f minutos\n", as.numeric(tempo)))
cat("================================================================\n")
cat("  TABELAS GERADAS (outputs/tabelas/):\n")
cat("    tab1_prevalencia_geral.csv\n")
cat("    tab1_descritiva_psr_tb.csv\n")
cat("    tab_testes_estacionaridade.csv\n")
cat("    tab_arima_diagnostico.csv\n")
cat("    tab_ensemble_forecast.csv\n")
cat("    tab_acuracia_modelos.csv\n")
cat("    tab_apc_joinpoint.csv\n")
cat("    tab_nb_irr.csv\n")
cat("    tab_or_logistico.csv\n")
cat("    tab_efeitos_mistos.csv\n")
cat("    tab_cox_hr.csv\n")
cat("  FIGURAS GERADAS (outputs/figuras/):\n")
cat("    fig1_heatmap_prevalencia.png\n")
cat("    fig2_serie_temporal_forecast.png\n")
cat("    fig3_comparacao_modelos.png\n")
cat("    fig4_joinpoint_tendencia.png\n")
cat("    fig5_forest_plot_or.png\n")
cat("    fig6_kaplan_meier.png\n")
cat("    fig7_tres_coinfeccoes_capital.png\n")
cat("    fig_PAINEL_GERAL_ARTIGO.png\n")
cat("    fig_decomposicao_stl.png\n")
cat("  MODELOS SALVOS (outputs/modelos/):\n")
cat("    todos_modelos.rds\n")
cat("================================================================\n")

# ── Informacoes de referencia metodologica ───────────────────────────────────
cat("\nMETODOLOGIA SUMARIA PARA O ARTIGO:\n")
cat("----------------------------------------------------------------\n")
cat("Fonte de dados:  SINAN-TB e SINAN-Sifilis / DATASUS (microdatasus)\n")
cat("Periodo:         2015-2025 (historico) | 2026-2029 (projecao)\n")
cat("Capitais:        Salvador, Rio de Janeiro, Porto Alegre, Vitoria\n")
cat("Grupos:          PSR, Mulheres, Idosos >=60, intersecoes\n")
cat("\nModelos utilizados:\n")
cat("  [1] Prevalencia + IC 95% Wilson (prop.test)\n")
cat("  [2] Series temporais: ARIMA (auto.arima), ETS, TBATS\n")
cat("  [3] Prophet (Meta/Facebook) com sazonalidade anual\n")
cat("  [4] Ensemble ARIMA+ETS+TBATS (media simples)\n")
cat("  [5] Regressao de Joinpoint (APC / AAPC) via segmented\n")
cat("  [6] Regressao Binomial Negativa (IRR)\n")
cat("  [7] Regressao Logistica Multipla (OR bruto e ajustado)\n")
cat("  [8] GLMM Binomial (efeitos aleatorios por capital)\n")
cat("  [9] Kaplan-Meier + Log-rank + Cox (HR)\n")
cat("  [10] Decomposicao STL + Testes ADF e KPSS\n")
cat("----------------------------------------------------------------\n")
cat("Software: R (versao >=4.3) | Pacotes: microdatasus,\n")
cat("          forecast, prophet, segmented, lme4, survival\n")
cat("----------------------------------------------------------------\n")

cat("\nINFORMACOES DA SESSAO R:\n")
print(sessionInfo())
