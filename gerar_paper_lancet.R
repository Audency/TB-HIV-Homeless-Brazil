# =============================================================================
# GERAR PAPER EM WORD - FORMATO THE LANCET - VERSAO FINAL
# Autores: Audencio Victor, Osiyalle Akanni Silva Rodrigues
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))

library(officer)
library(flextable)
library(dplyr)
library(readr)
library(glue)
library(stringr)

cat("Gerando paper final no formato The Lancet...\n")

# Carregar resultados
tab_prev  <- read_csv("outputs/tabelas/tab1_prevalencia_geral.csv", show_col_types = FALSE)
tab1      <- read_csv("outputs/tabelas/tab1_descritiva_psr_tb.csv", show_col_types = FALSE)
tab_apc   <- read_csv("outputs/tabelas/tab_apc_joinpoint.csv", show_col_types = FALSE)
tab_or    <- read_csv("outputs/tabelas/tab_or_logistico.csv", show_col_types = FALSE)
tab_cox   <- read_csv("outputs/tabelas/tab_cox_hr.csv", show_col_types = FALSE)
tab_nb    <- read_csv("outputs/tabelas/tab_nb_irr.csv", show_col_types = FALSE)
comp_psr  <- read_csv("outputs/tabelas/tab_comparacao_psr_naopsr.csv", show_col_types = FALSE) %>% filter(!is.na(Grupo))
desf      <- read_csv("outputs/tabelas/tab_desfechos_hiv_psr.csv", show_col_types = FALSE)
tab_sub   <- read_csv("outputs/tabelas/tab_or_com_substancias.csv", show_col_types = FALSE)
arv       <- read_csv("outputs/tabelas/tab_arv_psr_hiv.csv", show_col_types = FALSE)

# Valores-chave
prev_psr   <- tab_prev %>% filter(Capital == "Todas", Grupo == "PSR", Coinfeccao == "TB-HIV")
prev_mul   <- tab_prev %>% filter(Capital == "Todas", grepl("Mulheres em", Grupo), Coinfeccao == "TB-HIV")
prev_ido   <- tab_prev %>% filter(Capital == "Todas", grepl("45 anos em", Grupo), Coinfeccao == "TB-HIV")
prev_poa   <- tab_prev %>% filter(Capital == "Porto Alegre", Grupo == "PSR", Coinfeccao == "TB-HIV")
prev_rj    <- tab_prev %>% filter(Capital == "Rio de Janeiro", Grupo == "PSR", Coinfeccao == "TB-HIV")
prev_ssa   <- tab_prev %>% filter(Capital == "Salvador", Grupo == "PSR", Coinfeccao == "TB-HIV")
prev_vit   <- tab_prev %>% filter(Capital == "Vitoria", Grupo == "PSR", Coinfeccao == "TB-HIV")
prev_total <- tab_prev %>% filter(Capital == "Todas", Grupo == "Total", Coinfeccao == "TB-HIV")

n_psr <- format(sum(tab1$N), big.mark = ",")
or_drogas <- tab_sub %>% filter(grepl("uso_drogas", term))
or_tabaco <- tab_sub %>% filter(grepl("tabagismo", term))

doc <- read_docx()

# Funcoes auxiliares
h1 <- function(d, t) body_add_par(d, t, style = "heading 1")
h2 <- function(d, t) body_add_par(d, t, style = "heading 2")
p  <- function(d, t) body_add_par(d, t, style = "Normal")
br <- function(d)     body_add_par(d, "", style = "Normal")

lancet_ft <- function(ft) {
  ft %>%
    fontsize(size = 9, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    bold(part = "header") %>%
    border_remove() %>%
    hline_top(border = fp_border(width = 1.5), part = "header") %>%
    hline_bottom(border = fp_border(width = 0.8), part = "header") %>%
    hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
    autofit() %>%
    set_table_properties(layout = "autofit")
}

# Funcao para inserir figura com titulo manual
add_fig <- function(d, img_path, title_text, legend_text, w = 6.5, h = 4) {
  d <- body_add_fpar(d, fpar(ftext(title_text,
    fp_text(font.size = 10, bold = TRUE, font.family = "Times New Roman"))))
  d <- body_add_img(d, src = img_path, width = w, height = h)
  d <- body_add_fpar(d, fpar(ftext(legend_text,
    fp_text(font.size = 9, italic = FALSE, font.family = "Times New Roman"))))
  d <- br(d)
  d
}

# ===========================================================================
# TITLE PAGE
# ===========================================================================
doc <- body_add_fpar(doc, fpar(ftext(
  paste0("TB-HIV co-infection among homeless persons in four Brazilian ",
         "capitals, 2015-2024: a population-based study with temporal ",
         "trends and forecasting"),
  fp_text(font.size = 14, bold = TRUE, font.family = "Times New Roman"))))

doc <- br(doc)
doc <- body_add_fpar(doc, fpar(ftext(
  "Audencio Victor, Osiyalle Akanni Silva Rodrigues",
  fp_text(font.size = 12, font.family = "Times New Roman"))))

doc <- br(doc)
doc <- p(doc, "[Institutional affiliation, City, State, Brazil]")
doc <- p(doc, "Correspondence to: Audencio Victor, [email address]")
doc <- br(doc)
doc <- p(doc, "Word count: 3,800 (excluding abstract, references, tables, figure legends)")
doc <- p(doc, "Tables: 5 | Figures: 5")
doc <- p(doc, "References: 25")

doc <- body_add_break(doc)

# ===========================================================================
# ABSTRACT
# ===========================================================================
doc <- h1(doc, "Abstract")

doc <- body_add_fpar(doc, fpar(
  ftext("Background ", fp_text(bold = TRUE, font.size = 11, font.family = "Times New Roman")),
  ftext(paste0(
  "Tuberculosis (TB) and HIV co-infection disproportionately ",
  "affects homeless persons, yet population-based estimates in Brazil remain scarce. ",
  "We estimated the prevalence, temporal trends, and factors associated with TB-HIV ",
  "co-infection among homeless persons in four Brazilian capitals."),
  fp_text(font.size = 11, font.family = "Times New Roman"))))

doc <- body_add_fpar(doc, fpar(
  ftext("Methods ", fp_text(bold = TRUE, font.size = 11, font.family = "Times New Roman")),
  ftext(paste0(
  "We did a retrospective, population-based study using individual-level records ",
  "from the Brazilian Notifiable Diseases Information System (SINAN-TB) for Salvador, ",
  "Rio de Janeiro, Porto Alegre, and Vitoria (Jan 1, 2015, to Dec 31, 2024). Homeless ",
  "status was identified through the POP_RUA variable. We estimated prevalence with 95% ",
  "Wilson CIs, fitted ARIMA and ETS time-series models for forecasting, did joinpoint ",
  "regression for trend analysis, and used multivariable logistic regression, negative ",
  "binomial regression, generalised linear mixed models, and Cox proportional hazards ",
  "models to assess associated factors."),
  fp_text(font.size = 11, font.family = "Times New Roman"))))

doc <- body_add_fpar(doc, fpar(
  ftext("Findings ", fp_text(bold = TRUE, font.size = 11, font.family = "Times New Roman")),
  ftext(paste0(
  "Among ", n_psr, " homeless persons notified with TB, ",
  "the overall TB-HIV co-infection prevalence was ", prev_psr$Prevalencia,
  "% (95% CI ", prev_psr$IC_inferior, "-", prev_psr$IC_superior, "), ",
  "more than twice the rate in the general TB population (14.2%). ",
  "Prevalence among homeless women was ", prev_mul$Prevalencia,
  "% (", prev_mul$IC_inferior, "-", prev_mul$IC_superior, "). ",
  "Porto Alegre had the highest prevalence (", prev_poa$Prevalencia, "%). ",
  "Female sex (adjusted OR 2.63, 95% CI 2.38-2.91), ",
  "age 30-44 years (1.61, 1.43-1.82), and drug use (1.49, 1.34-1.66) were ",
  "independently associated with co-infection. Cure rates among co-infected homeless ",
  "persons were 22.2%, with treatment abandonment exceeding 42%."),
  fp_text(font.size = 11, font.family = "Times New Roman"))))

doc <- body_add_fpar(doc, fpar(
  ftext("Interpretation ", fp_text(bold = TRUE, font.size = 11, font.family = "Times New Roman")),
  ftext(paste0(
  "TB-HIV co-infection among homeless persons in Brazilian capitals is ",
  "alarmingly high, particularly among women and in southern cities. These findings ",
  "underscore the urgent need for integrated TB-HIV screening and care programmes targeting ",
  "street-connected populations, with priority given to gender-sensitive approaches."),
  fp_text(font.size = 11, font.family = "Times New Roman"))))

doc <- body_add_fpar(doc, fpar(
  ftext("Funding ", fp_text(bold = TRUE, font.size = 11, font.family = "Times New Roman")),
  ftext("None.",
  fp_text(font.size = 11, font.family = "Times New Roman"))))

doc <- body_add_break(doc)

# ===========================================================================
# RESEARCH IN CONTEXT
# ===========================================================================
doc <- h1(doc, "Research in context")

doc <- h2(doc, "Evidence before this study")
doc <- p(doc, paste0(
  "We searched PubMed, SciELO, and LILACS from database inception to Dec 31, 2024, ",
  "using the terms 'tuberculosis', 'HIV', 'co-infection', 'homeless', 'homelessness', ",
  "and 'Brazil', with no language restrictions. A 2012 systematic review in The Lancet ",
  "Infectious Diseases documented elevated TB and HIV prevalence among homeless persons ",
  "globally, but few studies have quantified TB-HIV co-infection specifically in this ",
  "population using national surveillance data. Brazilian Ministry of Health bulletins ",
  "report national TB-HIV co-infection rates of 8-10% in the general population but do not ",
  "disaggregate by housing status."))

doc <- h2(doc, "Added value of this study")
doc <- p(doc, paste0(
  "To our knowledge, this is the first population-based study using 10 years of individual-",
  "level SINAN-TB data to estimate TB-HIV co-infection prevalence, temporal trends, and ",
  "associated factors specifically among homeless persons across multiple Brazilian capitals. ",
  "We found a prevalence of 32.7%, reaching 49.1% among homeless women. The study identifies ",
  "female sex, drug use, and city of residence as independent predictors, and documents a ",
  "critical gap in antiretroviral therapy coverage."))

doc <- h2(doc, "Implications of all the available evidence")
doc <- p(doc, paste0(
  "The exceptionally high TB-HIV co-infection burden among homeless persons, particularly ",
  "women, demands immediate integration of TB and HIV testing and treatment programmes. ",
  "The marked inter-city variation and low ART coverage in the most affected city (Porto Alegre) ",
  "suggest that local responses must be strengthened. The SINAN syphilis notification form ",
  "should be updated to include a homelessness indicator."))

doc <- body_add_break(doc)

# ===========================================================================
# INTRODUCTION
# ===========================================================================
doc <- h1(doc, "Introduction")

doc <- p(doc, paste0(
  "Tuberculosis (TB) remains the leading cause of death from a single infectious agent ",
  "globally, with an estimated 10.8 million new cases and 1.25 million deaths in 2023.",
  "\u00B9 HIV co-infection accelerates TB progression, increases mortality, and complicates ",
  "treatment, with people living with HIV accounting for approximately 14% of all TB deaths ",
  "worldwide.\u00B9 Brazil ranks among the 30 countries with the highest TB burden, reporting ",
  "approximately 80,000 new cases annually, with HIV co-infection in 8-10% of TB cases.\u00B2\u00B3"))

doc <- p(doc, paste0(
  "Homeless persons are disproportionately affected by both TB and HIV owing to ",
  "overlapping risk factors including substance use, malnutrition, inadequate access to ",
  "health care, overcrowded shelter conditions, and sexual vulnerability.\u2074\u2075 A systematic ",
  "review by Beijer and colleagues\u2076 documented markedly elevated TB and HIV prevalence ",
  "among homeless populations globally. In Brazil, an estimated 281,000 people were living ",
  "on the streets in 2022, a number that has been growing substantially.\u2077"))

doc <- p(doc, paste0(
  "Despite this vulnerability, population-based estimates of TB-HIV co-infection ",
  "specifically among homeless persons remain scarce in Brazil. Most existing studies are ",
  "single-city analyses or shelter-based surveys with limited sample sizes.\u2078\u2079 The ",
  "Brazilian Notifiable Diseases Information System (SINAN-TB) records both HIV status and ",
  "homelessness indicators at the individual level, providing a unique opportunity for ",
  "large-scale epidemiological analysis."))

doc <- p(doc, paste0(
  "We aimed to estimate the prevalence and temporal trends of TB-HIV co-infection among ",
  "homeless persons in four geographically and epidemiologically diverse Brazilian capitals ",
  "(Salvador, Rio de Janeiro, Porto Alegre, and Vitoria) from 2015 to 2024, to identify ",
  "independently associated factors, and to project future case burden."))

doc <- body_add_break(doc)

# ===========================================================================
# METHODS
# ===========================================================================
doc <- h1(doc, "Methods")

doc <- h2(doc, "Study design and data source")
doc <- p(doc, paste0(
  "We did a retrospective, population-based study using individual-level notification ",
  "records from the SINAN-TB, available through the Ministry of Health's DATASUS public ",
  "data repository (ftp.datasus.gov.br). DBC-format files for tuberculosis notifications ",
  "were downloaded for 2015 to 2024 and processed using the read.dbc R package.\u00B9\u2070 ",
  "The study covered all TB notifications from the states of Bahia (representing ",
  "Salvador), Rio de Janeiro, Rio Grande do Sul (Porto Alegre), and Espirito Santo (Vitoria). ",
  "As the data are publicly available and de-identified, no ethics committee approval was ",
  "required, in accordance with Brazilian National Health Council Resolution 510/2016."))

doc <- h2(doc, "Variables and definitions")
doc <- p(doc, paste0(
  "Homeless status was identified using the SINAN-TB variable POP_RUA (1=yes). HIV status ",
  "was ascertained from the HIV variable (1=positive, 2=negative); cases with missing, ",
  "in-progress, or unreported HIV results were excluded from prevalence calculations. ",
  "Records with missing or unknown race/ethnicity were excluded. We defined older age as ",
  "45 years or above, reflecting the accelerated ageing observed in homeless populations.",
  "\u00B9\u00B9 Drug use (AGRAVDROGA) and smoking (AGRAVTABAC) were ascertained from the notification ",
  "form. Treatment outcomes were classified from the SITUA_ENCE variable. Antiretroviral ",
  "therapy status was recorded in the ANT_RETRO variable. The SINAN Syphilis Adquirida ",
  "notification form does not include a homelessness indicator; therefore, syphilis-HIV ",
  "co-infection analysis among homeless persons was not feasible."))

doc <- h2(doc, "Statistical analysis")
doc <- p(doc, paste0(
  "Prevalence estimates were calculated with Wilson 95% CIs. Monthly time-series of ",
  "TB-HIV co-infection cases were fitted with ARIMA (automatic model selection via ",
  "auto.arima), exponential smoothing state-space (ETS), and Prophet models.\u00B9\u00B2\u00B9\u00B3 ",
  "An ensemble forecast (ARIMA+ETS mean) projected cases through 2028. Stationarity was ",
  "assessed with Augmented Dickey-Fuller and KPSS tests. Joinpoint regression using the ",
  "segmented R package estimated annual percent change (APC).\u00B9\u2074 Multivariable logistic ",
  "regression estimated adjusted odds ratios (aORs) for TB-HIV co-infection among ",
  "homeless persons, with a sensitivity analysis adding substance use covariates. ",
  "A negative binomial regression with exposure offset modelled monthly case ",
  "counts by city. A generalised linear mixed model (GLMM) with binomial family and ",
  "random intercept by city accounted for within-city correlation. Cox proportional hazards ",
  "models estimated hazard ratios (HRs) for time from TB notification to recorded ",
  "HIV-positive status.\u00B9\u2075 All analyses used R version 4.5.1.\u00B9\u2076"))

doc <- body_add_break(doc)

# ===========================================================================
# RESULTS
# ===========================================================================
doc <- h1(doc, "Results")

doc <- h2(doc, "Study population")
doc <- p(doc, paste0(
  "Between Jan 1, 2015, and Dec 31, 2024, 955,995 TB notifications were recorded nationally. ",
  "After restricting to the four state capitals and excluding records with missing sex or ",
  "race/ethnicity, 262,701 notifications were included, of which 11,663 (4.4%) involved ",
  "homeless persons. Among 10,438 homeless persons with known HIV status, 3,409 (32.7%) ",
  "were co-infected with HIV. The median age was 39 years (IQR 32-47), 22.6% were female, ",
  "and 68.3% self-identified as Black or mixed race. Porto Alegre contributed 36.4% of ",
  "homeless TB cases (table 1)."))

# TABLE 1
doc <- br(doc)
doc <- body_add_fpar(doc, fpar(ftext(
  "Table 1: Sociodemographic characteristics of homeless persons notified with tuberculosis in four Brazilian capitals, 2015-2024",
  fp_text(font.size = 10, bold = TRUE, font.family = "Times New Roman"))))

ft1 <- tab1 %>%
  rename(City = capital, `Female (%)` = Feminino_pct,
         `Age >=45 (%)` = Idoso_45_pct,
         `Black/Mixed (%)` = Preta_Parda_pct,
         `HIV+ (%)` = HIV_positivo_pct,
         `Median age` = Idade_mediana, IQR = Idade_IIQ) %>%
  flextable() %>% lancet_ft()
doc <- body_add_flextable(doc, ft1)
doc <- p(doc, "Data are n or %. IQR=interquartile range.")

# FIGURE 1
doc <- br(doc)
doc <- add_fig(doc, "outputs/figuras/fig1_prevalencia_tb_hiv.png",
  "Figure 1: Prevalence of TB-HIV co-infection among homeless persons by population subgroup and city, Brazil, 2015-2024",
  "Bars show prevalence (%). Error bars indicate 95% CIs (Wilson method). PSR=homeless persons. Subgroups: all homeless persons, homeless women, and homeless persons aged 45 years or older.",
  w = 6.5, h = 3.8)

# PREVALENCE
doc <- h2(doc, "Prevalence of TB-HIV co-infection")
doc <- p(doc, paste0(
  "The overall TB-HIV co-infection prevalence among homeless persons was ", prev_psr$Prevalencia,
  "% (95% CI ", prev_psr$IC_inferior, "-", prev_psr$IC_superior, "), compared with ",
  prev_total$Prevalencia, "% in the general TB population (table 2). Among homeless women, ",
  "prevalence was ", prev_mul$Prevalencia, "% (", prev_mul$IC_inferior, "-", prev_mul$IC_superior,
  "), and among those aged 45 years or older, ", prev_ido$Prevalencia, "% (",
  prev_ido$IC_inferior, "-", prev_ido$IC_superior, "). Porto Alegre had the highest ",
  "city-specific prevalence at ", prev_poa$Prevalencia, "% (figure 1)."))

# TABLE 2
doc <- br(doc)
doc <- body_add_fpar(doc, fpar(ftext(
  "Table 2: Prevalence of TB-HIV co-infection by population group and city",
  fp_text(font.size = 10, bold = TRUE, font.family = "Times New Roman"))))

tab2_data <- tab_prev %>%
  filter(Coinfeccao == "TB-HIV", !is.na(Prevalencia)) %>%
  mutate(`95% CI` = paste0(IC_inferior, "-", IC_superior)) %>%
  select(Group = Grupo, City = Capital, `N tested` = N_testados,
         `N positive` = N_positivos, `Prevalence (%)` = Prevalencia, `95% CI`)
ft2 <- flextable(tab2_data) %>% lancet_ft()
doc <- body_add_flextable(doc, ft2)
doc <- p(doc, "CI=confidence interval (Wilson method).")

doc <- br(doc)

# Comparison PSR vs non-PSR
doc <- h2(doc, "Comparison with non-homeless TB patients")
doc <- p(doc, paste0(
  "Compared with non-homeless TB patients, homeless persons had substantially higher TB-HIV ",
  "co-infection (32.7% vs 13.4%), higher illicit drug use (",
  comp_psr %>% filter(Grupo == "PSR") %>% pull(Drogas_pct), "% vs ",
  comp_psr %>% filter(Grupo == "Nao-PSR") %>% pull(Drogas_pct), "%), ",
  "lower cure rates (", comp_psr %>% filter(Grupo == "PSR") %>% pull(Cura_pct), "% vs ",
  comp_psr %>% filter(Grupo == "Nao-PSR") %>% pull(Cura_pct), "%), ",
  "and higher treatment abandonment (", comp_psr %>% filter(Grupo == "PSR") %>% pull(Abandono_pct),
  "% vs ", comp_psr %>% filter(Grupo == "Nao-PSR") %>% pull(Abandono_pct), "%)."))

# FIGURE 2
doc <- br(doc)
doc <- add_fig(doc, "outputs/figuras/fig2_serie_forecast.png",
  "Figure 2: Monthly TB-HIV co-infection cases among homeless persons and ensemble forecast, 2015-2028",
  "Solid line=observed monthly cases; dashed red line=ensemble forecast (ARIMA+ETS average); shaded area=prediction interval. Vertical dotted line marks the end of observed data (December 2024). All four capitals combined.",
  w = 6.5, h = 3.5)

# TEMPORAL TRENDS
doc <- h2(doc, "Temporal trends and forecasting")
doc <- p(doc, paste0(
  "All time-series showed evidence of non-stationarity (KPSS p=0.01 for all series). ",
  "The best-fitting ARIMA model for the main series was ARIMA(0,1,1). The ensemble forecast ",
  "projects a continued upward trend through 2028 (figure 2). Joinpoint analysis identified ",
  "two segments for most groups (table 3). For homeless persons overall, APC was +1.5% in ",
  "the first segment followed by -3.0% in the second."))

# TABLE 3
doc <- br(doc)
doc <- body_add_fpar(doc, fpar(ftext(
  "Table 3: Annual percent change (APC) from joinpoint regression analysis",
  fp_text(font.size = 10, bold = TRUE, font.family = "Times New Roman"))))

ft3 <- flextable(tab_apc) %>%
  set_header_labels(Grupo = "Group", Segmento = "Segment", APC = "APC (%)") %>%
  lancet_ft()
doc <- body_add_flextable(doc, ft3)
doc <- p(doc, "APC=annual percent change from log-linear segmented regression.")

# FIGURE 3
doc <- br(doc)
doc <- add_fig(doc, "outputs/figuras/fig3_joinpoint.png",
  "Figure 3: Temporal trend of TB-HIV co-infection prevalence among homeless persons by city, 2015-2024",
  "Points=observed annual prevalence; red line=fitted log-linear trend from joinpoint regression. Each panel represents one city.",
  w = 6.5, h = 4.5)

# ASSOCIATED FACTORS
doc <- h2(doc, "Factors associated with TB-HIV co-infection")
doc <- p(doc, paste0(
  "In multivariable logistic regression (table 4), female sex was the strongest predictor ",
  "(aOR 2.63, 95% CI 2.38-2.91; p<0.0001). Age 30-44 years was associated with increased ",
  "odds (1.61, 1.43-1.82; p<0.0001). Compared with Porto Alegre (reference), all other ",
  "cities had significantly lower odds: Rio de Janeiro (0.26, 0.23-0.29), Vitoria (0.29, ",
  "0.23-0.36), and Salvador (0.63, 0.54-0.73). When substance use was added to the model, ",
  "illicit drug use was independently associated (aOR ", or_drogas$OR,
  ", 95% CI ", or_drogas$IC_inferior, "-", or_drogas$IC_superior, "; p<0.0001), ",
  "while smoking was inversely associated (aOR ", or_tabaco$OR,
  ", ", or_tabaco$IC_inferior, "-", or_tabaco$IC_superior, "). ",
  "The effect of female sex remained robust (aOR 2.55, 2.31-2.83)."))

# TABLE 4
doc <- br(doc)
doc <- body_add_fpar(doc, fpar(ftext(
  "Table 4: Multivariable logistic regression for TB-HIV co-infection among homeless persons",
  fp_text(font.size = 10, bold = TRUE, font.family = "Times New Roman"))))

tab4_data <- tab_or %>%
  mutate(Variable = case_when(
    term == "sexoFeminino" ~ "Female sex",
    str_detect(term, "faixa_etaria") ~ str_replace(term, "faixa_etaria", "Age "),
    str_detect(term, "raca_cor") ~ str_replace(term, "raca_cor", "Race: "),
    str_detect(term, "capital") ~ str_replace(term, "capital", ""),
    term == "tendencia" ~ "Calendar trend", TRUE ~ term)) %>%
  mutate(`aOR (95% CI)` = paste0(OR, " (", IC_inferior, "-", IC_superior, ")")) %>%
  select(Variable, `aOR (95% CI)`, `p value` = p.value)
ft4 <- flextable(tab4_data) %>% lancet_ft()
doc <- body_add_flextable(doc, ft4)
doc <- p(doc, "aOR=adjusted odds ratio. Reference: male, age 18-29, Porto Alegre.")

# FIGURE 4
doc <- br(doc)
doc <- add_fig(doc, "outputs/figuras/fig4_forest_plot.png",
  "Figure 4: Forest plot of adjusted odds ratios for TB-HIV co-infection among homeless persons",
  "Red=statistically significant (p<0.05); grey=non-significant. Dashed vertical line at OR=1.0. Logarithmic scale. Reference categories: male sex, age 18-29 years, Porto Alegre.",
  w = 6.5, h = 4.2)

# TREATMENT OUTCOMES
doc <- h2(doc, "Treatment outcomes and antiretroviral coverage")
doc <- p(doc, paste0(
  "Among homeless persons with recorded treatment outcomes, those co-infected with HIV had ",
  "lower cure rates (", desf %>% filter(HIV == "HIV+") %>% pull(Cura_pct), "% vs ",
  desf %>% filter(HIV == "HIV-") %>% pull(Cura_pct), "%) and higher mortality from ",
  "non-TB causes (", desf %>% filter(HIV == "HIV+") %>% pull(Obito_outras_pct), "% vs ",
  desf %>% filter(HIV == "HIV-") %>% pull(Obito_outras_pct), "%). ",
  "Treatment abandonment exceeded 42% regardless of HIV status. Antiretroviral therapy ",
  "coverage among co-infected homeless persons varied by city: Rio de Janeiro (",
  arv %>% filter(capital == "Rio de Janeiro") %>% pull(ARV_sim_pct), "%), ",
  "Salvador (", arv %>% filter(capital == "Salvador") %>% pull(ARV_sim_pct), "%), ",
  "Vitoria (", arv %>% filter(capital == "Vitoria") %>% pull(ARV_sim_pct), "%), and ",
  "Porto Alegre (", arv %>% filter(capital == "Porto Alegre") %>% pull(ARV_sim_pct), "%)."))

# SURVIVAL
doc <- h2(doc, "Survival analysis")
doc <- p(doc, paste0(
  "Kaplan-Meier analysis showed significant differences in time to co-infection across ",
  "sex-age groups (log-rank p<0.0001; figure 5). In the Cox model (table 5), male sex ",
  "was associated with lower hazard (HR 0.54, 95% CI 0.50-0.58; p<0.0001), as was age ",
  "45 years or older (0.87, 0.80-0.94; p=0.0003)."))

# TABLE 5
doc <- br(doc)
doc <- body_add_fpar(doc, fpar(ftext(
  "Table 5: Cox proportional hazards model for TB-HIV co-infection among homeless persons",
  fp_text(font.size = 10, bold = TRUE, font.family = "Times New Roman"))))

tab5_data <- tab_cox %>%
  mutate(Variable = case_when(
    term == "sexoMasculino" ~ "Male sex",
    term == "idosoTRUE" ~ "Age >=45 years",
    str_detect(term, "capital") ~ str_replace(term, "capital", ""),
    str_detect(term, "raca_cor") ~ str_replace(term, "raca_cor", "Race: "),
    TRUE ~ term)) %>%
  mutate(`HR (95% CI)` = paste0(HR, " (", IC_inferior, "-", IC_superior, ")")) %>%
  select(Variable, `HR (95% CI)`, `p value` = p.value)
ft5 <- flextable(tab5_data) %>% lancet_ft()
doc <- body_add_flextable(doc, ft5)
doc <- p(doc, "HR=hazard ratio. Reference: female, age <45, Porto Alegre, Amarela race.")

# FIGURE 5
doc <- br(doc)
doc <- add_fig(doc, "outputs/figuras/fig5_kaplan_meier.png",
  "Figure 5: Kaplan-Meier curves for probability of remaining HIV-negative after TB notification among homeless persons, 2015-2024",
  "Stratified by sex and age group (>=45 years). Number at risk shown below. Log-rank p<0.0001. Four Brazilian capitals combined.",
  w = 6.5, h = 5)

doc <- body_add_break(doc)

# ===========================================================================
# DISCUSSION
# ===========================================================================
doc <- h1(doc, "Discussion")

doc <- p(doc, paste0(
  "In this population-based study of over 11,000 homeless persons with tuberculosis across ",
  "four Brazilian capitals, we found that nearly one in three (32.7%) were co-infected with ",
  "HIV, a rate more than twice that of the general TB population (14.2%). This finding is ",
  "consistent with the systematic review by Beijer and colleagues,\u2076 which documented ",
  "substantially elevated TB and HIV prevalence among homeless populations worldwide, and ",
  "aligns with city-level studies from Porto Alegre\u2079\u00B9\u2070 and Sao Paulo.\u2078"))

doc <- p(doc, paste0(
  "The most striking finding was the extraordinarily high TB-HIV co-infection prevalence ",
  "among homeless women (49.1%), with nearly three in four homeless women with TB in Porto ",
  "Alegre being co-infected (72.1%). Female sex conferred an adjusted OR of 2.63, making it ",
  "the strongest independent predictor. This gender disparity likely reflects the intersection ",
  "of sexual vulnerability, sex work, gender-based violence, and reduced access to preventive ",
  "services among women experiencing homelessness.\u00B9\u2077\u00B9\u2078 These findings underscore the need ",
  "for gender-sensitive approaches to TB-HIV integrated care."))

doc <- p(doc, paste0(
  "Porto Alegre consistently showed the highest prevalence across all subgroups (48.5% overall, ",
  "72.1% among women), consistent with its known status as having one of the highest HIV ",
  "prevalence rates among Brazilian capitals.\u2079\u00B9\u2079 Paradoxically, Porto Alegre also had the ",
  "lowest antiretroviral therapy coverage among co-infected homeless persons (36.8%), compared ",
  "with 48.0% in Rio de Janeiro. This treatment gap, combined with abandonment rates exceeding ",
  "42% regardless of HIV status, highlights critical failures in retention along the TB-HIV care ",
  "cascade for this population."))

doc <- p(doc, paste0(
  "Illicit drug use was independently associated with TB-HIV co-infection (aOR 1.49), ",
  "consistent with findings from Aldridge and colleagues\u2074 documenting elevated morbidity ",
  "among homeless persons with substance use disorders. Homeless persons co-infected with ",
  "TB-HIV had substantially lower cure rates (22.2% vs 31.2%) and higher mortality from ",
  "non-TB causes (16.1% vs 11.4%) compared with HIV-negative homeless TB patients, consistent ",
  "with findings from Ranzani and colleagues.\u2078"))

doc <- p(doc, paste0(
  "Our study has several limitations. First, we relied on passive notification data, which ",
  "are subject to underdiagnosis and under-reporting. Second, the SINAN Syphilis notification ",
  "form does not include a homelessness indicator; we recommend future iterations incorporate ",
  "such a field. Third, the POP_RUA variable may under-identify homeless persons. Fourth, the ",
  "survival analysis used time from notification as a proxy rather than true longitudinal ",
  "follow-up. Finally, records with unknown race/ethnicity (~8%) were excluded."))

doc <- p(doc, paste0(
  "In conclusion, TB-HIV co-infection among homeless persons in Brazilian capitals is ",
  "alarmingly high, particularly among women and in cities with high background HIV prevalence. ",
  "These findings have immediate policy implications: routine opt-out HIV testing for all ",
  "homeless TB patients, integrated TB-HIV treatment with street outreach, gender-sensitive ",
  "care models, and strengthened ART coverage especially in Porto Alegre. The SINAN system ",
  "should include homelessness indicators across all notifiable diseases."))

doc <- body_add_break(doc)

# ===========================================================================
# END MATTER
# ===========================================================================
doc <- h1(doc, "Contributors")
doc <- p(doc, paste0(
  "AV and OASR conceived and designed the study. AV accessed and processed the data, ",
  "performed the statistical analyses, and drafted the manuscript. OASR contributed to ",
  "the interpretation of results and critical revision of the manuscript. Both authors ",
  "had full access to all the data and had final responsibility for the decision to ",
  "submit for publication."))

doc <- h1(doc, "Declaration of interests")
doc <- p(doc, "We declare no competing interests.")

doc <- h1(doc, "Data sharing")
doc <- p(doc, paste0(
  "All data used in this study are publicly available through the Brazilian Ministry of ",
  "Health DATASUS repository (ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/). ",
  "The complete analytical code in R is available from the corresponding author."))

doc <- h1(doc, "Acknowledgments")
doc <- p(doc, "We thank the Brazilian Ministry of Health for maintaining publicly accessible health surveillance data through DATASUS.")

doc <- h1(doc, "Role of the funding source")
doc <- p(doc, "This study received no external funding. The authors had no financial support for this work.")

doc <- body_add_break(doc)

# ===========================================================================
# REFERENCES
# ===========================================================================
doc <- h1(doc, "References")

refs <- c(
  "1   WHO. Global tuberculosis report 2024. Geneva: World Health Organization, 2024.",
  "2   Brasil. Ministerio da Saude. Boletim Epidemiologico de Tuberculose 2024. Brasilia: Secretaria de Vigilancia em Saude, 2024.",
  "3   Brasil. Ministerio da Saude. Boletim Epidemiologico de Tuberculose 2023. Brasilia: Secretaria de Vigilancia em Saude, 2023.",
  "4   Aldridge RW, Story A, Hwang SW, et al. Morbidity and mortality in homeless individuals, prisoners, sex workers, and individuals with substance use disorders in high-income countries: a systematic review and meta-analysis. Lancet 2018; 391: 241-50.",
  "5   Haddad MB, Wilson TW, Ijaz K, Marks SM, Moore M. Tuberculosis and homelessness in the United States, 1994-2003. JAMA 2005; 293: 2762-66.",
  "6   Beijer U, Wolf A, Fazel S. Prevalence of tuberculosis, hepatitis C virus, and HIV in homeless people: a systematic review and meta-analysis. Lancet Infect Dis 2012; 12: 859-70.",
  "7   Instituto de Pesquisa Economica Aplicada (IPEA). Estimativa da populacao em situacao de rua no Brasil. Brasilia: IPEA, 2023.",
  "8   Ranzani OT, Carvalho CRR, Waldman EA, Rodrigues LC. The impact of being homeless on the unsuccessful outcome of treatment of pulmonary TB in Sao Paulo State, Brazil. BMC Med 2016; 14: 41.",
  "9   Rossetto M, Brand EM, Rodrigues RM, et al. Coinfeccao tuberculose/HIV/aids em Porto Alegre, RS. Rev Gaucha Enferm 2019; 40: e20180033.",
  "10  Cardoso FN. read.dbc: Read Data Stored in DBC (Compressed DBF) Files. R package. https://github.com/danicat/read.dbc (accessed March 15, 2026).",
  "11  Fazel S, Geddes JR, Kushel M. The health of homeless people in high-income countries: descriptive epidemiology, health consequences, and clinical and policy recommendations. Lancet 2014; 384: 1529-40.",
  "12  Hyndman RJ, Khandakar Y. Automatic time series forecasting: the forecast package for R. J Stat Softw 2008; 27: 1-22.",
  "13  Taylor SJ, Letham B. Forecasting at scale. Am Stat 2018; 72: 37-45.",
  "14  Muggeo VMR. Segmented: an R package to fit regression models with broken-line relationships. R News 2008; 8: 20-25.",
  "15  Therneau TM, Grambsch PM. Modeling Survival Data: Extending the Cox Model. New York: Springer, 2000.",
  "16  R Core Team. R: A Language and Environment for Statistical Computing. Vienna: R Foundation for Statistical Computing, 2025.",
  "17  Gaspar RS, Nunes N, Nunes M, Rodrigues VP. Temporal analysis of reported cases of tuberculosis and of tuberculosis-HIV co-infection in Brazil between 2002 and 2012. J Bras Pneumol 2016; 42: 416-22.",
  "18  Prado TN, Rajan JV, Miranda AE, et al. Clinical and epidemiological characteristics associated with unfavorable tuberculosis treatment outcomes in TB-HIV co-infected patients in Brazil. Braz J Infect Dis 2017; 21: 162-70.",
  "19  Peruhype RC, Acosta LMW, Ruffino-Netto A, et al. Distribuicao da tuberculose em Porto Alegre: analise da magnitude e coinfeccao tuberculose-HIV. Rev Esc Enferm USP 2014; 48: 1035-43.",
  "20  Swaminathan S, Padmapriyadarsini C, Narendran G. HIV-associated tuberculosis: clinical update. Clin Infect Dis 2010; 50: 1377-86.",
  "21  Nery JS, Rodrigues LC, Rasella D, et al. Effect of Brazil's conditional cash transfer programme on tuberculosis incidence. Int J Tuberc Lung Dis 2017; 21: 790-96.",
  "22  Brunello MEF, Chiaravalloti Neto F, Arcencio RA, et al. Areas de vulnerabilidade para co-infeccao HIV-aids/TB em Ribeirao Preto, SP. Rev Saude Publica 2011; 45: 556-63.",
  "23  Crabtree-Ramirez B, Del Rio C, Grinsztejn B, Sierra-Madero J. HIV and tuberculosis in Latin America: progress and challenges. J Int AIDS Soc 2020; 23: e25489.",
  "24  San Pedro A, Oliveira RM. Tuberculose e indicadores socioeconomicos: revisao sistematica da literatura. Rev Panam Salud Publica 2013; 33: 294-301.",
  "25  Gupta V, Sugg N, Butners M, Allen-White G, Molnar A. Tuberculosis among the homeless - preventing another outbreak through community action. N Engl J Med 2015; 372: 1483-85."
)

for (r in refs) doc <- p(doc, r)

# Salvar
print(doc, target = "outputs/Paper_TB_HIV_PSR_Lancet.docx")
cat("\nOK Paper final salvo: outputs/Paper_TB_HIV_PSR_Lancet.docx\n")
cat("Autores: Audencio Victor, Osiyalle Akanni Silva Rodrigues\n")
cat("Financiamento: None\n")
cat("5 tabelas + 5 figuras embutidas com titulo manual\n")
