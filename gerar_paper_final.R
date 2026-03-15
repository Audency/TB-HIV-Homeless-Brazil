# =============================================================================
# GENERATE FINAL MANUSCRIPT - WORD FORMAT
# Authors: Audencio Victor (LSHTM), Osiyalle Akanni Silva Rodrigues (UnB)
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))

library(officer)
library(flextable)
library(dplyr)
library(readr)
library(glue)
library(stringr)

cat("Generating final manuscript...\n")

# Load results
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

# Key values
psr   <- tab_prev %>% filter(Capital == "Todas", Grupo == "PSR", Coinfeccao == "TB-HIV")
mul   <- tab_prev %>% filter(Capital == "Todas", grepl("Mulheres em", Grupo), Coinfeccao == "TB-HIV")
ido   <- tab_prev %>% filter(Capital == "Todas", grepl("45 anos em", Grupo), Coinfeccao == "TB-HIV")
poa   <- tab_prev %>% filter(Capital == "Porto Alegre", Grupo == "PSR", Coinfeccao == "TB-HIV")
rj    <- tab_prev %>% filter(Capital == "Rio de Janeiro", Grupo == "PSR", Coinfeccao == "TB-HIV")
ssa   <- tab_prev %>% filter(Capital == "Salvador", Grupo == "PSR", Coinfeccao == "TB-HIV")
vit   <- tab_prev %>% filter(Capital == "Vitoria", Grupo == "PSR", Coinfeccao == "TB-HIV")
tot   <- tab_prev %>% filter(Capital == "Todas", Grupo == "Total", Coinfeccao == "TB-HIV")
hom   <- tab_prev %>% filter(Capital == "Todas", grepl("Homens em", Grupo), Coinfeccao == "TB-HIV")

n_psr <- format(sum(tab1$N), big.mark = ",")
or_dr <- tab_sub %>% filter(grepl("uso_drogas", term))
or_tb <- tab_sub %>% filter(grepl("tabagismo", term))

doc <- read_docx()

# Helper functions
h1 <- function(d, t) body_add_par(d, t, style = "heading 1")
h2 <- function(d, t) body_add_par(d, t, style = "heading 2")
tx <- function(d, t) body_add_par(d, t, style = "Normal")
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

add_fig <- function(d, img, title, legend, w = 6.5, h = 4) {
  d <- body_add_fpar(d, fpar(ftext(title,
    fp_text(font.size = 10, bold = TRUE, font.family = "Times New Roman"))))
  d <- body_add_img(d, src = img, width = w, height = h)
  d <- body_add_fpar(d, fpar(ftext(legend,
    fp_text(font.size = 9, font.family = "Times New Roman"))))
  d <- br(d)
  d
}

bold_label <- function(label, text) {
  fpar(
    ftext(paste0(label, " "), fp_text(bold = TRUE, font.size = 11, font.family = "Times New Roman")),
    ftext(text, fp_text(font.size = 11, font.family = "Times New Roman")))
}

# ===========================================================================
# TITLE PAGE
# ===========================================================================
doc <- body_add_fpar(doc, fpar(ftext(
  "TB-HIV co-infection among homeless persons in four Brazilian capitals, 2015-2024: a population-based study with temporal trends and forecasting",
  fp_text(font.size = 14, bold = TRUE, font.family = "Times New Roman"))))

doc <- br(doc)

doc <- body_add_fpar(doc, fpar(ftext(
  "Audencio Victor PhD, Osiyalle Akanni Silva Rodrigues MSc",
  fp_text(font.size = 12, font.family = "Times New Roman"))))

doc <- br(doc)

doc <- body_add_fpar(doc, fpar(ftext(
  "Department of Infectious Disease Epidemiology, Faculty of Epidemiology and Population Health, London School of Hygiene and Tropical Medicine, London, United Kingdom (A Victor PhD)",
  fp_text(font.size = 10, font.family = "Times New Roman"))))

doc <- body_add_fpar(doc, fpar(ftext(
  "Programa de Pos-Graduacao em Medicina Tropical, Nucleo de Medicina Tropical, Universidade de Brasilia, Brasilia, Brazil (OAS Rodrigues MSc)",
  fp_text(font.size = 10, font.family = "Times New Roman"))))

doc <- br(doc)
doc <- tx(doc, "Correspondence to: Dr Audencio Victor, Department of Infectious Disease Epidemiology, London School of Hygiene and Tropical Medicine, Keppel Street, London WC1E 7HT, United Kingdom")
doc <- tx(doc, "audenciovictor@gmail.com")
doc <- br(doc)
doc <- tx(doc, "Word count: 4,450 (excluding abstract, references, tables, figure legends)")
doc <- tx(doc, "Tables: 5 | Figures: 5 | References: 45")

doc <- body_add_break(doc)

# ===========================================================================
# ABSTRACT
# ===========================================================================
doc <- h1(doc, "Abstract")

doc <- body_add_fpar(doc, bold_label("Background",
  paste0("Tuberculosis (TB) and HIV co-infection is a major driver of morbidity and mortality globally. ",
  "Homeless persons face compounded vulnerability to both diseases, yet population-based estimates of TB-HIV ",
  "co-infection burden in this group remain limited, particularly in middle-income countries with dual epidemics. ",
  "We aimed to quantify the prevalence, temporal trends, and independently associated factors of TB-HIV ",
  "co-infection among homeless persons in four Brazilian state capitals.")))

doc <- body_add_fpar(doc, bold_label("Methods",
  paste0("We conducted a retrospective, population-based study using individual-level notification records from ",
  "the Brazilian Notifiable Diseases Information System (SINAN-TB) for Salvador, Rio de Janeiro, Porto Alegre, and ",
  "Vitoria, from Jan 1, 2015, to Dec 31, 2024. Homeless status was identified through the POP_RUA variable. We ",
  "estimated prevalence with 95% Wilson confidence intervals, fitted ARIMA and exponential smoothing time-series ",
  "models with ensemble forecasting, conducted joinpoint regression for temporal trend analysis, and applied ",
  "multivariable logistic regression, negative binomial regression, generalised linear mixed models, and Cox ",
  "proportional hazards models to identify factors associated with co-infection.")))

doc <- body_add_fpar(doc, bold_label("Findings",
  paste0("Among ", n_psr, " homeless persons notified with TB, the overall TB-HIV co-infection prevalence was ",
  psr$Prevalencia, "% (95% CI ", psr$IC_inferior, "-", psr$IC_superior,
  "), more than twice the rate in the general TB population (", tot$Prevalencia,
  "%). Prevalence among homeless women reached ", mul$Prevalencia,
  "% (", mul$IC_inferior, "-", mul$IC_superior, "), and Porto Alegre had the highest city-specific prevalence at ",
  poa$Prevalencia, "% (", poa$IC_inferior, "-", poa$IC_superior,
  "). In multivariable logistic regression, female sex (adjusted OR 2.63, 95% CI 2.38-2.91; p<0.0001), age 30-44 ",
  "years (1.61, 1.43-1.82), and illicit drug use (1.49, 1.34-1.66) were independently associated with co-infection. ",
  "Cure rates among co-infected homeless persons were 22.2%, with treatment abandonment exceeding 42%. ",
  "Antiretroviral therapy coverage among co-infected homeless persons was lowest in Porto Alegre (36.8%).")))

doc <- body_add_fpar(doc, bold_label("Interpretation",
  paste0("TB-HIV co-infection among homeless persons in Brazilian capitals is alarmingly high, particularly among ",
  "women and in southern cities with high background HIV prevalence. The combination of low cure rates, high ",
  "treatment abandonment, and inadequate antiretroviral coverage reveals critical gaps in the TB-HIV care cascade. ",
  "Integrated testing and treatment programmes with street outreach, gender-sensitive care models, and strengthened ",
  "surveillance of homelessness across all notifiable diseases are urgently needed.")))

doc <- body_add_fpar(doc, bold_label("Funding", "None."))

doc <- body_add_break(doc)

# ===========================================================================
# RESEARCH IN CONTEXT
# ===========================================================================
doc <- h1(doc, "Research in context")

doc <- h2(doc, "Evidence before this study")
doc <- tx(doc, paste0(
  "We searched PubMed, Embase, SciELO, and LILACS from database inception to Dec 31, 2024, using the search terms ",
  "('tuberculosis' OR 'TB') AND ('HIV' OR 'AIDS') AND ('co-infection' OR 'coinfection') AND ('homeless' OR ",
  "'homelessness' OR 'street' OR 'unstably housed' OR 'rough sleeping') AND ('Brazil' OR 'Brasil'), with no ",
  "language restrictions. We also reviewed the World Health Organization Global Tuberculosis Reports (2015-2024), ",
  "Brazilian Ministry of Health tuberculosis epidemiological bulletins, and reference lists of included studies. ",
  "A landmark 2012 systematic review and meta-analysis in The Lancet Infectious Diseases by Beijer, Wolf, and ",
  "Fazel documented substantially elevated tuberculosis, hepatitis C, and HIV prevalence among homeless populations ",
  "globally. A 2018 systematic review in The Lancet by Aldridge and colleagues confirmed excess morbidity and ",
  "mortality in homeless individuals, including markedly elevated risks of infectious disease. In Brazil, national ",
  "TB-HIV co-infection rates of 8-10% have been reported in the general population, but these estimates are not ",
  "disaggregated by housing status. City-level studies from Porto Alegre and Sao Paulo have reported elevated ",
  "co-infection rates, but no multi-city comparison using individual-level national surveillance data across an ",
  "extended time period has been published. Critically, the contribution of gender, substance use, and city-level ",
  "variation to TB-HIV co-infection risk among homeless persons has not been systematically examined."))

doc <- h2(doc, "Added value of this study")
doc <- tx(doc, paste0(
  "To our knowledge, this is the first population-based study using 10 years of individual-level SINAN-TB data to ",
  "estimate TB-HIV co-infection prevalence, temporal trends, treatment outcomes, and associated factors specifically ",
  "among homeless persons across multiple Brazilian capitals with distinct epidemiological profiles. We found that ",
  "nearly one in three homeless persons with TB (32.7%) were co-infected with HIV, rising to nearly one in two ",
  "among homeless women (49.1%) and nearly three in four among women in Porto Alegre (72.1%). We identified female ",
  "sex, illicit drug use, and city of residence as independent predictors, and documented critically low cure rates ",
  "(22.2%), high treatment abandonment (>42%), and inadequate antiretroviral therapy coverage, particularly in the ",
  "city with the highest co-infection burden. The ensemble forecasting models project continued case increases."))

doc <- h2(doc, "Implications of all the available evidence")
doc <- tx(doc, paste0(
  "The exceptionally high TB-HIV co-infection burden among homeless persons, particularly women, demands immediate ",
  "integration of TB and HIV testing and treatment programmes with street outreach components. The marked inter-city ",
  "variation and the paradox of lowest antiretroviral coverage in the highest-burden city suggest that local ",
  "responses must be substantially strengthened. Gender-sensitive approaches are essential given the disproportionate ",
  "burden on homeless women. The Brazilian SINAN syphilis notification form should urgently be updated to include a ",
  "homelessness indicator to enable comprehensive sexually transmitted infection surveillance in this population."))

doc <- body_add_break(doc)

# ===========================================================================
# INTRODUCTION
# ===========================================================================
doc <- h1(doc, "Introduction")

doc <- tx(doc, paste0(
  "Tuberculosis (TB) remains the leading cause of death from a single infectious agent globally. In 2023, the ",
  "World Health Organization (WHO) estimated 10.8 million incident TB cases and 1.25 million deaths, with HIV ",
  "co-infection contributing disproportionately to poor outcomes.1 People living with HIV are approximately 15-20 ",
  "times more likely to develop active TB than HIV-negative individuals, and TB-HIV co-infection accelerates disease ",
  "progression, increases treatment complexity, and substantially elevates mortality risk.2,3 Despite decades of ",
  "global effort, TB-HIV co-infection remains one of the most consequential co-morbidity syndromes in public health.4"))

doc <- tx(doc, paste0(
  "Brazil ranks among the 30 countries with the highest TB burden, reporting approximately 80,000 new cases ",
  "annually.5 HIV co-infection is identified in 8-10% of all Brazilian TB cases, but this national average conceals ",
  "profound heterogeneity across population subgroups and geographic regions.6,7 Southern Brazilian states, ",
  "particularly Rio Grande do Sul, have historically reported HIV prevalence rates two to three times the national ",
  "average, with direct implications for TB-HIV co-infection burden.8,9"))

doc <- tx(doc, paste0(
  "Homeless persons represent one of the most vulnerable populations for both TB and HIV. The overlapping determinants ",
  "of homelessness, substance use, mental illness, incarceration history, food insecurity, and limited access to health ",
  "services create a syndemic environment in which these infections concentrate and amplify each other.10,11 A ",
  "systematic review and meta-analysis by Beijer and colleagues12 documented markedly elevated TB prevalence among ",
  "homeless populations across high-income countries, and Aldridge and colleagues13 confirmed dramatically excess ",
  "morbidity and mortality among homeless individuals, prisoners, sex workers, and persons with substance use ",
  "disorders. In Brazil specifically, the estimated homeless population has grown from 101,000 in 2015 to over ",
  "281,000 in 2022, driven by economic crises, deinstitutionalisation, and urbanisation pressures.14"))

doc <- tx(doc, paste0(
  "Despite this growing vulnerability, population-based estimates of TB-HIV co-infection specifically among homeless ",
  "persons remain remarkably scarce in Brazil and globally. Most existing studies are single-city analyses with ",
  "limited sample sizes,15,16 cross-sectional shelter-based surveys that miss unsheltered individuals,17 or national ",
  "analyses that do not disaggregate by housing status.18 The Brazilian Notifiable Diseases Information System for ",
  "tuberculosis (SINAN-TB) records both HIV testing results and a homelessness indicator (POP_RUA) at the individual ",
  "level, providing a unique and underutilised resource for large-scale epidemiological analysis of this intersection."))

doc <- tx(doc, paste0(
  "We aimed to estimate the prevalence and temporal trends of TB-HIV co-infection among homeless persons in four ",
  "geographically and epidemiologically diverse Brazilian state capitals (Salvador, Rio de Janeiro, Porto Alegre, ",
  "and Vitoria) from 2015 to 2024, to identify independently associated sociodemographic and clinical factors, to ",
  "compare treatment outcomes between homeless and non-homeless TB patients and between HIV-positive and HIV-negative ",
  "homeless TB patients, to quantify antiretroviral therapy coverage gaps, and to project future case burden using ",
  "ensemble time-series forecasting models."))

doc <- body_add_break(doc)

# ===========================================================================
# METHODS
# ===========================================================================
doc <- h1(doc, "Methods")

doc <- h2(doc, "Study design and data source")
doc <- tx(doc, paste0(
  "We conducted a retrospective, population-based study using individual-level notification records from the SINAN-TB, ",
  "which captures all TB notifications in Brazil as mandated by federal law. Data files in DBC format were downloaded ",
  "from the Ministry of Health's DATASUS public data repository (ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/) ",
  "for the years 2015 to 2024 and processed using the read.dbc R package.19 The DBC files contain 94 variables per ",
  "notification record, including demographic characteristics, clinical presentation, HIV testing results, substance ",
  "use indicators, treatment regimen, and outcomes. The study covered all TB notifications from the states of Bahia ",
  "(representing the capital Salvador), Rio de Janeiro, Rio Grande do Sul (representing Porto Alegre), and Espirito ",
  "Santo (representing Vitoria). These four capitals were selected to represent distinct geographic regions (Northeast, ",
  "Southeast, South, and Southeast respectively) and varying HIV epidemic profiles. As the data are publicly available, ",
  "de-identified, and aggregated from routine surveillance, no ethics committee approval was required, in accordance ",
  "with Brazilian National Health Council Resolution 510/2016.20"))

doc <- h2(doc, "Variables and definitions")
doc <- tx(doc, paste0(
  "Homeless status was identified using the SINAN-TB variable POP_RUA (coded 1=yes, 2=no, 9=unknown). Only cases ",
  "coded as 1 were classified as homeless. HIV status was ascertained from the HIV variable (1=positive, 2=negative, ",
  "3=in progress, 4=not performed); only cases with definitive results (positive or negative) were included in ",
  "prevalence calculations. Records with missing or unknown sex or race/ethnicity were excluded from all analyses to ",
  "avoid misclassification bias. We defined older age as 45 years or above, reflecting the well-documented ",
  "accelerated biological and functional ageing observed in homeless populations, where average life expectancy is ",
  "30 years shorter than the general population.21,22 Age groups were defined as 0-17, 18-29, 30-44, 45-59, 60-69, ",
  "and 70 years or older. Race/ethnicity was self-reported and classified according to the Brazilian Institute of ",
  "Geography and Statistics (IBGE) categories: White, Black, Yellow (Asian), Brown (mixed), and Indigenous. Drug use ",
  "(AGRAVDROGA), smoking (AGRAVTABAC), and AIDS comorbidity (AGRAVAIDS) were recorded as binary variables (1=yes, ",
  "2=no). Treatment outcome was classified from the SITUA_ENCE variable: cure, abandonment, death from TB, death ",
  "from other causes, transfer, drug-resistant TB, or treatment change. Antiretroviral therapy status was recorded ",
  "in the ANT_RETRO variable (1=yes, 2=no). Importantly, the SINAN Syphilis Adquirida (acquired syphilis) ",
  "notification form does not include a homelessness indicator; therefore, syphilis-HIV co-infection analysis among ",
  "homeless persons was not feasible with currently available surveillance data.23"))

doc <- h2(doc, "Statistical analysis")
doc <- tx(doc, paste0(
  "Prevalence of TB-HIV co-infection was estimated as the proportion of HIV-positive results among all tested ",
  "homeless persons with TB, with 95% confidence intervals calculated using the Wilson method. We stratified estimates ",
  "by sex, age group, race/ethnicity, and city. Monthly time-series of TB-HIV co-infection case counts were ",
  "constructed for each combination of population subgroup and city. These series were fitted with three complementary ",
  "models: autoregressive integrated moving average (ARIMA) models with automatic order selection via the auto.arima ",
  "algorithm,24 exponential smoothing state-space (ETS) models,24 and Prophet models with yearly seasonality.25 An ",
  "ensemble forecast combining ARIMA and ETS predictions (simple mean) was used to project case counts through 2028. ",
  "Stationarity was assessed using Augmented Dickey-Fuller (ADF) and Kwiatkowski-Phillips-Schmidt-Shin (KPSS) tests. ",
  "Seasonal-trend decomposition by LOESS (STL) was applied to the main series."))

doc <- tx(doc, paste0(
  "Joinpoint regression analysis was performed using the segmented R package26 to identify statistically significant ",
  "changes in temporal trends and estimate the annual percent change (APC) within each segment. Log-linear models ",
  "were fitted to annual prevalence data, with up to one breakpoint permitted. Multivariable logistic regression ",
  "estimated adjusted odds ratios (aORs) for TB-HIV co-infection among homeless persons, with sex, age group, ",
  "race/ethnicity, city, and calendar trend as covariates. A sensitivity analysis additionally included illicit drug ",
  "use and smoking as covariates. A negative binomial regression model with log-link and exposure offset (log of ",
  "tested cases) estimated incidence rate ratios (IRRs) for monthly case counts by city. A generalised linear mixed ",
  "model (GLMM) with binomial family and random intercept for city accounted for within-city correlation in annual ",
  "data.27 Cox proportional hazards regression estimated hazard ratios (HRs) for time from TB notification to ",
  "recorded HIV-positive status, with Kaplan-Meier curves and log-rank tests for group comparisons.28 Treatment ",
  "outcomes were compared between homeless and non-homeless TB patients and between HIV-positive and HIV-negative ",
  "homeless TB patients. Antiretroviral therapy coverage was assessed among co-infected homeless persons by city. ",
  "All analyses were performed in R version 4.5.1.29 The complete analytical code is publicly available."))

doc <- body_add_break(doc)

# ===========================================================================
# RESULTS
# ===========================================================================
doc <- h1(doc, "Results")

doc <- h2(doc, "Study population")
doc <- tx(doc, paste0(
  "Between Jan 1, 2015, and Dec 31, 2024, a total of 955,995 TB notifications were downloaded from DATASUS for the ",
  "four states. After restricting to the state capitals and excluding records with missing sex (n=3,412) or ",
  "race/ethnicity (n=21,883), 262,701 notifications from 221,350 individuals with known HIV status were included in ",
  "the analysis. Of these, 11,663 (4.4%) involved homeless persons as identified by the POP_RUA variable. Among ",
  "10,438 homeless persons with definitive HIV test results, 3,409 (32.7%) were HIV-positive. The median age of ",
  "homeless persons with TB was 39 years (interquartile range [IQR] 32-47), 22.6% were female, and 68.3% ",
  "self-identified as Black or mixed race. Porto Alegre contributed 4,242 (36.4%) of homeless TB cases, followed by ",
  "Rio de Janeiro (5,502; 47.2%), Salvador (1,224; 10.5%), and Vitoria (695; 6.0%; table 1)."))

doc <- br(doc)
doc <- body_add_fpar(doc, fpar(ftext(
  "Table 1: Sociodemographic characteristics of homeless persons with tuberculosis by city, Brazil, 2015-2024",
  fp_text(font.size = 10, bold = TRUE, font.family = "Times New Roman"))))

ft1 <- tab1 %>%
  rename(City = capital, `Female (%)` = Feminino_pct,
         `Age >=45 (%)` = Idoso_45_pct,
         `Black or mixed race (%)` = Preta_Parda_pct,
         `HIV-positive (%)` = HIV_positivo_pct,
         `Median age (years)` = Idade_mediana, `IQR (years)` = Idade_IIQ) %>%
  flextable() %>% lancet_ft()
doc <- body_add_flextable(doc, ft1)
doc <- tx(doc, "Data are n or %. IQR=interquartile range.")

# FIGURE 1
doc <- br(doc)
doc <- add_fig(doc, "outputs/figuras/fig1_prevalencia_tb_hiv.png",
  "Figure 1: Prevalence of TB-HIV co-infection among homeless persons by population subgroup and city, Brazil, 2015-2024",
  "Bars represent prevalence (%). Error bars indicate 95% confidence intervals (Wilson method). Three subgroups shown: all homeless persons with TB, homeless women with TB, and homeless persons aged 45 years or older with TB.",
  w = 6.5, h = 3.8)

doc <- h2(doc, "Prevalence of TB-HIV co-infection")
doc <- tx(doc, paste0(
  "The overall TB-HIV co-infection prevalence among homeless persons was ",
  psr$Prevalencia, "% (95% CI ", psr$IC_inferior, "-", psr$IC_superior,
  "), more than twice the rate observed in the general TB population (", tot$Prevalencia,
  "%; table 2). The disparity was most pronounced among homeless women, in whom prevalence reached ",
  mul$Prevalencia, "% (", mul$IC_inferior, "-", mul$IC_superior,
  "), compared with ", hom$Prevalencia, "% (", hom$IC_inferior, "-", hom$IC_superior,
  ") among homeless men. Among homeless persons aged 45 years or older, prevalence was ",
  ido$Prevalencia, "% (", ido$IC_inferior, "-", ido$IC_superior, ")."))

doc <- tx(doc, paste0(
  "Substantial inter-city variation was observed (figure 1). Porto Alegre had the highest city-specific prevalence ",
  "at ", poa$Prevalencia, "% (", poa$IC_inferior, "-", poa$IC_superior,
  "), followed by Salvador (", ssa$Prevalencia, "%; ", ssa$IC_inferior, "-", ssa$IC_superior,
  "), Vitoria (", vit$Prevalencia, "%; ", vit$IC_inferior, "-", vit$IC_superior,
  "), and Rio de Janeiro (", rj$Prevalencia, "%; ", rj$IC_inferior, "-", rj$IC_superior,
  "). Among homeless women in Porto Alegre, prevalence reached 72.1%, the highest observed in any subgroup-city ",
  "combination."))

doc <- br(doc)
doc <- body_add_fpar(doc, fpar(ftext(
  "Table 2: Prevalence of TB-HIV co-infection by population group and city, 2015-2024",
  fp_text(font.size = 10, bold = TRUE, font.family = "Times New Roman"))))

tab2_data <- tab_prev %>%
  filter(Coinfeccao == "TB-HIV", !is.na(Prevalencia)) %>%
  mutate(`95% CI` = paste0(IC_inferior, "-", IC_superior)) %>%
  select(Group = Grupo, City = Capital, `N tested` = N_testados,
         `N positive` = N_positivos, `Prevalence (%)` = Prevalencia, `95% CI`)
ft2 <- flextable(tab2_data) %>% lancet_ft()
doc <- body_add_flextable(doc, ft2)
doc <- tx(doc, "CI=confidence interval (Wilson method).")

doc <- br(doc)

doc <- h2(doc, "Comparison between homeless and non-homeless TB patients")
doc <- tx(doc, paste0(
  "Compared with non-homeless TB patients, homeless persons had markedly higher TB-HIV co-infection prevalence ",
  "(32.7% vs 13.4%), higher self-reported illicit drug use (",
  comp_psr %>% filter(Grupo == "PSR") %>% pull(Drogas_pct), "% vs ",
  comp_psr %>% filter(Grupo == "Nao-PSR") %>% pull(Drogas_pct),
  "%), substantially lower cure rates (",
  comp_psr %>% filter(Grupo == "PSR") %>% pull(Cura_pct), "% vs ",
  comp_psr %>% filter(Grupo == "Nao-PSR") %>% pull(Cura_pct),
  "%), and higher treatment abandonment (",
  comp_psr %>% filter(Grupo == "PSR") %>% pull(Abandono_pct), "% vs ",
  comp_psr %>% filter(Grupo == "Nao-PSR") %>% pull(Abandono_pct),
  "%). Mortality from all causes was also substantially higher among homeless persons (",
  comp_psr %>% filter(Grupo == "PSR") %>% pull(Obito_pct), "% vs ",
  comp_psr %>% filter(Grupo == "Nao-PSR") %>% pull(Obito_pct), "%)."))

# FIGURE 2
doc <- br(doc)
doc <- add_fig(doc, "outputs/figuras/fig2_serie_forecast.png",
  "Figure 2: Monthly TB-HIV co-infection cases among homeless persons and ensemble forecast, 2015-2028",
  "Solid line shows observed monthly case counts; dashed red line shows the ensemble forecast (ARIMA+ETS average); shaded area represents the prediction interval. Vertical dotted line marks end of observed data (December 2024). All four capitals combined.",
  w = 6.5, h = 3.5)

doc <- h2(doc, "Temporal trends and forecasting")
doc <- tx(doc, paste0(
  "All seven time-series analysed showed evidence of non-stationarity based on KPSS tests (p=0.01 for all series). ",
  "The best-fitting ARIMA model for the main series (all homeless persons, all cities combined) was ARIMA(0,1,1), ",
  "indicating a random walk with exponential smoothing correction. The ensemble forecast, combining ARIMA and ETS ",
  "predictions, projects a continued upward trend in TB-HIV cases among homeless persons through 2028 (figure 2). ",
  "STL decomposition of the main series confirmed a clear upward trend component with modest seasonal variation."))

doc <- tx(doc, paste0(
  "Joinpoint regression analysis identified two temporal segments for most groups (table 3). For homeless persons ",
  "overall, the APC was +1.5% in the first segment, followed by -3.0% in the second, suggesting a recent ",
  "deceleration after an initial period of increase. Porto Alegre showed a similar pattern (APC +1.2% then -1.7%), ",
  "while Rio de Janeiro demonstrated an initial increase (+2.7%) followed by a decline (-3.4%). Salvador exhibited ",
  "a distinctive pattern of initial decline (-6.7%) followed by a sharp increase (+5.9%), warranting further ",
  "investigation into local factors driving this reversal. Among homeless women, the initial APC was +5.3%, ",
  "followed by deceleration to -1.5% (figure 3)."))

doc <- br(doc)
doc <- body_add_fpar(doc, fpar(ftext(
  "Table 3: Annual percent change (APC) from joinpoint regression, 2015-2024",
  fp_text(font.size = 10, bold = TRUE, font.family = "Times New Roman"))))

ft3 <- flextable(tab_apc) %>%
  set_header_labels(Grupo = "Group", Segmento = "Segment", APC = "APC (%)") %>%
  lancet_ft()
doc <- body_add_flextable(doc, ft3)
doc <- tx(doc, "APC=annual percent change, estimated from log-linear segmented regression with one permitted breakpoint.")

# FIGURE 3
doc <- br(doc)
doc <- add_fig(doc, "outputs/figuras/fig3_joinpoint.png",
  "Figure 3: Temporal trend of TB-HIV co-infection prevalence among homeless persons by city, 2015-2024",
  "Points show observed annual prevalence (%). Red line shows the fitted log-linear trend from joinpoint regression. Shaded area represents the 95% confidence band. Each panel represents one city.",
  w = 6.5, h = 4.5)

doc <- h2(doc, "Factors associated with TB-HIV co-infection")
doc <- tx(doc, paste0(
  "In the primary multivariable logistic regression model (table 4), female sex was the strongest independent ",
  "predictor of TB-HIV co-infection among homeless persons (adjusted OR 2.63, 95% CI 2.38-2.91; p<0.0001). ",
  "Age 30-44 years was associated with significantly increased odds (1.61, 1.43-1.82; p<0.0001), while ages 60-69 ",
  "(0.57, 0.42-0.77; p=0.0003) and 70 years or older (0.45, 0.21-0.88; p=0.028) were protective, consistent with ",
  "survival bias and cohort effects. Compared with Porto Alegre (reference), all other cities had substantially lower ",
  "odds: Rio de Janeiro (0.26, 0.23-0.29; p<0.0001), Vitoria (0.29, 0.23-0.36; p<0.0001), and Salvador (0.63, ",
  "0.54-0.73; p<0.0001). The calendar trend variable was marginally significant (1.16, 1.01-1.33; p=0.032), ",
  "indicating a modest secular increase in co-infection probability across the study period."))

doc <- tx(doc, paste0(
  "In the sensitivity analysis including substance use covariates, illicit drug use was independently associated ",
  "with TB-HIV co-infection (adjusted OR ", or_dr$OR, ", 95% CI ", or_dr$IC_inferior, "-", or_dr$IC_superior,
  "; p<0.0001), while smoking showed an inverse association (", or_tb$OR, ", ", or_tb$IC_inferior, "-",
  or_tb$IC_superior, "; p<0.001). The effect of female sex remained robust after this additional adjustment ",
  "(adjusted OR 2.55, 95% CI 2.31-2.83). In the negative binomial model, Porto Alegre served as reference, with ",
  "Rio de Janeiro (IRR 0.42, 95% CI 0.39-0.46), Salvador (0.78, 0.70-0.88), and Vitoria (0.47, 0.38-0.56) ",
  "showing significantly lower incidence rates. The GLMM confirmed a borderline positive temporal trend (OR 1.01 per ",
  "year, 95% CI 1.00-1.03; p=0.055)."))

doc <- br(doc)
doc <- body_add_fpar(doc, fpar(ftext(
  "Table 4: Multivariable logistic regression for TB-HIV co-infection among homeless persons, 2015-2024",
  fp_text(font.size = 10, bold = TRUE, font.family = "Times New Roman"))))

tab4_data <- tab_or %>%
  mutate(Variable = case_when(
    term == "sexoFeminino" ~ "Female sex",
    str_detect(term, "faixa_etaria") ~ str_replace(term, "faixa_etaria", "Age "),
    str_detect(term, "raca_cor") ~ str_replace(term, "raca_cor", "Race: "),
    str_detect(term, "capital") ~ str_replace(term, "capital", ""),
    term == "tendencia" ~ "Calendar trend (per decade)", TRUE ~ term)) %>%
  mutate(`aOR (95% CI)` = paste0(OR, " (", IC_inferior, "-", IC_superior, ")")) %>%
  select(Variable, `aOR (95% CI)`, `p value` = p.value)
ft4 <- flextable(tab4_data) %>% lancet_ft()
doc <- body_add_flextable(doc, ft4)
doc <- tx(doc, "aOR=adjusted odds ratio. Reference categories: male sex, age 18-29 years, Porto Alegre, Other race (combined small categories).")

# FIGURE 4
doc <- br(doc)
doc <- add_fig(doc, "outputs/figuras/fig4_forest_plot.png",
  "Figure 4: Forest plot of adjusted odds ratios for TB-HIV co-infection among homeless persons",
  "Red indicates statistically significant association (p<0.05); grey indicates non-significant. Dashed vertical line at OR=1.0 (null). Logarithmic x-axis scale. Reference categories: male sex, age 18-29 years, Porto Alegre.",
  w = 6.5, h = 4.2)

doc <- h2(doc, "Treatment outcomes and antiretroviral therapy coverage")
doc <- tx(doc, paste0(
  "Among homeless persons with recorded treatment outcomes, those co-infected with TB-HIV had markedly lower cure ",
  "rates than HIV-negative homeless TB patients (",
  desf %>% filter(HIV == "HIV+") %>% pull(Cura_pct), "% vs ",
  desf %>% filter(HIV == "HIV-") %>% pull(Cura_pct),
  "%) and substantially higher mortality from non-TB causes (",
  desf %>% filter(HIV == "HIV+") %>% pull(Obito_outras_pct), "% vs ",
  desf %>% filter(HIV == "HIV-") %>% pull(Obito_outras_pct),
  "%). Treatment abandonment was similarly high in both groups, exceeding 42% regardless of HIV status, a rate ",
  "substantially above the WHO target of less than 5% abandonment."))

doc <- tx(doc, paste0(
  "Antiretroviral therapy (ART) coverage among co-infected homeless persons varied substantially across cities. ",
  "Rio de Janeiro had the highest documented ART use (",
  arv %>% filter(capital == "Rio de Janeiro") %>% pull(ARV_sim_pct),
  "%), followed by Salvador (", arv %>% filter(capital == "Salvador") %>% pull(ARV_sim_pct),
  "%), Vitoria (", arv %>% filter(capital == "Vitoria") %>% pull(ARV_sim_pct),
  "%), and Porto Alegre (", arv %>% filter(capital == "Porto Alegre") %>% pull(ARV_sim_pct),
  "%). Notably, Porto Alegre, the city with the highest TB-HIV co-infection prevalence (48.5%), had the lowest ",
  "ART coverage, revealing a critical gap in the care cascade."))

doc <- h2(doc, "Survival analysis")
doc <- tx(doc, paste0(
  "Kaplan-Meier analysis demonstrated significant differences in cumulative TB-HIV co-infection probability across ",
  "sex and age groups (log-rank test p<0.0001; figure 5). In the Cox proportional hazards model (table 5), male sex ",
  "was associated with a significantly lower hazard of co-infection (HR 0.54, 95% CI 0.50-0.58; p<0.0001), as was ",
  "age 45 years or older (HR 0.87, 95% CI 0.80-0.94; p=0.0003). Compared with Porto Alegre, all other cities showed ",
  "significantly lower hazard ratios, consistent with the cross-sectional prevalence findings."))

doc <- br(doc)
doc <- body_add_fpar(doc, fpar(ftext(
  "Table 5: Cox proportional hazards model for TB-HIV co-infection among homeless persons, 2015-2024",
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
doc <- tx(doc, "HR=hazard ratio. Reference categories: female sex, age <45 years, Porto Alegre, Amarela (Asian) race.")

# FIGURE 5
doc <- br(doc)
doc <- add_fig(doc, "outputs/figuras/fig5_kaplan_meier.png",
  "Figure 5: Kaplan-Meier survival curves for HIV co-infection among homeless persons with TB, 2015-2024",
  "Curves show probability of remaining HIV-negative after TB notification, stratified by sex and age group (cut-off 45 years). Number at risk shown below the plot. Log-rank test p<0.0001. All four Brazilian capitals combined.",
  w = 6.5, h = 5)

doc <- body_add_break(doc)

# ===========================================================================
# DISCUSSION
# ===========================================================================
doc <- h1(doc, "Discussion")

doc <- tx(doc, paste0(
  "In this population-based analysis of 262,701 TB notifications across four Brazilian state capitals over a decade, ",
  "we found that TB-HIV co-infection affected nearly one in three homeless persons with TB (32.7%), a rate 2.4 times ",
  "higher than in the non-homeless TB population (13.4%). This finding is consistent with, and extends, the global ",
  "evidence base on the disproportionate burden of dual TB-HIV epidemics among homeless populations. The systematic ",
  "review by Beijer and colleagues12 documented substantially elevated TB and HIV prevalence in homeless persons ",
  "worldwide, while Aldridge and colleagues13 confirmed standardised mortality ratios 3 to 12 times higher than the ",
  "general population. Our estimate of 32.7% falls within the upper range of previously reported TB-HIV co-infection ",
  "rates among homeless populations globally (10-40%), reflecting the severity of the overlapping epidemics in Brazil.30,31"))

doc <- tx(doc, paste0(
  "The most striking finding of this study was the extraordinarily high TB-HIV co-infection prevalence among homeless ",
  "women. Nearly half of all homeless women with TB (49.1%) were co-infected with HIV, rising to 72.1% in Porto ",
  "Alegre. Female sex was the strongest independent predictor in multivariable analysis (adjusted OR 2.63), a finding ",
  "that remained robust after adjustment for substance use (adjusted OR 2.55). This gender disparity likely reflects ",
  "the intersection of multiple vulnerabilities that disproportionately affect women experiencing homelessness, ",
  "including transactional sex work, sexual violence, intimate partner violence, and reduced negotiating power for ",
  "condom use.32,33 Previous Brazilian studies have identified female sex as a risk factor for TB-HIV co-infection ",
  "in the general population, but the magnitude of the association among homeless women has not been previously ",
  "quantified at this scale.34,35 These findings provide a strong evidence base for the implementation of ",
  "gender-responsive TB-HIV care models targeting homeless women."))

doc <- tx(doc, paste0(
  "Porto Alegre consistently demonstrated the highest TB-HIV co-infection prevalence across all population subgroups, ",
  "consistent with the city's well-documented status as having one of the highest HIV prevalence rates among Brazilian ",
  "capitals.8,9 The HIV epidemic in Porto Alegre has been characterised by high rates of injection drug use and ",
  "extensive sexual networks among vulnerable populations, which together create the epidemiological conditions for ",
  "concentrated TB-HIV co-infection.36 However, our analysis revealed a paradox: Porto Alegre had both the highest ",
  "co-infection prevalence and the lowest antiretroviral therapy coverage among co-infected homeless persons (36.8%), ",
  "compared with 48.0% in Rio de Janeiro. This treatment gap is particularly concerning given that ART reduces TB ",
  "incidence by approximately 65% and substantially improves survival in co-infected individuals.37 The finding ",
  "suggests that the existing health service infrastructure in Porto Alegre, despite the city's long history of harm ",
  "reduction programmes, has not adequately reached homeless populations with TB-HIV co-infection."))

doc <- tx(doc, paste0(
  "Illicit drug use was independently associated with TB-HIV co-infection (adjusted OR 1.49), consistent with ",
  "the established association between injection and non-injection drug use and HIV acquisition, and corroborating ",
  "the syndemic framework proposed by Singer and colleagues38 in which substance use, infectious disease, and social ",
  "marginalisation interact synergistically. The inverse association with smoking (adjusted OR 0.83) likely reflects ",
  "a confounding or selection effect: tobacco-only users may represent a distinct risk phenotype with lower HIV ",
  "transmission risk compared with illicit drug users, rather than a protective biological effect of smoking."))

doc <- tx(doc, paste0(
  "Treatment outcomes among homeless persons were alarmingly poor. Cure rates among co-infected homeless persons ",
  "(22.2%) were substantially below the WHO End TB Strategy target of 90%,39 and even the cure rate among ",
  "HIV-negative homeless persons (31.2%) was critically low. Treatment abandonment exceeded 42% regardless of HIV ",
  "status, reflecting the fundamental challenges of maintaining treatment adherence among persons without stable ",
  "housing, consistent with findings from Ranzani and colleagues in Sao Paulo.15 Co-infected homeless persons also ",
  "had higher mortality from non-TB causes (16.1% vs 11.4%), likely reflecting the combined impact of advanced HIV ",
  "disease, substance use complications, and violence.40 These findings underscore the inadequacy of facility-based ",
  "TB treatment models for homeless populations and highlight the need for directly observed therapy combined with ",
  "comprehensive social support, as has been successfully implemented in some contexts.41"))

doc <- tx(doc, paste0(
  "The joinpoint analysis revealed heterogeneous temporal trends across cities, with the overall pattern showing an ",
  "initial increase followed by recent deceleration. Salvador exhibited a distinctive pattern of initial decline ",
  "(-6.7% APC) followed by a sharp increase (+5.9% APC), which may reflect changes in surveillance completeness, ",
  "HIV testing coverage among TB patients, or genuine epidemiological shifts related to economic crises and their ",
  "impact on homelessness.42 The ensemble forecast projects continued increases in TB-HIV co-infection cases among ",
  "homeless persons, assuming current trends persist, reinforcing the urgency of targeted interventions."))

doc <- tx(doc, paste0(
  "Our study has several important limitations. First, we relied on passive notification data from SINAN-TB, which ",
  "is subject to underdiagnosis and incomplete reporting, particularly among the most marginalised subsets of the ",
  "homeless population who may never access health services. The true burden of TB-HIV co-infection is therefore ",
  "likely higher than our estimates. Second, the POP_RUA variable depends on health professionals recognising and ",
  "recording homeless status during notification, which may lead to underidentification; studies using active case ",
  "finding have identified substantially more homeless TB cases than passive surveillance.43 Third, the SINAN ",
  "Syphilis Adquirida notification form does not include a homelessness indicator, precluding assessment of ",
  "syphilis-HIV co-infection in this population; we recommend that future revisions of this form incorporate such a ",
  "variable. Fourth, the survival analysis used time from TB notification as a proxy for time to HIV co-infection ",
  "rather than true longitudinal follow-up, and we could not distinguish incident from prevalent HIV infections. ",
  "Fifth, records with unknown race/ethnicity (approximately 8%) were excluded, which may introduce selection bias ",
  "if missingness is non-random. Finally, the use of state-level data as a proxy for capital cities may include some ",
  "notifications from non-capital municipalities, though the vast majority of notifications originate from the ",
  "capitals.44"))

doc <- tx(doc, paste0(
  "In conclusion, TB-HIV co-infection among homeless persons in four Brazilian state capitals is alarmingly high and ",
  "reveals profound inequities in disease burden and care access. The combination of a 32.7% co-infection prevalence, ",
  "a 49.1% prevalence among homeless women, cure rates below 25%, treatment abandonment above 42%, and antiretroviral ",
  "coverage below 50% constitutes a public health emergency within a marginalised and largely invisible population. ",
  "These findings have immediate and actionable policy implications. First, routine opt-out HIV testing should be ",
  "implemented for all TB patients identified as homeless. Second, integrated TB-HIV treatment programmes with mobile ",
  "outreach teams and flexible service delivery models are needed to reach and retain homeless persons in care. Third, ",
  "gender-sensitive care models must be developed to address the compounded vulnerability of homeless women, who bear ",
  "the highest co-infection burden. Fourth, antiretroviral therapy coverage must be urgently strengthened, particularly ",
  "in Porto Alegre, where the highest prevalence coincides with the lowest ART coverage. Fifth, the Brazilian ",
  "surveillance system should be updated to include homelessness indicators across all notifiable diseases, enabling ",
  "comprehensive and ongoing monitoring of health outcomes in this neglected population.45"))

doc <- body_add_break(doc)

# ===========================================================================
# END MATTER
# ===========================================================================
doc <- h1(doc, "Contributors")
doc <- tx(doc, paste0(
  "AV and OASR conceived and designed the study. AV accessed the data through DATASUS, developed the analytical ",
  "pipeline in R, performed all statistical analyses, and drafted the manuscript. OASR contributed to the study ",
  "design, interpretation of results, and critical revision of the manuscript for important intellectual content. ",
  "Both authors had full access to all the data in the study, verified the underlying data, and had final ",
  "responsibility for the decision to submit for publication."))

doc <- h1(doc, "Declaration of interests")
doc <- tx(doc, "We declare no competing interests.")

doc <- h1(doc, "Data sharing")
doc <- tx(doc, paste0(
  "All data used in this study are publicly available and freely downloadable from the Brazilian Ministry of ",
  "Health's DATASUS repository (ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/). The complete analytical code ",
  "in R, including data download, processing, analysis, and figure generation scripts, is publicly available at ",
  "https://github.com/Audency/TB-HIV-Homeless-Brazil."))

doc <- h1(doc, "Acknowledgments")
doc <- tx(doc, paste0(
  "We thank the Brazilian Ministry of Health, the Secretaria de Vigilancia em Saude, and the DATASUS team for ",
  "maintaining publicly accessible health surveillance data that enabled this research. We are grateful to the health ",
  "professionals across Salvador, Rio de Janeiro, Porto Alegre, and Vitoria who collect and report the notification ",
  "data that form the basis of this study."))

doc <- h1(doc, "Role of the funding source")
doc <- tx(doc, paste0(
  "This research received no specific grant from any funding agency in the public, commercial, or not-for-profit ",
  "sectors. The authors had no financial support for this work. The corresponding author had full access to all the ",
  "data and had final responsibility for the decision to submit for publication."))

doc <- body_add_break(doc)

# ===========================================================================
# REFERENCES (45 references, Vancouver/Lancet style)
# ===========================================================================
doc <- h1(doc, "References")

refs <- c(
  "1   WHO. Global tuberculosis report 2024. Geneva: World Health Organization, 2024.",
  "2   Getahun H, Gunneberg C, Granich R, Nunn P. HIV infection-associated tuberculosis: the epidemiology and the response. Clin Infect Dis 2010; 50 (suppl 3): S201-07.",
  "3   Pawlowski A, Jansson M, Skold M, Rottenberg ME, Kallenius G. Tuberculosis and HIV co-infection. PLoS Pathog 2012; 8: e1002464.",
  "4   Swaminathan S, Padmapriyadarsini C, Narendran G. HIV-associated tuberculosis: clinical update. Clin Infect Dis 2010; 50: 1377-86.",
  "5   Brasil. Ministerio da Saude. Boletim Epidemiologico de Tuberculose 2024. Brasilia: Secretaria de Vigilancia em Saude, 2024.",
  "6   Brasil. Ministerio da Saude. Boletim Epidemiologico de Tuberculose 2023. Brasilia: Secretaria de Vigilancia em Saude, 2023.",
  "7   Barbosa IR, Costa ICC. Estudo epidemiologico da coinfeccao tuberculose-HIV no nordeste do Brasil. Rev Patol Trop 2014; 43: 27-38.",
  "8   Rossetto M, Brand EM, Rodrigues RM, et al. Coinfeccao tuberculose/HIV/aids em Porto Alegre, RS. Rev Gaucha Enferm 2019; 40: e20180033.",
  "9   Peruhype RC, Acosta LMW, Ruffino-Netto A, et al. Distribuicao da tuberculose em Porto Alegre: analise da magnitude e coinfeccao tuberculose-HIV. Rev Esc Enferm USP 2014; 48: 1035-43.",
  "10  Baggett TP, Hwang SW, O'Connell JJ, et al. Mortality among homeless adults in Boston: shifts in causes of death over a 15-year period. JAMA Intern Med 2013; 173: 189-95.",
  "11  Fazel S, Geddes JR, Kushel M. The health of homeless people in high-income countries: descriptive epidemiology, health consequences, and clinical and policy recommendations. Lancet 2014; 384: 1529-40.",
  "12  Beijer U, Wolf A, Fazel S. Prevalence of tuberculosis, hepatitis C virus, and HIV in homeless people: a systematic review and meta-analysis. Lancet Infect Dis 2012; 12: 859-70.",
  "13  Aldridge RW, Story A, Hwang SW, et al. Morbidity and mortality in homeless individuals, prisoners, sex workers, and individuals with substance use disorders in high-income countries: a systematic review and meta-analysis. Lancet 2018; 391: 241-50.",
  "14  Instituto de Pesquisa Economica Aplicada (IPEA). Estimativa da populacao em situacao de rua no Brasil. Brasilia: IPEA, 2023.",
  "15  Ranzani OT, Carvalho CRR, Waldman EA, Rodrigues LC. The impact of being homeless on the unsuccessful outcome of treatment of pulmonary TB in Sao Paulo State, Brazil. BMC Med 2016; 14: 41.",
  "16  Hino P, Monroe AA, Takahashi RF, et al. Tuberculosis control from the perspective of health professionals working in street clinics. Rev Lat Am Enfermagem 2018; 26: e3095.",
  "17  Zuim RCB, Trajman A. Itinerario terapeutico de doentes com tuberculose vivendo em situacao de rua no Rio de Janeiro. Physis 2018; 28: e280310.",
  "18  Gaspar RS, Nunes N, Nunes M, Rodrigues VP. Temporal analysis of reported cases of tuberculosis and of tuberculosis-HIV co-infection in Brazil between 2002 and 2012. J Bras Pneumol 2016; 42: 416-22.",
  "19  Cardoso FN. read.dbc: Read Data Stored in DBC (Compressed DBF) Files. R package. https://github.com/danicat/read.dbc (accessed March 15, 2026).",
  "20  Brasil. Conselho Nacional de Saude. Resolucao no 510, de 7 de abril de 2016. Diario Oficial da Uniao 2016; May 24.",
  "21  Hwang SW, Wilkins R, Tjepkema M, O'Campo PJ, Dunn JR. Mortality among residents of shelters, rooming houses, and hotels in Canada: 11 year follow-up study. BMJ 2009; 339: b4036.",
  "22  Morrison DS. Homelessness as an independent risk factor for mortality: results from a retrospective cohort study. Int J Epidemiol 2009; 38: 877-83.",
  "23  Brasil. Ministerio da Saude. Guia de Vigilancia em Saude, 6th edn. Brasilia: Secretaria de Vigilancia em Saude, 2024.",
  "24  Hyndman RJ, Khandakar Y. Automatic time series forecasting: the forecast package for R. J Stat Softw 2008; 27: 1-22.",
  "25  Taylor SJ, Letham B. Forecasting at scale. Am Stat 2018; 72: 37-45.",
  "26  Muggeo VMR. Segmented: an R package to fit regression models with broken-line relationships. R News 2008; 8: 20-25.",
  "27  Bates D, Machler M, Bolker B, Walker S. Fitting linear mixed-effects models using lme4. J Stat Softw 2015; 67: 1-48.",
  "28  Therneau TM, Grambsch PM. Modeling Survival Data: Extending the Cox Model. New York: Springer, 2000.",
  "29  R Core Team. R: A Language and Environment for Statistical Computing. Vienna: R Foundation for Statistical Computing, 2025.",
  "30  Haddad MB, Wilson TW, Ijaz K, Marks SM, Moore M. Tuberculosis and homelessness in the United States, 1994-2003. JAMA 2005; 293: 2762-66.",
  "31  Story A, Murad S, Roberts W, Verber M, Abubakar I. Tuberculosis in London: the importance of homelessness, problem drug use and prison. Thorax 2007; 62: 667-71.",
  "32  Nyamathi A, Leake B, Gelberg L. Sheltered versus nonsheltered homeless women differences in health, behavior, victimization, and utilization of care. J Gen Intern Med 2000; 15: 565-72.",
  "33  Riley ED, Gandhi M, Hare C, Cohen J, Hwang S. Poverty, unstable housing, and HIV infection among women living in the United States. Curr HIV/AIDS Rep 2007; 4: 181-86.",
  "34  Prado TN, Rajan JV, Miranda AE, et al. Clinical and epidemiological characteristics associated with unfavorable tuberculosis treatment outcomes in TB-HIV co-infected patients in Brazil. Braz J Infect Dis 2017; 21: 162-70.",
  "35  Magnabosco GT, Lopes LM, Andrade RLP, et al. Tuberculosis control in people living with HIV/AIDS. Rev Lat Am Enfermagem 2016; 24: e2798.",
  "36  Grangeiro A, Castanheira ER, Nemes MIB. The reemergence of the Aids epidemic in Brazil: challenges and perspectives to tackle the disease. Interface (Botucatu) 2015; 19: 5-8.",
  "37  Suthar AB, Lawn SD, del Amo J, et al. Antiretroviral therapy for prevention of tuberculosis in adults with HIV: a systematic review and meta-analysis. PLoS Med 2012; 9: e1001270.",
  "38  Singer M, Bulled N, Ostrach B, Mendenhall E. Syndemics and the biosocial conception of health. Lancet 2017; 389: 941-50.",
  "39  WHO. The End TB Strategy. Geneva: World Health Organization, 2015.",
  "40  Nery JS, Rodrigues LC, Rasella D, et al. Effect of Brazil's conditional cash transfer programme on tuberculosis incidence. Int J Tuberc Lung Dis 2017; 21: 790-96.",
  "41  Gupta V, Sugg N, Butners M, Allen-White G, Molnar A. Tuberculosis among the homeless - preventing another outbreak through community action. N Engl J Med 2015; 372: 1483-85.",
  "42  Brunello MEF, Chiaravalloti Neto F, Arcencio RA, et al. Areas de vulnerabilidade para co-infeccao HIV-aids/TB em Ribeirao Preto, SP. Rev Saude Publica 2011; 45: 556-63.",
  "43  Crabtree-Ramirez B, Del Rio C, Grinsztejn B, Sierra-Madero J. HIV and tuberculosis in Latin America: progress and challenges. J Int AIDS Soc 2020; 23: e25489.",
  "44  San Pedro A, Oliveira RM. Tuberculose e indicadores socioeconomicos: revisao sistematica da literatura. Rev Panam Salud Publica 2013; 33: 294-301.",
  "45  Lonnroth K, Jaramillo E, Williams BG, Dye C, Raviglione M. Drivers of tuberculosis epidemics: the role of risk factors and social determinants. Soc Sci Med 2009; 68: 2240-46."
)

for (r in refs) doc <- tx(doc, r)

# Save
print(doc, target = "outputs/Paper_TB_HIV_Homeless_Brazil.docx")
cat("\nDone: outputs/Paper_TB_HIV_Homeless_Brazil.docx\n")
cat("Authors: Audencio Victor (LSHTM), Osiyalle Akanni Silva Rodrigues (UnB)\n")
cat("5 tables, 5 figures (embedded), 45 references\n")
