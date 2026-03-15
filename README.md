# TB-HIV Co-infection Among Homeless Persons in Brazil

**Authors:** Audencio Victor, Osiyalle Akanni Silva Rodrigues

## Overview

Population-based analysis of TB-HIV co-infection among homeless persons in four Brazilian capitals (Salvador, Rio de Janeiro, Porto Alegre, Vitoria), 2015-2024.

**Data source:** SINAN-TB / DATASUS (Brazilian Ministry of Health)

## Key Findings

- **32.7%** TB-HIV co-infection prevalence among homeless persons (vs 14.2% general population)
- **49.1%** among homeless women
- **48.5%** in Porto Alegre (highest among cities)
- Female sex strongest predictor (aOR 2.63)
- Drug use independently associated (aOR 1.49)
- ART coverage only 36.8% in Porto Alegre

## Repository Structure

```
analise_COMPLETA.R        # Main analysis script (downloads data, runs all models)
gerar_paper_lancet.R      # Generates the Word manuscript (Lancet format)
outputs/
  tabelas/                # 14 CSV tables
  figuras/                # 7 PNG figures (300 dpi, Lancet style)
  Paper_TB_HIV_PSR_Lancet.docx  # Complete manuscript
```

## How to Reproduce

1. Open R (version >= 4.5)
2. Run: `source("analise_COMPLETA.R")`
   - Downloads TB and Syphilis data from DATASUS FTP (~10 min)
   - Runs all statistical analyses
   - Generates figures and tables
3. Run: `source("gerar_paper_lancet.R")`
   - Generates the Word manuscript

## Statistical Methods

- Prevalence with 95% Wilson CIs
- Time series: ARIMA, ETS, Prophet, ensemble forecasting
- Joinpoint regression (APC)
- Multivariable logistic regression (OR)
- Negative binomial regression (IRR)
- GLMM with random intercept by city
- Cox proportional hazards (HR)
- Kaplan-Meier survival analysis

## License

This project uses publicly available data from DATASUS.
