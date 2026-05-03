# Investigating Giraffe Body Mass
**MSc Bioinformatics | BIOLM0039 (Programming in R) | Final mark: 78**

Mixed-effects modelling of giraffe body mass using GLMM in R, 
identifying environmental and observational predictors across 
4 wildlife parks.

## Overview
This project fits and compares generalised linear mixed models 
(GLMMs) to assess which factors significantly predict giraffe 
body mass, accounting for observer and park-level clustering 
as crossed random effects.

**Key finding:** Distance to water was the only significant 
fixed-effect predictor (p = 0.025), with a ~1.2% increase in 
body mass per km further from water.

---

## Methods

- Data cleaning: outlier removal, long to wide pivot, and typo corrections
- Distribution assessment: Box-Cox, fitdistr, histogram
- Model families tested: Gaussian (log & cube-root), Gamma
- Model selection: AIC/BIC from ANOVA comparison
- Diagnostics: DHARMa residuals, VIF collinearity, check_model()
- Random effects: crossed park + observer intercepts
- Visualisation: ggplot2

---

## Key packages

`glmmTMB`, `DHARMa`, `performance`, `tidyverse`, `MASS`, `caret`

---

## Structure

BIOLM0039_exam.Rmd   # full analysis in R markdown doc
data/
  mass_sj19031.tsv   # giraffe observation data (about 8700 observations)

---

## Results summary

| Predictor           | Estimate | p-value |
|---------------------|----------|---------|
| Distance to water   | +0.012   | 0.025 * |
| Landscape (hilly)   | -0.041   | n.s.    |
| Vegetation (open)   | -0.039   | n.s.    |

Conditional $R^2$ = 0.221 | Marginal $R^2$ = 0.027

Random effects explained ~19.5% of variance vs ~2.7% 
from fixed effects, suggesting park/observer biases 
outweigh environmental predictors in this dataset.

---

## Usage
Open `BIOLM0039_exam.Rmd` in RStudio and knit to HTML.
All required packages are listed in the setup chunk.
