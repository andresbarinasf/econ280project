
# Load required libraries
library(readr)      # to read CSVs
library(dplyr)      # for data manipulation
library(plm)        # for fixed effects regression
library(broom)      # for tidying regression output
library(writexl)    # to write Excel files
library(stargazer) 
library(fixest)

# Set paths 
setwd("/Users/andresbarinasf/Documents/GitHub/econ280project/")
output_path <- file.path("./results","table2.tex")

# Load data
df <- read_csv("./data/ms_blel_jpal_wide.csv")

# Run regressions and collect results
results_list <- list()

# 1. OLS 
ols_m <- lm(m_theta_mle2 ~ treat + m_theta_mle1, data = df)
results_list[["OLS_m_theta"]] <- tidy(ols_m, conf.int = TRUE)

# 2. OLS 
ols_h <- lm(h_theta_mle2 ~ treat + h_theta_mle1, data = df)
results_list[["OLS_h_theta"]] <- tidy(ols_h, conf.int = TRUE)

# 3. Fixed effects (within) 
fe_m <- plm(m_theta_mle2 ~ treat + m_theta_mle1, data = df, 
            index = "strata", model = "within")
results_list[["FE_m_theta"]] <- tidy(fe_m, conf.int = TRUE)

# 4. Fixed effects (within) 
fe_h <- plm(h_theta_mle2 ~ treat + h_theta_mle1, data = df, 
            index = "strata", model = "within")
results_list[["FE_h_theta"]] <- tidy(fe_h, conf.int = TRUE)


# Export to Overleagf
stargazer(ols_m, ols_h, fe_m, fe_h,
  type = "latex",
  title = "Treatment Effects on Test Scores",
  dep.var.labels = c("Math Score", "Hindi Score", "Math Score", "Hindi Score"),
  covariate.labels = c("Treatment", "Math Baseline Score", "Hindi Baseline Score"),
  no.space = TRUE,
  omit.stat = c("f", "ser"),  
  out = output_path
)

cat("Regressions completed. Output saved to:\n", output_path, "\n")