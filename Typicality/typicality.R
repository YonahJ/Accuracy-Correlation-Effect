# =====================================================
# question source as proxy to typicality
# =====================================================
library(dplyr)
library(tidyr)
library(lme4)


df_typ <- read.csv("typicallity_scores_with_mean_z.csv")

colnames(df_typ)

df_typ_long <- df_typ %>%
  pivot_longer(
    cols = c(gpt_score, sonnet_score, deepseek_score),
    names_to = "Model",
    values_to = "score"
  ) %>%
  mutate(Model = str_remove(Model, "_score"))

typ_me <- lmer(
  score ~ 
    Type +
    (1 | question) +
    (1 | Model),
  data = df_typ_long,
  REML = FALSE
)

summary(typ_me)
# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: score ~ Type + (1 | question) + (1 | Model)
#    Data: df_typ_long
# 
#       AIC       BIC    logLik -2*log(L)  df.resid 
#    8375.0    8402.3   -4182.5    8365.0      1735 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.2475 -0.2260  0.2799  0.5376  1.8365 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  question (Intercept) 2.27991  1.5099  
#  Model    (Intercept) 0.05931  0.2435  
#  Residual             5.44927  2.3344  
# Number of obs: 1740, groups:  question, 580; Model, 3
# 
# Fixed effects:
#             Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)   8.0837     0.1660   4.3085  48.694 4.51e-07 ***
# TypeMarket   -1.3985     0.2892 579.4402  -4.835 1.71e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#            (Intr)
# TypeMarket -0.162


typ_null <- lmer(score ~
                   (1 | question) +
                   (1 | Model),
                 data = df_typ_long,
                 REML = FALSE
                   )

anova(typ_null, typ_me)
# Models:
# typ_null: score ~ (1 | question) + (1 | Model)
# typ_me: score ~ Type + (1 | question) + (1 | Model)
#          npar    AIC    BIC  logLik -2*log(L)  Chisq Df Pr(>Chisq)    
# typ_null    4 8395.9 8417.8 -4194.0    8387.9                         
# typ_me      5 8375.0 8402.3 -4182.5    8365.0 22.921  1  1.688e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# z-score
df_typ_long_z <- df_typ_long %>%
  group_by(Model) %>%
  mutate(score_z = scale(score)[,1]) %>%
  ungroup()

typ_me_z <- lmer(
  score_z ~ 
    Type +
    (1 | question) +
    (1 | Model),
  data = df_typ_long_z
)

summary(typ_me_z)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: score_z ~ Type + (1 | question) + (1 | Model)
#    Data: df_typ_long_z
# 
# REML criterion at convergence: 4768.9
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.0335 -0.1762  0.2457  0.5451  1.8975 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev. 
#  question (Intercept) 3.062e-01 5.533e-01
#  Model    (Intercept) 1.884e-32 1.373e-16
#  Residual             6.802e-01 8.248e-01
# Number of obs: 1740, groups:  question, 580; Model, 3
# 
# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   0.04468    0.03183 565.71189   1.404    0.161    
# TypeMarket   -0.47987    0.10432 565.71189  -4.600 5.22e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#            (Intr)
# TypeMarket -0.305
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')