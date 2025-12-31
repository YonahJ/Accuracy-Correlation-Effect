source("setup.R")

# ========================================
# Full Model
m_full_0 <- lmer(
  Corr ~ accuracy_std + Condition + q_set + H_Src + H_Src:accuracy_std 
  + (1 | Model),
  data = data,
  REML = FALSE
)

# Null Model
m_null_0 <- lmer(
  Corr ~ Condition + q_set + H_Src 
  + (1 | Model),
  data = data,
  REML = FALSE
)

# ========================================
# likelihood ratio test
anova(m_null_0, m_full_0)
# m_null_0: Corr ~ Condition + q_set + H_Src + (1 | Model)
# m_full_0: Corr ~ accuracy_std + Condition + q_set + H_Src + H_Src:accuracy_std + (1 | Model)
#          npar     AIC     BIC logLik -2*log(L) Chisq Df Pr(>Chisq)    
# m_null_0   10 -437.92 -400.75 228.96   -457.92                        
# m_full_0   12 -557.32 -512.72 290.66   -581.32 123.4  2  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(m_full_0, update(m_full_0, . ~ . - H_Src)) #without forecasters type (H_Src)
# update(m_full_0, . ~ . - H_Src): Corr ~ accuracy_std + Condition + q_set + (1 | Model) + accuracy_std:H_Src
# m_full_0: Corr ~ accuracy_std + Condition + q_set + H_Src + H_Src:accuracy_std + (1 | Model)
#                                 npar     AIC     BIC logLik -2*log(L)  Chisq Df Pr(>Chisq)    
# update(m_full_0, . ~ . - H_Src)   11 -529.20 -488.31 275.60   -551.20                         
# m_full_0                          12 -557.32 -512.72 290.66   -581.32 30.127  1  4.047e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(m_full_0, update(m_full_0, . ~ . - accuracy_std)) #without accuracy
# m_full_0: Corr ~ accuracy_std + Condition + q_set + H_Src + H_Src:accuracy_std + (1 | Model)
# update(m_full_0, . ~ . - accuracy_std): Corr ~ Condition + q_set + H_Src + (1 | Model) + accuracy_std:H_Src
#                                        npar     AIC     BIC logLik -2*log(L) Chisq Df Pr(>Chisq)
# m_full_0                                 12 -557.32 -512.72 290.66   -581.32                    
# update(m_full_0, . ~ . - accuracy_std)   12 -557.32 -512.72 290.66   -581.32     0  0   

anova(m_full_0, update(m_full_0, . ~ . - H_Src:accuracy_std)) #without H_Src:accuracy
# update(m_full_0, . ~ . - H_Src:accuracy_std): Corr ~ accuracy_std + Condition + q_set + H_Src + (1 | Model)
# m_full_0: Corr ~ accuracy_std + Condition + q_set + H_Src + H_Src:accuracy_std + (1 | Model)
#                                              npar     AIC     BIC logLik -2*log(L)  Chisq Df Pr(>Chisq)
# update(m_full_0, . ~ . - H_Src:accuracy_std)   11 -558.80 -517.92 290.40   -580.80                     
# m_full_0                                       12 -557.32 -512.72 290.66   -581.32 0.5181  1     0.4716

# ========================================
#order-invariant LRTs with drop1() on the full model; this gives χ² tests for removing each fixed term
drop1(m_full_0, test = "Chisq")
# Single term deletions using Satterthwaite's method:
# 
# Model:
# Corr ~ accuracy_std + Condition + q_set + H_Src + H_Src:accuracy_std + (1 | Model)
#                      Sum Sq  Mean Sq NumDF  DenDF F value    Pr(>F)    
# Condition          0.067968 0.013594     5 290.52  1.7931    0.1141    
# q_set              0.139349 0.139349     1 286.37 18.3810 2.474e-05 ***
# accuracy_std:H_Src 0.003932 0.003932     1 285.77  0.5186    0.4720    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# ========================================
# Interaction not significant → drop the interaction term and test main effects:
m_add_0 <- update(m_full_0, . ~ . - H_Src:accuracy_std) # boundary (singular) fit: see help('isSingular')
drop1(m_add_0, test = "Chisq")  # χ² tests for forecasters_type and accuracy_c
# Single term deletions using Satterthwaite's method:
# 
# Model:
# Corr ~ accuracy_std + Condition + q_set + H_Src + (1 | Model)
#               Sum Sq Mean Sq NumDF  DenDF  F value    Pr(>F)    
# accuracy_std 1.19045 1.19045     1 303.99 156.7443 < 2.2e-16 ***
# Condition    0.06796 0.01359     5 290.52   1.7896    0.1148    
# q_set        0.13936 0.13936     1 286.37  18.3495 2.513e-05 ***
# H_Src        0.24087 0.24087     1 285.76  31.7150 4.269e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# ========================================
######## Final
# Final model with REML for reporting
m_final_0 <- update(m_add_0, REML = TRUE)
summary(m_final_0)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Corr ~ accuracy_std + Condition + q_set + H_Src + (1 | Model)
#    Data: data
# 
# REML criterion at convergence: -519.1
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.6925 -0.5140  0.0556  0.5912  3.5952 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Model    (Intercept) 0.005375 0.07331 
#  Residual             0.007807 0.08836 
# Number of obs: 304, groups:  Model, 16
# 
# Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)           0.610982   0.022593  25.370933  27.043  < 2e-16 ***
# accuracy_std          0.080335   0.006532 294.989643  12.299  < 2e-16 ***
# ConditionP1          -0.002774   0.018303 287.653413  -0.152   0.8796    
# ConditionP2           0.023438   0.018066 286.384721   1.297   0.1956    
# ConditionP3           0.019310   0.017971 285.256561   1.075   0.2835    
# ConditionScratchpad   0.030054   0.015747 278.713447   1.909   0.0574 .  
# ConditionWith News    0.040012   0.017992 284.599747   2.224   0.0269 *  
# q_setMarket          -0.043152   0.010224 278.752388  -4.220  3.3e-05 ***
# H_SrcSPF             -0.056297   0.010135 278.192726  -5.555  6.5e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) accrc_ CndtP1 CndtP2 CndtP3 CndtnS CndtWN q_stMr
# accurcy_std  0.048                                                 
# ConditionP1 -0.286  0.190                                          
# ConditionP2 -0.294  0.104  0.460                                   
# ConditionP3 -0.300  0.014  0.445  0.449                            
# CndtnScrtch -0.349 -0.127  0.399  0.416  0.429                     
# CndtnWthNws -0.303 -0.051  0.432  0.442  0.449  0.437              
# q_setMarket -0.229 -0.132 -0.025 -0.014 -0.002  0.017  0.007       
# H_SrcSPF    -0.224  0.000  0.000  0.000  0.000  0.000  0.000  0.000

# save fitted obj
#saveRDS(m_final_0, file = "fitted_objects/ace_me_r.rds")
#writeLines(capture.output(sessionInfo()), "fitted_objects/sessionInfo_ace_me_r.txt")

confint(m_final_0,  method = "Wald")
#                             2.5 %      97.5 %
# .sig01                         NA          NA
# .sigma                         NA          NA
# (Intercept)          0.5667010475  0.65526270
# accuracy_std         0.0675323570  0.09313695
# ConditionP1         -0.0386475355  0.03309957
# ConditionP2         -0.0119711077  0.05884802
# ConditionP3         -0.0159116804  0.05453195
# ConditionScratchpad -0.0008103372  0.06091737
# ConditionWith News   0.0047471324  0.07527628
# q_setMarket         -0.0631913806 -0.02311238
# H_SrcSPF            -0.0761618526 -0.03643225

r.squaredGLMM(m_final_0)
#            R2m       R2c
# [1,] 0.3790321 0.6322219

# ========================================
# Diagnostic: heteroscedasticity
check_model(m_final_0)
# diagnostic plot
ggsave("checks/raw/mFull_Check.png", 
       width = 12, height = 10, dpi = 300)

# individual checks
check_heteroscedasticity(m_final_0)
# Warning: Heteroscedasticity (non-constant error variance) detected (p < .001).
# Warning message:
#   In sqrt(insight::get_deviance(x)/(insight::n_obs(x) - sum(!is.na(estimates)))) :
#   NaNs produced
check_normality(m_final_0)
# Warning: Non-normality of residuals detected (p < .001)
check_outliers(m_final_0)
# 2 outliers detected: cases 263, 264.
# - Based on the following method and threshold: cook (0.7).
# - For variable: (Whole model).

# ========================================
#Influence
m_final_lme4_0 <- lme4::lmer(
  Corr ~ accuracy_std + Condition + q_set + H_Src +
    (1 | Model),
  data = data,
  REML = TRUE,
  control = lme4::lmerControl(calc.derivs = TRUE)  # ensures devfun available
)

infl <- influence(m_final_lme4_0, obs = TRUE)

# Cook’s distance
cooks <- cooks.distance(infl)
sort(cooks, decreasing = TRUE)[1:10]
# 0.88826110 0.53400649 0.15900106 0.08173129 0.05918167 0.04540907 0.03100584 0.01702598 0.01554748 0.01453708

# influential cases
plot(infl, which = "cook")
which.max(cooks.distance(infl))         # most influential 264 
sort(cooks.distance(infl), decreasing=TRUE)[1:5]  # top 5
# 0.88826110 0.53400649 0.15900106 0.08173129 0.05918167

influential_ids <- order(cooks.distance(infl), decreasing = TRUE)[1:2]
data[influential_ids, ]
#   Model       Condition    BS Company   q_set accuracy H_Src   Corr accuracy_std Corr_std
#   <chr>       <fct>     <dbl> <chr>     <chr>    <dbl> <chr>  <dbl>        <dbl>    <dbl>
# 1 Mistral8x7B P1        0.673 MistralAI data     0.327 PUB   0.182         -8.78    -2.61
# 2 Mistral8x7B P1        0.673 MistralAI data     0.327 SPF   0.0561        -8.78    -3.42

# dropping influential observations
m_drop2 <- update(m_final_lme4_0, data = data[-influential_ids, ])

cbind(
  original = fixef(m_final_lme4_0),
  no_outliers = fixef(m_drop2),
  change = fixef(m_drop2) - fixef(m_final_lme4_0)
)
#                         original  no_outliers        change
# (Intercept)          0.610981875  0.617426073  0.0064441980
# accuracy_std         0.080334653  0.159635217  0.0793005638 <=
# ConditionP1         -0.002773983 -0.004701708 -0.0019277252
# ConditionP2          0.023438455  0.046591371  0.0231529156
# ConditionP3          0.019310136  0.022754165  0.0034440286
# ConditionScratchpad  0.030053517  0.005762865 -0.0242906521
# ConditionWith News   0.040011705  0.029223974 -0.0107877311
# q_setMarket         -0.043151882 -0.046598044 -0.0034461616
# H_SrcSPF            -0.056297052 -0.055835905  0.0004611473

check_model(m_drop2)
ggsave("checks/raw/mFull_outliers_removed_Check.png", 
       width = 12, height = 10, dpi = 300)

# ========================================
# Diagnostic: Leverage
lev_vals <- hatvalues(m_final_lme4_0)
ord <- order(lev_vals, decreasing = TRUE)
# top leverage observations
lev_vals[ord][1:10]
# 263       264       153       154       141       142       145       146       293       294 
# 0.3572636 0.3572636 0.1503762 0.1503762 0.1286823 0.1286823 0.1232545 0.1232545 0.1230488 0.1230488 
ord[1:10]
# 263 264 153 154 141 142 145 146 293 294
dev.off()
plot(lev_vals, pch = 16, main = "Leverage", ylab = "Leverage", xlab = "Observation")
abline(h = quantile(lev_vals, 0.99), lty = 2)

# ========================================
# leave one model out
g <- getME(m_final_lme4_0, "flist")[["Model"]]
levs <- levels(g)

# leave-one-model-out refits
get_fixef <- function(level_to_drop) {
  keep <- g != level_to_drop
  fit  <- update(m_final_lme4_0, data = data[keep, ])
  fixef(fit)
}

fixef_list <- lapply(levs, get_fixef)
names(fixef_list) <- paste0("drop_", levs)

# results in a matrix
fixef_mat <- do.call(rbind, fixef_list)
# accuracy slope across lomo
acc_vals <- fixef_mat[, "accuracy_std"]

#summary
range(acc_vals)   # min/max across lomo
# 0.06668951 0.16504716
mean(acc_vals)    # 0.08391569
sd(acc_vals)    # variability  0.02196959
acc_vals   # one value per left-out Model
# drop_Claude2.1 drop_Claude3.5Sonnet    drop_Claude3Haiku     drop_Claude3Opus drop_Gemini1.5 Flash 
# 0.08197471           0.07696777           0.07239756           0.07867003           0.08036647 
# drop_Gemini1.5 Pro     drop_GPT3.5Turbo            drop_GPT4      drop_GPT4 Turbo           drop_GPT4o 
# 0.07984735           0.06668951           0.07993955           0.08023797           0.08007301 
# drop_llama2 70B      drop_llama3 70B       drop_llama3 8B   drop_Mistral Large    drop_Mistral8x22B 
# 0.08017576           0.07902597           0.08098208           0.07979986           0.08045623 
# drop_Mistral8x7B 
# 0.16504716  

op <- par(mar = c(6,4,2,1))
plot(acc_vals, pch = 16, ylab = "Estimate of accuracy_std",
     xlab = "", xaxt = "n", main = "Leave-one-model-out")
abline(h = fixef(m_final_lme4_0)["accuracy_std"], lty = 2)
axis(1, at = seq_along(acc_vals), labels = levs, las = 2, cex.axis = 0.6)
par(op)

# ========================================
# Bootsrapping 
boot_out <- bootMer(m_final_lme4_0,
                    FUN = fixef,  # extract fixed effects
                    nsim = 1000,  # number of bootstrap samples
                    type = "parametric")
plot(boot_out, index=3)
confint(boot_out)
#                           2.5 %      97.5 %
# (Intercept)          0.562686511  0.65505845
# accuracy_std         0.067903653  0.09343592
# ConditionP1         -0.037621764  0.03544396
# ConditionP2         -0.012744940  0.05767958
# ConditionP3         -0.017151026  0.05255298
# ConditionScratchpad  0.001331307  0.06127179
# ConditionWith News   0.002316856  0.07214399
# q_setMarket         -0.063107485 -0.02263781
# H_SrcSPF            -0.075803750 -0.03582397

#### parametric
acc_idx  <- which(names(fixef(m_final_lme4_0)) == "accuracy_std")
acc_draw <- boot_out$t[, acc_idx]
acc_ci   <- quantile(acc_draw, c(0.025, 0.975))
acc_hat  <- fixef(m_final_lme4_0)["accuracy_std"]

df_param <- data.frame(beta = acc_draw)

ggplot(df_param, aes(x = beta)) +
  geom_density(fill = "gray80") +
  geom_vline(xintercept = acc_hat, linetype = 2) +
  geom_vline(xintercept = acc_ci, linetype = 3) +
  labs(title = "Parametric bootstrap for accuracy_std",
       subtitle = sprintf("Point estimate = %.3f; 95%% CI = [%.3f, %.3f]",
                          acc_hat, acc_ci[1], acc_ci[2]),
       x = "Bootstrap estimates", y = "Density")


#### nonparametric bootstrap
stat_acc <- function(data, idx) {
  fit <- lme4::lmer(
    Corr ~ accuracy_std + Condition + q_set + H_Src +
      (1 | Model),
    data = data[idx, ], REML = TRUE
  )
  fixef(fit)["accuracy_std"]
}

boot_acc <- boot(data, statistic = stat_acc, R = 1000)     # resampling rows
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge with max|grad| = 0.00215831 (tol = 0.002, component 1)
boot.ci(boot_acc, type = c("perc","bca"))                  # CI
# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 1000 bootstrap replicates
# 
# CALL : 
# boot.ci(boot.out = boot_acc, type = c("perc", "bca"))
# 
# Intervals : 
# Level     Percentile            BCa          
# 95%   ( 0.0564,  0.1726 )   ( 0.0525,  0.1677 )  
# Calculations and Intervals on Original Scale

np_draw <- boot_acc$t[, 1]    # nonparametric draws of accuracy_std

df_both <- rbind(
  data.frame(beta = acc_draw,       type = "Parametric"),
  data.frame(beta = np_draw,        type = "Nonparametric")
)

ggplot(df_both, aes(x = beta, fill = type)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = acc_hat, linetype = 2) +
  labs(title = "Bootstrap distributions of accuracy_std",
       subtitle = "Parametric (bootMer) vs Nonparametric (boot::boot)",
       x = "Bootstrap estimates", y = "Density") +
  theme(legend.position = "top")


# all fixed eff
# bootstrap draws for all fixed effects
par_names <- names(fixef(m_final_lme4_0))
boot_mat  <- as.data.frame(boot_out$t)
colnames(boot_mat) <- par_names

# 95% percentile CIs
ci_df <- data.frame(
  term = par_names,
  est  = as.numeric(fixef(m_final_lme4_0)),
  lo   = apply(boot_mat, 2, function(x) quantile(x, 0.025)),
  hi   = apply(boot_mat, 2, function(x) quantile(x, 0.975))
)

ci_df$term <- factor(ci_df$term, levels = ci_df$term[order(ci_df$est)])

ggplot(ci_df, aes(x = term, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0) +
  geom_hline(yintercept = 0, linetype = 3) +
  coord_flip() +
  labs(title = "Parametric bootstrap 95% CIs for fixed effects",
       x = "", y = "Estimate")
