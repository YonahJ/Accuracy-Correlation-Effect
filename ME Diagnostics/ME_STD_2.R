source("setup.R")

# ========================================
# Full Model
m_full <- lmer(
  Corr_std ~ accuracy_std + Condition + q_set + H_Src + H_Src:accuracy_std 
  + (1 | Model),
  data = data,
  REML = FALSE
)

# Null Model
m_null <- lmer(
  Corr_std ~ Condition + q_set + H_Src 
  + (1 | Model),
  data = data,
  REML = FALSE
)

# ========================================
# likelihood ratio test
anova(m_null, m_full)
# m_null: Corr_std ~ Condition + q_set + H_Src + (1 | Model)
# m_full: Corr_std ~ accuracy_std + Condition + q_set + H_Src + H_Src:accuracy_std + (1 | Model)
# npar    AIC    BIC  logLik -2*log(L) Chisq Df Pr(>Chisq)    
# m_null   10 693.08 730.25 -336.54    673.08                        
# m_full   12 573.68 618.28 -274.84    549.68 123.4  2  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(m_full, update(m_full, . ~ . - H_Src))#without forecasters type (H_Src)
# update(m_full, . ~ . - H_Src): Corr_std ~ accuracy_std + Condition + q_set + (1 | Model) + accuracy_std:H_Src
# m_full: Corr_std ~ accuracy_std + Condition + q_set + H_Src + H_Src:accuracy_std + (1 | Model)
# npar    AIC    BIC  logLik -2*log(L)  Chisq Df Pr(>Chisq)    
# update(m_full, . ~ . - H_Src)   11 601.81 642.69 -289.90    579.81                         
# m_full                          12 573.68 618.28 -274.84    549.68 30.127  1  4.047e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(m_full, update(m_full, . ~ . - accuracy_std))#without accuracy
# m_full: Corr_std ~ accuracy_std + Condition + q_set + H_Src + H_Src:accuracy_std + (1 | Model)
# update(m_full, . ~ . - accuracy_std): Corr_std ~ Condition + q_set + H_Src + (1 | Model) + accuracy_std:H_Src
# npar    AIC    BIC  logLik -2*log(L) Chisq Df Pr(>Chisq)
# m_full                                 12 573.68 618.28 -274.84    549.68                    
# update(m_full, . ~ . - accuracy_std)   12 573.68 618.28 -274.84    549.68     0  0  

anova(m_full, update(m_full, . ~ . - H_Src:accuracy_std))#without H_Src:accuracy
# update(m_full, . ~ . - H_Src:accuracy_std): Corr_std ~ accuracy_std + Condition + q_set + H_Src + (1 | Model)
# m_full: Corr_std ~ accuracy_std + Condition + q_set + H_Src + H_Src:accuracy_std + (1 | Model)
# npar    AIC    BIC  logLik -2*log(L)  Chisq Df Pr(>Chisq)
# update(m_full, . ~ . - H_Src:accuracy_std)   11 572.20 613.09 -275.10    550.20                     
# m_full                                       12 573.68 618.28 -274.84    549.68 0.5181  1     0.4716

# ========================================
#order-invariant LRTs with drop1() on the full model; this gives χ² tests for removing each fixed term)
drop1(m_full, test = "Chisq")
# Single term deletions using Satterthwaite's method:
# 
# Model:
# Corr_std ~ accuracy_std + Condition + q_set + H_Src + H_Src:accuracy_std + (1 | Model)
#                    Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
# Condition          2.8058  0.5612     5 290.52  1.7931    0.1141    
# q_set              5.7525  5.7525     1 286.37 18.3810 2.474e-05 ***
# accuracy_std:H_Src 0.1623  0.1623     1 285.77  0.5186    0.4720    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# ========================================
# Interaction not significant → drop the interaction term and test main effects:
m_add <- update(m_full, . ~ . - H_Src:accuracy_std)
drop1(m_add, test = "Chisq")  # χ² tests for forecasters_type and accuracy_c
# Single term deletions using Satterthwaite's method:
# 
# Model:
# Corr_std ~ accuracy_std + Condition + q_set + H_Src + (1 | Model)
#              Sum Sq Mean Sq NumDF  DenDF  F value    Pr(>F)    
# accuracy_std 49.143  49.143     1 303.99 156.7443 < 2.2e-16 ***
# Condition     2.805   0.561     5 290.52   1.7896    0.1148    
# q_set         5.753   5.753     1 286.37  18.3495 2.513e-05 ***
# H_Src         9.943   9.943     1 285.76  31.7150 4.269e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# ========================================
######## Final
# Final model with REML for reporting
m_final <- update(m_add, REML = TRUE)

summary(m_final)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Corr_std ~ accuracy_std + Condition + q_set + H_Src + (1 | Model)
#    Data: data
# 
# REML criterion at convergence: 578.5
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.6925 -0.5140  0.0556  0.5912  3.5952 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Model    (Intercept) 0.2219   0.4710  
#  Residual             0.3223   0.5677  
# Number of obs: 304, groups:  Model, 16
# 
# Fixed effects:
#                      Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)           0.14215    0.14516  25.37094   0.979   0.3367    
# accuracy_std          0.51615    0.04197 294.98964  12.299  < 2e-16 ***
# ConditionP1          -0.01782    0.11760 287.65341  -0.152   0.8796    
# ConditionP2           0.15059    0.11608 286.38472   1.297   0.1956    
# ConditionP3           0.12407    0.11546 285.25656   1.075   0.2835    
# ConditionScratchpad   0.19309    0.10118 278.71345   1.909   0.0574 .  
# ConditionWith News    0.25708    0.11560 284.59975   2.224   0.0269 *  
# q_setMarket          -0.27725    0.06569 278.75239  -4.220  3.3e-05 ***
# H_SrcSPF             -0.36171    0.06512 278.19273  -5.555  6.5e-08 ***
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
#saveRDS(m_final, file = "fitted_objects/ace_me_std.rds")
#writeLines(capture.output(sessionInfo()), "fitted_objects/sessionInfo_ace_me_std.txt")

confint(m_final,  method = "Wald")
#                            2.5 %     97.5 %
# .sig01                        NA         NA
# .sigma                        NA         NA
# (Intercept)         -0.142358595  0.4266523
# accuracy_std         0.433897115  0.5984073
# ConditionP1         -0.248311396  0.2126656
# ConditionP2         -0.076914669  0.3781000
# ConditionP3         -0.102232950  0.3503692
# ConditionScratchpad -0.005206439  0.3913957
# ConditionWith News   0.030500448  0.4836520
# q_setMarket         -0.406006230 -0.1484977
# H_SrcSPF            -0.489341842 -0.2340781

r.squaredGLMM(m_final)
#            R2m       R2c
# [1,] 0.3790321 0.6322219

# ========================================
# Diagnostic: heteroscedasticity
check_model(m_final)
# diagnostic plot
ggsave("checks/std/mFullStd_Check.png", 
       width = 12, height = 10, dpi = 300)

# individual checks
check_heteroscedasticity(m_final)
# Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.017).
check_normality(m_final)
# Warning: Non-normality of residuals detected (p < .001).
check_outliers(m_final)
# 2 outliers detected: cases 263, 264.
# - Based on the following method and threshold: cook (0.7).
# - For variable: (Whole model)

# ========================================
# Influence
m_final_lme4 <- lme4::lmer(
  Corr_std ~ accuracy_std + Condition + q_set + H_Src +
    (1 | Model),
  data = data,
  REML = TRUE,
  control = lme4::lmerControl(calc.derivs = TRUE)  # ensures devfun available
)

infl <- influence(m_final_lme4, obs = TRUE)

# Cook’s distance
cooks <- cooks.distance(infl)
sort(cooks, decreasing = TRUE)[1:10]
# 0.88826106 0.53400645 0.15900107 0.08173130 0.05918168 0.04540907 0.03100584 0.01702598 0.01554748 0.01453708

# influential cases
plot(infl, which = "cook")
which.max(cooks.distance(infl))         # most influential 264 
sort(cooks.distance(infl), decreasing=TRUE)[1:5]  # top 5
# 0.88826106 0.53400645 0.15900107 0.08173130 0.05918168

influential_ids <- order(cooks.distance(infl), decreasing = TRUE)[1:2]
data[influential_ids, ]
# Model        Condition    BS Company   q_set accuracy H_Src   Corr accuracy_std Corr_std
# <chr>        <fct>     <dbl> <chr>     <chr>    <dbl> <chr>  <dbl>        <dbl>    <dbl>
# 1 Mistral8x7B P1        0.673 MistralAI data     0.327 PUB   0.182         -8.78    -2.61
# 2 Mistral8x7B P1        0.673 MistralAI data     0.327 SPF   0.0561        -8.78    -3.42

# dropping influential observations
m_drop2 <- update(m_final_lme4, data = data[-influential_ids, ])

cbind(
  original = fixef(m_final_lme4),
  no_outliers = fixef(m_drop2),
  change = fixef(m_drop2) - fixef(m_final_lme4)
)
#                       original no_outliers       change
# (Intercept)          0.1421468  0.18355097  0.041404135
# accuracy_std         0.5161522  1.02566033  0.509508137 <=
# ConditionP1         -0.0178229 -0.03020859 -0.012385687
# ConditionP2          0.1505927  0.29935074  0.148758069
# ConditionP3          0.1240681  0.14619609  0.022127969
# ConditionScratchpad  0.1930946  0.03702655 -0.156068057
# ConditionWith News   0.2570762  0.18776478 -0.069311449
# q_setMarket         -0.2772519 -0.29939362 -0.022141675
# H_SrcSPF            -0.3617100 -0.35874711  0.002962883

check_model(m_drop2)
ggsave("checks/std/mFullStd_outliers_removed_Check.png", 
       width = 12, height = 10, dpi = 300)

# ========================================
# Diagnostic: Leverage
lev_vals <- hatvalues(m_final_lme4)
ord <- order(lev_vals, decreasing = TRUE)
# top leverage observations
lev_vals[ord][1:10]
#       263       264       153       154       141       142       145       146       293       294 
# 0.3572636 0.3572636 0.1503762 0.1503762 0.1286823 0.1286823 0.1232545 0.1232545 0.1230488 0.123048
ord[1:10]
# 263 264 153 154 141 142 145 146 293 294
dev.off()
plot(lev_vals, pch = 16, main = "Leverage", ylab = "Leverage", xlab = "Observation")
abline(h = quantile(lev_vals, 0.99), lty = 2)

# ========================================
# leave one model out
g <- getME(m_final_lme4, "flist")[["Model"]]
levs <- levels(g)

# leave-one-model-out refits
get_fixef <- function(level_to_drop) {
  keep <- g != level_to_drop
  fit  <- update(m_final_lme4, data = data[keep, ])
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
# 0.4284818 1.0604322
mean(acc_vals)    # 0.5391604
sd(acc_vals)    # variability   0.1411552
acc_vals   # one value per left-out Model:Company
# drop_Claude2.1 drop_Claude3.5Sonnet    drop_Claude3Haiku     drop_Claude3Opus drop_Gemini1.5 Flash 
# 0.5266896            0.4945199            0.4651562            0.5054570            0.5163566 
# drop_Gemini1.5 Pro     drop_GPT3.5Turbo            drop_GPT4      drop_GPT4 Turbo           drop_GPT4o 
# 0.5130213            0.4284818            0.5136136            0.5155310            0.5144711 
# drop_llama2 70B      drop_llama3 70B       drop_llama3 8B   drop_Mistral Large    drop_Mistral8x22B 
# 0.5151313            0.5077439            0.5203119            0.5127161            0.5169333 
# drop_Mistral8x7B 
# 1.0604322

op <- par(mar = c(6,4,2,1))
plot(acc_vals, pch = 16, ylab = "Estimate of accuracy_std",
     xlab = "", xaxt = "n", main = "Leave-one-model-out")
abline(h = fixef(m_final_lme4)["accuracy_std"], lty = 2)
axis(1, at = seq_along(acc_vals), labels = levs, las = 2, cex.axis = 0.6)
par(op)

# ========================================
# Bootsrapping 
boot_out <- bootMer(m_final_lme4,
                    FUN = fixef,  # extract fixed effects
                    nsim = 1000,  # number of bootstrap samples
                    type = "parametric")
plot(boot_out, index=3)
confint(boot_out)
#                           2.5 %     97.5 %
# (Intercept)         -0.14264465  0.4370791
# accuracy_std         0.42572844  0.5987442
# ConditionP1         -0.24930690  0.2106648
# ConditionP2         -0.08663746  0.3749369
# ConditionP3         -0.10203413  0.3576589
# ConditionScratchpad -0.01256454  0.3965707
# ConditionWith News   0.03609430  0.4750713
# q_setMarket         -0.41325530 -0.1512598
# H_SrcSPF            -0.48418838 -0.2300517

#### parametric
acc_idx  <- which(names(fixef(m_final_lme4)) == "accuracy_std")
acc_draw <- boot_out$t[, acc_idx]
acc_ci   <- quantile(acc_draw, c(0.025, 0.975))
acc_hat  <- fixef(m_final_lme4)["accuracy_std"]

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
    Corr_std ~ accuracy_std + Condition + q_set + H_Src +
      (1 | Model),
    data = data[idx, ], REML = TRUE
  )
  fixef(fit)["accuracy_std"]
}

boot_acc <- boot(data, statistic = stat_acc, R = 1000)     # resampling rows
boot.ci(boot_acc, type = c("perc","bca"))                  # CI
# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 1000 bootstrap replicates
# 
# CALL : 
#   boot.ci(boot.out = boot_acc, type = c("perc", "bca"))
# 
# Intervals : 
# Level     Percentile            BCa          
# 95%   ( 0.3628,  1.0950 )   ( 0.3540,  1.0540 )  
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
par_names <- names(fixef(m_final_lme4))
boot_mat  <- as.data.frame(boot_out$t)
colnames(boot_mat) <- par_names

# 95% percentile CIs
ci_df <- data.frame(
  term = par_names,
  est  = as.numeric(fixef(m_final_lme4)),
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
