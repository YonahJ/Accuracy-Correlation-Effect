# =====================================================
# ACE ANALYSIS 
# =====================================================
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(stringr)
library(cowplot)

df <- read.csv("all_h_m.csv",
               stringsAsFactors = FALSE, check.names = TRUE)
truth_col <- "resolved_to"
spf_col   <- "SPF_forecast"
pub_col   <- "Pub_forecast"
qset_col  <- "Type"   # "Data" / "Market"
model_bs_cols <- grep("^BS", names(df), value = TRUE)
model_ids     <- str_trim(sub("^BS[\\._ ]*", "", model_bs_cols))
model_map <- data.frame(
  Model   = model_ids,
  F_col   = model_ids,
  BS_col  = model_bs_cols,
  stringsAsFactors = FALSE
)

# We now compute Corr(e_h, e_m), Corr(H, M), Cov(e_h, e_m), SDs and accuracy 
results_list <- list()
q_values <- unique(df[[qset_col]])

for (i in seq_len(nrow(model_map))) {
  mid    <- model_map$Model[i]
  f_col  <- model_map$F_col[i]
  bs_col <- model_map$BS_col[i]
  
  for (human in c("SPF", "PUB")) {
    
    h_col <- if (human == "SPF") spf_col else pub_col
    
    for (q in q_values) {
      idx <- which(df[[qset_col]] == q &
                     !is.na(df[[truth_col]]) &
                     !is.na(df[[h_col]]) &
                     !is.na(df[[f_col]]) &
                     !is.na(df[[bs_col]]))
      
      if (length(idx) < 5) next
      
      sub <- df[idx, ]
      
      e_h <- sub[[h_col]] - sub[[truth_col]]
      e_m <- sub[[f_col]] - sub[[truth_col]]
      
      corr_em <- cor(e_h, e_m, use = "complete.obs")
      corr_hm <- cor(sub[[h_col]], sub[[f_col]], use = "complete.obs")
      cov_em  <- cov(e_h, e_m, use = "complete.obs")
      sd_h    <- sd(e_h,  na.rm = TRUE)
      sd_m    <- sd(e_m,  na.rm = TRUE)
      acc_m   <- 1 - mean(sub[[bs_col]], na.rm = TRUE)
      
      results_list[[length(results_list) + 1L]] <- data.frame(
        Model    = mid,
        Human    = human,
        q_set    = q,
        Corr_err = corr_em,
        Corr_HM  = corr_hm,
        Cov_err  = cov_em,
        Sd_h     = sd_h,
        Sd_m     = sd_m,
        Accuracy = acc_m,
        stringsAsFactors = FALSE
      )
    }
  }
}

results <- do.call(rbind, results_list)

# =====================================================
# 0. LOAD PREPROCESSED RESULTS TABLE
# =====================================================
df <- results %>%
  mutate(
    q_set = tolower(q_set),     # "data" / "market"
    H_Src = Human,              # SPF / PUB
    Corr = Corr_HM,
    accuracy_std = as.numeric(scale(Accuracy))
  )
df <- df %>%
  separate(Model, into = c("model_name", "prompt"), sep = "_", remove = FALSE)

df <- df %>%
  mutate(ai_lab = case_when(
    model_name %in% c("gpt4T", "gpt4o", "gpt3.5", "gpt4") ~ "OpenAI",
    model_name %in% c("C2.1", "C3H", "C3O", "C3.5S") ~ "Anthropic",
    model_name %in% c("G1.5f", "G1.5P") ~ "Google",
    model_name %in% c("L2.70b", "L3.8b", "L3.70b") ~ "Meta",
    model_name %in% c("ML", "M22B", "M7B") ~ "Mistral"
  ))

# =====================================================
# 1. EMPIRICAL MIXED-EFFECTS MODELS
# =====================================================
# Metadata
llm_meta <- df %>% distinct(model_name, q_set, prompt, Accuracy)
llm_meta_data   <- llm_meta %>% filter(q_set == "data")
llm_meta_market <- llm_meta %>% filter(q_set == "market")

# Empirical ALL
m_full_all <- lmer(Corr ~ accuracy_std + q_set + H_Src + prompt + (1 | model_name),
                   data = df, REML = FALSE)
summary(m_full_all)
empirical_beta_all <- fixef(m_full_all)["accuracy_std"]

# Empirical DATA
df_data <- df %>% filter(q_set == "data")
m_full_data_emp <- lmer(Corr ~ accuracy_std + H_Src + prompt + (1 | model_name),
                        data = df_data, REML = FALSE)
empirical_beta_data <- fixef(m_full_data_emp)["accuracy_std"]

# Empirical MARKET
df_market <- df %>% filter(q_set == "market")
m_full_market_emp <- lmer(Corr ~ accuracy_std + H_Src + prompt + (1 | model_name),
                          data = df_market, REML = FALSE)
empirical_beta_market <- fixef(m_full_market_emp)["accuracy_std"]

# =====================================================
# 2. INDEPENDENT-ERRORS NULL
# =====================================================
# =====================================================
# 2.1. MIXED-EFFECTS NULL
# =====================================================
brier_to_sd <- function(acc, k = 0.8, floor_sd = 0.05) {
  pmax(floor_sd, k * sqrt(pmax(1 - acc, 1e-4)))
}

simulate_independent_null_mixed <- function(llm_meta,
                                            human_sd_spf = 0.1,
                                            human_sd_pub = 0.1,
                                            N_events = 1000,
                                            formula,
                                            return_data = FALSE) {
  
  true_p <- runif(N_events)
  y <- rbinom(N_events, 1, true_p)
  
  human_spf <- pmin(pmax(true_p + rnorm(N_events,0,human_sd_spf),0),1)
  human_pub <- pmin(pmax(true_p + rnorm(N_events,0,human_sd_pub),0),1)
  
  sds <- brier_to_sd(llm_meta$Accuracy)
  
  llm_forecasts <- lapply(sds, function(s)
    pmin(pmax(true_p + rnorm(N_events,0,s),0),1))
  
  acc_sim <- sapply(llm_forecasts, function(f) 1 - mean((f - y)^2))
  acc_std <- as.numeric(scale(acc_sim))
  
  rows <- list()
  idx <- 1
  for (i in seq_along(llm_forecasts)) {
    f_i <- llm_forecasts[[i]]
    base <- llm_meta[i,]
    
    rows[[idx]] <- base %>%
      mutate(H_Src="SPF", Corr=cor(f_i, human_spf), accuracy_std=acc_std[i])
    idx <- idx + 1
    
    rows[[idx]] <- base %>%
      mutate(H_Src="PUB", Corr=cor(f_i, human_pub), accuracy_std=acc_std[i])
    idx <- idx + 1
  }
  
  sim_data <- bind_rows(rows)
  m_sim <- lmer(formula, data=sim_data, REML=FALSE)
  beta_sim <- fixef(m_sim)["accuracy_std"]
  
  if (return_data) return(list(beta=beta_sim, data=sim_data))
  return(beta_sim)
}

# =====================================================
# 2.2. SIMPLE LINEAR MODEL NULL
# =====================================================
simulate_independent_null_lm <- function(llm_meta,
                                         human_sd_spf = 0.1,
                                         human_sd_pub = 0.1,
                                         N_events = 1000,
                                         formula,
                                         return_data = FALSE) {
  
  true_p <- runif(N_events)
  y <- rbinom(N_events, 1, true_p)
  
  human_spf <- pmin(pmax(true_p + rnorm(N_events, 0, human_sd_spf), 0), 1)
  human_pub <- pmin(pmax(true_p + rnorm(N_events, 0, human_sd_pub), 0), 1)
  
  sds <- brier_to_sd(llm_meta$Accuracy)
  
  llm_forecasts <- lapply(sds, function(s)
    pmin(pmax(true_p + rnorm(N_events, 0, s), 0), 1))
  
  acc_sim <- sapply(llm_forecasts, function(f) 1 - mean((f - y)^2))
  acc_std <- as.numeric(scale(acc_sim))
  
  rows <- list()
  idx <- 1
  for (i in seq_along(llm_forecasts)) {
    f_i <- llm_forecasts[[i]]
    base <- llm_meta[i, ]
    
    rows[[idx]] <- base %>%
      mutate(H_Src = "SPF", Corr = cor(f_i, human_spf), accuracy_std = acc_std[i])
    idx <- idx + 1
    
    rows[[idx]] <- base %>%
      mutate(H_Src = "PUB", Corr = cor(f_i, human_pub), accuracy_std = acc_std[i])
    idx <- idx + 1
  }
  
  sim_data <- bind_rows(rows)
  
  # Simple linear model (no random effects)
  m_sim <- lm(formula, data = sim_data)
  beta_sim <- coef(m_sim)["accuracy_std"]
  
  if (return_data) return(list(beta = beta_sim, data = sim_data))
  return(beta_sim)
}

# =====================================================
# 3. NULL SIMULATIONS
# =====================================================
# =====================================================
# 3.1. LINEAR MODEL NULL SIMULATIONS (300 iterations)
# =====================================================
set.seed(123)
N_iter_lm <- 300
N_events <- 1000

# ALL
null_slopes_lm_all <- replicate(
  N_iter_lm,
  simulate_independent_null_lm(
    llm_meta,
    formula = Corr ~ accuracy_std + q_set + H_Src + prompt
  )
)
lm_null_mean_all <- mean(null_slopes_lm_all)
lm_null_CI_all <- quantile(null_slopes_lm_all, c(0.025, 0.975))

# DATA
null_slopes_lm_data <- replicate(
  N_iter_lm,
  simulate_independent_null_lm(
    llm_meta_data,
    formula = Corr ~ accuracy_std + H_Src + prompt
  )
)
lm_null_mean_data <- mean(null_slopes_lm_data)
lm_null_CI_data <- quantile(null_slopes_lm_data, c(0.025, 0.975))

# MARKET
null_slopes_lm_market <- replicate(
  N_iter_lm,
  simulate_independent_null_lm(
    llm_meta_market,
    formula = Corr ~ accuracy_std + H_Src + prompt
  )
)
lm_null_mean_market <- mean(null_slopes_lm_market)
lm_null_CI_market <- quantile(null_slopes_lm_market, c(0.025, 0.975))

# =====================================================
# 3.2. MIXED-EFFECTS NULL SIMULATIONS (100 iterations)
# =====================================================
set.seed(123)
N_iter <- 100     
N_events <- 1000

# ALL
null_slopes_mixed_all <- replicate(
  N_iter,
  simulate_independent_null_mixed(
    llm_meta,
    formula = Corr ~ accuracy_std + q_set + H_Src + prompt + (1 | model_name)
  )
)
mixed_null_mean_all <- mean(null_slopes_mixed_all)
mixed_null_CI_all <- quantile(null_slopes_mixed_all, c(0.025, 0.975))

# DATA
null_slopes_mixed_data <- replicate(
  N_iter,
  simulate_independent_null_mixed(
    llm_meta_data,
    formula = Corr ~ accuracy_std + H_Src + prompt + (1 | model_name)
  )
)
mixed_null_mean_data <- mean(null_slopes_mixed_data)
mixed_null_CI_data <- quantile(null_slopes_mixed_data, c(0.025, 0.975))

# MARKET
null_slopes_mixed_market <- replicate(
  N_iter,
  simulate_independent_null_mixed(
    llm_meta_market,
    formula = Corr ~ accuracy_std + H_Src + prompt + (1 | model_name)
  )
)
mixed_null_mean_market <- mean(null_slopes_mixed_market)
mixed_null_CI_market <- quantile(null_slopes_mixed_market, c(0.025, 0.975))

################################################
#### RESULTS ###################################
################################################
################################################
#### RESULTS (I): EMPIRICAL EVIDENCE FOR ACE####
################################################
# FULL
m_mixed <- lmer(
  Corr_HM ~ accuracy_std + q_set + Human + prompt +
    (1 | model_name),
  data = df
)
summary(m_mixed)
#saveRDS(m_mixed, file = "fitted_objects/ace_me.rds")
# NULL
m_mixed_null <- lmer(
  Corr_HM ~ q_set + Human + prompt +
    (1 | model_name),
  data = df
)
summary(m_mixed_null)
#saveRDS(m_mixed_null, file = "fitted_objects/null_me.rds")

# Mixed effects model with interactions
m_mixed <- lmer(
  Corr_HM ~ 
    accuracy_std * q_set +
    accuracy_std * Human +
    Human * q_set +
    prompt +
    (1 | model_name),
  data = df
)
summary(m_mixed)
#saveRDS(m_mixed, file = "fitted_objects/ace_me_interaction.rds")

#################################################
### RESULTS (II): ACCURACY VS. CORRELATED ERRORS
#################################################
cat("=== EMPIRICAL ACE ===\n")
cat("All questions:", empirical_beta_all, "\n") # 0.08055562 
cat("Data:", empirical_beta_data, "\n") # 0.07133686
cat("Market:", empirical_beta_market, "\n\n") #  0.05680507

cat("=== LINEAR MODEL NULL ACE ===\n")
cat("All mean:", lm_null_mean_all,
    "CI:", lm_null_CI_all, "\n") # All mean: 0.03421652 CI: 0.03021935 0.03862388 
cat("Data mean:", lm_null_mean_data,
    "CI:", lm_null_CI_data, "\n") # Data mean: 0.04222731 CI: 0.03614855 0.04841683
cat("Market mean:", lm_null_mean_market,
    "CI:", lm_null_CI_market, "\n\n") # Market mean: 0.02465455 CI: 0.01897432 0.02988593

cat("=== MIXED-EFFECTS NULL ACE ===\n")
cat("All mean:", mixed_null_mean_all,
    "CI:", mixed_null_CI_all, "\n") # All mean: 0.02944001 CI: 0.02456925 0.03408666 
cat("Data mean:", mixed_null_mean_data,
    "CI:", mixed_null_CI_data, "\n") # Data mean: 0.03708396 CI: 0.02848239 0.04451585  
cat("Market mean:", mixed_null_mean_market,
    "CI:", mixed_null_CI_market, "\n") # Market mean: 0.02132053 CI: 0.01556189 0.02700183 

#############################################################
### RESULTS (II): PLOT: EMPIRICAL VS. INDEPENDENT ERRORS NULL
#############################################################
set.seed(321)
sim_rep_indep <- simulate_independent_null_mixed(
  llm_meta     = llm_meta,
  human_sd_spf = 0.1,
  human_sd_pub = 0.1,
  N_events     = N_events,
  formula      = Corr ~ accuracy_std + q_set + H_Src + prompt + (1 | model_name),
  return_data  = TRUE
)

# Simulated null (keep q_set so it facets into data/market)
sim_df_indep <- sim_rep_indep$data %>%
  mutate(
    H_Src = "Simulated (Independent Errors)"
  )

# Empirical data for plotting (rename H_Src labels)
data_plot <- df %>%
  select(accuracy_std, Corr, H_Src, q_set) %>%
  mutate(
    H_Src = ifelse(H_Src == "SPF", "Superforecasters", "General Public")
  )

combined_df <- bind_rows(
  data_plot,
  sim_df_indep %>% select(accuracy_std, Corr, H_Src, q_set)
)

sim_name <- "Simulated (Independent Errors)"

# Build facet labels: "data\nEmpirical ??_data = ...", etc.
lab_data <- sprintf(
  "Data\nEmpirical \u03b2_data = %.3f | Indep.-errors null \u03b2_data \u2248 %.3f",
  empirical_beta_data,  mixed_null_mean_data
)
lab_market <- sprintf(
  "Market \nEmpirical \u03b2_market = %.3f | Indep.-errors null \u03b2_market \u2248 %.3f",
  empirical_beta_market, mixed_null_mean_market
)

facet_labs <- as_labeller(c(
  "data"   = lab_data,
  "market" = lab_market
))

# FIGURE 2
p <- ggplot() +
  # 1) Simulated null line + 95% CI (grey), drawn FIRST (in the back)
  geom_smooth(
    data = combined_df %>% dplyr::filter(H_Src == sim_name),
    aes(x = accuracy_std, y = Corr, color = H_Src, fill = H_Src),
    method    = "lm",
    se        = TRUE,
    size      = 1,
    alpha     = 0.25,
    fullrange = TRUE
  ) +
  # 2) Empirical points on top
  geom_point(
    data = combined_df %>% dplyr::filter(H_Src != sim_name),
    aes(x = accuracy_std, y = Corr, color = H_Src),
    alpha = 0.6,
    size  = 2
  ) +
  # 3) Empirical lines + 95% CI, on top of simulated
  geom_smooth(
    data = combined_df %>% dplyr::filter(H_Src != sim_name),
    aes(x = accuracy_std, y = Corr, color = H_Src, fill = H_Src),
    method    = "lm",
    se        = TRUE,
    size      = 1,
    alpha     = 0.25,
    fullrange = TRUE
  ) +
  facet_wrap(~ q_set, scales = "free_x", labeller = facet_labs) +
  scale_color_manual(
    name   = "Forecaster Type / Model",
    values = c(
      "General Public"                 = "#E31A1C",
      "Superforecasters"               = "#1F78B4",
      "Simulated (Independent Errors)" = "grey30"
    )
  ) +
  scale_fill_manual(
    name   = "Forecaster Type / Model",
    values = c(
      "General Public"                 = "#E31A1C",
      "Superforecasters"               = "#1F78B4",
      "Simulated (Independent Errors)" = "grey70"
    )
  ) +
  labs(
    x = "LLM Accuracy (standardised 1 - Brier Score)",
    y = "Human-LLM Correlation",
    subtitle = sprintf(
      "Overall: Empirical \u03b2_all = %.3f | Indep.-errors mixed null \u03b2_all ~ %.3f",
      empirical_beta_all, mixed_null_mean_all
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(
      face = "bold", 
      size = 10,
      lineheight = 1.1
    ),
    strip.clip = "off",
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 12,
      margin = margin(b = 10)
    ),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 15, unit = "pt")
  )

print(p)
ggsave("Figures/ACE_independent_errors_only.pdf", p,
       width = 12, height = 6, dpi = 300)
ggsave("Figures/ACE_independent_errors_only.png", p,
       width = 9, height = 5, dpi = 300)


#############################################################
### RESULTS (II): PLOTTING  Corr(e_h, e_m), Corr(H, M), and SDS vs. ACCURACY
#############################################################
# Standardise accuracy
results <- results %>%
  mutate(
    Accuracy_std = as.numeric(scale(Accuracy)),
    HumanLabel   = ifelse(Human == "SPF", "Superforecasters", "General Public"),
    q_set_lab    = case_when(
      q_set %in% c("Data", "data")     ~ "Data questions",
      q_set %in% c("Market", "market") ~ "Market questions",
      TRUE                             ~ as.character(q_set)
    ),
    Sd_prod = Sd_h * Sd_m   # ??_h ??_m for later plotting
  )

results <- results %>%
  separate(Model, into = c("LLM", "Prompt"), sep = "_", remove = FALSE)

results <- results %>%
  mutate(ai_lab = case_when(
    LLM %in% c("gpt4T", "gpt4o", "gpt3.5", "gpt4") ~ "OpenAI",
    LLM %in% c("C2.1", "C3H", "C3O", "C3.5S") ~ "Anthropic",
    LLM %in% c("G1.5f", "G1.5P") ~ "Google",
    LLM %in% c("L2.70b", "L3.8b", "L3.70b") ~ "Meta",
    LLM %in% c("ML", "M22B", "M7B") ~ "Mistral"
  ))

# Corr(e_h, e_m) vs standardised Accuracy 
p_corr_err <- ggplot(results, aes(x = Accuracy_std, y = Corr_err, color = HumanLabel, fill = HumanLabel)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, size = 0.8, alpha = 0.2) +
  facet_grid(HumanLabel ~ q_set_lab) +
  scale_color_manual(values = c("General Public" = "#E31A1C", "Superforecasters" = "#1F78B4")) +
  scale_fill_manual(values = c("General Public" = "#E31A1C", "Superforecasters" = "#1F78B4")) +
  labs(
    x = "Model accuracy (standardised 1 - mean Brier score)",
    y = "Corr(e_h, e_m)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )

# Corr(H, M) vs standardised Accuracy
p_corr_hm <- ggplot(results, aes(x = Accuracy_std, y = Corr_HM, color = HumanLabel, fill = HumanLabel)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, size = 0.8, alpha = 0.2) +
  facet_grid(HumanLabel ~ q_set_lab) +
  scale_color_manual(values = c("General Public" = "#E31A1C", "Superforecasters" = "#1F78B4")) +
  scale_fill_manual(values = c("General Public" = "#E31A1C", "Superforecasters" = "#1F78B4")) +
  labs(
    x = "Model accuracy (standardised 1 - mean Brier score)",
    y = "Corr(H, M)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )

# Cov(e_h, e_m) vs standardised Accuracy
p_cov <- ggplot(results, aes(x = Accuracy_std, y = Cov_err, color = HumanLabel, fill = HumanLabel)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, size = 0.8, alpha = 0.2) +
  facet_grid(HumanLabel ~ q_set_lab) +
  scale_color_manual(values = c("General Public" = "#E31A1C", "Superforecasters" = "#1F78B4")) +
  scale_fill_manual(values = c("General Public" = "#E31A1C", "Superforecasters" = "#1F78B4")) +
  labs(
    x = "Model accuracy (standardised 1 - mean Brier score)",
    y = "Cov(e_h, e_m)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )

# Product of SDs vs standardised Accuracy
p_sd_prod <- ggplot(results, aes(x = Accuracy_std, y = Sd_prod, color = HumanLabel, fill = HumanLabel)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, size = 0.8, alpha = 0.2) +
  facet_grid(HumanLabel ~ q_set_lab) +
  scale_color_manual(values = c("General Public" = "#E31A1C", "Superforecasters" = "#1F78B4")) +
  scale_fill_manual(values = c("General Public" = "#E31A1C", "Superforecasters" = "#1F78B4")) +
  labs(
    x = "Model accuracy (standardised 1 - mean Brier score)",
    y = expression(sigma[h] * sigma[m])
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )

# COMBINE PLOTS with better dimensions
make_all_panels <- function(p_corr_hm, p_corr_err, p_cov, p_sd_prod,
                            filename = NULL,
                            width = 12, height = 10, dpi = 300) {
  # Combine in a 2x2 grid with labels A-D
  combined <- plot_grid(
    p_corr_hm,
    p_corr_err,
    p_cov,
    p_sd_prod,
    labels = c("A", "B", "C", "D"),
    label_size = 14,
    ncol = 2,
    align = "hv"
  )
  
  # SAVING FILE
  if (!is.null(filename)) {
    ggsave(filename, combined, width = width, height = height, dpi = dpi)
  }
  
  combined
}

fig_all <- make_all_panels(p_corr_hm, p_corr_err, p_cov, p_sd_prod,
                           filename = "Figures/ACE_all_panels.png")
fig_all <- make_all_panels(p_corr_hm, p_corr_err, p_cov, p_sd_prod,
                           filename = "Figures/ACE_all_panels.pdf")

fig_all

#############################################################
### RESULTS (II): MIXED EFFECTS MODELS WITH CORRELATED ERRORS
#############################################################
# Standarising Corr_err (For clean, comparable effect sizes between Accuracy and Corr(e???,e???.)
results$Corr_err_std <- as.numeric(scale(results$Corr_err))
colnames(results)


# SELECTED MODEL
m_mixed_std <- lmer(
  Corr_HM ~ Accuracy_std + Corr_err_std +
    HumanLabel * q_set_lab +
    Accuracy_std * q_set_lab +
    Accuracy_std * HumanLabel +
    Prompt +
    (1 | LLM),
  data = results
)

summary(m_mixed_std)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Corr_HM ~ Accuracy_std + Corr_err_std + HumanLabel * q_set_lab +  
#     Accuracy_std * q_set_lab + Accuracy_std * HumanLabel + Prompt +      (1 | LLM)
#    Data: results
# 
# REML criterion at convergence: -555.7
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -5.1355 -0.4066  0.0607  0.5336  4.9003 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  LLM      (Intercept) 0.005655 0.07520 
#  Residual             0.006308 0.07942 
# Number of obs: 304, groups:  LLM, 16
# 
# Fixed effects:
#                                                        Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                           4.920e-01  2.802e-02  5.195e+01  17.557  < 2e-16 ***
# Accuracy_std                                          6.096e-02  8.142e-03  2.818e+02   7.487 9.06e-13 ***
# Corr_err_std                                          1.015e-01  1.767e-02  2.848e+02   5.741 2.41e-08 ***
# HumanLabelSuperforecasters                            3.140e-02  1.540e-02  2.776e+02   2.039  0.04237 *  
# q_set_labMarket questions                             8.963e-02  2.011e-02  2.804e+02   4.458 1.20e-05 ***
# PromptNews                                            4.517e-02  1.634e-02  2.785e+02   2.764  0.00609 ** 
# PromptReas                                            3.555e-02  1.431e-02  2.752e+02   2.485  0.01355 *  
# Promptspf1                                           -7.074e-04  1.650e-02  2.821e+02  -0.043  0.96584    
# Promptspf2                                            1.424e-02  1.631e-02  2.813e+02   0.873  0.38343    
# Promptspf3                                            1.485e-02  1.630e-02  2.792e+02   0.911  0.36320    
# HumanLabelSuperforecasters:q_set_labMarket questions  3.567e-02  2.713e-02  2.804e+02   1.315  0.18967    
# Accuracy_std:q_set_labMarket questions               -5.736e-02  1.387e-02  2.865e+02  -4.135 4.68e-05 ***
# Accuracy_std:HumanLabelSuperforecasters               3.686e-04  9.370e-03  2.744e+02   0.039  0.96865    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
check_collinearity(m_mixed_std)
# Check for Multicollinearity
# Low Correlation
# 
# Term  VIF     VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
# Accuracy_std 2.16 [ 1.85,  2.57]     1.47      0.46     [0.39, 0.54]
# HumanLabel 2.86 [ 2.42,  3.43]     1.69      0.35     [0.29, 0.41]
# q_set_lab 4.87 [ 4.04,  5.93]     2.21      0.21     [0.17, 0.25]
# Prompt 1.21 [ 1.10,  1.43]     1.10      0.83     [0.70, 0.91]
# Accuracy_std:q_set_lab 1.15 [ 1.06,  1.38]     1.07      0.87     [0.72, 0.94]
# Accuracy_std:HumanLabel 1.77 [ 1.54,  2.09]     1.33      0.57     [0.48, 0.65]
# 
# Moderate Correlation
# 
# Term  VIF     VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
# HumanLabel:q_set_lab 6.65 [ 5.48,  8.13]     2.58      0.15     [0.12, 0.18]
# 
# High Correlation
# 
# Term   VIF     VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
# Corr_err_std 14.07 [11.47, 17.31]     3.75      0.07     [0.06, 0.09]

# LRT
m_mixed_std <- lmer(
  Corr_HM ~ Accuracy_std + Corr_err_std +
    HumanLabel * q_set_lab +
    Accuracy_std * q_set_lab +
    Accuracy_std * HumanLabel +
    Prompt +
    (1 | LLM),
  data = results,
  REML = FALSE
)
# No Corr_err_std
m_no_corr <- lmer(
  Corr_HM ~ Accuracy_std +
    HumanLabel * q_set_lab +
    Accuracy_std * q_set_lab +
    Accuracy_std * HumanLabel +
    Prompt +
    (1 | LLM),
  data = results,
  REML = FALSE
)
# No Accuracy_std
m_no_acc <- lmer(
  Corr_HM ~ Corr_err_std +
    HumanLabel * q_set_lab +
    Prompt +
    (1 | LLM),
  data = results,
  REML = FALSE
)

anova(m_no_corr, m_mixed_std)  # Test for Corr_err_std
# m_no_corr: Corr_HM ~ Accuracy_std + HumanLabel * q_set_lab + Accuracy_std * q_set_lab + Accuracy_std * HumanLabel + Prompt + (1 | LLM)
# m_mixed_std: Corr_HM ~ Accuracy_std + Corr_err_std + HumanLabel * q_set_lab + Accuracy_std * q_set_lab + Accuracy_std * HumanLabel + Prompt + (1 | LLM)
# npar     AIC     BIC logLik -2*log(L)  Chisq Df Pr(>Chisq)    
# m_no_corr     14 -585.28 -533.24 306.64   -613.28                         
# m_mixed_std   15 -615.87 -560.11 322.94   -645.87 32.595  1  1.135e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
anova(m_no_acc, m_mixed_std)   # Test for Accuracy_std
# m_no_acc: Corr_HM ~ Corr_err_std + HumanLabel * q_set_lab + Prompt + (1 | LLM)
# m_mixed_std: Corr_HM ~ Accuracy_std + Corr_err_std + HumanLabel * q_set_lab + Accuracy_std * q_set_lab + Accuracy_std * HumanLabel + Prompt + (1 | LLM)
# npar     AIC     BIC logLik -2*log(L)  Chisq Df Pr(>Chisq)    
# m_no_acc      12 -547.51 -502.90 285.75   -571.51                         
# m_mixed_std   15 -615.87 -560.11 322.94   -645.87 74.364  3   4.96e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


###
# Saving ACE Data
colnames(results)
results_export <- results %>%
  select(-Model, -HumanLabel, -q_set_lab) %>%
  rename(
    HumanCrowd = Human,
    QuestionSource = q_set,
    AI_Lab = ai_lab,
    Model = LLM
  )

#write.csv(results_export, "ace_data_me.csv", row.names = FALSE)





