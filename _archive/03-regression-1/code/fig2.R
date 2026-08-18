rm(list = ls())
source(file.path(here::here(), "class3", "code", "helper.R"))

pacman::p_load(here, tidyverse, patchwork, latex2exp, ggpubfigs, leaps)

# Load the dataset
prostate_data <- read_csv(file.path(here(), "class3", "data", "prostate_cancer.csv"))

# Identify the response and predictor variables
response_var <- "logPSA"
predictor_vars <- setdiff(names(prostate_data), response_var)
n <- nrow(prostate_data)

# Fit the full model to get the error variance estimate (s^2) for Cp calculation
full_model <- lm(logPSA ~ ., data = prostate_data)
s2 <- summary(full_model)$sigma^2

# Initialize a list to store results from all models
all_models_results <- list()

# --- NEW: Start with the Null Model (k=0) ---
null_model <- lm(logPSA ~ 1, data = prostate_data)
null_summary <- summary(null_model)
p_null <- 1 # Intercept only

all_models_results[[1]] <- data.frame(
  k = 0,
  RSS = sum(residuals(null_model)^2),
  Adjusted_R2 = null_summary$adj.r.squared,
  Cp = (sum(residuals(null_model)^2) / s2) - (n - 2 * p_null),
  BIC = BIC(null_model)
)

# --- Loop for models with predictors (k=1 to 8) ---
for (k in 1:length(predictor_vars)) {
  predictor_combinations <- combn(predictor_vars, k, simplify = FALSE)
  for (combo in predictor_combinations) {
    formula_str <- paste(response_var, "~", paste(combo, collapse = " + "))
    model <- lm(as.formula(formula_str), data = prostate_data)
    model_summary <- summary(model)

    p <- k + 1 # k predictors + intercept

    all_models_results[[length(all_models_results) + 1]] <- data.frame(
      k = k,
      RSS = sum(residuals(model)^2),
      Adjusted_R2 = model_summary$adj.r.squared,
      Cp = (sum(residuals(model)^2) / s2) - (n - 2 * p),
      BIC = BIC(model)
    )
  }
}

# --- Data Wrangling and Plotting (Identical to before) ---
results_df <- do.call(rbind, all_models_results)

results_long <- results_df %>%
  pivot_longer(
    cols = c("RSS", "Adjusted_R2", "Cp", "BIC"),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(Metric = factor(Metric, levels = c("RSS", "Adjusted_R2", "Cp", "BIC")))

best_per_k <- results_long %>%
  group_by(Metric, k) %>%
  summarize(
    Optimal_Value = case_when(
      first(Metric) == "Adjusted_R2" ~ max(Value),
      TRUE ~ min(Value)
    ),
    .groups = 'drop'
  )

best_overall <- best_per_k %>%
  group_by(Metric) %>%
  slice(
    case_when(
      first(Metric) == "Adjusted_R2" ~ which.max(Optimal_Value),
      TRUE ~ which.min(Optimal_Value)
    )
  )

# --- Generate the Plot (with x-axis from 0 to 8) ---
fig2 <- ggplot(results_long, aes(x = k, y = Value)) +
  geom_point(alpha = 0.4, color = "gray50") +
  geom_line(data = best_per_k, aes(y = Optimal_Value), color = "#0072B2", linewidth = 1) +
  geom_point(data = best_overall, aes(y = Optimal_Value), color = "#D55E00", size = 4, shape = 18) +
  geom_label(
    data = best_overall,
    aes(x = k, y = Optimal_Value, label = round(Optimal_Value, 2)),
    vjust = ifelse(best_overall$Metric != "Adjusted_R2", -0.8, 1.8),
    fontface = "bold",
    fill = "white",
    alpha = 0.7
  ) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(
    title = "Best Subset Selection (Including Null Model)",
    x = "Number of Predictors (k)",
    y = "Metric Value"
  ) +
  # Ensure x-axis shows all integers from 0 to 8
  scale_x_continuous(breaks = 0:8) +
  theme_bw(base_size = 14)

ggsave(file.path(here(), "class3", "figures", "fig2.pdf"),
       fig2,
       device = pdf,
       width = 15,
       height = 6,
       units = "in")

