rm(list = ls())
source(file.path(here::here(), "class3", "code", "helper.R"))

# install.packages(c("glmnet", "ggplot2", "tidyr", "patchwork", "ggsci", "here"))
pacman::p_load(glmnet, ggplot2, tidyr, patchwork, ggsci, here)

# Set a seed for reproducibility
set.seed(1234)

#################################
## 1. SIMULATION PARAMETERS    ##
#################################
p <- 45         # Number of predictors
n <- 50         # Number of training observations
nsims <- 100    # Number of simulation runs
n_test <- 1000  # Number of observations in the test set
sigma <- 5      # Irreducible error standard deviation (sigma^2 = 25)

# Generate true coefficients
beta_true <- rnorm(p, mean = 0, sd = 2.5)
#beta_true <- ifelse(abs(beta_true) > 5, beta_true,  0)

# Define lambda grid
lambda_grid <- 10^seq(3, -2, length.out = 100)

#################################
## 2. RUN THE SIMULATION       ##
#################################

# --- Create arrays to store results for BOTH models ---
all_predictions_ridge <- array(0, dim = c(n_test, length(lambda_grid), nsims))
all_predictions_lasso <- array(0, dim = c(n_test, length(lambda_grid), nsims))
# Store training R-squared for each simulation
r_squared_ridge_all_sims <- matrix(0, nrow = nsims, ncol = length(lambda_grid))
r_squared_lasso_all_sims <- matrix(0, nrow = nsims, ncol = length(lambda_grid))


# Generate a single, large test set
X_test <- matrix(rnorm(n_test * p), n_test, p)
f_test <- as.vector(X_test %*% beta_true)

# Main simulation loop
cat("Running simulation for Ridge and Lasso...\n")
for (i in 1:nsims) {
  X_train <- matrix(rnorm(n * p), n, p)
  # ❗ CORRECTED LINE: Convert y_train to a vector
  y_train <- as.vector(X_train %*% beta_true + rnorm(n, 0, sigma))

  # --- Fit both models ---
  fit_ridge <- glmnet(X_train, y_train, alpha = 0, lambda = lambda_grid,
                      standardize = FALSE, intercept = FALSE)
  fit_lasso <- glmnet(X_train, y_train, alpha = 1, lambda = lambda_grid,
                      standardize = FALSE, intercept = FALSE)

  # --- Store predictions on the TEST set ---
  all_predictions_ridge[, , i] <- predict(fit_ridge, newx = X_test)
  all_predictions_lasso[, , i] <- predict(fit_lasso, newx = X_test)

  # --- Calculate R-squared on the TRAINING set (This part now works) ---
  ss_total <- sum((y_train - mean(y_train))^2)

  # R-squared for Ridge
  y_pred_train_ridge <- predict(fit_ridge, newx = X_train)
  ss_resid_ridge <- colSums((y_train - y_pred_train_ridge)^2)
  r_squared_ridge_all_sims[i, ] <- 1 - ss_resid_ridge / ss_total

  # R-squared for Lasso
  y_pred_train_lasso <- predict(fit_lasso, newx = X_train)
  ss_resid_lasso <- colSums((y_train - y_pred_train_lasso)^2)
  r_squared_lasso_all_sims[i, ] <- 1 - ss_resid_lasso / ss_total
}
cat("Simulation complete.\n")

#################################
## 3. CALCULATE METRICS        ##
#################################

# --- Calculations for Ridge ---
mean_pred_ridge <- apply(all_predictions_ridge, c(1, 2), mean)
bias_sq_ridge <- colMeans((mean_pred_ridge - f_test)^2)
variance_ridge <- colMeans(apply(all_predictions_ridge, c(1, 2), var))
test_mse_ridge <- bias_sq_ridge + variance_ridge
avg_r_squared_ridge <- colMeans(r_squared_ridge_all_sims)

# --- Calculations for Lasso ---
mean_pred_lasso <- apply(all_predictions_lasso, c(1, 2), mean)
bias_sq_lasso <- colMeans((mean_pred_lasso - f_test)^2)
variance_lasso <- colMeans(apply(all_predictions_lasso, c(1, 2), var))
test_mse_lasso <- bias_sq_lasso + variance_lasso
avg_r_squared_lasso <- colMeans(r_squared_lasso_all_sims)


#####################################################
## 4. GENERATE THE FIGURE (with ggplot2)           ##
#####################################################

# --- Create and combine data frames for plotting ---
plot_data_ridge <- data.frame(Model = "Ridge", r_squared = avg_r_squared_ridge,
                              `Bias^2` = bias_sq_ridge, Variance = variance_ridge, MSE = test_mse_ridge, check.names = FALSE)
plot_data_lasso <- data.frame(Model = "Lasso", r_squared = avg_r_squared_lasso,
                              `Bias^2` = bias_sq_lasso, Variance = variance_lasso, MSE = test_mse_lasso, check.names = FALSE)
plot_data <- rbind(plot_data_ridge, plot_data_lasso)

# Reshape data for ggplot
plot_data_long <- pivot_longer(plot_data, cols = c("Bias^2", "Variance", "MSE"),
                               names_to = "Metric", values_to = "Value")

# --- Find minimum MSE points for annotation ---
min_mse_ridge <- plot_data_ridge[which.min(plot_data_ridge$MSE), ]
min_mse_lasso <- plot_data_lasso[which.min(plot_data_lasso$MSE), ]
min_mse_points <- rbind(min_mse_ridge, min_mse_lasso)

# --- Generate the single, improved plot ---
p1 <- ggplot(plot_data_long, aes(x = r_squared, y = Value, color = Metric, linetype = Model)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = sigma^2, linetype = "dashed", color = "black") +
  # geom_point(data = min_mse_points |> pivot_longer(cols = -c(Model, r_squared),  names_to = "Metric"),
  #            aes(x = r_squared, y = value, color = Metric, shape = Model),
  #            size = 4, stroke = 1.5, show.legend = FALSE) +
  scale_color_aaas() +
  scale_linetype_manual(name = "Model", values = c("Ridge" = "solid", "Lasso" = "dotted")) +
  labs(
    x = expression(paste("Training ", R^2)),
    y = "Mean Squared Error",
    title = "Bias-Variance Tradeoff vs. Model Fit (Ridge does better)",
    color = "Metric"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0))
















# Load necessary libraries
# install.packages(c("glmnet", "ggplot2", "tidyr", "patchwork", "ggsci", "here"))
library(glmnet)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ggsci)
library(here)

# Set a seed for reproducibility
set.seed(1234)

#################################
## 1. SIMULATION PARAMETERS    ##
#################################
p <- 45         # Number of predictors
n <- 50         # Number of training observations
nsims <- 100    # Number of simulation runs
n_test <- 1000  # Number of observations in the test set
sigma <- 5      # Irreducible error standard deviation (sigma^2 = 25)

# Generate true coefficients
beta_true <- rnorm(p, mean = 0, sd = 2.5)
beta_true <- ifelse(abs(beta_true) > 5, beta_true,  0) ## very sparse dataset

# Define lambda grid
lambda_grid <- 10^seq(3, -2, length.out = 100)

#################################
## 2. RUN THE SIMULATION       ##
#################################

# --- Create arrays to store results for BOTH models ---
all_predictions_ridge <- array(0, dim = c(n_test, length(lambda_grid), nsims))
all_predictions_lasso <- array(0, dim = c(n_test, length(lambda_grid), nsims))
# Store training R-squared for each simulation
r_squared_ridge_all_sims <- matrix(0, nrow = nsims, ncol = length(lambda_grid))
r_squared_lasso_all_sims <- matrix(0, nrow = nsims, ncol = length(lambda_grid))


# Generate a single, large test set
X_test <- matrix(rnorm(n_test * p), n_test, p)
f_test <- as.vector(X_test %*% beta_true)

# Main simulation loop
cat("Running simulation for Ridge and Lasso...\n")
for (i in 1:nsims) {
  X_train <- matrix(rnorm(n * p), n, p)
  # ❗ CORRECTED LINE: Convert y_train to a vector
  y_train <- as.vector(X_train %*% beta_true + rnorm(n, 0, sigma))

  # --- Fit both models ---
  fit_ridge <- glmnet(X_train, y_train, alpha = 0, lambda = lambda_grid,
                      standardize = FALSE, intercept = FALSE)
  fit_lasso <- glmnet(X_train, y_train, alpha = 1, lambda = lambda_grid,
                      standardize = FALSE, intercept = FALSE)

  # --- Store predictions on the TEST set ---
  all_predictions_ridge[, , i] <- predict(fit_ridge, newx = X_test)
  all_predictions_lasso[, , i] <- predict(fit_lasso, newx = X_test)

  # --- Calculate R-squared on the TRAINING set (This part now works) ---
  ss_total <- sum((y_train - mean(y_train))^2)

  # R-squared for Ridge
  y_pred_train_ridge <- predict(fit_ridge, newx = X_train)
  ss_resid_ridge <- colSums((y_train - y_pred_train_ridge)^2)
  r_squared_ridge_all_sims[i, ] <- 1 - ss_resid_ridge / ss_total

  # R-squared for Lasso
  y_pred_train_lasso <- predict(fit_lasso, newx = X_train)
  ss_resid_lasso <- colSums((y_train - y_pred_train_lasso)^2)
  r_squared_lasso_all_sims[i, ] <- 1 - ss_resid_lasso / ss_total
}
cat("Simulation complete.\n")

#################################
## 3. CALCULATE METRICS        ##
#################################

# --- Calculations for Ridge ---
mean_pred_ridge <- apply(all_predictions_ridge, c(1, 2), mean)
bias_sq_ridge <- colMeans((mean_pred_ridge - f_test)^2)
variance_ridge <- colMeans(apply(all_predictions_ridge, c(1, 2), var))
test_mse_ridge <- bias_sq_ridge + variance_ridge
avg_r_squared_ridge <- colMeans(r_squared_ridge_all_sims)

# --- Calculations for Lasso ---
mean_pred_lasso <- apply(all_predictions_lasso, c(1, 2), mean)
bias_sq_lasso <- colMeans((mean_pred_lasso - f_test)^2)
variance_lasso <- colMeans(apply(all_predictions_lasso, c(1, 2), var))
test_mse_lasso <- bias_sq_lasso + variance_lasso
avg_r_squared_lasso <- colMeans(r_squared_lasso_all_sims)


#####################################################
## 4. GENERATE THE FIGURE (with ggplot2)           ##
#####################################################

# --- Create and combine data frames for plotting ---
plot_data_ridge <- data.frame(Model = "Ridge", r_squared = avg_r_squared_ridge,
                              `Bias^2` = bias_sq_ridge, Variance = variance_ridge, MSE = test_mse_ridge, check.names = FALSE)
plot_data_lasso <- data.frame(Model = "Lasso", r_squared = avg_r_squared_lasso,
                              `Bias^2` = bias_sq_lasso, Variance = variance_lasso, MSE = test_mse_lasso, check.names = FALSE)
plot_data <- rbind(plot_data_ridge, plot_data_lasso)

# Reshape data for ggplot
plot_data_long <- pivot_longer(plot_data, cols = c("Bias^2", "Variance", "MSE"),
                               names_to = "Metric", values_to = "Value")

# --- Find minimum MSE points for annotation ---
min_mse_ridge <- plot_data_ridge[which.min(plot_data_ridge$MSE), ]
min_mse_lasso <- plot_data_lasso[which.min(plot_data_lasso$MSE), ]
min_mse_points <- rbind(min_mse_ridge, min_mse_lasso)

# --- Generate the single, improved plot ---
p2 <- ggplot(plot_data_long, aes(x = r_squared, y = Value, color = Metric, linetype = Model)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = sigma^2, linetype = "dashed", color = "black") +
  # geom_point(data = min_mse_points |> pivot_longer(cols = -c(Model, r_squared),  names_to = "Metric"),
  #            aes(x = r_squared, y = value, color = Metric, shape = Model),
  #            size = 4, stroke = 1.5, show.legend = FALSE) +
  scale_color_aaas() +
  scale_linetype_manual(name = "Model", values = c("Ridge" = "solid", "Lasso" = "dotted")) +
  labs(
    x = expression(paste("Training ", R^2)),
    y = "Mean Squared Error",
    title = "Bias-Variance Tradeoff vs. Model Fit (Lasso does better)",
    color = "Metric"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0))

fig4 <- p1 + p2

ggsave(file.path(here(), "class3", "figures", "fig4.pdf"),
       fig4,
       device = pdf,
       width = 15,
       height = 6,
       units = "in")
