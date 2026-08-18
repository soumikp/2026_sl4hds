rm(list = ls())
source(file.path(here::here(), "class3", "code", "helper.R"))

pacman::p_load(glmnet, ggplot2, tidyr, patchwork, ggsci, here)

# Set a seed for reproducibility
set.seed(314)

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

# Define lambda grid
lambda_grid <- 10^seq(3, -2, length.out = 100)

#################################
## 2. RUN THE SIMULATION       ##
#################################

# Create arrays to store results
all_predictions <- array(0, dim = c(n_test, length(lambda_grid), nsims))
l2_norm_ratios_all_sims <- matrix(0, nrow = nsims, ncol = length(lambda_grid))

# Generate a single, large test set
X_test <- matrix(rnorm(n_test * p), n_test, p)

# Convert f_test from a matrix to a vector
f_test <- as.vector(X_test %*% beta_true)

# Main simulation loop
cat("Running simulation...\n")
for (i in 1:nsims) {
  X_train <- matrix(rnorm(n * p), n, p)
  y_train <- X_train %*% beta_true + rnorm(n, 0, sigma)

  fit_ridge <- glmnet(X_train, y_train, alpha = 0, lambda = lambda_grid,
                      standardize = FALSE, intercept = FALSE)

  all_predictions[, , i] <- predict(fit_ridge, newx = X_test)

  fit_ols <- glmnet(X_train, y_train, alpha = 0, lambda = 1e-5, standardize=FALSE, intercept=FALSE)
  beta_ols <- coef(fit_ols)[-1]
  norm_ols <- sqrt(sum(beta_ols^2))

  beta_ridge <- coef(fit_ridge)[-1, ]
  norm_ridge <- sqrt(colSums(beta_ridge^2))

  l2_norm_ratios_all_sims[i, ] <- norm_ridge / norm_ols
}
cat("Simulation complete.\n")

#################################
## 3. CALCULATE BIAS & VARIANCE ##
#################################

# Average prediction across simulations
mean_predictions <- apply(all_predictions, c(1, 2), mean)

# Calculate squared bias, variance, and MSE (This line now works!)
bias_sq <- colMeans((mean_predictions - f_test)^2)
variance <- colMeans(apply(all_predictions, c(1, 2), var))
test_mse <- bias_sq + variance

# Average L2 norm ratios across simulations
l2_norm_ratios <- colMeans(l2_norm_ratios_all_sims)


#####################################################
## 4. GENERATE THE FIGURES (with ggplot2)          ##
#####################################################

# Create a data frame from the simulation results
plot_data <- data.frame(
  lambda = lambda_grid,
  norm_ratio = l2_norm_ratios,
  `Bias^2` = bias_sq,
  Variance = variance,
  MSE = test_mse,
  check.names = FALSE # Allows the "^" in "Bias^2"
)

# Use tidyr::pivot_longer to reshape the data for easy plotting with ggplot
plot_data_long <- pivot_longer(
  plot_data,
  cols = c("Bias^2", "Variance", "MSE"),
  names_to = "Metric",
  values_to = "Value"
)

# Find the minimum MSE point for annotation
min_mse_point <- data.frame(
  lambda = lambda_grid[which.min(test_mse)],
  norm_ratio = l2_norm_ratios[which.min(test_mse)],
  Value = min(test_mse)
)

# --- Plot 1: MSE vs. Lambda ---
p1 <- ggplot(plot_data_long, aes(x = lambda, y = Value, color = Metric)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = sigma^2, linetype = "dashed", color = "black") +
  geom_point(data = min_mse_point, aes(x = lambda, y = Value),
             color =  pal_aaas("default")(2)[2], shape = 4, size = 4, stroke = 1.5) +
  scale_x_log10() +
  scale_color_aaas() +
  labs(
    x = expression(lambda),
    y = "Mean Squared Error"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

# --- Plot 2: MSE vs. L2 Norm Ratio ---
p2 <- ggplot(plot_data_long, aes(x = norm_ratio, y = Value, color = Metric)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = sigma^2, linetype = "dashed", color = "black") +
  geom_point(data = min_mse_point, aes(x = norm_ratio, y = Value),
             color = pal_aaas("default")(2)[2], shape = 4, size = 4, stroke = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_color_aaas() +
  labs(
    x = expression(paste("||", hat(beta)[lambda]^R, "||"[2], " / ", "||", hat(beta), "||"[2])),
    y = "Mean Squared Error"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

# Arrange the two plots side by side using patchwork
fig3 <- (p1 + p2 & theme(legend.position = "bottom")) + plot_layout(guides = "collect")


ggsave(file.path(here(), "class3", "figures", "fig3.pdf"),
       fig3,
       device = pdf,
       width = 12,
       height = 6,
       units = "in")
