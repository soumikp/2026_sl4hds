rm(list = ls())
pacman::p_unload(pacman::p_loaded())

# Load necessary libraries
pacman::p_load(ggplot2, e1071, patchwork, here) # For combining plots

# Set seed for reproducibility
set.seed(42)

# 1. Generate data that IS linearly separable with a clear margin
n <- 80
class1 <- data.frame(
  x1 = rnorm(n/2, mean = 1, sd = 0.5),
  x2 = rnorm(n/2, mean = 3, sd = 0.5),
  y = -1
)
class2 <- data.frame(
  x1 = rnorm(n/2, mean = 3, sd = 0.5),
  x2 = rnorm(n/2, mean = 1, sd = 0.5),
  y = 1
)
data_mmc <- rbind(class1, class2)
data_mmc$y <- as.factor(data_mmc$y)

# 2. Fit a Maximal Margin Classifier
# This is an SVM with a linear kernel and a very high cost to forbid margin violations
mmc_model <- svm(y ~ ., data = data_mmc, type = 'C-classification', kernel = 'linear', cost = 1000)

# 3. Extract model parameters for plotting
w <- t(mmc_model$coefs) %*% mmc_model$SV
b <- -mmc_model$rho
support_vectors_mmc <- data_mmc[mmc_model$index, ]

# 4. Create the MMC plot
fig2a <- ggplot(data_mmc, aes(x = x1, y = x2, color = y)) +
  geom_point(size = 2.5) +
  # Highlight support vectors (points on the margin)
  #geom_point(data = support_vectors_mmc, aes(x = x1, y = x2), color = 'purple', size = 5, shape = 1) +
  # Add the separating hyperplane
  geom_abline(intercept = -b, slope = -w[1, 1], color = "red", linetype = "solid", size = 1) +
  geom_abline(intercept = -b  + 0.5, slope = -w[1, 1]*1.5, color = "red", linetype = "dashed", size = 1) +
  geom_abline(intercept = -b  + 0.5, slope = -w[1, 1]*0.25, color = "red", linetype = "dotted", size = 1) +
  # Add the margin lines
  #geom_abline(intercept = (1 - b), slope = -w[1, 1], color = "blue", linetype = "dashed") +
  #geom_abline(intercept = (-1 - b), slope = -w[1, 1] , color = "blue", linetype = "dashed") +
  scale_color_manual(values = c("-1" = "black", "1" = "black")) +
  coord_cartesian(xlim = range(data_mmc$x1), ylim = range(data_mmc$x2)) +
  labs(
    title = "",
    x = "Feature 1",
    y = "Feature 2"
  ) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


fig2b <- ggplot(data_mmc, aes(x = x1, y = x2, color = y)) +
  geom_point(size = 2.5) +
  # Highlight support vectors (points on the margin)
  #geom_point(data = support_vectors_mmc, aes(x = x1, y = x2), color = 'purple', size = 5, shape = 1) +
  # Add the separating hyperplane
  geom_abline(intercept = -b, slope = -w[1, 1], color = "red", linetype = "solid", size = 1) +
  # Add the margin lines
  geom_abline(intercept = (0.13 - b), slope = -w[1, 1], color = "blue", linetype = "dashed") +
  geom_abline(intercept = (-0.13 - b), slope = -w[1, 1] , color = "blue", linetype = "dashed") +
  scale_color_manual(values = c("-1" = "orange", "1" = "darkgreen")) +
  coord_cartesian(xlim = range(data_mmc$x1), ylim = range(data_mmc$x2)) +
  labs(
    title = "",
    x = "Feature 1",
    y = "Feature 2"
  ) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


ggsave(
  file.path(here(), "class6", "figures", "fig2.pdf"),
  plot = (fig2a/fig2b),
  width = 6,
  height = 10
)
