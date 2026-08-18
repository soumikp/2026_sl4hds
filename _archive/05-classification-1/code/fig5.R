rm(list = ls())
pacman::p_unload(pacman::p_loaded())
pacman::p_load(tidyverse, MASS, class, patchwork, mvtnorm, here)

# Set a seed for reproducibility
set.seed(321)

# --- 2. Generate Synthetic Data ---
# The data is a mixture of Gaussian distributions to create a non-linear boundary.
# We'll create 100 observations for each of the two classes.

# Parameters for the "Orange" class (mixture of 2 normals)
mu_orange1 <- c(-1.5, 1)
mu_orange2 <- c(1.5, 2.5)
sigma_shared <- matrix(c(1.5, 0.5, 0.5, 1.5), 2, 2)
orange_data <- rbind(
  mvrnorm(n = 50, mu = mu_orange1, Sigma = sigma_shared),
  mvrnorm(n = 50, mu = mu_orange2, Sigma = sigma_shared)
)

# Parameters for the "Blue" class (mixture of 2 normals)
mu_blue1 <- c(2, 0)
mu_blue2 <- c(0, -2)
blue_data <- rbind(
  mvrnorm(n = 50, mu = mu_blue1, Sigma = sigma_shared),
  mvrnorm(n = 50, mu = mu_blue2, Sigma = sigma_shared)
)

# Combine into a single tibble
data <- tibble(
  x1 = c(orange_data[, 1], blue_data[, 1]),
  x2 = c(orange_data[, 2], blue_data[, 2]),
  class = factor(rep(c("Orange", "Blue"), each = 100))
)

# --- 3. Create a Grid for Decision Boundaries ---
# This grid will be used to calculate and visualize the decision regions.
grid_range <- data %>%
  summarise(across(where(is.numeric), list(min = min, max = max)))
grid <- expand.grid(
  x1 = seq(grid_range$x1_min, grid_range$x1_max, length.out = 1000),
  x2 = seq(grid_range$x2_min, grid_range$x2_max, length.out = 1000)
)

# --- 4. Calculate the True Bayes Decision Boundary ---
# The Bayes boundary occurs where the probability of being in either class is equal.
# Since we generated the data, we know the true probability densities.
prob_orange <- 0.5 *
  dmvnorm(grid, mu_orange1, sigma_shared) +
  0.5 * dmvnorm(grid, mu_orange2, sigma_shared)
prob_blue <- 0.5 *
  dmvnorm(grid, mu_blue1, sigma_shared) +
  0.5 * dmvnorm(grid, mu_blue2, sigma_shared)

grid$true_class <- factor(ifelse(prob_orange > prob_blue, "Orange", "Blue"))
grid$boundary_line <- prob_orange - prob_blue # The contour where this is zero is the boundary

# --- 5. Plot 1: The Simulated Data and Bayes Boundary (Fig 2.13) ---
plot1_bayes <- ggplot() +
  # Background grid representing the true class regions
  geom_point(
    data = grid,
    aes(x = x1, y = x2, color = true_class),
    size = 0.5,
    alpha = 0.4,
    shape = "."
  ) +
  # The Bayes decision boundary (purple dashed line)
  geom_contour(
    data = grid,
    aes(x = x1, y = x2, z = boundary_line),
    breaks = 0,
    color = "purple",
    linetype = "dashed",
    linewidth = 1
  ) +
  # The simulated data points (hollow circles)
  geom_point(
    data = data,
    aes(x = x1, y = x2, color = class),
    shape = 1,
    size = 2.5,
    stroke = 0.8
  ) +
  scale_color_manual(values = c("Orange" = "darkred", "Blue" = "forestgreen")) +
  labs(
    title = "Simulated Data\nBayes Decision Boundary",
    x = expression(X[1]),
    y = expression(X[2])
  ) +
  theme_bw(base_size = 14) +
  coord_fixed() + # Ensure aspect ratio is 1
  guides(color = "none") # Hide the legend

print(plot1_bayes)

# --- 6. Run KNN and Prepare for Comparison Plots ---
# We need the data points (train) and the grid points (test)
train_x <- data %>% dplyr::select(x1, x2)
train_y <- data$class

# Predict on the grid for K=1 and K=100
grid$knn_pred_1 <- knn(
  train = train_x,
  test = grid[, c("x1", "x2")],
  cl = train_y,
  k = 1
)
grid$knn_pred_100 <- knn(
  train = train_x,
  test = grid[, c("x1", "x2")],
  cl = train_y,
  k = 50
)

# --- 7. Plot 2: KNN Comparison Plots (Fig 2.16) ---
# Base plot elements to be reused
base_plot <- ggplot() +
  geom_point(
    data = data,
    aes(x = x1, y = x2, color = class),
    shape = 1,
    size = 2.5,
    stroke = 0.8
  ) +
  geom_contour(
    data = grid,
    aes(x = x1, y = x2, z = boundary_line),
    breaks = 0,
    color = "purple",
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_color_manual(values = c("Orange" = "darkred", "Blue" = "forestgreen")) +
  labs(x = expression(X[1]), y = expression(X[2])) +
  theme_bw(base_size = 14) +
  coord_fixed() +
  guides(color = "none")

# Plot for K=1 (overly flexible)
plot_k1 <- base_plot +
  geom_contour(
    data = grid,
    aes(x = x1, y = x2, z = as.numeric(knn_pred_1)),
    breaks = 1.5,
    color = "black"
  ) +
  labs(title = "KNN: K=1")

# Plot for K=100 (not flexible enough)
plot_k100 <- base_plot +
  geom_contour(
    data = grid,
    aes(x = x1, y = x2, z = as.numeric(knn_pred_100)),
    breaks = 1.5,
    color = "black"
  ) +
  labs(title = "KNN: K=50")

# Combine the two KNN plots using patchwork
fig5 <- plot_k1 / plot_k100

ggsave(
  file.path(here(), "class5", "figures", "fig5.pdf"),
  fig5,
  height = 10,
  width = 6,
  units = "in",
  device = pdf
)
