rm(list = ls())

pacman::p_load(tidyverse, MASS, mvtnorm, patchwork, car)       # car: for the ellipse() function


# Class means (mu vectors) - can be the same as before
mu <- list(
  class1 = c(-2, -1),
  class2 = c(0, 3),
  class3 = c(2.5, -0.5)
)

# Class-specific covariance matrices (Sigma)
sigma <- list(
  class1 = matrix(c(2, -1.5, -1.5, 2), 2, 2), # Negative correlation
  class2 = matrix(c(1.5, 0.5, 0.5, 0.5), 2, 2),  # Positive correlation, smaller variance
  class3 = matrix(c(2.5, 0, 0, 2.5), 2, 2)    # No correlation, larger variance
)

# Class priors (assuming equal-sized classes)
priors <- c(class1 = 1/3, class2 = 1/3, class3 = 1/3)


# --- 2. Generate Sample Data (for the Right Panel) ---

# Set a seed for reproducibility
set.seed(42)

# Generate 250 samples from each class, using their specific covariance matrix
n_samples <- 250
sample_data <- bind_rows(
  MASS::mvrnorm(n = n_samples, mu = mu$class1, Sigma = sigma$class1) %>% as_tibble() %>% mutate(class = "Class 1"),
  MASS::mvrnorm(n = n_samples, mu = mu$class2, Sigma = sigma$class2) %>% as_tibble() %>% mutate(class = "Class 2"),
  MASS::mvrnorm(n = n_samples, mu = mu$class3, Sigma = sigma$class3) %>% as_tibble() %>% mutate(class = "Class 3")
) %>%
  rename(X1 = V1, X2 = V2) %>%
  mutate(class = as.factor(class))


# Create a grid of points covering the plot area
grid_range <- range(sample_data$X1, sample_data$X2)
grid <- expand.grid(
  X1 = seq(grid_range[1] - 2, grid_range[2] + 2, length.out = 200),
  X2 = seq(grid_range[1] - 2, grid_range[2] + 2, length.out = 200)
)



# Calculate posterior probabilities using class-specific covariance matrices
grid_densities <- grid %>%
  mutate(
    dens_1 = dmvnorm(., mean = mu$class1, sigma = sigma$class1) * priors["class1"],
    dens_2 = dmvnorm(., mean = mu$class2, sigma = sigma$class2) * priors["class2"],
    dens_3 = dmvnorm(., mean = mu$class3, sigma = sigma$class3) * priors["class3"]
  )

# Determine the class with the highest probability at each grid point
grid_bayes <- grid_densities %>%
  mutate(
    max_dens = pmax(dens_1, dens_2, dens_3),
    bayes_class = factor(case_when(
      max_dens == dens_1 ~ "Class 1",
      max_dens == dens_2 ~ "Class 2",
      TRUE ~ "Class 3"
    ), levels = c("Class 1", "Class 2", "Class 3"))
  )

# Generate data for the 95% probability ellipses using class-specific sigmas
ellipse_data <- map_df(1:3, function(i) {
  ellipse_points <- car::ellipse(center = mu[[i]], shape = sigma[[i]], radius = sqrt(qchisq(0.95, df = 2)), draw=FALSE)
  as.data.frame(ellipse_points) %>%
    mutate(class = paste("Class", i))
}) %>% as_tibble()


# Create the plot for the left panel
plot_left <- ggplot() +
  geom_raster(data = grid_bayes, aes(x = X1, y = X2, fill = bayes_class), alpha = 0.3, show.legend = FALSE) +
  geom_contour(data = grid_bayes, aes(x = X1, y = X2, z = as.numeric(bayes_class)),
               breaks = c(1.5, 2.5), color = "black", linetype = "dashed") +
  geom_path(data = ellipse_data, aes(x = x, y = y, color = class), size = 1, show.legend=FALSE) +
  scale_fill_manual(values = c("Class 1" = "#2ca02c", "Class 2" = "#ff7f0e", "Class 3" = "#1f77b4")) +
  scale_color_manual(values = c("Class 1" = "#2ca02c", "Class 2" = "#ff7f0e", "Class 3" = "#1f77b4")) +
  labs(
    title = "Theoretical Bayes Classifier (QDA)",
    subtitle = "Bayes decision boundaries are now quadratic",
    x = expression(X[1]),
    y = expression(X[2])
  ) +
  coord_fixed(ratio = 1) +
  theme_big_simple()



# Train the QDA model using the generated sample data
qda_model <- qda(class ~ X1 + X2, data = sample_data)

# Predict the class for each point on the grid
grid_qda_preds <- predict(qda_model, newdata = grid)$class

# Add QDA predictions to the grid data
grid_qda <- grid %>%
  mutate(qda_class = grid_qda_preds)

# Create the plot for the right panel
plot_right <- ggplot() +
  geom_raster(data = grid_qda, aes(x = X1, y = X2, fill = qda_class), alpha = 0.3, show.legend = FALSE) +
  geom_contour(data = grid_qda, aes(x = X1, y = X2, z = as.numeric(qda_class)),
               breaks = c(1.5, 2.5), color = "black") +
  geom_point(data = sample_data, aes(x = X1, y = X2, color = class, shape = class), size = 2) +
  scale_fill_manual(values = c("Class 1" = "#2ca02c", "Class 2" = "#ff7f0e", "Class 3" = "#1f77b4")) +
  scale_color_manual(values = c("Class 1" = "#2ca02c", "Class 2" = "#ff7f0e", "Class 3" = "#1f77b4"), name = "True Class") +
  scale_shape_manual(values = c(16, 17, 15), name = "True Class") +
  labs(
    title = "Quadratic Discriminant Analysis",
    subtitle = "Decision boundaries from a QDA model trained on sample data",
    x = expression(X[1]),
    y = expression(X[2])
  ) +
  coord_fixed(ratio = 1) +
  theme_big_simple()

fig4 <- plot_left + plot_right

ggsave(file.path(here(), "class5", "figures", "fig4.pdf"),
       fig4,
       device = pdf,
       width = 18,
       height = 10,
       units = "in")
