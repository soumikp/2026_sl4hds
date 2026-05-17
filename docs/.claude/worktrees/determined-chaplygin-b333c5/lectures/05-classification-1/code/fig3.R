rm(list = ls())

pacman::p_load(tidyverse, MASS, mvtnorm, patchwork, ggpubsci)

# Class means (mu vectors)
mu <- list(
  class1 = c(-2, -1),
  class2 = c(0, 2),
  class3 = c(3.0, 1)
)

# Common covariance matrix (Sigma)
sigma <- matrix(c(2, 1.5, 1.5, 2), 2, 2)

# Class priors (assuming equal-sized classes)
priors <- c(class1 = 1/3, class2 = 1/3, class3 = 1/3)



# Set a seed for reproducibility
set.seed(42)

# Generate 50 samples from each class
n_samples <- 100
sample_data <- bind_rows(
  MASS::mvrnorm(n = n_samples, mu = mu$class1, Sigma = sigma) %>% as_tibble() %>% mutate(class = "Class 1"),
  MASS::mvrnorm(n = n_samples, mu = mu$class2, Sigma = sigma) %>% as_tibble() %>% mutate(class = "Class 2"),
  MASS::mvrnorm(n = n_samples, mu = mu$class3, Sigma = sigma) %>% as_tibble() %>% mutate(class = "Class 3")
) %>%
  rename(X1 = V1, X2 = V2) %>%
  mutate(class = as.factor(class))


# Create a grid of points covering the plot area
grid_range <- range(sample_data$X1, sample_data$X2)
grid <- expand.grid(
  X1 = seq(grid_range[1] - 1, grid_range[2] + 1, length.out = 200),
  X2 = seq(grid_range[1] - 1, grid_range[2] + 1, length.out = 200)
)


# Calculate the posterior probability for each class at each grid point
# (proportional to prior * density)
grid_densities <- grid %>%
  mutate(
    dens_1 = dmvnorm(., mean = mu$class1, sigma = sigma) * priors["class1"],
    dens_2 = dmvnorm(., mean = mu$class2, sigma = sigma) * priors["class2"],
    dens_3 = dmvnorm(., mean = mu$class3, sigma = sigma) * priors["class3"]
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

# Generate data for the 95% probability ellipses
ellipse_data <- map_df(1:3, function(i) {
  # ellipse function needs covariance matrix and center
  # qchisq(0.95, df=2) gives the radius for a 95% confidence ellipse
  ellipse_points <- car::ellipse(center = mu[[i]], shape = sigma, radius = sqrt(qchisq(0.95, df = 2)), draw=FALSE)
  as.data.frame(ellipse_points) %>%
    mutate(class = paste("Class", i))
}) %>% as_tibble()


plot_left <- ggplot() +
  # Background colors representing the Bayes classifier regions
  geom_raster(data = grid_bayes, aes(x = X1, y = X2, fill = bayes_class), alpha = 0.3, show.legend = FALSE) +
  # Decision boundaries (contours where class changes)
  geom_contour(data = grid_bayes, aes(x = X1, y = X2, z = as.numeric(bayes_class)),
               breaks = c(1.5, 2.5), color = "black", linetype = "dashed") +
  # 95% probability ellipses
  geom_path(data = ellipse_data, aes(x = x, y = y, color = class), size = 1, show.legend=FALSE) +
  scale_fill_manual(values = c("Class 1" = "#2ca02c", "Class 2" = "#ff7f0e", "Class 3" = "#1f77b4")) +
  scale_color_manual(values = c("Class 1" = "#2ca02c", "Class 2" = "#ff7f0e", "Class 3" = "#1f77b4")) +
  labs(
    title = "Theoretical Bayes Classifier",
    subtitle = "Bayes decision boundaries (dashed) and 95% prob. ellipses",
    x = expression(X[1]),
    y = expression(X[2])
  ) +
  coord_fixed(ratio = 1) +
  theme_big_simple()


# Train the LDA model using the generated sample data
lda_model <- lda(class ~ X1 + X2, data = sample_data)

# Predict the class for each point on the grid
grid_lda_preds <- predict(lda_model, newdata = grid)$class

# Add LDA predictions to the grid data
grid_lda <- grid %>%
  mutate(lda_class = grid_lda_preds)

plot_right <- ggplot() +
  # Background colors representing the LDA classifier regions
  geom_raster(data = grid_lda, aes(x = X1, y = X2, fill = lda_class), alpha = 0.3, show.legend = FALSE) +
  # Decision boundaries from the LDA model
  geom_contour(data = grid_lda, aes(x = X1, y = X2, z = as.numeric(lda_class)),
               breaks = c(1.5, 2.5), color = "black") +
  # Scatter plot of the actual sample data
  geom_point(data = sample_data, aes(x = X1, y = X2, color = class, shape = class), size = 2) +
  scale_fill_manual(values = c("Class 1" = "#2ca02c", "Class 2" = "#ff7f0e", "Class 3" = "#1f77b4")) +
  scale_color_manual(values = c("Class 1" = "#2ca02c", "Class 2" = "#ff7f0e", "Class 3" = "#1f77b4"), name = "True Class") +
  scale_shape_manual(values = c(16, 17, 15), name = "True Class") +
  labs(
    title = "Linear Discriminant Analysis",
    subtitle = "Decision boundaries from an LDA model trained on sample data",
    x = expression(X[1]),
    y = expression(X[2])
  ) +
  coord_fixed(ratio = 1) +
  theme_big_simple()


# combine and display

fig3 <- plot_left + plot_right

ggsave(file.path(here(), "class5", "figures", "fig3.pdf"),
       fig3,
       device = pdf,
       width = 18,
       height = 10,
       units = "in")
