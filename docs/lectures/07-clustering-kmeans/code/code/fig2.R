rm(list = ls())
pacman::p_unload(pacman::p_loaded())
pacman::p_load(mvtnorm, ggplot2, patchwork, dplyr, purrr, ggsci)

# Generate three distinct clusters
set.seed(123)
cluster1 <- rmvnorm(100, mean = c(0, 0), sigma = matrix(c(1, 0.8, 0.8, 1), 2))
cluster2 <- rmvnorm(150, mean = c(5, 5), sigma = matrix(c(2, -1.5, -1.5, 2), 2))
cluster3 <- rmvnorm(50, mean = c(0, 6), sigma = matrix(c(0.5, 0, 0, 0.5), 2))

# Combine into a single data frame
data <- as.data.frame(rbind(cluster1, cluster2, cluster3)) |>
  add_column(true = c(rep(c("A", "B", "C"), times = c(100, 150, 50))))
names(data) <- c("Feature 1", "Feature 2", "True")

# Plot the raw data
unclustered <- ggplot(data, aes(x = `Feature 1`, y = `Feature 2`)) +
  geom_point(aes(color= `True`)) +
  theme_bw() +
  scale_color_nejm() +
  labs(title = "Before GMM clustering", color = "True cluster") +
  theme(legend.position = "bottom")


# Fit the GMM using mclust
library(mclust)
gmm_model <- Mclust(data[,1:2])


# Create a grid of points to evaluate the density on
grid <- expand.grid(
  `Feature 1` = seq(min(data$`Feature 1`), max(data$`Feature 1`), length.out = 100),
  `Feature 2` = seq(min(data$`Feature 2`), max(data$`Feature 2`), length.out = 100)
)


contour_data <- map_df(1:gmm_model$G, ~{
  # Extract mean and covariance for the k-th cluster
  mean_k <- gmm_model$parameters$mean[, .x]
  sigma_k <- gmm_model$parameters$variance$sigma[, , .x]
  # Calculate the density at each grid point for this cluster
  density <- dmvnorm(grid, mean = mean_k, sigma = sigma_k)
  # Return a data frame with grid points, density, and cluster class
  tibble(grid, density = density, class = as.factor(.x))
})

# Plot the clustering results
clustered <- data %>%
  add_column(class = as.factor(gmm_model$classification)) %>%
  ggplot(aes(x = `Feature 1`, y = `Feature 2`, color = class)) +
  geom_point(alpha = 0.6) +
  geom_contour(data = contour_data, aes(z = density), linewidth = 0.6) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_aaas() +
  labs(title = "After GMM clustering", color = "GMM Cluster")


# Display plots side-by-side
unclustered + clustered
