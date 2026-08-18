rm(list = ls())
pacman::p_load(ggplot2, MASS, tidyverse, ggsci)

set.seed(42) # reproducibility
n <- 100     # number of points

# define the center (mean) of the data cloud: Population ~40, Ad Spending ~20
mu <- c(40, 20)

# uneven variances: make Population (X) have a larger variance than Ad Spending (Y)
sd_pop <- 12
sd_ad <- 6
var_pop <- sd_pop^2  # 144
var_ad <- sd_ad^2   # 36

# correlation: positive correlation (e.g., 0.7)
rho <- 0.7
cov_pa <- rho * sd_pop * sd_ad # 0.7 * 12 * 6 = 50.4

# covariance matrix
sigma <- matrix(c(var_pop, cov_pa, cov_pa, var_ad), nrow = 2)

# bivariate normal data simulation
data <- MASS::mvrnorm(n, mu = mu, Sigma = sigma)
df <- as.data.frame(data)
colnames(df) <- c("Population", "Ad_Spending")

# --- 2. Perform PCA ---
# Run PCA on the data.
# We set center = TRUE (the default) to center the data at (0,0) first.
# We set scale. = FALSE because we *want* the analysis to be sensitive
# to the raw, uneven variances.
pca_result <- prcomp(df, center = TRUE, scale. = FALSE)

# Get the center (mean) of the original data
data_mean <- pca_result$center

# Get the eigenvectors (the directions)
eigenvectors <- pca_result$rotation

# Get the standard deviations of the principal components (sqrt of eigenvalues)
# This tells us the "magnitude" of variance in each direction.
sdevs <- pca_result$sdev

# Create a data frame for the PC lines (segments)
# We'll draw them centered at the data mean
# We scale the vector length by its sdev to show magnitude
vec_scale_factor <- 2.5 # An aesthetic choice to make lines visibly long

pc_vectors_df <- data.frame(
  PC = c("PC1", "PC2"),
  x_center = data_mean[1],
  y_center = data_mean[2],
  x_vec = eigenvectors[1, ], # x-component of eigenvector
  y_vec = eigenvectors[2, ], # y-component of eigenvector
  sdev = sdevs
)

pc1_scores <- pca_result$x[, 1]
eigenvec_pc1 <- eigenvectors[, 1]

df_proj_pc1 <- data.frame(
  Population = data_mean[1] + pc1_scores * eigenvec_pc1[1],
  Ad_Spending = data_mean[2] + pc1_scores * eigenvec_pc1[2]
)

pc2_scores <- pca_result$x[, 2]
eigenvec_pc2 <- eigenvectors[, 2]

df_proj_pc2 <- data.frame(
  Population = data_mean[1] + pc2_scores * eigenvec_pc2[1],
  Ad_Spending = data_mean[2] + pc2_scores * eigenvec_pc2[2]
)

ggplot(df, aes(x = Population, y = Ad_Spending)) +
  geom_point(color = pal_nejm("default")(3)[3], size = 2, alpha = 0.8) +
  geom_point(data = df_proj_pc1, aes(x = Population, y = Ad_Spending),
             alpha = 0.8, shape = 2, color = pal_nejm("default")(3)[1]) +
  geom_point(data = df_proj_pc2, aes(x = Population, y = Ad_Spending),
             alpha = 0.8, shape = 3, color = pal_nejm("default")(3)[2]) +
  geom_segment(data = pc_vectors_df,
               aes(x = x_center - x_vec * sdev * vec_scale_factor,
                   y = y_center - y_vec * sdev * vec_scale_factor,
                   xend = x_center + x_vec * sdev * vec_scale_factor,
                   yend = y_center + y_vec * sdev * vec_scale_factor,
                   color = PC,
                   linetype = PC),
               linewidth = 1, alpha = 0.75) +
  scale_color_nejm() +
  scale_linetype_manual(values = c("PC1" = "solid", "PC2" = "dashed")) +
  labs(x = "Variable 1", y = "Variable 2") +
  coord_cartesian(xlim = c(10, 70), ylim = c(0, 35), ratio = 1) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )
