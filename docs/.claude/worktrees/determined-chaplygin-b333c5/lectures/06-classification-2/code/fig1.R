rm(list = ls())
pacman::p_unload(pacman::p_loaded())

# Load necessary libraries
pacman::p_load(ggplot2, MASS, here) # For generating multivariate normal data

# Set a seed for reproducibility
set.seed(42)

# Define parameters for two classes: Benign and Malignant
# Class 1: Benign
mu_benign <- c(2, 3)
cov_benign <- matrix(c(1, 0.5, 0.5, 1), 2)
n_benign <- 500

# Class 2: Malignant
mu_malignant <- c(4.5, 5.5)
cov_malignant <- matrix(c(1.5, -0.7, -0.7, 1.5), 2)
n_malignant <- 50

# Generate data points
benign_data <- mvrnorm(n = n_benign, mu = mu_benign, Sigma = cov_benign)
malignant_data <- mvrnorm(
  n = n_malignant,
  mu = mu_malignant,
  Sigma = cov_malignant
)

# Combine into a single data frame
tumor_data <- as.data.frame(rbind(benign_data, malignant_data))
colnames(tumor_data) <- c("Cell_Size_Uniformity", "Cell_Shape_Uniformity")

# Add class labels
tumor_data$Class <- factor(c(
  rep("Benign", n_benign),
  rep("Malignant", n_malignant)
))


fig1 <- ggplot(
  tumor_data,
  aes(x = Cell_Size_Uniformity, y = Cell_Shape_Uniformity, color = Class)
) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("Benign" = "#0072B2", "Malignant" = "#D55E00")
  ) +
  labs(
    title = "Tumor Classification based on Biomarkers",
    subtitle = "How would you draw a line to separate the classes?",
    x = "Cell Size Uniformity",
    y = "Cell Shape Uniformity",
    color = "Tumor Class"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  file.path(here(), "class6", "figures", "fig1.pdf"),
  plot = fig1,
  width = 6,
  height = 6
)
