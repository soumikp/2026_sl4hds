# --- SETUP ---
# Install packages if you don't have them already
# install.packages(c("ggplot2", "e1071"))

# Load necessary libraries
library(ggplot2)
library(e1071)

# Set seed for reproducibility
set.seed(32)


# --- 1. GENERATE TOY DATASET ---
# Create overlapping data to ensure margin violations are necessary
n <- 50
class1 <- data.frame(
  x1 = rnorm(n/2, mean = 1.5, sd = 0.8),
  x2 = rnorm(n/2, mean = 3, sd = 1.2),
  y = -1
)
class2 <- data.frame(
  x1 = rnorm(n/2, mean = 3, sd = 0.8),
  x2 = rnorm(n/2, mean = 1.5, sd = 0.8),
  y = 1
)
data <- rbind(class1, class2)
data$y <- as.factor(data$y)


# --- 2. FIT THE SUPPORT VECTOR CLASSIFIER ---
# Use a moderate cost 'C' to allow for slack
svc_model <- svm(y ~ ., data = data, type = 'C-classification', kernel = 'linear', cost = 1, scale = FALSE)


# --- 3. EXTRACT MODEL & PLOTTING PARAMETERS ---
# Extract weights (beta) and intercept (beta_0)
w <- t(svc_model$coefs) %*% svc_model$SV
b <- -svc_model$rho

# Calculate slope and intercept for ggplot's abline geometry
slope <- -w[1, 1] / w[1, 2]
intercept <- -b / w[1, 2]
intercept_margin_plus <- (1 - b) / w[1, 2]
intercept_margin_minus <- (-1 - b) / w[1, 2]


# --- 4. CALCULATE SLACK (ξ) FOR EACH POINT ---
# The decision value is f(x) = beta^T * x + beta_0
decision_values <- as.vector((as.matrix(data[, c("x1", "x2")]) %*% t(w)) + b)
# The slack is xi = max(0, 1 - y_i * f(x_i))
slack_values <- pmax(0, 1 - as.numeric(as.character(data$y)) * decision_values)
data$slack <- slack_values

# Identify points to annotate
point_inside_margin <- data[data$slack > 0 & data$slack < 1, ][1, ]
point_misclassified <- data[data$slack > 1, ][1, ]

# --- 5. PRE-CALCULATE ANNOTATION COORDINATES ---
# Calculate the squared L2 norm of the weight vector w
w_norm_sq <- w[1,1]^2 + w[1,2]^2

# Calculate coordinates for the perpendicular margin line (2M)
x_mid <- 2.5
y_mid <- slope * x_mid + intercept
x_start <- x_mid + w[1,1] / w_norm_sq
y_start <- y_mid + w[1,2] / w_norm_sq
x_end <- x_mid - w[1,1] / w_norm_sq
y_end <- y_mid - w[1,2] / w_norm_sq

# Create dedicated data frames for annotations
margin_line_df <- data.frame(x = x_start, y = y_start, xend = x_end, yend = y_end)
margin_label_df <- data.frame(x = x_mid - 0.5, y = y_mid + 0.1, label = "2*M")


# --- 6. CREATE THE VISUALIZATION ---
svm_plot <- ggplot(data, aes(x = x1, y = x2, color = y)) +
  geom_point(size = 3, alpha = 0.75) +
  scale_color_manual(values = c("-1" = "orange", "1" = "#006400")) +

  # Plot Hyperplane and Margins
  geom_abline(intercept = intercept, slope = slope, color = "red", linetype = "solid", size = 1) +
  geom_abline(intercept = intercept_margin_plus, slope = slope, color = "blue", linetype = "dashed",  size = 1)+
  geom_abline(intercept = intercept_margin_minus, slope = slope, color = "blue", linetype = "dashed",  size = 1) +

  geom_abline(intercept = intercept_margin_plus + 1, slope = slope, color = "forestgreen", linetype = "dotted", size = 1) +
  geom_abline(intercept = intercept_margin_minus - 1, slope = slope, color = "forestgreen", linetype = "dotted", size = 1) +

  # Final plot styling
  labs(
    title = "Visualizing the SVM Optimization Problem",
    x = "Feature 1",
    y = "Feature 2"
  ) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  coord_equal()

# Print the final plot
print(svm_plot)

