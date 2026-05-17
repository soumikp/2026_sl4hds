# Install necessary packages if you don't have them
# install.packages("randomForest")
# install.packages("ggplot2")

pacman::p_load(randomForest, ggplot2)

# --- 1. Simulate Data ---
set.seed(42) # for reproducibility
n <- 200 # number of observations

# Create predictors
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
x4 <- rnorm(n) # A less important variable
x5 <- rnorm(n) # A noise variable

# We'll make x1 and x2 the most important, x3 moderately important,
# x4 less important, and x5 just noise.
y <- 10 * x1 + 5 * x2 + 2 * x3 + 0.5 * x4 + x5

# Combine into a data frame
sim_data <- data.frame(y, x1, x2, x3, x4)

# --- 2. Fit Random Regression Forest ---
# We must set importance = TRUE to calculate it
rf_model <- randomForest(
  formula = y ~ .,
  data = sim_data,
  ntree = 500,
  importance = TRUE
)

# Print the model summary (optional)
# print(rf_model)

# --- 3. Extract and Format Variable Importance ---

# Extract importance measures
# type=1 corresponds to %IncMSE (Mean Squared Error) for regression
importance_data <- importance(rf_model, type = 2)

# Convert to a data frame for ggplot
importance_df <- data.frame(
  Variable = rownames(importance_data),
  Importance = 100*(importance_data[, 1])/max(importance_data[, 1])
)

# --- 4. Plot with ggplot2 ---

ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") + # Use geom_col for bar chart
  coord_flip() + # Flip coordinates to make it horizontal
  labs(
    title = "Random Forest Variable Importance.",
    subtitle = latex2exp::TeX("True model: $Y = 10 X_1 + 5 X_2 + 2 X_3 + 0.5 X_4 + \\epsilon$"),
    x = "",
    y = "Importance"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )
