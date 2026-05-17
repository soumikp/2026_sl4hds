# We'll use the `caret` package for a streamlined K-Fold CV example.
# For Moving Window, we'll implement it manually to show the logic clearly.
if (!require(caret)) install.packages("caret")
library(caret)

# --- 1. Generate Sample Time Series Data ---
# Let's create a simple dataset with a clear time trend.
set.seed(42)
n_samples <- 100
time_steps <- 1:n_samples
# Create a trend and add some noise
data_values <- 0.5 * time_steps + rnorm(n_samples, mean = 0, sd = 5)
df <- data.frame(Time = time_steps, Value = data_values)

cat("--- Sample Time Series Data (first 6 rows) ---\n")
print(head(df))
cat("\n")


# --- 2. K-Fold Cross-Validation Demonstration ---
# This is how one might INCORRECTLY apply K-Fold CV to time series data.
cat("--- Running K-Fold Cross-Validation ---\n")

# K-Fold CV shuffles the data by default, which is problematic for time series.
# This means the model can be trained on future data to predict the past,
# leading to unrealistically good performance metrics.

# Set up the control for 10-fold CV
train_control_kfold <- trainControl(method = "cv", number = 10)

# Train a simple linear model using K-Fold CV
start_time_kfold <- Sys.time()
model_kfold <- train(Value ~ Time, data = df, method = "lm", trControl = train_control_kfold)
end_time_kfold <- Sys.time()

run_time_kfold <- end_time_kfold - start_time_kfold

cat("K-Fold CV Results (RMSE):\n")
print(model_kfold$results$RMSE)
cat("Average RMSE from K-Fold CV:", mean(model_kfold$results$RMSE), "\n")
cat("Run-time for K-Fold CV:", run_time_kfold, "seconds\n\n")


# --- 3. Moving Window Cross-Validation Demonstration ---
# This is the CORRECT approach for time series data.
cat("--- Running Moving Window Cross-Validation ---\n")

# We will implement this manually to see the process.
# Parameters for our moving window:
initial_train_size <- 50 # Start with 50 data points for training
validation_size <- 10 # Predict the next 10 points
step_size <- 10 # Move the window forward by 10 points

# Store the performance metric (RMSE) for each window
rmse_scores_mw <- c()

start_time_mw <- Sys.time()

current_pos <- 1
while (current_pos + initial_train_size + validation_size -1 <= n_samples) {

  # Define the training and validation sets based on position
  train_indices <- current_pos:(current_pos + initial_train_size - 1)
  validation_indices <- (current_pos + initial_train_size):(current_pos + initial_train_size + validation_size - 1)

  train_set <- df[train_indices, ]
  validation_set <- df[validation_indices, ]

  # Train a simple linear model on the training set
  model_mw <- lm(Value ~ Time, data = train_set)

  # Predict on the validation set
  predictions <- predict(model_mw, newdata = validation_set)

  # Calculate RMSE and store it
  rmse <- sqrt(mean((predictions - validation_set$Value)^2))
  rmse_scores_mw <- c(rmse_scores_mw, rmse)

  cat(sprintf("Window: Train [%d:%d], Validate [%d:%d], RMSE: %.4f\n",
              min(train_indices), max(train_indices),
              min(validation_indices), max(validation_indices), rmse))

  # Move the window forward
  current_pos <- current_pos + step_size
}

end_time_mw <- Sys.time()
run_time_mw <- end_time_mw - start_time_mw

cat("\nMoving Window CV Results (RMSE for each window):\n")
print(rmse_scores_mw)
cat("Average RMSE from Moving Window CV:", mean(rmse_scores_mw), "\n")
cat("Run-time for Moving Window CV:", run_time_mw, "seconds\n")


# --- 4. Conclusion from Code ---
# The average RMSE from K-Fold is likely to be lower (more optimistic) than the
# more realistic estimate from the Moving Window CV.
# The run-time for the manual moving window loop can be longer, especially
# if the model is re-trained frequently on a large dataset.
# The `caret` package has optimizations, while our loop is for demonstration.
# However, the sequential nature of Moving Window CV fundamentally limits parallelization.
