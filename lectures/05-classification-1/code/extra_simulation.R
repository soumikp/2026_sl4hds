# Multiclass Classification and Variable Selection in R
# This script demonstrates how to simulate data for a multiclass
# classification problem and then use penalized regression (LASSO)
# for simultaneous model fitting and variable selection.

# --- 1. Load Necessary Libraries ---
# install.packages(c("glmnet", "caret", "ggplot2", "tidyr"))
library(glmnet)
library(caret)
library(ggplot2)
library(tidyr)


# --- 2. Simulate Multiclass Data ---

set.seed(42) # for reproducibility

# Simulation parameters
n <- 1000   # Number of observations
p <- 50     # Total number of predictors
n_classes <- 4 # Number of classes

# Generate the predictor matrix from a standard normal distribution
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(X) <- paste0("V", 1:p)

# Define which variables are "important" for which class comparison.
# We will use class 1 as the reference (baseline) class.
# The coefficients define the log-odds of being in one class vs. the reference.

# Betas for Class 2 vs Class 1: V1-V5 are important
betas_class2 <- c(rep(2.5, 5), rep(0, p - 5))

# Betas for Class 3 vs Class 1: V6-V10 are important
betas_class3 <- c(rep(0, 5), rep(-2.5, 5), rep(0, p - 10))

# Betas for Class 4 vs Class 1: V11-V15 are important
betas_class4 <- c(rep(0, 10), rep(2.5, 5), rep(0, p - 15))

# Variables V16 through V50 are pure noise for all classes.

# Calculate the linear predictors (log-odds relative to class 1)
# Note: The log-odds for the reference class is 0.
z2 <- X %*% betas_class2
z3 <- X %*% betas_class3
z4 <- X %*% betas_class4

# Apply the softmax function to get class probabilities
prob1 <- 1 / (1 + exp(z2) + exp(z3) + exp(z4))
prob2 <- exp(z2) / (1 + exp(z2) + exp(z3) + exp(z4))
prob3 <- exp(z3) / (1 + exp(z2) + exp(z3) + exp(z4))
prob4 <- exp(z4) / (1 + exp(z2) + exp(z3) + exp(z4))

probs <- cbind(prob1, prob2, prob3, prob4)

# Generate the final class labels by sampling based on the probabilities
y_labels <- apply(probs, 1, function(row) sample(1:n_classes, 1, prob = row))
y <- as.factor(y_labels)
levels(y) <- c("ClassA", "ClassB", "ClassC", "ClassD")

# Combine into a final dataset
sim_data <- data.frame(X, class = y)


# --- 3. Split Data into Training and Testing Sets ---

train_indices <- createDataPartition(sim_data$class, p = 0.7, list = FALSE)
train_x <- X[train_indices, ]
train_y <- y[train_indices]

test_x <- X[-train_indices, ]
test_y <- y[-train_indices]


# --- 4. Perform Variable Selection and Modeling with glmnet ---

# We use cross-validation to find the optimal lambda (penalty parameter)
# alpha = 1 specifies LASSO penalty, which is good for variable selection
# family = "multinomial" is for multiclass classification
cv_fit <- cv.glmnet(train_x, train_y, family = "multinomial", alpha = 1)

# Plot the cross-validation results
plot(cv_fit)
title("Multinomial CV-Lasso", line = 2.5)

# Two common choices for lambda:
# lambda.min: gives the model with the minimum cross-validated error
# lambda.1se: gives a more regularized (simpler) model that is within
#             one standard error of the minimum. Often preferred for parsimony.
cat("Optimal lambda (lambda.min):", cv_fit$lambda.min, "\n")
cat("Optimal lambda (lambda.1se):", cv_fit$lambda.1se, "\n")


# --- 5. Inspect the Selected Variables ---

# Extract the coefficients at the optimal lambda (lambda.1se)
# glmnet returns a list of coefficient matrices, one for each class
coefficients <- coef(cv_fit, s = "lambda.1se")

print("Coefficients for each class:")
print(coefficients)

# Identify which variables have non-zero coefficients for any class
# This is our selected set of variables.
selected_vars_list <- lapply(coefficients, function(class_coefs) {
  rownames(class_coefs)[which(class_coefs != 0)]
})

# We can see that different variables are selected for different classes.
cat("\nSelected variables for Class B:\n")
print(setdiff(selected_vars_list$ClassB, "(Intercept)"))

cat("\nSelected variables for Class C:\n")
print(setdiff(selected_vars_list$ClassC, "(Intercept)"))

cat("\nSelected variables for Class D:\n")
print(setdiff(selected_vars_list$ClassD, "(Intercept)"))


# --- 6. Evaluate Model Performance on the Test Set ---

# Make predictions on the test data
predictions <- predict(cv_fit, newx = test_x, s = "lambda.1se", type = "class")

# Create and print the confusion matrix
confusion_mat <- confusionMatrix(data = as.factor(predictions), reference = test_y)

cat("\n--- Model Performance on Test Set ---\n")
print(confusion_mat)

cat("\nOverall Accuracy:", confusion_mat$overall['Accuracy'], "\n")
