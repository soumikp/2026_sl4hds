rm(list = ls())

pacman::p_load(tidyverse, pROC, caTools) # For splitting the data

# We'll create a dataset with two predictor variables (x1, x2) and a binary outcome (y).
# We'll make the classes slightly separated so the model has something to learn.
set.seed(42) # for reproducibility

# Number of samples
n <- 500

# Generate data for the '0' class (Negative Class)
class_0 <- tibble(
  x1 = rnorm(n, mean = 0, sd = 1.5),
  x2 = rnorm(n, mean = 0, sd = 1.5),
  y = 0
)

# Generate data for the '1' class (Positive Class)
class_1 <- tibble(
  x1 = rnorm(n, mean = 2, sd = 2),
  x2 = rnorm(n, mean = 2, sd = 2),
  y = 1
)

# Combine into a single dataframe
sim_data <- bind_rows(class_0, class_1) %>%
  mutate(y = as.factor(y)) # Convert outcome to a factor, which is good practice for classification

# It's crucial to evaluate the model on data it hasn't seen before.
split <- sample.split(sim_data$y, SplitRatio = 0.7)
train_data <- subset(sim_data, split == TRUE)
test_data <- subset(sim_data, split == FALSE)


# We'll predict 'y' using our two predictor variables.
log_model <- glm(y ~ x1 + x2, data = train_data, family = "binomial")
summary(log_model)


# We need the predicted probabilities for the positive class ('1').
# The 'type = "response"' argument ensures we get probabilities.
predictions <- predict(log_model, newdata = test_data, type = "response")


# The roc() function from the pROC library takes the actual outcomes (test_data$y)
# and the predicted probabilities for the positive class.
roc_curve <- roc(
  response = test_data$y,
  predictor = predictions,
  levels = c("0", "1")
)

# You can print the object to get the AUC value
print(roc_curve)


# The ggroc() function from pROC makes plotting with ggplot2 easy.
fig1 <- ggroc(roc_curve, legacy.axes = TRUE) +
  geom_segment(
    aes(x = 0, xend = 1, y = 0, yend = 1),
    color = "grey",
    linetype = "dashed"
  ) + # Add the 45-degree line
  labs(
    title = "ROC Curve for Logistic Regression",
    subtitle = paste("Area Under Curve (AUC):", round(auc(roc_curve), 3)),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_bw()

# ggsave(file.path(here(), "class5", "figures", "fig1.pdf"),
#        fig3,
#        device = pdf,
#        width = 6,
#        height = 6,
#        units = "in")
