rm(list = ls())
source(file.path(here::here(), "class4", "code", "helper.R"))

pacman::p_load(tidyverse, patchwork, here, ggpubfigs, caret, ggsci)

wage_df <- read_csv(file.path(here(), "class4", "data", "wages.csv")) |>
  filter(year >= 2008)

output <- list(RMSE = NULL, predictions = NULL)

max_degree <- 4
for(fixed_degree in 1:max_degree){
  train_control <- trainControl(method = "cv", number = 10)
  set.seed(314) # for reproducibility
  poly_model_fixed <- train(
    wage ~ poly(age, degree = get("fixed_degree")),
    data = wage_df,
    method = "lm",
    trControl = train_control
  )
  # RMSE (CV-based) of the final linear model.
  output$RMSE <- c(output$RMSE, poly_model_fixed$results$RMSE)
  # Generate a sequence of ages for creating a smooth prediction curve.
  age_grid <- seq(min(wage_df$age), max(wage_df$age), length.out = 25)
  # Create a data frame for prediction.
  predict_df <- data.frame(age = age_grid)
  # Get predictions from the fitted model.
  output$predictions <- cbind(output$predictions, predict(poly_model_fixed, newdata = predict_df))
}

fig2 <- data.frame(output$predictions) |>
  add_column(age = seq(min(wage_df$age), max(wage_df$age), length.out = 25)) |>
  pivot_longer(cols = -age) |>
  mutate(degree = as.numeric(gsub("\\D+", "", name))) |>
  left_join(data.frame(RMSE = output$RMSE) |> add_column(degree = c(1:max_degree))) |>
  rowwise() |>
  mutate(groups = paste0(degree, " (", sprintf("%0.3f", RMSE), ")")) |>
  ggplot() +
  geom_line(aes(x = age, y = value, color = groups)) +
  geom_point(data = wage_df, aes(x = age, y = wage), alpha = 0.2) +
  theme_big_simple() +
  scale_color_aaas() +
  labs(color = "Degree (RMSE)", x = "Age", y = "Wage ($)",
       caption = "RMSE based on 10-fold CV", title = "") +
  guides(color = guide_legend(byrow = T), linewdith = guide_legend())

ggsave(file.path(here(), "class4", "figures", "fig2.pdf"),
       fig2,
       device = pdf,
       width = 12,
       height = 6,
       units = "in")
