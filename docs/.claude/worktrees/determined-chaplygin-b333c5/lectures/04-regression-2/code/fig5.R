rm(list = ls())
source(file.path(here::here(), "class4", "code", "helper.R"))

pacman::p_load(tidyverse, splines, here, ggpubfigs, caret, ggsci)

wage_df <- read_csv(file.path(here(), "class4", "data", "wages.csv")) |>
  filter(year >= 2008) |> select(wage, age)

reg_spline <- lm(wage ~ bs(age, 3), data = wage_df)
nat_spline <- lm(wage ~ ns(age, 3), data = wage_df)

pred_df <- data.frame(age = seq(min(wage_df$age), max(wage_df$age), length.out = 100))

pred_reg <- cbind(pred_df$age, predict(reg_spline, newdata = pred_df, interval = "confidence"))
pred_nat <- cbind(pred_df$age, predict(nat_spline, newdata = pred_df, interval = "confidence"))

plot_data <- data.frame(rbind(pred_reg, pred_nat)) |>
  add_column(type = rep(c("Regular", "Natural"), each = 100))

fig6 <- plot_data |>
  ggplot() +
  geom_point(data = wage_df, aes(x = age, y = wage), alpha = 0.25) +
  geom_line(aes(x = V1, y = fit, color = type, linetype = type), linewidth = 1) +
  geom_ribbon(aes(x = V1, ymin = lwr, ymax = upr, group = type, linetype = type, color = type, fill = type), alpha = 0.1) +
  scale_color_aaas() +
  scale_fill_aaas() +
  theme_big_simple() +
  labs(color = "Spline type", x = "Age", y = "Wage ($)", linetype = "Spline type", fill ="Spline type")


ggsave(file.path(here(), "class4", "figures", "fig5.pdf"),
       fig5,
       device = pdf,
       width = 12,
       height = 12,
       units = "in")
