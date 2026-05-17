rm(list = ls())
pacman::p_load(tidyverse, ggsci) ## loess comes with stats package - loaded in base

set.seed(123) # for reproducibility
df <- tibble(
  x = seq(0, 10, length.out = 500),
  y = sin(x) + rnorm(500, sd = 0.5)
)

df <- df |>
  mutate(span25 = predict(loess(y ~ x, span = 0.25))) |>
  mutate(span10 = predict(loess(y ~ x, span = 0.10))) |>
  mutate(span05 = predict(loess(y ~ x, span = 0.05))) |>
  mutate(span01 = predict(loess(y ~ x, span = 0.01)))

df |> pivot_longer(cols = -c(x, y)) |>
  ggplot(aes(x = x, group = name)) +
  geom_point(aes(y = y), alpha = 0.15) +
  geom_line(aes(y = value, color = name), linewidth = 1) +
  scale_color_aaas() + theme_big_simple() +
  theme(legend.position = "bottom") + labs(color = "span")
