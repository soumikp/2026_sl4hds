rm(list = ls())
source(file.path(here::here(), "class4", "code", "helper.R"))

pacman::p_load(tidyverse, patchwork, here, ggpubfigs, caret, ggsci)

wage_df <- read_csv(file.path(here(), "class4", "data", "wages.csv")) |>
  filter(year >= 2008)

fig3 <- wage_df |>
  mutate(age_group = cut(age, 12)) |>
  left_join(wage_df |>
              mutate(age_group = cut(age, 12)) |>
              group_by(age_group) |>
              summarise(m = mean(wage)) |>
              ungroup() |>
              mutate(low =  as.numeric(gsub("^\\(([-0-9.]+).*", "\\1", age_group)),
                     high = as.numeric(gsub(".*,([-0-9.]+)\\]$", "\\1", age_group)))) |>
  ggplot(aes(x = age, y = wage)) +
  geom_point() +
  geom_vline(aes(xintercept = low)) +
  geom_vline(aes(xintercept = high)) +
  geom_linerange(aes(xmin = low, xmax = high, y = m), color = "red", linewidth = 1) +
  theme_big_simple() +
  labs(x = "Age", y = "Wage ($)", title = "")

ggsave(file.path(here(), "class4", "figures", "fig3.pdf"),
       fig3,
       device = pdf,
       width = 12,
       height = 6,
       units = "in")
