library(ggplot2)
library(dplyr)
library(tidyr)
library(ggsci)

x <- seq(0, 1, length.out = 500)

plot_data <- tibble(
  x = x,
  `Function 1` = 1 - x,
  `Function 2` = (1 - x)^2,
  `Function 3` = (exp(-3 * x) - exp(-3)) / (1 - exp(-3)),
  `Function 4` = (1 + cos(pi * x)) / 2
) |>
  pivot_longer(
    cols = -x,
    names_to = "function_name",
    values_to = "y"
  ) |>
  mutate(`1 - y` = 1 - y) |>
  pivot_longer(
    cols = c(y, `1 - y`),
    names_to = "facet",
    values_to = "value"
  ) |>
  mutate(
    facet = factor(
      facet,
      levels = c("y", "1 - y"),
      labels = c("y = f(x)", "1 - y = 1 - f(x)")
    )
  )

p <- ggplot(plot_data, aes(x, value, color = function_name)) +
  geom_line(linewidth = 1.15) +
  facet_grid(cols = vars(facet)) +
  scale_color_jama(name = NULL) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = 0.01)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = 0.01)
  ) +
  labs(x = "x", y = "Value") +
  theme_classic(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "grey95", color = "grey30"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = "inverse_relationship_facets.png",
  plot = p,
  width = 10,
  height = 5,
  dpi = 300,
  bg = "white"
)
