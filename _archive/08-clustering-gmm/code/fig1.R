pacman::p_unload(pacman::p_loaded())
pacman::p_load(stats, tidyverse, ggsci, patchwork, here)

set.seed(42)
cluster1 <- data.frame(x = rnorm(50, mean = 0, sd = 1.9), y = rnorm(50, mean = 4, sd = 1.9))
cluster2 <- data.frame(x = rnorm(50, mean = -2, sd = 1.1), y = rnorm(50, mean = -2, sd = 1.1))
cluster3 <- data.frame(x = rnorm(50, mean = 3, sd = 1.1), y = rnorm(50, mean = -3, sd = 1.1))
data <- cbind(rbind(cluster1, cluster2, cluster3),
              group = rep(c(1, 2, 3), each = 50))

true <- data |>
  mutate(group = as.factor(group)) |>
  ggplot(aes(x = x, y = y, color = group, shape = group)) +
  geom_point() +
  scale_color_aaas() +
  theme_bw() +
  labs(title = "Simulating data from three (well-separated) groups") +
  theme(legend.position = "none")


km_2 <- kmeans(data, centers = 2, nstart = 25)
km_3 <- kmeans(data, centers = 3, nstart = 25)
km_4 <- kmeans(data, centers = 4, nstart = 25)

estimated <- data |>
  select(-group) |>
  add_column("K = 2" = km_2$cluster) |>
  add_column("K = 3" = km_3$cluster) |>
  add_column("K = 4" = km_4$cluster) |>
  pivot_longer(cols = -c(x, y)) |>
  mutate(value = as.factor(value)) |>
  ggplot(aes(x = x, y = y, color = value, shape = value)) +
  geom_point() +
  facet_grid(cols = vars(name)) +
  theme_bw() +
  scale_color_aaas() +
  labs(title = "k-mean clustering results from three different  values of k") +
  theme(legend.position = "none")

fig1 <- true/estimated

ggsave(
  file.path(here(), "class8", "figures", "fig1.pdf"),
  plot = fig1,
  width = 6,
  height = 10
)
