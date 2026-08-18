pacman::p_load(ggplot2, MASS, scales, patchwork)

set.seed(42)

n <- 200
d <- 6

mu <- rep(0, d)
A <- matrix(rnorm(18), nrow = 6, ncol = 3)
Sigma <- crossprod(t(A))


data_raw <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
data_df <- as.data.frame(data_raw)
colnames(data_df) <- paste0("V", 1:d)

pca_model <- prcomp(data_df, center = TRUE, scale. = TRUE)

pca_summary <- summary(pca_model)
scree_data <- data.frame(
  Component = 1:length(pca_summary$importance[2, ]),
  VarianceExplained = pca_summary$importance[2, ]
)

(ggplot(scree_data, aes(x = Component, y = VarianceExplained)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = c(1:6))+
  labs(x = "Principal Component",
    y = "PVE"
  ) +
  theme_bw(base_size = 12)) /
  (ggplot(scree_data, aes(x = Component, y = cumsum(VarianceExplained))) +
  geom_line(color = "darkred") +
  geom_point(color = "darkred", size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = c(1:6))+
  labs(x = "Principal Component",
    y = "Cumulative PVE"
  ) +
  theme_bw(base_size = 12)
)
