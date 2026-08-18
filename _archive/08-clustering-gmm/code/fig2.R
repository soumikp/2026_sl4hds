pacman::p_load(MASS, tidyverse, ggdendro, gridExtra, here)

# --- 1. set up simulation parameters ---
# we'll make 4 classes, 10 points each
n_points <- 10

# class 1: bottom-left
mu1 <- c(0, 0)
cov1 <- matrix(c(1, 0.3, 0.3, 1), 2) # a little correlation

# class 2: bottom-right
mu2 <- c(6, 0)
cov2 <- matrix(c(1.5, -0.5, -0.5, 1.5), 2) # different shape

# class 3: top-left
mu3 <- c(0, 6)
cov3 <- matrix(c(0.8, 0, 0, 0.8), 2) # tighter, no correlation

# class 4: top-right
mu4 <- c(6, 6)
cov4 <- matrix(c(1, 0, 0, 1), 2) # same as class 1, just moved

# set a seed so you get the same "random" data every time you run this
set.seed(42)

# use mvrnorm to simulate the points
data1 <- as.data.frame(mvrnorm(n = n_points, mu = mu1, Sigma = cov1))
data2 <- as.data.frame(mvrnorm(n = n_points, mu = mu2, Sigma = cov2))
data3 <- as.data.frame(mvrnorm(n = n_points, mu = mu3, Sigma = cov3))
data4 <- as.data.frame(mvrnorm(n = n_points, mu = mu4, Sigma = cov4))

# set column names
colnames(data1) <- c("x", "y")
colnames(data2) <- c("x", "y")
colnames(data3) <- c("x", "y")
colnames(data4) <- c("x", "y")

# we add a 'true_class' column so we can see what it *should* look like
sim_data <- bind_rows(
  data1 %>% mutate(true_class = "Class 1"),
  data2 %>% mutate(true_class = "Class 2"),
  data3 %>% mutate(true_class = "Class 3"),
  data4 %>% mutate(true_class = "Class 4")
)

print(
  ggplot(sim_data, aes(x = x, y = y, color = true_class)) +
    geom_point(alpha = 0.8) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(title = "Simulated Data (True Classes)", color = "True Class") +
    coord_fixed() # makes x and y axes have the same scale
)

# --- 4. prepare data for clustering ---
# for clustering, we *don't* use the labels. it's unsupervised!
# so let's just get the x and y columns
data_for_clustering <- sim_data %>% select(x, y)

# --- 5. calculate the distance matrix ---
# hierarchical clustering works on a distance matrix
# this calculates the euclidean distance between every single pair of points
dist_matrix <- dist(data_for_clustering, method = "euclidean")

# --- 6. run hierarchical clustering ---
# now we run hclust 4 times, one for each linkage method

# single linkage: distance = closest points
hc_single <- hclust(dist_matrix, method = "single")

# complete linkage: distance = farthest points
hc_complete <- hclust(dist_matrix, method = "complete")

# average linkage: distance = average of all pairs
hc_average <- hclust(dist_matrix, method = "average")

# ward's method: minimizes the increase in variance
# 'ward.D2' is the one you usually want
hc_ward <- hclust(dist_matrix, method = "centroid")

# --- 7. plot the dendrograms using ggplot ---
# we'll use ggdendrogram() and then arrange them in a grid

# helper theme to clean up the plots (no axis labels)
theme_dendro <- theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# create the 4 plots
# ggdendrogram automatically converts the hclust object
p_single <- ggdendrogram(hc_single, rotate = FALSE) +
  theme_dendro +
  ggtitle("Single Linkage")

p_complete <- ggdendrogram(hc_complete, rotate = FALSE) +
  theme_dendro +
  ggtitle("Complete Linkage")

p_average <- ggdendrogram(hc_average, rotate = FALSE) +
  theme_dendro +
  ggtitle("Average Linkage")

p_ward <- ggdendrogram(hc_ward, rotate = FALSE) +
  theme_dendro +
  ggtitle("Centroid Linkage")

fig2 <- grid.arrange(p_single, p_complete, p_average, p_ward, nrow = 2)

ggsave(
  file.path(here(), "class8", "figures", "fig2.pdf"),
  plot = fig2,
  width = 12,
  height = 12
)
