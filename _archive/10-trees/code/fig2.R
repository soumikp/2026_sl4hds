rm(list = ls())
pacman::p_load(tidyverse, ISLR2, tree, treeheatr, patchwork, ggsci, rpart, parttree)

data("Hitters", package = "ISLR2")

scatter <- data.frame(Hitters) |>
  ggplot(aes(x = Years, y = Hits)) +
  geom_point(aes(color = log(Salary), size = log(Salary))) +
  theme_bw() +
  guides(size = "none",
         color = guide_colorbar(barwidth = unit(5, "cm"),
                               barheight = unit(0.5, "cm"))) +
  theme(legend.position = "bottom") +
  scale_color_gsea() +
  labs(color = "log(Salary in $1000s)")

salary_tree = rpart(Salary ~ Hits + Years,
                    data=Hitters |>
                      mutate(Salary = log(Salary)) |>
                      select(Salary, Hits, Years))

scatter <- scatter + geom_parttree(data = salary_tree)

tree_plot <- heat_tree(
  x = Hitters |> mutate(Salary = log(Salary)) |> select(Salary, Hits, Years),
  target_lab = 'Salary',
  task = 'regression')
