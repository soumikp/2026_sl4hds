rm(list = ls())
pacman::p_load(tidyverse, ISLR2, tree, parttree, rpart, rattle, patchwork)

## decision tree (max of two predictor variables)
iris_tree = rpart(Species ~ Petal.Length + Petal.Width, data=iris)

# start plotting base
p = ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(col = Species)) +
  theme(legend.position = "bottom")
# tree partitions to the plot (borders only)
p <- p + geom_parttree(data = iris_tree)
# fill and highlight predictions
p <- p + geom_parttree(data = iris_tree, aes(fill = Species), alpha=0.1)

par(mfrow = c(1, 1))
p
fancyRpartPlot(iris_tree, yesno=0,split.col="black",nn.col="black",
               caption="",branch.col="black")
par(mfrow = c(1, 1))
