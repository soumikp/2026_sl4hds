rm(list = ls())
pacman::p_load(tidyverse, ggpubfigs, patchwork, here)

function1 <- function(x, pi = 0.5){
  pi*dnorm(x, 3, 1)
}

function2 <- function(x, pi = 0.5){
  pi*dnorm(x, 0, 1)
}


p1 <- ggplot() +
  xlim(-5, 7) +
  geom_function(fun = function1, color = "red", args = list(pi = 0.3)) +
  geom_function(fun = function2, color = "blue", args = list(pi = 0.7)) +
  theme_big_simple() + labs(title = "70% (blue) - 30% (red)", x = "", y = "")

p2 <- ggplot() +
  xlim(-5, 7) +
  geom_function(fun = function1, color = "red", args = list(pi = 0.5)) +
  geom_function(fun = function2, color = "blue", args = list(pi = 0.5)) +
  theme_big_simple() + labs(title = "50% (blue) - 50% (red)", x = "", y = "")

p3 <- ggplot() +
  xlim(-5, 7) +
  geom_function(fun = function1, color = "red", args = list(pi = 0.7)) +
  geom_function(fun = function2, color = "blue", args = list(pi = 0.3)) +
  theme_big_simple() + labs(title = "30% (blue) - 70% (red)", x = "", y = "")

fig2 <- p1 + p2 + p3

ggsave(file.path(here(), "class5", "figures", "fig2.pdf"),
       fig2,
       device = pdf,
       width = 18,
       height = 6,
       units = "in")

