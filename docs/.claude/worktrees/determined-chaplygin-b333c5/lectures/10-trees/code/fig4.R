f1 <- function(x){
  2*x*(1-x)
}

f2 <- function(x){
  -x*log(x) - (1-x)*log(1-x)
}

ggplot() +
  geom_function(fun = f1, color = "red") +
  geom_function(fun = f2, color = "blue") +
  annotate("text",
           x = 0.5,
           y = 0.4,
           label = "Gini (for two classes)",
           color = "red") +
  annotate("text",
           x = 0.5,
           y = 0.6,
           label = "Entropy (for two classes)",
           color = "blue") +
  theme_bw() +
  scale_x_continuous(limits = c(0.001, 0.999))

