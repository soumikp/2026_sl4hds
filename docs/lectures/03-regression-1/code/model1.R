rm(list = ls())
source(file.path(here::here(), "class3", "code", "helper.R"))

pacman::p_load(here, tidyverse, patchwork, latex2exp, ggpubfigs)

data <- read_csv(file.path(here(), "class3", "data", "neonatal_brain_volumes.csv"))

model <- lm(`brain volume` ~ GA, data = data)

summary(model)

confint(model, parm = "GA")

predict(model, newdata = data.frame(1, GA = mean(data$GA)),
        interval = "confidence")
predict(model, newdata = data.frame(1, GA = mean(data$GA)),
        interval = "prediction")
