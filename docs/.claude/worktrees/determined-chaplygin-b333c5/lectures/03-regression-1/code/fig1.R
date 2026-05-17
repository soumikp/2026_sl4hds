rm(list = ls())
source(file.path(here::here(), "class3", "code", "helper.R"))

pacman::p_load(here, tidyverse, patchwork, latex2exp, ggpubfigs)

data <- read_csv(file.path(here(), "class3", "data", "neonatal_brain_volumes.csv"))

model <- lm(`brain volume` ~ GA, data = data)

fig1 <- (data |>
           ggplot(aes(y = `brain volume`, x = GA)) +
           geom_point(alpha = 0.5) +
           geom_smooth(method = "lm", color = "blue", se = FALSE, fill = "blue") +
           labs(x = "Predictor: Gestational age (in weeks)", y = "Outcome: Brain volume (in mL)",
                title = "Linear model (confidence interval)") +
           geom_errorbar(aes(x = mean(data$GA),
                             ymin = predict(model, newdata = data.frame(1, GA = mean(data$GA)), interval = "confidence")[,2],
                             ymax = predict(model, newdata = data.frame(1, GA = mean(data$GA)), interval = "confidence")[,3]),
                         width = 1, color = "red") +
           theme_big_simple()) +
  (data |>
     ggplot(aes(y = `brain volume`, x = GA)) +
     geom_point(alpha = 0.5) +
     geom_smooth(method = "lm", color = "blue", se = FALSE, fill = "blue") +
     labs(x = "Predictor: Gestational age (in weeks)", y = "Outcome: Brain volume (in mL)",
          title = "Linear model (prediction interval)") +
     geom_errorbar(aes(x = mean(data$GA),
                       ymin = predict(model, newdata = data.frame(1, GA = mean(data$GA)), interval = "prediction")[,2],
                       ymax = predict(model, newdata = data.frame(1, GA = mean(data$GA)), interval = "prediction")[,3]),
                   width = 1, color = "darkgreen") +
     theme_big_simple())

ggsave(file.path(here(), "class3", "figures", "fig1.pdf"),
       fig1,
       device = pdf,
       width = 15,
       height = 6,
       units = "in")
