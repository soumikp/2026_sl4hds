rm(list = ls())
pacman::p_load(mgcv, gratia, dplyr, ggplot2, patchwork, gam)

# Set a seed for reproducibility
set.seed(42)
# Create 200 data points
n <- 1000
data <- tibble(
  # Predictor for the natural spline (will transform later)
  x1 = runif(n, 0, 5),
  # Predictor for the step function
  x2 = runif(n, 0, 1)
) %>%
  # Create the response variable 'y'
  mutate(
    # A true step-function effect for x2
    fx2 = case_when(
      x2 < 0.3 ~ 4,
      x2 < 0.8 ~ -7,
      TRUE ~ 0
    ),
    # Combine the effects and add random noise (sin transform for x1)
    y = 2 * sin(x1) + fx2 + rnorm(n, sd = 0.5)
  ) |>
  select(y, x1, x2)


# Visualize to figure out what sort of transformations to use for x1 and x2
(data |> ggplot() + geom_point(aes(x1, y)))  +
(data |> ggplot() + geom_point(aes(x2, y)))

# Making cutpoint for x2
data <- data |> mutate(x2_factor = cut(x2, c(-Inf, 0.25, 0.80, Inf)))


## poly

model_poly <- gam(y ~ poly(x1, degree = 3), ## makes cutpoints for x2
                  data = data)

##3 piecewise constant

model_pc <- gam(y ~ cut(x2, c(-Inf, 0.25, 0.75, Inf)), ## makes cutpoints for x2
    data = data)

## loess

model_loess <- gam(y ~ gam::lo(x1, span = 0.10), ## makes cutpoints for x2
                   data = data)


## cubic spline: bs()
## natural spline: ns()

# Fitting the model
model_1 <- gam(y ~ ns(x1, df = 4) + ## picks df - 1 number of knots for natural spline for x1
                 cut(x2, c(-Inf, 0.25, 0.80, Inf)), ## makes cutpoints for x2
               data = data)

model_2 <- gam(y ~ bs(x1, df = 6) + ## picks cubic spline with 3 cutpoints (df = 2*number of cutpoints)
                 cut(x2, c(-Inf, 0.25, 0.80, Inf)), ## makes cutpoints for x2
               data = data)

model_3 <- gam(y ~ bs(x1, df = 6) + ## picks cubic spline with 3 cutpoints (df = 2*number of cutpoints)
                 cut(x2, c(-Inf, 0.25, Inf)), ## makes cutpoints for x2
               data = data)

anova(model_1, model_2)

anova(model_3, model_2) ## smaller model always baseline

# Fitting the model
model_1 <- gam(y ~ ns(x1, df = 4) + ## picks df - 1 number of knots for natural spline for x1
                 cut(x2, c(-Inf, 0.25, 0.80, Inf)), ## makes cutpoints for x2
               data = data)
summary(model_1) ## might as well choose natural splines (better extrapolation)
