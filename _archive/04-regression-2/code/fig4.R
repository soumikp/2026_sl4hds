rm(list = ls())
source(file.path(here::here(), "class4", "code", "helper.R"))

pacman::p_load(tidyverse, patchwork, here, ggpubfigs, caret, ggsci, splines)

wage_df <- read_csv(file.path(here(), "class4", "data", "wages.csv")) |>
  filter(year >= 2008)

wage_df <- data.frame(age = wage_df$age, wage = wage_df$wage)

# We will use a knot at age = 50 for all models
knot_age <- 50
plot_y_limits <- c(min(wage_df$wage, na.rm=TRUE), max(wage_df$wage, na.rm=TRUE) + 10)


# -----------------------------------------------------------------------------
# 3. GENERATE PLOTS
# -----------------------------------------------------------------------------

# --- Plot 1: Piecewise Linear Regression ---
# This model fits two separate cubic polynomials, one on each side of the knot.
# This can result in a discontinuity (a "jump") at the knot.
p1 <- ggplot(wage_df, aes(x = age, y = wage)) +
  geom_point(color = 'grey70', shape = 1, alpha = 0.8) +
  # Fit model for data to the left of the knot
  geom_smooth(data = subset(wage_df, age <= knot_age),
              method = "lm",
              formula = y ~ poly(x, 1),
              se = FALSE,
              color = "orange",
              linewidth = 1) +
  # Fit model for data to the right of the knot
  geom_smooth(data = subset(wage_df, age >= knot_age),
              method = "lm",
              formula = y ~ poly(x, 1),
              se = FALSE,
              color = "orange",
              linewidth = 1) +
  geom_vline(xintercept = knot_age, linetype = "dashed", color = "grey40") +
  labs(title = "Piecewise Linear", x = "Age", y = "Wage") +
  theme_classic(base_size = 12) +
  coord_cartesian(ylim = plot_y_limits)


# --- Plot 2: Piecewise Cubic Regression ---
# This model fits two separate cubic polynomials, one on each side of the knot.
# This can result in a discontinuity (a "jump") at the knot.
p2 <- ggplot(wage_df, aes(x = age, y = wage)) +
  geom_point(color = 'grey70', shape = 1, alpha = 0.8) +
  # Fit model for data to the left of the knot
  geom_smooth(data = subset(wage_df, age <= knot_age),
              method = "lm",
              formula = y ~ poly(x, 3),
              se = FALSE,
              color = "#6a3d9a",
              linewidth = 1) +
  # Fit model for data to the right of the knot
  geom_smooth(data = subset(wage_df, age >= knot_age),
              method = "lm",
              formula = y ~ poly(x, 3),
              se = FALSE,
              color = "#6a3d9a",
              linewidth = 1) +
  geom_vline(xintercept = knot_age, linetype = "dashed", color = "grey40") +
  labs(title = "Piecewise Cubic", x = "Age", y = "Wage") +
  theme_classic(base_size = 12) +
  coord_cartesian(ylim = plot_y_limits)

# --- Plot 3: Cubic Spline ---
# A cubic spline is a piecewise cubic polynomial that is continuous and also
# has continuous first and second derivatives at the knots. This makes it smooth.
# We use the bs() function from the 'splines' package to generate the basis.
p3 <- ggplot(wage_df, aes(x = age, y = wage)) +
  geom_point(color = 'grey70', shape = 1, alpha = 0.8) +
  geom_smooth(method = "lm",
              formula = y ~ bs(x, knots = knot_age, degree = 3),
              se = FALSE,
              color = "#e31a1c",
              linewidth = 1) +
  geom_vline(xintercept = knot_age, linetype = "dashed", color = "grey40") +
  labs(title = "Cubic Spline", x = "Age", y = "Wage") +
  theme_classic(base_size = 12) +
  coord_cartesian(ylim = plot_y_limits)

# --- Plot 4: Linear Spline ---
# A linear spline fits piecewise linear lines and is continuous at the knots.
# We use bs() again, but set degree = 1 for linear.
p4 <- ggplot(wage_df, aes(x = age, y = wage)) +
  geom_point(color = 'grey70', shape = 1, alpha = 0.8) +
  geom_smooth(method = "lm",
              formula = y ~ bs(x, knots = knot_age, degree = 1),
              se = FALSE,
              color = "#b15928",
              linewidth = 1) +
  geom_vline(xintercept = knot_age, linetype = "dashed", color = "grey40") +
  labs(title = "Linear Spline", x = "Age", y = "Wage") +
  theme_classic(base_size = 12) +
  coord_cartesian(ylim = plot_y_limits)


# -----------------------------------------------------------------------------
# 4. COMBINE AND DISPLAY PLOTS
# -----------------------------------------------------------------------------
# We use the 'patchwork' library to arrange the four plots in a 2x2 grid.
fig4 <- (p1 |p2) / (p4 | p3)

ggsave(file.path(here(), "class4", "figures", "fig4.pdf"),
       fig4,
       device = pdf,
       width = 8,
       height = 8,
       units = "in")

# To save the plot to a file:
# ggsave("spline_plots.png", plot = final_plot, width = 10, height = 8, dpi = 300)
