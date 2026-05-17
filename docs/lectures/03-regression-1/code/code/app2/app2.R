rm(list = ls())
source(file.path(here::here(), "class3", "code", "helper.R"))

# install.packages("pacman") # Run this once if you don't have pacman
pacman::p_load(shiny, plotly, dplyr, shinycssloaders, readr, here, bslib, glmnet)

# ==============================================================================
#                      Data Loading and Preprocessing
# ==============================================================================
# Load the insurance dataset using the specified path
# Using a failsafe for running locally vs. in a specific project structure
insurance_file <- "insurance.csv"
if (file.exists(file.path(here(), "class3", "data", "insurance.csv"))) {
  insurance_file <- file.path(here(), "class3", "data", "insurance.csv")
}
insurance_df_raw <- read_csv(insurance_file)


# Preprocess the data: select columns, scale outcome and center/scale predictors
insurance_df <- insurance_df_raw %>%
  select(charges, age, bmi) %>%
  mutate(
    charges_scaled = charges / 10000, # Scale the outcome variable
    age_scaled = as.numeric(scale(age)),
    bmi_scaled = as.numeric(scale(bmi))
  )

# --- Prepare data for glmnet ---
x_vars <- as.matrix(insurance_df[, c("age_scaled", "bmi_scaled")])
y_var <- insurance_df$charges_scaled

# --- Fit models to get solution paths ---
# Note: glmnet's lambda is different from the penalty term. We use 's' for prediction.
ridge_fit <- glmnet(x_vars, y_var, alpha = 0)
lasso_fit <- glmnet(x_vars, y_var, alpha = 1)


# --- Get OLS estimates for baseline ---
lm_fit <- lm(charges_scaled ~ age_scaled + bmi_scaled, data = insurance_df)
ols_estimates <- coef(lm_fit)
ols_age <- ols_estimates["age_scaled"]
ols_bmi <- ols_estimates["bmi_scaled"]


# ==============================================================================
#                      User Interface (UI)
# ==============================================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  titlePanel("Visualizing OLS, Ridge, and Lasso Regression"),

  sidebarLayout(
    sidebarPanel(
      h4("About this App"),
      p("This app visualizes the RSS surface and the constraints for Ridge and Lasso regression. It's a 3D version of the classic 2D contour plot."),
      p("The `s` slider controls the penalty strength. As `s` increases, the constraint region shrinks, pulling the solution (red dot) toward the origin."),

      hr(),
      h4("Model Parameters"),
      sliderInput("s_penalty", "Penalty Strength (s)",
                  min = 0, max = 0.5, value = 0.1, step = 0.005),

      hr(),
      h4("Plotting Parameters"),
      uiOutput("beta1_slider"),
      uiOutput("beta2_slider"),
      sliderInput("resolution", "Grid Resolution for Plot",
                  min = 10, max = 100, value = 40, step = 5)
    ),

    mainPanel(
      tabsetPanel(
        id = "regression_tabs",
        tabPanel("OLS Regression",
                 shinycssloaders::withSpinner(plotlyOutput("ols_plot", height = "600px"), type = 6, color = "#007bff")
        ),
        tabPanel("Ridge Regression",
                 shinycssloaders::withSpinner(plotlyOutput("ridge_plot", height = "600px"), type = 6, color = "#007bff")
        ),
        tabPanel("Lasso Regression",
                 shinycssloaders::withSpinner(plotlyOutput("lasso_plot", height = "600px"), type = 6, color = "#007bff")
        )
      )
    )
  )
)

# ==============================================================================
#                      Server Logic
# ==============================================================================
server <- function(input, output) {

  # --- Dynamically render sliders for coefficient ranges ---
  output$beta1_slider <- renderUI({
    range_span <- 1
    sliderInput("beta1_range", "Coefficient Range for scaled(age)",
                min = -range_span, max = range_span,
                value = c(-range_span, range_span), step = 0.05)
  })

  output$beta2_slider <- renderUI({
    range_span <- 1
    sliderInput("beta2_range", "Coefficient Range for scaled(bmi)",
                min = -range_span, max = range_span,
                value = c(-range_span, range_span), step = 0.05)
  })

  # --- Helper function to calculate RSS for any given beta coefficients ---
  calculate_rss <- function(b1, b2) {
    if (is.na(b1) || is.na(b2)) return(max(base_rss_data()$rss_matrix, na.rm = TRUE) * 2)

    residuals_no_intercept <- insurance_df$charges_scaled - (b1 * insurance_df$age_scaled + b2 * insurance_df$bmi_scaled)
    b0 <- mean(residuals_no_intercept)
    y_pred <- b0 + b1 * insurance_df$age_scaled + b2 * insurance_df$bmi_scaled
    sum((insurance_df$charges_scaled - y_pred)^2)
  }

  # --- Reactive expression to calculate the BASE RSS grid ---
  base_rss_data <- reactive({
    req(input$beta1_range, input$beta2_range)
    beta1_vals <- seq(input$beta1_range[1], input$beta1_range[2], length.out = input$resolution)
    beta2_vals <- seq(input$beta2_range[1], input$beta2_range[2], length.out = input$resolution)
    rss_matrix <- outer(beta1_vals, beta2_vals, Vectorize(calculate_rss))
    list(beta1_vals = beta1_vals, beta2_vals = beta2_vals, rss_matrix = t(rss_matrix))
  })

  # --- OLS Plot ---
  output$ols_plot <- renderPlotly({
    plot_data <- base_rss_data()
    ols_rss_val <- calculate_rss(ols_age, ols_bmi)

    plot_ly(x = ~plot_data$beta1_vals, y = ~plot_data$beta2_vals, z = ~plot_data$rss_matrix,
            type = "surface", colorscale = "Viridis", showscale = FALSE, name = 'RSS Surface') %>%
      # Add OLS marker
      add_trace(type = 'scatter3d', mode = 'markers', x = ols_age, y = ols_bmi,
                z = ols_rss_val,
                marker = list(color = 'red', size = 5), name = 'OLS Estimate') %>%
      # Add vertical line for OLS
      add_trace(type = 'scatter3d', mode = 'lines',
                x = c(ols_age, ols_age),
                y = c(ols_bmi, ols_bmi),
                z = c(min(plot_data$rss_matrix, na.rm = TRUE), ols_rss_val),
                line = list(color = 'red', width = 4), name = 'OLS Minimum') %>%
      layout(title = "OLS: RSS Surface",
             scene = list(xaxis = list(title = "β₁ (Coef for age)"),
                          yaxis = list(title = "β₂ (Coef for bmi)"),
                          zaxis = list(title = "RSS")),
             showlegend = TRUE)
  })

  # ============================================================================ #
  # MODIFIED FUNCTION: Creates Ridge/Lasso plots with 3D constraint volumes #
  # ============================================================================ #
  create_constrained_plot <- function(penalty_type) {
    plot_data <- base_rss_data()
    s_val <- input$s_penalty

    # Get model-specific info
    if (penalty_type == "ridge") {
      fit_model <- ridge_fit
      title_text <- "Ridge: RSS with L2 Constraint (Cylinder)"
      coefs <- predict(fit_model, s = s_val, type = 'coefficients')
      b1_sol <- coefs["age_scaled", 1]
      b2_sol <- coefs["bmi_scaled", 1]

      radius <- sqrt(b1_sol^2 + b2_sol^2)
      num_segments <- 50
      theta <- seq(0, 2 * pi, length.out = num_segments)
      x_circle <- radius * cos(theta)
      y_circle <- radius * sin(theta)

      z_min <- min(plot_data$rss_matrix, na.rm = TRUE)
      z_max <- max(plot_data$rss_matrix, na.rm = TRUE)

      x_verts <- c(x_circle, x_circle)
      y_verts <- c(y_circle, y_circle)
      z_verts <- c(rep(z_min, num_segments), rep(z_max, num_segments))

      faces <- list()
      for (i in 1:(num_segments - 1)) {
        faces[[length(faces) + 1]] <- c(i, i + 1, i + num_segments)
        faces[[length(faces) + 1]] <- c(i + num_segments, i + num_segments + 1, i + 1)
      }
      faces[[length(faces) + 1]] <- c(num_segments, 1, num_segments + num_segments)
      faces[[length(faces) + 1]] <- c(num_segments + num_segments, num_segments + 1, 1)

      for (i in 2:(num_segments - 1)) {
        faces[[length(faces) + 1]] <- c(1, i, i + 1)
      }
      for (i in (num_segments + 1 + 1):(num_segments * 2 - 1)) {
        faces[[length(faces) + 1]] <- c(num_segments + 1, i + 1, i)
      }
      faces_matrix <- do.call(rbind, faces) - 1

      constraint_data <- list(
        x = x_verts, y = y_verts, z = z_verts,
        i = faces_matrix[, 1], j = faces_matrix[, 2], k = faces_matrix[, 3]
      )

    } else { # Lasso
      fit_model <- lasso_fit
      title_text <- "Lasso: RSS with L1 Constraint (Cuboid)"
      coefs <- predict(fit_model, s = s_val, type = 'coefficients')
      b1_sol <- coefs["age_scaled", 1]
      b2_sol <- coefs["bmi_scaled", 1]

      sum_abs_coef <- abs(b1_sol) + abs(b2_sol)
      x_diamond_corners <- c(sum_abs_coef, 0, -sum_abs_coef, 0)
      y_diamond_corners <- c(0, sum_abs_coef, 0, -sum_abs_coef)

      z_min <- min(plot_data$rss_matrix, na.rm = TRUE)
      z_max <- max(plot_data$rss_matrix, na.rm = TRUE)

      x_verts <- c(x_diamond_corners, x_diamond_corners)
      y_verts <- c(y_diamond_corners, y_diamond_corners)
      z_verts <- c(rep(z_min, 4), rep(z_max, 4))

      faces_matrix <- matrix(c(0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7, 0, 4, 5, 0, 5, 1,
                               1, 5, 6, 1, 6, 2, 2, 6, 7, 2, 7, 3, 3, 7, 4, 3, 4, 0),
                             ncol = 3, byrow = TRUE)

      constraint_data <- list(
        x = x_verts, y = y_verts, z = z_verts,
        i = faces_matrix[, 1], j = faces_matrix[, 2], k = faces_matrix[, 3]
      )
    }

    sol_rss_val <- calculate_rss(b1_sol, b2_sol)
    ols_rss_val <- calculate_rss(ols_age, ols_bmi)

    p <- plot_ly() %>%
      add_trace(x = ~plot_data$beta1_vals, y = ~plot_data$beta2_vals, z = ~plot_data$rss_matrix,
                type = "surface", colorscale = "Viridis", showscale = FALSE, opacity = 0.6,
                name = 'RSS Surface') %>%
      # Add the 3D constraint volume
      add_trace(
        type = 'mesh3d',
        x = constraint_data$x, y = constraint_data$y, z = constraint_data$z,
        i = constraint_data$i, j = constraint_data$j, k = constraint_data$k,
        color = '#17becf', opacity = 0.3,
        name = paste0(penalty_type, ' Constraint')
      ) %>%
      # Add OLS marker
      add_trace(type = 'scatter3d', mode = 'markers', x = ols_age, y = ols_bmi, z = ols_rss_val,
                marker = list(color = 'red', size = 5), name = 'OLS Estimate') %>%
      # Add vertical line for OLS minimum
      add_trace(type = 'scatter3d', mode = 'lines',
                x = c(ols_age, ols_age), y = c(ols_bmi, ols_bmi),
                z = c(min(plot_data$rss_matrix, na.rm = TRUE), ols_rss_val),
                line = list(color = 'red', width = 4), name = 'OLS Minimum') %>%
      # Add regularized solution marker
      add_trace(type = 'scatter3d', mode = 'markers', x = b1_sol, y = b2_sol, z = sol_rss_val,
                marker = list(color = 'blue', size = 5, symbol = 'circle'), name = paste0(penalty_type, ' Estimate')) %>%
      # Add vertical line for regularized solution
      add_trace(type = 'scatter3d', mode = 'lines',
                x = c(b1_sol, b1_sol), y = c(b2_sol, b2_sol),
                z = c(min(plot_data$rss_matrix, na.rm = TRUE), sol_rss_val),
                line = list(color = 'blue', width = 4), name = paste0(penalty_type, ' Solution')) %>%
      layout(title = title_text,
             scene = list(xaxis = list(title = "β₁ (Coef for age)"),
                          yaxis = list(title = "β₂ (Coef for bmi)"),
                          zaxis = list(title = "RSS")),
             showlegend = TRUE)
    p
  }

  # --- Render Ridge and Lasso Plots ---
  output$ridge_plot <- renderPlotly({ create_constrained_plot("ridge") })
  output$lasso_plot <- renderPlotly({ create_constrained_plot("lasso") })
}

# Run the application
shinyApp(ui = ui, server = server)
