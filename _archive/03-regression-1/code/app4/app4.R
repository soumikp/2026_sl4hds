rm(list = ls())
source(file.path(here::here(), "class3", "code", "helper.R"))

# Load necessary packages
pacman::p_load(shiny, plotly, MASS, glmnet, pls, leaps)

# ==============================================================================
# Shiny UI (User Interface)
# ==============================================================================
ui <- fluidPage(
  # App title
  titlePanel("Comparing Regularized Regression Methods"),

  # Sidebar layout with input controls
  sidebarLayout(
    sidebarPanel(
      h4("Model Parameters"),
      # Slider for correlation rho
      sliderInput(
        inputId = "rho",
        label = "Correlation (ρ) between X1 and X2:",
        min = -0.99,
        max = 0.99,
        value = 0.5,
        step = 0.01
      ),
      # Slider for sample size n
      sliderInput(
        inputId = "n",
        label = "Sample Size (n):",
        min = 50,
        max = 1000,
        value = 100,
        step = 50
      ),
      # Numeric input for error variance
      numericInput(
        inputId = "error_var",
        label = "Error Variance (σ²):",
        value = 4,
        min = 0.1,
        max = 50,
        step = 0.5
      ),
      hr(),
      # Action button to trigger data generation
      actionButton("regen_data", "Generate Data / Update Plots", icon = icon("refresh"), class = "btn-primary"),
      hr(),
      p("This app visualizes the effect of model parameters on various regression techniques. All data is centered and scaled, and no intercept term is fitted. Optimal models are selected via 10-fold cross-validation.")
    ),

    # Main panel for displaying outputs
    mainPanel(
      # Create a row for side-by-side panels
      fluidRow(
        # Left panel for the 3D scatterplot
        column(width = 6,
               h3("3D Scatterplot"),
               plotlyOutput("scatterplot")
        ),
        # Right panel for the coefficient paths
        column(width = 6,
               h3("Coefficient Paths"),
               plotOutput("pathplot")
        )
      )
    )
  )
)


# ==============================================================================
# Shiny Server (Backend Logic)
# ==============================================================================
server <- function(input, output) {

  # An eventReactive expression to generate data and fit models ONLY when the button is clicked.
  # ignoreNULL = FALSE ensures it runs once on startup.
  models_data <- eventReactive(input$regen_data, {
    # For reproducibility with the same inputs
    set.seed(Sys.time()) # Use current time for more randomness on each click

    # --- 1. Generate and Pre-process Data ---
    n <- input$n
    rho <- input$rho
    beta_true <- c(8, 2)

    cov_matrix <- matrix(c(1, rho, rho, 1), nrow = 2)
    X <- mvrnorm(n, mu = c(0, 0), Sigma = cov_matrix)

    error_sd <- sqrt(input$error_var)
    epsilon <- rnorm(n, mean = 0, sd = error_sd)
    Y <- X %*% beta_true + epsilon

    # === PRE-PROCESSING STEP ===
    X <- scale(X)
    colnames(X) <- c("X1", "X2")

    # --- 2. Fit Models and Find Optimal Points via Cross-Validation ---

    # -- OLS --
    fit_ols <- lm(Y ~ X - 1)
    coef_ols <- coef(fit_ols)

    # -- Ridge --
    fit_ridge <- glmnet(X, Y, alpha = 0, standardize = FALSE, intercept = FALSE)
    coef_ridge_path <- t(as.matrix(fit_ridge$beta))
    cv_fit_ridge <- cv.glmnet(X, Y, alpha = 0, standardize = FALSE, intercept = FALSE)
    coef_ridge_opt <- as.vector(coef(cv_fit_ridge, s = "lambda.min"))[-1]

    # -- Lasso --
    fit_lasso <- glmnet(X, Y, alpha = 1, standardize = FALSE, intercept = FALSE)
    coef_lasso_path <- t(as.matrix(fit_lasso$beta))
    cv_fit_lasso <- cv.glmnet(X, Y, alpha = 1, standardize = FALSE, intercept = FALSE)
    coef_lasso_opt <- as.vector(coef(cv_fit_lasso, s = "lambda.min"))[-1]

    # -- PLS --
    fit_pls <- plsr(Y ~ X, ncomp = 2, validation = "CV")
    coef_pls_1 <- coef(fit_pls, ncomp = 1)
    coef_pls_2 <- coef(fit_pls, ncomp = 2)
    coef_pls_path <- rbind(c(0, 0), c(coef_pls_1), c(coef_pls_2))
    cv_results_pls <- RMSEP(fit_pls)
    opt_ncomp_pls <- which.min(cv_results_pls$val[1, ,-1])
    coef_pls_opt <- c(coef(fit_pls, ncomp = opt_ncomp_pls))

    # -- Best Subset --
    # Path is based on 1-var model and then full OLS
    rss1 <- sum(resid(lm(Y ~ X[, 1] - 1))^2)
    rss2 <- sum(resid(lm(Y ~ X[, 2] - 1))^2)
    if (rss1 < rss2) {
      coef_bs_1 <- c(coef(lm(Y ~ X[, 1] - 1)), 0)
    } else {
      coef_bs_1 <- c(0, coef(lm(Y ~ X[, 2] - 1)))
    }
    coef_bs_path <- rbind(c(0, 0), coef_bs_1, coef_ols)

    # Optimal Best Subset (via manual 10-fold Cross-Validation)
    k <- 10
    folds <- sample(1:k, n, replace = TRUE)
    cv_errors <- matrix(NA, k, 2, dimnames = list(NULL, paste(1:2)))

    for (j in 1:k) {
      test_indices <- which(folds == j)
      train_X <- X[-test_indices, ]
      train_Y <- Y[-test_indices]
      test_X <- X[test_indices, ]
      test_Y <- Y[test_indices]

      fit_bs_cv <- regsubsets(x = train_X, y = train_Y, nvmax = 2, intercept = FALSE)

      for (i in 1:2) { # For model sizes 1 and 2
        coef_raw <- coef(fit_bs_cv, id = i)
        coef_full <- rep(0, 2); names(coef_full) <- c("X1", "X2")
        coef_full[names(coef_raw)] <- coef_raw

        pred <- test_X %*% coef_full
        cv_errors[j, i] <- mean((test_Y - pred)^2)
      }
    }

    mean_cv_errors <- apply(cv_errors, 2, mean)
    opt_size_bs <- which.min(mean_cv_errors)

    fit_bs_final <- regsubsets(x = X, y = Y, nvmax = 2, intercept = FALSE)
    coef_bs_opt_raw <- coef(fit_bs_final, id = opt_size_bs)
    coef_bs_opt <- rep(0, 2); names(coef_bs_opt) <- c("X1", "X2")
    coef_bs_opt[names(coef_bs_opt_raw)] <- coef_bs_opt_raw

    # Return a list containing all data and model coefficients
    list(
      X = X, Y = Y,
      coef_ols = coef_ols,
      coef_ridge_path = coef_ridge_path, coef_lasso_path = coef_lasso_path,
      coef_pls_path = coef_pls_path, coef_bs_path = coef_bs_path,
      coef_ridge_opt = coef_ridge_opt, coef_lasso_opt = coef_lasso_opt,
      coef_pls_opt = coef_pls_opt, coef_bs_opt = coef_bs_opt
    )
  }, ignoreNULL = FALSE) # End of eventReactive

  # --- 3. Render Outputs ---
  output$scatterplot <- renderPlotly({
    data <- models_data()
    plot_df <- data.frame(X1 = data$X[, 1], X2 = data$X[, 2], Y = as.vector(data$Y))
    plot_ly(data = plot_df, x = ~X1, y = ~X2, z = ~Y, type = 'scatter3d', mode = 'markers',
            marker = list(size = 4, color = ~Y, colorscale = 'Viridis', opacity = 0.8)) %>%
      layout(title = "Data (X Scaled)", scene = list(xaxis = list(title = 'X1'), yaxis = list(title = 'X2'), zaxis = list(title = 'Y')))
  })

  output$pathplot <- renderPlot({
    models <- models_data()

    all_coefs <- rbind(models$coef_ols, models$coef_ridge_path, models$coef_lasso_path, models$coef_pls_path,
                       models$coef_bs_path, models$coef_ridge_opt, models$coef_lasso_opt,
                       models$coef_pls_opt, models$coef_bs_opt)
    xlim <- range(all_coefs[, 1], 0, na.rm = TRUE) + c(-0.2, 0.2)
    ylim <- range(all_coefs[, 2], 0, na.rm = TRUE) + c(-0.2, 0.2)

    plot(0, 0, type = "n", xlim = xlim, ylim = ylim, xlab = expression(beta[1]), ylab = expression(beta[2]),
         main = "Coefficient Estimation Paths", cex.lab = 1.2)
    grid()
    abline(h = 0, lty = 3, col = "gray50"); abline(v = 0, lty = 3, col = "gray50")

    # Plot paths
    lines(models$coef_bs_path[, 1], models$coef_bs_path[, 2], col = "blue", lwd = 2.5)
    lines(models$coef_lasso_path[, 1], models$coef_lasso_path[, 2], col = "green", lwd = 2.5)
    lines(models$coef_pls_path[, 1], models$coef_pls_path[, 2], col = "orange", lwd = 2.5)
    lines(models$coef_ridge_path[, 1], models$coef_ridge_path[, 2], col = "red", lwd = 2.5)

    # Plot OLS point
    points(models$coef_ols[1], models$coef_ols[2], pch = 19, cex = 2, col = "black")

    # Plot OPTIMAL points
    points(models$coef_bs_opt[1], models$coef_bs_opt[2], pch = 15, cex = 2.5, col = "blue")      # Square
    points(models$coef_lasso_opt[1], models$coef_lasso_opt[2], pch = 17, cex = 2, col = "green")  # Triangle
    points(models$coef_pls_opt[1], models$coef_pls_opt[2], pch = 18, cex = 2.5, col = "orange")   # Diamond
    points(models$coef_ridge_opt[1], models$coef_ridge_opt[2], pch = 8, cex = 2.5, col = "red")      # Star

    # Updated Legend
    legend("topleft",
           legend = c("Best Subset", "Lasso", "PLS", "Ridge", "Least Squares", "Optimal BS (CV)",
                      "Optimal Lasso (CV)", "Optimal PLS (CV)", "Optimal Ridge (CV)"),
           col = c("blue", "green", "orange", "red", "black", "blue", "green", "orange", "red"),
           lwd = c(2.5, 2.5, 2.5, 2.5, NA, NA, NA, NA, NA),
           pch = c(NA, NA, NA, NA, 19, 15, 17, 18, 8),
           pt.cex = c(NA, NA, NA, NA, 1.5, 2, 1.5, 2, 2),
           bty = "n", cex = 1.0)
  })
}

# ==============================================================================
# Run the Shiny Application
# ==============================================================================
shinyApp(ui = ui, server = server)
