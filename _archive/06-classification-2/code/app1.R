# Interactive SVM Explorer Shiny App
# This app demonstrates concepts from Chapter 9 of "An Introduction to Statistical Learning".
# It allows users to interactively explore separating hyperplanes, maximal margin classifiers,
# polynomial transformations, the kernel trick, and various kernel functions.

# --- 1. Load Required Libraries ---
# Make sure you have these packages installed:
# install.packages(c("shiny", "ggplot2", "e1071", "MASS", "plotly"))

library(shiny)
library(ggplot2)
library(e1071)
library(MASS)
library(plotly)

# --- 2. Generate Synthetic Data ---
# We'll create two sets of data.
# `separable_data`: Linearly separable data for most tabs.
# `non_separable_data`: Data that is not linearly separable, to demonstrate the kernel trick.

set.seed(123)

# Linearly separable data
mean1 <- c(-1.5, -1.5)
cov1 <- matrix(c(1, 0.8, 0.8, 1), ncol = 2)
class1_data <- mvrnorm(n = 50, mu = mean1, Sigma = cov1)

mean2 <- c(1.5, 1.5)
cov2 <- matrix(c(1, 0.8, 0.8, 1), ncol = 2)
class2_data <- mvrnorm(n = 50, mu = mean2, Sigma = cov2)

separable_data <- as.data.frame(rbind(class1_data, class2_data))
names(separable_data) <- c("X1", "X2")
separable_data$Y <- factor(rep(c(-1, 1), each = 50))


# Non-linearly separable data (for the Kernel Trick tab)
set.seed(42)
n_points <- 200
radius_inner <- 2
radius_outer <- 4
angle <- runif(n_points, 0, 2 * pi)
radius <- c(rnorm(n_points / 2, radius_inner, 0.3), rnorm(n_points / 2, radius_outer, 0.3))
non_separable_data <- data.frame(
  X1 = radius * cos(angle),
  X2 = radius * sin(angle),
  Y = factor(rep(c(-1, 1), each = n_points / 2))
)


# --- 3. Shiny UI (User Interface) ---
ui <- navbarPage(
  "Interactive SVM Explorer",

  # --- Tab 1: Separating Hyperplane ---
  tabPanel("1. Separating Hyperplane",
           sidebarLayout(
             sidebarPanel(
               h4("Hyperplane Equation:"),
               p("β₀ + β₁X₁ + β₂X₂ = 0"),
               sliderInput("beta0", "Intercept (β₀):", min = -5, max = 5, value = 0, step = 0.1),
               sliderInput("beta1", "Coefficient (β₁):", min = -5, max = 5, value = -1, step = 0.1),
               sliderInput("beta2", "Coefficient (β₂):", min = -5, max = 5, value = 1, step = 0.1)
             ),
             mainPanel(
               plotOutput("hyperplanePlot", height = "550px")
             )
           )),

  # --- Tab 2: Maximal Margin Classifier ---
  tabPanel("2. Maximal Margin Classifier",
           sidebarLayout(
             sidebarPanel(
               p("This tab shows the optimal maximal margin hyperplane (solid black line) found by an SVM."),
               p("You can adjust the margin width of a sample hyperplane (blue) to see how it affects separation."),
               sliderInput("margin_width", "Margin Width:", min = 0.1, max = 5, value = 1, step = 0.1)
             ),
             mainPanel(
               plotOutput("marginPlot", height = "550px")
             )
           )),

  # --- Tab 3: Polynomial Transformations ---
  tabPanel("3. Polynomial Kernel",
           sidebarLayout(
             sidebarPanel(
               p("Fit a Support Vector Machine with a polynomial kernel to see non-linear decision boundaries."),
               sliderInput("poly_degree", "Polynomial Degree:", min = 1, max = 10, value = 2, step = 1),
               sliderInput("poly_cost", "Cost Parameter:", min = 0.1, max = 100, value = 1, step = 0.1)
             ),
             mainPanel(
               plotOutput("polyPlot", height = "550px")
             )
           )),

  # --- Tab 4: The Kernel Trick ---
  tabPanel("4. The Kernel Trick",
           mainPanel(
             h4("Visualizing the Kernel Trick"),
             p("The data on the left is not linearly separable in 2D. A polynomial kernel implicitly maps the data to a higher dimension (right) where it becomes linearly separable by a plane."),
             fluidRow(
               splitLayout(
                 plotOutput("kernel2dPlot", height = "550px"),
                 plotlyOutput("kernel3dPlot", height = "550px")
               )
             )
           ))
)

# --- 4. Shiny Server (Logic) ---
server <- function(input, output) {

  # --- Tab 1 Logic: Separating Hyperplane ---
  output$hyperplanePlot <- renderPlot({
    b0 <- input$beta0
    b1 <- input$beta1
    b2 <- input$beta2

    ggplot(separable_data, aes(x = X1, y = X2, color = Y)) +
      geom_point(size = 3) +
      geom_abline(intercept = -b0 / b2, slope = -b1 / b2, color = "blue", size = 1.2) +
      scale_color_manual(values = c("-1" = "salmon", "1" = "skyblue")) +
      labs(title = "Linearly Separable Data with Custom Hyperplane",
           x = "Feature X₁", y = "Feature X₂") +
      coord_cartesian(xlim = range(separable_data$X1), ylim = range(separable_data$X2)) +
      theme_minimal(base_size = 16)
  })

  # --- Tab 2 Logic: Maximal Margin Classifier ---
  output$marginPlot <- renderPlot({
    # Fit the actual SVM to find the maximal margin
    svm_model <- svm(Y ~ ., data = separable_data, kernel = "linear", cost = 1000)

    # Extract coefficients for plotting the maximal margin hyperplane
    w <- t(svm_model$coefs) %*% svm_model$SV
    b_svm <- -svm_model$rho

    ggplot(separable_data, aes(x = X1, y = X2, color = Y)) +
      geom_point(size = 3) +
      # Plot maximal margin hyperplane (black)
      geom_abline(intercept = b_svm / w[1, 2], slope = -w[1, 1] / w[1, 2],
                  color = "black", size = 1.2, linetype = "solid") +
      # Plot user-defined hyperplane (blue)
      #geom_abline(intercept = 0.5, slope = -1, color = "blue", size = 1, linetype = "dashed") +
      # Plot user-defined margins
      geom_abline(intercept = 0.5 + input$margin_width * sqrt(2), slope = -w[1, 1] / w[1, 2],
                  color = "blue", linetype = "dotted", size = 1) +
      geom_abline(intercept = 0.5 - input$margin_width * sqrt(2), slope = -w[1, 1] / w[1, 2],
                  color = "blue", linetype = "dotted", size = 1) +
      scale_color_manual(values = c("-1" = "salmon", "1" = "skyblue")) +
      labs(title = "Maximal Margin Classifier (Black) vs. Sample Hyperplane (Blue)",
           x = "Feature X₁", y = "Feature X₂") +
      coord_cartesian(xlim = range(separable_data$X1), ylim = range(separable_data$X2)) +
      theme_minimal(base_size = 16)
  })

  # --- Tab 3 & 5 Logic: Utility function for plotting decision boundaries ---
  plot_svm_boundary <- function(data, svm_model, title) {
    grid_range <- apply(data[, 1:2], 2, range)
    x1_grid <- seq(from = grid_range[1, 1], to = grid_range[2, 1], length.out = 500)
    x2_grid <- seq(from = grid_range[1, 2], to = grid_range[2, 2], length.out = 500)
    grid_data <- expand.grid(X1 = x1_grid, X2 = x2_grid)

    grid_preds <- predict(svm_model, grid_data)

    ggplot() +
      geom_point(data = grid_data, aes(x = X1, y = X2, color = grid_preds), size = 0.5, alpha = 0.2) +
      geom_point(data = data, aes(x = X1, y = X2, color = Y), size = 3) +
      scale_color_manual(values = c("-1" = "salmon", "1" = "skyblue")) +
      guides(color = "none") +
      labs(title = title, x = "Feature X₁", y = "Feature X₂") +
      theme_minimal(base_size = 16)
  }

  # --- Tab 3 Logic: Polynomial Transformations ---
  output$polyPlot <- renderPlot({
    svm_poly <- svm(Y ~ ., data = separable_data, kernel = "polynomial",
                    degree = input$poly_degree, cost = input$poly_cost)
    plot_svm_boundary(separable_data, svm_poly,
                      paste("SVM with Polynomial Kernel (Degree =", input$poly_degree, ")"))
  })

  # --- Tab 4 Logic: The Kernel Trick ---
  output$kernel2dPlot <- renderPlot({
    ggplot(non_separable_data, aes(x = X1, y = X2, color = Y)) +
      geom_point(size = 3) +
      scale_color_manual(values = c("-1" = "salmon", "1" = "skyblue")) +
      labs(title = "Original 2D Space (Not Linearly Separable)",
           x = "Feature X₁", y = "Feature X₂") +
      theme_minimal(base_size = 16)
  })

  output$kernel3dPlot <- renderPlotly({
    # Kernel transformation: (x1, x2) -> (x1^2, x2^2, sqrt(2)*x1*x2)
    transformed_data <- data.frame(
      Z1 = non_separable_data$X1^2,
      Z2 = non_separable_data$X2^2,
      Z3 = sqrt(2) * non_separable_data$X1 * non_separable_data$X2,
      Y = non_separable_data$Y
    )

    plot_ly(transformed_data, x = ~Z1, y = ~Z2, z = ~Z3, color = ~Y,
            colors = c("salmon", "skyblue"), type = "scatter3d", mode = "markers") %>%
      layout(title = "Transformed 3D Space (Linearly Separable)")
  })


}

# --- 5. Run the Shiny App ---
shinyApp(ui = ui, server = server)

