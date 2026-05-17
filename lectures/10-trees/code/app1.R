# Load necessary libraries
library(shiny)
library(rpart)
library(ggplot2)
library(RColorBrewer)
library(scales) # For scales::col_numeric

# We create data with a clear rectangular structure that a tree can find.
generate_data <- function(n, noise_sd) {
  # X1 and X2 are our features
  X1 <- runif(n, 0, 10)
  X2 <- runif(n, 0, 10)

  # Y is our continuous outcome.
  # We'll define Y based on which quadrant the point falls in.
  Y <- 10 # Base value (bottom-left quadrant)
  Y <- Y + ifelse(X1 > 5, 15, 0)  # Right half
  Y <- Y + ifelse(X2 > 6, 20, 0)  # Top half
  Y <- Y + ifelse(X1 > 5 & X2 > 2, -10, 0) # An interaction

  # Add random noise
  Y <- Y + rnorm(n, 0, noise_sd)

  data.frame(X1, X2, Y)
}


# --- 2. Define the User Interface (UI) ---
ui <- fluidPage(
  titlePanel("Interactive Demo: Recursive Binary Splitting"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Simulation Controls"),
      p("Generate new data to see how the tree adapts."),
      actionButton("resample", "Generate New Data", icon = icon("refresh")),
      sliderInput("n_points", "Number of Data Points:",
                  min = 100, max = 1000, value = 300, step = 50),
      sliderInput("noise_sd", "Noise Level (Std. Dev):",
                  min = 0, max = 15, value = 4, step = 0.5),

      hr(),

      h4("Tree Controls"),
      p("Control the tree's complexity. 'Max Depth' shows the recursive splitting."),
      sliderInput("max_depth", "Max Tree Depth:",
                  min = 0, max = 10, value = 2, step = 1),
      sliderInput("min_bucket", "Min. Points per Leaf:",
                  min = 1, max = 50, value = 10),

      hr(),
      uiOutput("explanation_text")
    ),

    mainPanel(
      width = 9,
      plotOutput("tree_plot", height = "700px")
    )
  )
)


# --- 3. Define the Server Logic ---
server <- function(input, output) {

  # --- Reactive Data Generation ---
  # This reactive expression generates new data when the button is clicked
  reactive_data <- eventReactive(input$resample, {
    generate_data(input$n_points, input$noise_sd)
  }, ignoreNULL = FALSE) # ignoreNULL=FALSE triggers on app start


  # --- Reactive Tree Fitting ---
  # This reactive expression re-fits the tree model whenever the
  # data or the tree control sliders change.
  reactive_tree <- reactive({
    df <- reactive_data()

    # Handle Depth 0 case (no splits, just the root)
    if (input$max_depth == 0) {
      return(NULL)
    }

    # Fit the regression tree using "anova" method
    rpart(
      formula = Y ~ X1 + X2,
      data = df,
      method = "anova",
      control = rpart.control(
        maxdepth = input$max_depth,
        minbucket = input$min_bucket,
        cp = 0  # Set complexity parameter to 0 to let maxdepth be the main control
      )
    )
  })


  # --- Render the Main Plot ---
  output$tree_plot <- renderPlot({
    df <- reactive_data()
    model <- reactive_tree()

    # Define a consistent color palette for Y
    y_range <- range(df$Y)
    color_pal <- RColorBrewer::brewer.pal(11, "RdYlBu")

    # Base plot: The scatter plot of actual data
    g <- ggplot(df, aes(x = X1, y = X2)) +
      geom_point(aes(color = Y), size = 2.5, alpha = 0.8) +
      scale_color_gradientn(
        colors = color_pal,
        name = "Y (Actual)",
        limits = y_range
      ) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 10)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")


    if (is.null(model)) {
      # --- Plot for Depth 0 (Global Mean) ---
      # No splits, so the "prediction" is just the global mean.
      mean_y <- mean(df$Y)

      # We need a color function to map the mean_y to the palette
      mean_y_color <- scales::col_numeric(color_pal, domain = y_range)(mean_y)

      g <- g +
        geom_rect(
          aes(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
          fill = mean_y_color,
          alpha = 0.5
        ) +
        labs(title = "Depth 0: Predicting the Global Mean",
             subtitle = paste("Predicted Value (Mean Y) =", round(mean_y, 2)))

    } else {
      # --- Plot for Depth > 0 (Splits) ---
      # Create a fine grid to get the predicted value for every pixel
      grid_x <- seq(0, 10, length.out = 200)
      grid_y <- seq(0, 10, length.out = 200)
      grid <- expand.grid(X1 = grid_x, X2 = grid_y)

      # Get predictions from the model for the entire grid
      grid$PredY <- predict(model, newdata = grid)

      # Add the predicted regions as a background raster
      g <- g +
        geom_raster(
          data = grid,
          aes(x = X1, y = X2, fill = PredY),
          alpha = 0.6,
          interpolate = FALSE # FALSE keeps the sharp rectangle edges
        ) +
        # Use the *same* color scale for the fill
        scale_fill_gradientn(
          colors = color_pal,
          name = "Y (Predicted)",
          limits = y_range,
          guide = "none" # Hide this legend (it's redundant)
        ) +
        labs(title = paste("Regression Tree with Max Depth =", input$max_depth),
             subtitle = "The background shows the tree's predicted value for each region.")
    }

    return(g)
  })


  # --- Render the Explanation Text ---
  output$explanation_text <- renderUI({
    depth <- input$max_depth

    if (depth == 0) {
      HTML(paste("<b>Depth 0:</b> The model is just the 'root'.",
                 "It finds the single best value to predict for all points:",
                 "the <b>global mean</b> of Y."))
    } else if (depth == 1) {
      HTML(paste("<b>Depth 1:</b> The model makes its <b>first binary split</b>.",
                 "It scans both X1 and X2 to find the single best split-point",
                 "(e.g., 'X1 > 4.8') that minimizes the Residual Sum of Squares (RSS).",
                 "This creates two new regions (leaves)."))
    } else {
      HTML(paste0("<b>Depth ", depth, ":</b> The model <b>recursively</b> splits",
                  " the regions created by the previous levels.",
                  " Each new split is chosen to best reduce the RSS *within that specific region*.",
                  " This continues until the 'Max Depth' or 'Min. Points' limit is hit."))
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
