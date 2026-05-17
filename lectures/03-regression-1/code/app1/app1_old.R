# Load necessary libraries
library(shiny)
library(leaps)
library(ggplot2)
library(plotly)

# Define the user interface (UI)
ui <- navbarPage(
  "Subset Selection Methods in Regression",

  # --- Tab 1: Data Upload and Instructions ---
  tabPanel("Data Upload & Instructions",
           sidebarLayout(
             sidebarPanel(
               # File input for user to upload their data
               fileInput("file_upload", "Upload Your CSV Data",
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               tags$hr(),
               p("Please ensure your data is in CSV format."),
               p("The first column of the dataset will be treated as the outcome (dependent) variable, and all other columns will be treated as the covariates (independent variables)."),
               tags$hr(),
               h4("Example Data"),
               p("If you don't have a dataset, you can use the built-in 'mtcars' dataset to explore the app's functionality."),
               actionButton("load_demo_data", "Load mtcars Dataset")
             ),
             mainPanel(
               h3("Welcome to the Subset Selection Demonstrator!"),
               p("This application is designed to help students and practitioners understand and compare different methods for selecting the best subset of predictors in a linear regression model."),
               p("1. Upload your dataset using the panel on the left, or load the demo 'mtcars' data."),
               p("2. Once the data is loaded, a preview will appear below."),
               p("3. Navigate to the other tabs to see the results of each selection method applied to your data."),
               tags$hr(),
               h4("Data Preview"),
               # Show a preview of the uploaded data
               tableOutput("data_head")
             )
           )
  ),

  # --- Tab 2: Best Subset Selection ---
  tabPanel("Best Subset Selection",
           sidebarLayout(
             sidebarPanel(
               h4("Example R Code"),
               p("This is the basic R code structure for performing best subset selection using the 'leaps' package."),
               verbatimTextOutput("best_subset_code")
             ),
             mainPanel(
               h3("Best Subset Selection"),
               p("Best Subset Selection is the most exhaustive method for choosing predictors. It fits a separate least squares regression for each possible combination of the p predictors. This means it evaluates all pC1 models with one predictor, all pC2 models with two predictors, and so on, up to the single model with all p predictors. In total, it considers 2^p models."),
               p(strong("Pros:"), " It is guaranteed to find the model with the lowest residual sum of squares (RSS) for each subset size."),
               p(strong("Cons:"), " It can be computationally expensive and infeasible for datasets with a large number of predictors (p > 40). It can also suffer from overfitting, especially when p is large."),
               tags$hr(),
               h4("Interactive Model Selection Plots"),
               p("These plots help you decide the optimal number of variables to include. The red dot indicates the model that optimizes each criterion. Hover over points to see their values."),
               plotlyOutput("best_subset_plot", height = "600px"),
               tags$hr(),
               h4("Model Summary"),
               p("The summary below shows which variables are included in the best model for a given subset size. An asterisk (*) indicates that a variable is included in the model."),
               verbatimTextOutput("best_subset_summary")
             )
           )
  ),

  # --- Tab 3: Forward Stepwise Selection ---
  tabPanel("Forward Stepwise Selection",
           sidebarLayout(
             sidebarPanel(
               h4("Example R Code"),
               p("This is the basic R code structure for performing forward stepwise selection."),
               verbatimTextOutput("forward_code")
             ),
             mainPanel(
               h3("Forward Stepwise Selection"),
               p("Forward Stepwise Selection is a more computationally efficient alternative to best subset selection. It begins with a null model (a model containing no predictors) and then iteratively adds one predictor at a time. At each step, it adds the predictor that provides the greatest additional improvement to the model fit (e.g., results in the lowest RSS)."),
               p(strong("Pros:"), " It is computationally much faster than best subset selection and can be used when p is very large."),
               p(strong("Cons:"), " It is a greedy algorithm, meaning it makes the best choice at each step. This does not guarantee that it will find the best possible overall model out of all 2^p models."),
               tags$hr(),
               h4("Interactive Model Selection Plots"),
               plotlyOutput("forward_plot", height = "600px"),
               tags$hr(),
               h4("Model Summary"),
               verbatimTextOutput("forward_summary")
             )
           )
  ),

  # --- Tab 4: Backward Stepwise Selection ---
  tabPanel("Backward Stepwise Selection",
           sidebarLayout(
             sidebarPanel(
               h4("Example R Code"),
               p("This is the basic R code structure for performing backward stepwise selection."),
               verbatimTextOutput("backward_code")
             ),
             mainPanel(
               h3("Backward Stepwise Selection"),
               p("Backward Stepwise Selection starts with the full model, which contains all p predictors. It then iteratively removes the least useful predictor, one at a time. At each step, it removes the predictor that has the smallest impact on the model fit (e.g., the one with the highest p-value)."),
               p(strong("Pros:"), " Like forward selection, it is much faster than best subset selection. It's often preferred over forward selection when the number of predictors p is not much larger than the number of observations n."),
               p(strong("Cons:"), " It is also a greedy algorithm and is not guaranteed to find the optimal overall model. It cannot be used if the number of predictors p is greater than the number of observations n."),
               tags$hr(),
               h4("Interactive Model Selection Plots"),
               plotlyOutput("backward_plot", height = "600px"),
               tags$hr(),
               h4("Model Summary"),
               verbatimTextOutput("backward_summary")
             )
           )
  )
)

# Define the server logic
server <- function(input, output, session) {

  # --- Reactive Data Handling ---
  data_holder <- reactiveVal()

  observeEvent(input$file_upload, {
    req(input$file_upload)
    tryCatch({
      df <- read.csv(input$file_upload$datapath, header = TRUE)
      if(ncol(df) < 2) {
        stop("Data must have at least one outcome and one predictor column.")
      }
      data_holder(df)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error", duration = 5)
    })
  })

  observeEvent(input$load_demo_data, {
    data_holder(mtcars)
  })

  # --- Data Preview Output ---
  output$data_head <- renderTable({
    req(data_holder())
    head(data_holder())
  })

  # --- Core Modeling Logic ---
  models <- reactive({
    req(data_holder())
    df <- data_holder()

    # Ensure data is numeric for regression, show notification for conversion
    converted_cols <- c()
    df[] <- lapply(names(df), function(col_name) {
      if(is.character(df[[col_name]]) || is.factor(df[[col_name]])) {
        converted_cols <<- c(converted_cols, col_name)
        as.numeric(as.factor(df[[col_name]]))
      } else {
        df[[col_name]]
      }
    })

    if (length(converted_cols) > 0) {
      showNotification(paste("Converted non-numeric columns to factors:", paste(converted_cols, collapse=", ")),
                       type="warning", duration=8)
    }

    y_name <- names(df)[1]
    formula_str <- paste(y_name, "~ .")
    formula_obj <- as.formula(formula_str)

    # Run the three types of subset selection
    best_subset <- regsubsets(formula_obj, data = df, nvmax = ncol(df) - 1, really.big = TRUE)
    forward <- regsubsets(formula_obj, data = df, nvmax = ncol(df) - 1, method = "forward")
    backward <- regsubsets(formula_obj, data = df, nvmax = ncol(df) - 1, method = "backward")

    list(best = best_subset, forward = forward, backward = backward)
  })

  # --- Reusable Interactive Plotting Function ---
  render_selection_plotly <- function(model_summary) {
    # Prepare data for plotting
    plot_data <- data.frame(
      num_vars = 1:length(model_summary$rss),
      rss = model_summary$rss,
      adjr2 = model_summary$adjr2,
      cp = model_summary$cp,
      bic = model_summary$bic
    )

    theme_custom <- theme_bw() + theme(plot.title = element_text(hjust = 0.5))

    # Plot 1: RSS
    p_rss <- ggplot(plot_data, aes(x = num_vars, y = rss, text = paste("Vars:", num_vars, "<br>RSS:", round(rss, 2)))) +
      geom_line(color="steelblue") + geom_point(color="steelblue") +
      geom_point(data = plot_data[which.min(plot_data$rss), ], color = "red", size = 3) +
      labs(x = "Number of Variables", y = "RSS") + theme_custom

    # Plot 2: Adjusted R^2
    p_adjr2 <- ggplot(plot_data, aes(x = num_vars, y = adjr2, text = paste("Vars:", num_vars, "<br>Adj R2:", round(adjr2, 3)))) +
      geom_line(color="steelblue") + geom_point(color="steelblue") +
      geom_point(data = plot_data[which.max(plot_data$adjr2), ], color = "red", size = 3) +
      labs(x = "Number of Variables", y = "Adjusted R²") + theme_custom

    # Plot 3: Mallows' Cp
    p_cp <- ggplot(plot_data, aes(x = num_vars, y = cp, text = paste("Vars:", num_vars, "<br>Cp:", round(cp, 2)))) +
      geom_line(color="steelblue") + geom_point(color="steelblue") +
      geom_point(data = plot_data[which.min(plot_data$cp), ], color = "red", size = 3) +
      labs(x = "Number of Variables", y = "Cp") + theme_custom

    # Plot 4: BIC
    p_bic <- ggplot(plot_data, aes(x = num_vars, y = bic, text = paste("Vars:", num_vars, "<br>BIC:", round(bic, 2)))) +
      geom_line(color="steelblue") + geom_point(color="steelblue") +
      geom_point(data = plot_data[which.min(plot_data$bic), ], color = "red", size = 3) +
      labs(x = "Number of Variables", y = "BIC") + theme_custom

    # Combine plots into an interactive subplot
    subplot(
      ggplotly(p_rss, tooltip = "text"),
      ggplotly(p_adjr2, tooltip = "text"),
      ggplotly(p_cp, tooltip = "text"),
      ggplotly(p_bic, tooltip = "text"),
      nrows = 2, shareX = TRUE, titleY = TRUE
    ) %>% layout(showlegend = FALSE)
  }

  # --- R Code Skeletons ---
  output$best_subset_code <- renderText({
    paste(
      "library(leaps)",
      "",
      "# Assuming 'my_data' is your dataframe",
      "# with the first column as the outcome",
      "best_model <- regsubsets(",
      "  outcome ~ .,",
      "  data = my_data,",
      "  nvmax = ncol(my_data) - 1",
      ")",
      "",
      "summary(best_model)",
      sep = "\n"
    )
  })

  output$forward_code <- renderText({
    paste(
      "library(leaps)",
      "",
      "forward_model <- regsubsets(",
      "  outcome ~ .,",
      "  data = my_data,",
      "  nvmax = ncol(my_data) - 1,",
      "  method = \"forward\"",
      ")",
      "",
      "summary(forward_model)",
      sep = "\n"
    )
  })

  output$backward_code <- renderText({
    paste(
      "library(leaps)",
      "",
      "backward_model <- regsubsets(",
      "  outcome ~ .,",
      "  data = my_data,",
      "  nvmax = ncol(my_data) - 1,",
      "  method = \"backward\"",
      ")",
      "",
      "summary(backward_model)",
      sep = "\n"
    )
  })

  # --- Outputs for Best Subset Tab ---
  output$best_subset_plot <- renderPlotly({
    req(models())
    render_selection_plotly(summary(models()$best))
  })
  output$best_subset_summary <- renderPrint({
    req(models())
    summary(models()$best)
  })

  # --- Outputs for Forward Stepwise Tab ---
  output$forward_plot <- renderPlotly({
    req(models())
    render_selection_plotly(summary(models()$forward))
  })
  output$forward_summary <- renderPrint({
    req(models())
    summary(models()$forward)
  })

  # --- Outputs for Backward Stepwise Tab ---
  output$backward_plot <- renderPlotly({
    req(models())
    render_selection_plotly(summary(models()$backward))
  })
  output$backward_summary <- renderPrint({
    req(models())
    summary(models()$backward)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

