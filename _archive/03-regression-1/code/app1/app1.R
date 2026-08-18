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
               tags$hr(),
               h4("Animation of Selection Process"),
               p("This animation shows the best set of coefficients for a given model size. Use the slider below to step through the process. Press the play button for an animation."),
               uiOutput("best_subset_slider_ui"),
               plotlyOutput("best_subset_animation_plot"),
               verbatimTextOutput("best_subset_animation_text"),
               tags$hr(),
               h4("Interactive Model Selection Plots"),
               p("These plots help you decide the optimal number of variables to include. The red dot indicates the model that optimizes each criterion. Hover over points to see their values."),
               plotlyOutput("best_subset_plot", height = "600px"),
               tags$hr(),
               h4("Model Summary"),
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
               p("Forward Stepwise Selection is a more computationally efficient alternative. It begins with a null model and then iteratively adds one predictor at a time that provides the greatest additional improvement to the model fit."),
               tags$hr(),
               h4("Animation of Selection Process"),
               p("This animation shows which variable is added to the model at each step. Use the slider and play button to see the process."),
               uiOutput("forward_slider_ui"),
               plotlyOutput("forward_animation_plot"),
               verbatimTextOutput("forward_animation_text"),
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
               p("Backward Stepwise Selection starts with the full model containing all predictors. It then iteratively removes the least useful predictor, one at a time."),
               tags$hr(),
               h4("Animation of Selection Process"),
               p("This animation shows which variable is removed from the model at each step. Use the slider and play button to see the process."),
               uiOutput("backward_slider_ui"),
               plotlyOutput("backward_animation_plot"),
               verbatimTextOutput("backward_animation_text"),
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

  # Processed data reactive
  processed_data <- reactive({
    req(data_holder())
    df <- data_holder()

    converted_cols <- c()
    df_numeric <- as.data.frame(lapply(names(df), function(col_name) {
      if(is.character(df[[col_name]]) || is.factor(df[[col_name]])) {
        converted_cols <<- c(converted_cols, col_name)
        as.numeric(as.factor(df[[col_name]]))
      } else { df[[col_name]] }
    }))
    names(df_numeric) <- names(df)

    if (length(converted_cols) > 0) {
      showNotification(paste("Converted non-numeric columns to factors:", paste(converted_cols, collapse=", ")),
                       type="warning", duration=8)
    }

    df_numeric <- as.data.frame((df_numeric))
    return(df_numeric)
  })

  all_predictors <- reactive({
    req(processed_data())
    colnames(processed_data())[-1]
  })

  # --- Data Preview Output ---
  output$data_head <- renderTable({
    req(data_holder())
    head(data_holder())
  })

  # --- Core Modeling Logic ---
  models <- reactive({
    req(processed_data())
    df <- processed_data()
    y_name <- names(df)[1]
    formula_obj <- as.formula(paste(y_name, "~ ."))

    list(
      best = regsubsets(formula_obj, data = df, nvmax = ncol(df) - 1, really.big = TRUE),
      forward = regsubsets(formula_obj, data = df, nvmax = ncol(df) - 1, method = "forward"),
      backward = regsubsets(formula_obj, data = df, nvmax = ncol(df) - 1, method = "backward")
    )
  })

  # --- Function to calculate true BIC for each model size ---
  calculate_true_bic <- function(model_obj, data) {
    num_vars <- 1:(ncol(data) - 1)
    sapply(num_vars, function(i) {
      vars_in_model <- names(which(summary(model_obj)$outmat[i, ] == "*"))
      vars_in_model <- vars_in_model[vars_in_model != "(Intercept)"]

      if (length(vars_in_model) == 0) return(NA) # Should not happen for i > 0

      y_name <- names(data)[1]
      formula <- as.formula(paste(y_name, "~", paste(vars_in_model, collapse = " + ")))
      fit <- lm(formula, data = data)
      return(BIC(fit))
    })
  }

  # --- Coefficient and Plotting Helpers ---
  get_coeffs_at_step <- function(reg_summary, data, step) {
    vars_in_model <- names(which(reg_summary$outmat[step, ] == "*"))
    vars_in_model <- vars_in_model[vars_in_model != "(Intercept)"]

    all_vars <- all_predictors()

    if (length(vars_in_model) == 0) {
      return(data.frame(Variable = all_vars, Coefficient = 0))
    }

    y_name <- names(data)[1]
    formula <- as.formula(paste(y_name, "~", paste(vars_in_model, collapse = " + ")))

    model_coeffs_with_intercept <- coef(lm(formula, data = data))
    model_coeffs <- model_coeffs_with_intercept[names(model_coeffs_with_intercept) != "(Intercept)"]

    full_coeffs <- setNames(rep(0, length(all_vars)), all_vars)
    full_coeffs[names(model_coeffs)] <- model_coeffs

    return(data.frame(Variable = names(full_coeffs), Coefficient = full_coeffs, stringsAsFactors = FALSE))
  }

  plot_null_model <- function(title) {
    coeffs_df <- data.frame(Variable = all_predictors(), Coefficient = 0)
    p <- ggplot(coeffs_df, aes(x = Variable, y = Coefficient)) + geom_col() + coord_flip() +
      labs(title = title, x = "", y = "Standardized Coefficient") + theme_bw()
    ggplotly(p)
  }

  # --- Dynamic Sliders for Animations ---
  output$best_subset_slider_ui <- renderUI({
    req(data_holder())
    sliderInput("best_subset_step", "Model Size (Number of Predictors):", min = 0, max = ncol(data_holder()) - 1, value = 0, step = 1, animate = animationOptions(interval = 1500, loop = FALSE))
  })
  output$forward_slider_ui <- renderUI({
    req(data_holder())
    sliderInput("forward_step", "Step (Number of Predictors Added):", min = 0, max = ncol(data_holder()) - 1, value = 0, step = 1, animate = animationOptions(interval = 1500, loop = FALSE))
  })
  output$backward_slider_ui <- renderUI({
    req(data_holder())
    num_predictors <- ncol(data_holder()) - 1
    sliderInput("backward_step", "Step (Number of Predictors Removed):", min = 0, max = num_predictors, value = 0, step = 1, animate = animationOptions(interval = 1500, loop = FALSE))
  })

  # --- Animation Plotting and Text Logic ---

  # BEST SUBSET
  output$best_subset_animation_plot <- renderPlotly({
    req(models(), input$best_subset_step)
    if (input$best_subset_step == 0) {
      return(plot_null_model("Null Model (Intercept-Only)"))
    }
    coeffs_df <- get_coeffs_at_step(summary(models()$best), processed_data(), input$best_subset_step)
    p <- ggplot(coeffs_df, aes(x = reorder(Variable, Coefficient), y = Coefficient, fill = Coefficient, text = paste("Variable:", Variable, "<br>Coefficient:", round(Coefficient, 3)))) +
      geom_col() + coord_flip() + scale_fill_gradient2(low = "#3182bd", mid = "grey80", high = "#e6550d") +
      labs(title = paste("Coefficients for Best", input$best_subset_step, "Predictor Model"), x = "", y = "Standardized Coefficient") +
      theme_bw() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })
  output$best_subset_animation_text <- renderText({
    req(models(), input$best_subset_step)
    if (input$best_subset_step == 0) {
      return("Model Size 0: Null model (Intercept-only).")
    }
    s <- summary(models()$best)
    vars <- names(which(s$outmat[input$best_subset_step, ] == "*"))
    vars <- vars[vars != "(Intercept)"]
    paste("Best model with", input$best_subset_step, "predictor(s):\n", paste(vars, collapse = ", "))
  })

  # FORWARD
  output$forward_animation_plot <- renderPlotly({
    req(models(), input$forward_step)
    if (input$forward_step == 0) {
      return(plot_null_model("Step 0: Null Model (Intercept-Only)"))
    }
    coeffs_df <- get_coeffs_at_step(summary(models()$forward), processed_data(), input$forward_step)
    p <- ggplot(coeffs_df, aes(x = reorder(Variable, Coefficient), y = Coefficient, fill = Coefficient, text = paste("Variable:", Variable, "<br>Coefficient:", round(Coefficient, 3)))) +
      geom_col() + coord_flip() + scale_fill_gradient2(low = "#3182bd", mid = "grey80", high = "#e6550d") +
      labs(title = paste("Model After Step", input$forward_step), x = "", y = "Standardized Coefficient") +
      theme_bw() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })
  output$forward_animation_text <- renderText({
    req(models(), input$forward_step)
    s <- summary(models()$forward)
    step <- input$forward_step
    if (step == 0) {
      return("Step 0: Start with the null model (Intercept-only).")
    }
    vars_now <- names(which(s$outmat[step, ] == "*"))
    vars_now <- vars_now[vars_now != "(Intercept)"]
    if (step == 1) {
      paste("Step 1: Added predictor '", vars_now, "' to the model.")
    } else {
      vars_before <- names(which(s$outmat[step - 1, ] == "*"))
      vars_before <- vars_before[vars_before != "(Intercept)"]
      added_var <- setdiff(vars_now, vars_before)
      paste("Step", step, ": Added predictor '", added_var, "' to the model.")
    }
  })

  # BACKWARD
  output$backward_animation_plot <- renderPlotly({
    req(models(), input$backward_step)
    model_size <- length(all_predictors()) - input$backward_step
    if (model_size <= 0) {
      return(plot_null_model(paste("After Step", input$backward_step, ": Null Model")))
    }
    coeffs_df <- get_coeffs_at_step(summary(models()$backward), processed_data(), model_size)
    p <- ggplot(coeffs_df, aes(x = reorder(Variable, Coefficient), y = Coefficient, fill = Coefficient, text = paste("Variable:", Variable, "<br>Coefficient:", round(Coefficient, 3)))) +
      geom_col() + coord_flip() + scale_fill_gradient2(low = "#3182bd", mid = "grey80", high = "#e6550d") +
      labs(title = paste("Model After Step", input$backward_step), x = "", y = "Standardized Coefficient") +
      theme_bw() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })
  output$backward_animation_text <- renderText({
    req(models(), input$backward_step)
    s <- summary(models()$backward)
    num_predictors <- length(all_predictors())
    step <- input$backward_step

    if (step == 0) {
      return("Step 0: Start with the full model containing all predictors.")
    }

    size_before <- num_predictors - step + 1
    size_after <- num_predictors - step

    vars_before <- names(which(s$outmat[size_before, ] == "*"))
    vars_before <- vars_before[vars_before != "(Intercept)"]

    vars_after <- if(size_after > 0) names(which(s$outmat[size_after, ] == "*")) else character(0)
    vars_after <- vars_after[vars_after != "(Intercept)"]

    removed_var <- setdiff(vars_before, vars_after)

    paste0("Step ", step, ": Removed predictor '", removed_var, "' from the model.")
  })

  # --- Reusable Interactive Plotting Function ---
  render_selection_plotly <- function(model_summary, true_bic) {
    plot_data <- data.frame(num_vars = 1:length(model_summary$rss), rss = model_summary$rss, adjr2 = model_summary$adjr2, cp = model_summary$cp, bic = true_bic)
    p_rss <- ggplot(plot_data, aes(x=num_vars, y=rss, text=paste("Vars:", num_vars, "<br>RSS:", round(rss,2)))) + geom_line(color="steelblue") + geom_point(color="steelblue") +geom_point(data=plot_data[which.min(plot_data$rss), ], color="red", size=3) + labs(y="RSS", x="Number of Variables") + theme_bw()
    p_adjr2 <- ggplot(plot_data, aes(x=num_vars, y=adjr2, text=paste("Vars:", num_vars, "<br>Adj R2:", round(adjr2,3)))) + geom_line(color="steelblue") + geom_point(color="steelblue") + geom_point(data=plot_data[which.max(plot_data$adjr2), ], color="red", size=3) + labs(y="Adjusted R²", x="Number of Variables") + theme_bw()
    p_cp <- ggplot(plot_data, aes(x=num_vars, y=cp, text=paste("Vars:", num_vars, "<br>Cp:", round(cp,2)))) + geom_line(color="steelblue") + geom_point(color="steelblue") + geom_point(data=plot_data[which.min(plot_data$cp), ], color="red", size=3) + labs(y="Mallows' Cp", x="Number of Variables") + theme_bw()
    p_bic <- ggplot(plot_data, aes(x=num_vars, y=bic, text=paste("Vars:", num_vars, "<br>BIC:", round(bic,2)))) + geom_line(color="steelblue") + geom_point(color="steelblue") + geom_point(data=plot_data[which.min(plot_data$bic), ], color="red", size=3) + labs(y="BIC", x="Number of Variables") + theme_bw()
    subplot(ggplotly(p_rss, tooltip="text"), ggplotly(p_adjr2, tooltip="text"), ggplotly(p_cp, tooltip="text"), ggplotly(p_bic, tooltip="text"), nrows=2, shareX=TRUE, titleY=TRUE) %>% layout(showlegend=FALSE)
  }

  # --- R Code Skeletons ---
  output$best_subset_code <- renderText({"library(leaps)\n\n# Assuming 'my_data' is your dataframe\nbest_model <- regsubsets(\n  outcome ~ .,\n  data = my_data,\n  nvmax = ncol(my_data) - 1\n)\n\nsummary(best_model)"})
  output$forward_code <- renderText({"library(leaps)\n\n# Assuming 'my_data' is your dataframe\nforward_model <- regsubsets(\n  outcome ~ .,\n  data = my_data,\n  nvmax = ncol(my_data) - 1,\n  method = \"forward\"\n)\n\nsummary(forward_model)"})
  output$backward_code <- renderText({"library(leaps)\n\n# Assuming 'my_data' is your dataframe\nbackward_model <- regsubsets(\n  outcome ~ .,\n  data = my_data,\n  nvmax = ncol(my_data) - 1,\n  method = \"backward\"\n)\n\nsummary(backward_model)"})

  # --- Main Outputs (Plots and Summaries) ---
  output$best_subset_plot <- renderPlotly({ req(models()); render_selection_plotly(summary(models()$best), calculate_true_bic(models()$best, processed_data())) })
  output$best_subset_summary <- renderPrint({ req(models()); summary(models()$best) })
  output$forward_plot <- renderPlotly({ req(models()); render_selection_plotly(summary(models()$forward), calculate_true_bic(models()$forward, processed_data())) })
  output$forward_summary <- renderPrint({ req(models()); summary(models()$forward) })
  output$backward_plot <- renderPlotly({ req(models()); render_selection_plotly(summary(models()$backward), calculate_true_bic(models()$backward, processed_data())) })
  output$backward_summary <- renderPrint({ req(models()); summary(models()$backward) })
}

# Run the application
shinyApp(ui = ui, server = server)

