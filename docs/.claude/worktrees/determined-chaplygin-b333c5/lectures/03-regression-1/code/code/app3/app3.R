rm(list = ls())
source(file.path(here::here(), "class3", "code", "helper.R"))

pacman::p_load(shiny, tidyverse, caret, glmnet, DT, latex2exp, ragg, data.table, DT)

options(shiny.useragg = TRUE)

# --------------------
# UI Definition
# --------------------
ui <- fluidPage(
  titlePanel("Ridge & Lasso Regression Explorer"),

  sidebarLayout(
    sidebarPanel(
      h4("1. Data Input"),
      fileInput("file1", "Upload Your CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),

      checkboxInput("header", "Data has Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                   selected = ","),

      tags$hr(),

      checkboxInput("demo_data", "Use Demo Data (mtcars)", FALSE),

      tags$hr(),

      h4("2. Model Configuration"),
      p("Model will be trained on the full dataset using 5-fold cross-validation."),

      tags$hr(),

      actionButton("run_analysis", "Run Analysis", icon = icon("play"), class = "btn-success")
    ),

    mainPanel(
      tabsetPanel(
        id = "main_tabs",

        tabPanel("About / Help",
                 uiOutput("about_section")
        ),

        # --- Data Preview Tab ---
        tabPanel("Data Preview",
                 #h3("Uploaded Data Preview"),
               #  DTOutput("data_table"),
                 h3("Data Summary"),
                 verbatimTextOutput("data_summary")
        ),

        # --- Ridge Regression Tab ---
        tabPanel("Ridge Regression",
                 h3("Ridge Model Results"),
                 fluidRow(
                   column(4,
                          h4("Cross-Validation for Lambda"),
                          plotOutput("ridge_cv_plot")
                   ),
                   column(4,
                          h4("Coefficient Path vs. Lambda"),
                          plotOutput("ridge_path_lambda_plot")
                   ),
                   column(4,
                          h4("Coefficient Path vs. L2-Norm of Coefficients"),
                          plotOutput("ridge_path_norm_plot") # Renamed for consistency
                   )
                 )#,
                 #fluidRow(
                  # column(12,
                   #       h4("Coefficient Path vs. L2-Norm of Coefficients"),
                    #      plotOutput("ridge_path_norm_plot") # Renamed for consistency
                   #)
                 #),
                 #h4("Cross-Validated Performance Summary"),
                 #verbatimTextOutput("ridge_summary"),
                # h4("Final Model Coefficients"),
               #  DTOutput("ridge_coeffs")
        ),

        # --- Lasso Regression Tab ---
        tabPanel("Lasso Regression",
                 h3("Lasso Model Results"),
                 fluidRow(
                   column(4,
                          h4("Cross-Validation for Lambda"),
                          plotOutput("lasso_cv_plot")
                   ),
                   column(4,
                          h4("Coefficient Path vs. Lambda"),
                          plotOutput("lasso_path_lambda_plot")
                   ),
                   column(4,
                          h4("Coefficient Path vs. L1-Norm of Coefficients"), # Updated header
                          plotOutput("lasso_path_norm_plot") # Renamed for consistency
                   )
                 )#,
                 #fluidRow(
                #   column(12,
                 #         h4("Coefficient Path vs. L1-Norm of Coefficients"), # Updated header
                #          plotOutput("lasso_path_norm_plot") # Renamed for consistency
                #   )
                # ),
                 #h4("Cross-Validated Performance Summary"),
                 #verbatimTextOutput("lasso_summary"),
                 #h4("Final Model Coefficients (Note the Zeros)"),
                 #DTOutput("lasso_coeffs")
        )#,

        # --- About/Help Tab ---

      )
    )
  )
)

# --------------------
# Server Logic
# --------------------
server <- function(input, output) {

  # --- Reactive Data Input ---
  dataset <- reactive({
    if (input$demo_data) {
      data <- mtcars
    } else {
      req(input$file1)
      tryCatch({
        data <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep)
      }, error = function(e) {
        stop(safeError(e))
      })
    }
    data %>% mutate(across(everything(), as.numeric))
  })

  # --- Model Training (No Test/Train Split) ---
  analysis_results <- eventReactive(input$run_analysis, {

    data <- dataset()

    withProgress(message = 'Running Analysis...', value = 0, {

      # Use the full dataset for training
      x <- as.matrix(data[, -1])
      y <- data[[1]]

      incProgress(0.2, detail = "Setting up CV...")
      set.seed(123)
      train_control <- trainControl(method = "cv", number = 5) # 5-fold CV
      lambda_grid <- 10^seq(-3, 2, length = 100)

      incProgress(0.3, detail = "Training Ridge model...")
      ridge_model <- suppressWarnings(train(x = x, y = y,
                                            method = "glmnet",
                                            trControl = train_control,
                                            tuneGrid = expand.grid(alpha = 0, lambda = lambda_grid),
                                            preProcess = c("center", "scale")
      ))

      incProgress(0.3, detail = "Training Lasso model...")
      lasso_model <- suppressWarnings(train(x = x, y = y,
                                            method = "glmnet",
                                            trControl = train_control,
                                            tuneGrid = expand.grid(alpha = 1, lambda = lambda_grid),
                                            preProcess = c("center", "scale")
      ))

      incProgress(0.2, detail = "Finalizing results...")

      list(
        ridge_model = ridge_model,
        lasso_model = lasso_model
      )
    })
  })

  # --- Helper function to create coefficient path plots vs Lambda ---
  create_path_lambda_plot <- function(glmnet_final_model, best_lambda, model_name) {
    req(glmnet_final_model)

    coeffs <- as.matrix(coef(glmnet_final_model, s = glmnet_final_model$lambda))

    coeffs_df <- as.data.frame(coeffs) %>%
      rownames_to_column("Variable") %>%
      pivot_longer(cols = starts_with("s"), names_to = "Lambda_Index", values_to = "Coefficient") %>%
      mutate(Lambda = glmnet_final_model$lambda[as.numeric(str_replace(Lambda_Index, "s", "")) + 1] ) %>%
      filter(Variable != "(Intercept)")

    p <- ggplot(coeffs_df, aes(x = Lambda, y = Coefficient, color = Variable)) +
      geom_line(lwd = 1) +
      scale_x_log10() +
      labs(title = paste0(model_name, " Coefficient Path vs. Lambda"),
           x = TeX("$\\lambda$ (log scale)"), # Using latex2exp
           y = "Standardized Coefficients") +
      theme_bw(base_size = 14) +
      theme(legend.position = "right")

    if (!is.na(best_lambda) && is.numeric(best_lambda)) {
      p <- p +
        geom_vline(xintercept = best_lambda, linetype = "dashed", color = "darkgrey", size = 1) +
        annotate("text", x = best_lambda, y = max(coeffs_df$Coefficient, na.rm = TRUE),
                 label = TeX(paste0("Optimal $\\lambda$: ", round(best_lambda, 3))), # Using latex2exp
                 hjust = -0.1, vjust = 0, color = "darkgrey", size = 4)
    }

    return(p)
  }

  # --- Helper function to create coefficient path plots vs L1/L2 Norm (UPDATED) ---
  create_path_norm_plot <- function(glmnet_final_model, model_name) {
    req(glmnet_final_model)

    coeffs_matrix <- as.matrix(coef(glmnet_final_model))[-1, ] # Exclude intercept

    if (model_name == "Ridge") {
      # For Ridge, use L2 norm
      norms <- apply(coeffs_matrix, 2, function(x) sqrt(sum(x^2)))
      norm_label <- TeX("$\\frac{||\\hat{\\beta}_{\\lambda}||_2}{||\\hat{\\beta}||_2}$")
      plot_title <- "Coefficient Path vs. L2-Norm"
    } else if (model_name == "Lasso") {
      # For Lasso, use L1 norm
      norms <- apply(coeffs_matrix, 2, function(x) sum(abs(x)))
      norm_label <- TeX("$\\frac{||\\hat{\\beta}_{\\lambda}||_1}{||\\hat{\\beta}||_1}$")
      plot_title <- "Coefficient Path vs. L1-Norm"
    } else {
      stop("Invalid model_name for create_path_norm_plot")
    }

    max_norm <- max(norms, na.rm = TRUE)

    coeffs_df_norm <- as.data.frame(coeffs_matrix) %>%
      rownames_to_column("Variable") %>%
      pivot_longer(cols = starts_with("s"), names_to = "Lambda_Index", values_to = "Coefficient") %>%
      mutate(Normalized_Norm = norms[as.numeric(str_replace(Lambda_Index, "s", "")) + 1] / max_norm )

    ggplot(coeffs_df_norm, aes(x = Normalized_Norm, y = Coefficient, color = Variable)) +
      geom_line(lwd = 1) +
      labs(title = paste0(model_name, " ", plot_title),
           x = norm_label,
           y = "Standardized Coefficients") +
      theme_bw(base_size = 14) +
      theme(legend.position = "right")
  }


  # --- Outputs for Data Preview Tab ---
  output$data_table <- renderDT({
    datatable(head(dataset(), 100), options = list(scrollX = TRUE))
  })

  output$data_summary <- renderPrint({
    summary(dataset())
  })

  # --- Outputs for Ridge Regression Tab ---
  output$ridge_cv_plot <- renderPlot({
    req(analysis_results())
    ggplot(analysis_results()$ridge_model) +
      theme_bw(base_size = 14) +
      labs(title = "Ridge Cross-Validation Results")
  })

  output$ridge_path_lambda_plot <- renderPlot({
    req(analysis_results())
    caret_model <- analysis_results()$ridge_model
    create_path_lambda_plot(
      glmnet_final_model = caret_model$finalModel,
      best_lambda = caret_model$bestTune$lambda,
      model_name = "Ridge"
    )
  })

  output$ridge_path_norm_plot <- renderPlot({ # Renamed
    req(analysis_results())
    caret_model <- analysis_results()$ridge_model
    create_path_norm_plot( # Now calls the updated function
      glmnet_final_model = caret_model$finalModel,
      model_name = "Ridge"
    )
  })

  output$ridge_summary <- renderPrint({
    req(analysis_results())
    print(analysis_results()$ridge_model)
  })

  # output$ridge_coeffs <- renderDT({
  #   req(analysis_results())
  #   ridge_model <- analysis_results()$ridge_model
  #   if(is.na(ridge_model$bestTune$lambda)) return("Could not determine optimal coefficients.")
  #
  #   coeffs <- coef(ridge_model$finalModel, ridge_model$bestTune$lambda)
  #
  #   coeffs_df <- data.frame(
  #     Variable = rownames(coeffs),
  #     Coefficient = as.numeric(coeffs)
  #   ) %>%
  #     filter(Variable != "(Intercept)", Coefficient != 0) %>%
  #     arrange(desc(abs(Coefficient)))
  #
  #   datatable(coeffs_df, rownames = FALSE, options = list(pageLength = 10))
  # })

  # --- Outputs for Lasso Regression Tab ---
  output$lasso_cv_plot <- renderPlot({
    req(analysis_results())
    ggplot(analysis_results()$lasso_model) +
      theme_bw(base_size = 14) +
      labs(title = "Lasso Cross-Validation Results")
  })

  output$lasso_path_lambda_plot <- renderPlot({
    req(analysis_results())
    caret_model <- analysis_results()$lasso_model
    create_path_lambda_plot(
      glmnet_final_model = caret_model$finalModel,
      best_lambda = caret_model$bestTune$lambda,
      model_name = "Lasso"
    )
  })

  output$lasso_path_norm_plot <- renderPlot({ # Renamed
    req(analysis_results())
    caret_model <- analysis_results()$lasso_model
    create_path_norm_plot( # Now calls the updated function
      glmnet_final_model = caret_model$finalModel,
      model_name = "Lasso"
    )
  })

  output$lasso_summary <- renderPrint({
    req(analysis_results())
    print(analysis_results()$lasso_model)
  })

  # output$lasso_coeffs <- renderDT({
  #   req(analysis_results())
  #   lasso_model <- analysis_results()$lasso_model
  #   if(is.na(lasso_model$bestTune$lambda)) return("Could not determine optimal coefficients.")
  #
  #   coeffs <- coef( lasso_model$finalModel, lasso_model$bestTune$lambda)
  #
  #   coeffs_df <- data.frame(
  #     Variable = rownames(coeffs),
  #     Coefficient = as.numeric(coeffs)
  #   ) %>%
  #     filter(Variable != "(Intercept)", Coefficient != 0) %>%
  #     arrange(desc(abs(Coefficient)))
  #
  #   datatable(coeffs_df, rownames = FALSE, options = list(pageLength = 10))
  # })

  # --- Outputs for About/Help Tab ---
  output$about_section <- renderUI({
    HTML(
      "<h3>About This App</h3>
      <p>This application demonstrates and compares two types of regularized linear regression: <strong>Ridge</strong> and <strong>Lasso</strong>.
      The models are trained on the full dataset using 5-fold cross-validation to select the optimal tuning parameter (lambda).</p>

      <h4>How to Use</h4>
      <ol>
        <li>Upload a CSV file or select the 'Use Demo Data' checkbox. The app assumes the <strong>first column is the outcome (Y)</strong> and all others are predictors (X).</li>
        <li>Click the 'Run Analysis' button.</li>
        <li>Explore the results in the 'Ridge Regression' and 'Lasso Regression' tabs.</li>
      </ol>

      <hr>

      <h3>Skeleton Code for Implementation</h3>
      <p>Below is basic R code showing how to run these models and generate the key outputs outside of Shiny.</p>

      <h4>Ridge Regression</h4>
      <pre><code class='language-r'>
# 1. Load libraries
library(caret)
library(glmnet)
library(tidyverse)
library(latex2exp)

# 2. Prepare data (using mtcars as an example)
data(mtcars)
y <- mtcars$mpg
x <- as.matrix(mtcars[, -1])

# 3. Set up 5-fold cross-validation
set.seed(123)
train_control <- trainControl(method = 'cv', number = 5)

# 4. Define the tuning grid
tune_grid_ridge <- expand.grid(alpha = 0, lambda = 10^seq(-3, 2, length=100))

# 5. Train the model
ridge_model <- train(x = x,
                     y = y,
                     method = 'glmnet',
                     trControl = train_control,
                     tuneGrid = tune_grid_ridge,
                     preProcess = c('center', 'scale'))

# --- Model Output and Plots ---

# 6. View full CV summary and best tune
print(ridge_model)

# 7. Get final coefficients for the best model
coef(ridge_model$finalModel, s = ridge_model$bestTune$lambda)

# 8. Plot the cross-validation results
ggplot(ridge_model) + theme_bw(base_size = 14)

# 9. Plot coefficient path vs. Lambda
ridge_final <- ridge_model$finalModel
ridge_coeffs <- as.matrix(coef(ridge_final, s = ridge_final$lambda))
ridge_coeffs_df <- as.data.frame(ridge_coeffs) %>%
  rownames_to_column('Variable') %>%
  pivot_longer(cols = starts_with('s'), names_to = 'Lambda_Index', values_to = 'Coefficient') %>%
  mutate(Lambda = ridge_final$lambda[as.numeric(str_replace(Lambda_Index, 's', '')) + 1]) %>%
  filter(Variable != '(Intercept)')

ggplot(ridge_coeffs_df, aes(x = Lambda, y = Coefficient, color = Variable)) +
  geom_line(lwd = 1) +
  scale_x_log10() +
  labs(title = 'Ridge Coefficient Path vs. Lambda', x = TeX('$\\lambda$ (log scale)'), y = 'Standardized Coefficients') +
  theme_bw(base_size = 14)

# 10. Plot coefficient path vs. L2-Norm
ridge_coeffs_matrix <- as.matrix(coef(ridge_final))[-1, ]
l2_norms <- apply(ridge_coeffs_matrix, 2, function(x) sqrt(sum(x^2)))
max_l2_norm <- max(l2_norms)
ridge_coeffs_l2_df <- as.data.frame(ridge_coeffs_matrix) %>%
  rownames_to_column('Variable') %>%
  pivot_longer(cols = starts_with('s'), names_to = 'Lambda_Index', values_to = 'Coefficient') %>%
  mutate(Normalized_Norm = l2_norms[as.numeric(str_replace(Lambda_Index, 's', '')) + 1] / max_l2_norm)

ggplot(ridge_coeffs_l2_df, aes(x = Normalized_Norm, y = Coefficient, color = Variable)) +
  geom_line(lwd = 1) +
  labs(title = 'Ridge Coefficient Path vs. L2-Norm', x = TeX('$\\frac{||\\hat{\\beta}_{\\lambda}||_2}{||\\hat{\\beta}||_2}$'), y = 'Standardized Coefficients') +
  theme_bw(base_size = 14)
      </code></pre>

      <h4>Lasso Regression</h4>
      <pre><code class='language-r'>
# (Steps 1-3 are the same as above)

# 4. Define the tuning grid
tune_grid_lasso <- expand.grid(alpha = 1, lambda = 10^seq(-3, 2, length=100))

# 5. Train the model
lasso_model <- train(x = x,
                     y = y,
                     method = 'glmnet',
                     trControl = train_control,
                     tuneGrid = tune_grid_lasso,
                     preProcess = c('center', 'scale'))

# --- Model Output and Plots ---

# 6. View full CV summary and best tune
print(lasso_model)

# 7. Get final coefficients for the best model
coef(lasso_model$finalModel, s = lasso_model$bestTune$lambda)

# 8. Plot the cross-validation results
ggplot(lasso_model) + theme_bw(base_size = 14)

# 9. Plot coefficient path vs. Lambda
lasso_final <- lasso_model$finalModel
lasso_coeffs <- as.matrix(coef(lasso_final, s = lasso_final$lambda))
lasso_coeffs_df <- as.data.frame(lasso_coeffs) %>%
  rownames_to_column('Variable') %>%
  pivot_longer(cols = starts_with('s'), names_to = 'Lambda_Index', values_to = 'Coefficient') %>%
  mutate(Lambda = lasso_final$lambda[as.numeric(str_replace(Lambda_Index, 's', '')) + 1]) %>%
  filter(Variable != '(Intercept)')

ggplot(lasso_coeffs_df, aes(x = Lambda, y = Coefficient, color = Variable)) +
  geom_line(lwd = 1) +
  scale_x_log10() +
  labs(title = 'Lasso Coefficient Path vs. Lambda', x = TeX('$\\lambda$ (log scale)'), y = 'Standardized Coefficients') +
  theme_bw(base_size = 14)

# 10. Plot coefficient path vs. L1-Norm
lasso_coeffs_matrix <- as.matrix(coef(lasso_final))[-1, ]
l1_norms <- apply(lasso_coeffs_matrix, 2, function(x) sum(abs(x)))
max_l1_norm <- max(l1_norms)
lasso_coeffs_l1_df <- as.data.frame(lasso_coeffs_matrix) %>%
  rownames_to_column('Variable') %>%
  pivot_longer(cols = starts_with('s'), names_to = 'Lambda_Index', values_to = 'Coefficient') %>%
  mutate(Normalized_Norm = l1_norms[as.numeric(str_replace(Lambda_Index, 's', '')) + 1] / max_l1_norm)

ggplot(lasso_coeffs_l1_df, aes(x = Normalized_Norm, y = Coefficient, color = Variable)) +
  geom_line(lwd = 1) +
  labs(title = 'Lasso Coefficient Path vs. L1-Norm', x = TeX('$\\frac{||\\hat{\\beta}_{\\lambda}||_1}{||\\hat{\\beta}||_1}$'), y = 'Standardized Coefficients') +
  theme_bw(base_size = 14)
      </code></pre>"
    )
  })

}

# --------------------
# Run the App
# --------------------
shinyApp(ui = ui, server = server)
