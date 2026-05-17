# hey, let's load the packages we need
# shiny is for the app itself
# MASS helps us simulate from a multivariate normal distribution (cool, right?)
# ggplot2 makes our plots look nice
# dplyr is just handy for data manipulation
library(shiny)
library(MASS)
library(ggplot2)
library(dplyr)

# --- user interface part ---
# this is what the user sees and interacts with
ui <- fluidPage(
  # a nice title for our app
  titlePanel("k-means Clustering Animation (Lloyd's Algorithm)"),

  # setting up the layout with a sidebar for inputs and a main area for the plot
  sidebarLayout(

    # this is the sidebar panel for all our user inputs
    sidebarPanel(
      h4("1. Simulate Data"),
      p("create data from three different groups (bivariate normal distributions)."),

      # we'll use tabs to keep the inputs for each class organized
      tabsetPanel(
        type = "tabs",
        # inputs for the first class
        tabPanel("Class 1",
                 numericInput("n1", "number of points", 100, min = 1),
                 numericInput("mu1_x", "mean of x", 1),
                 numericInput("mu1_y", "mean of y", 5),
                 numericInput("sigma1_x", "sd of x", 1),
                 numericInput("sigma1_y", "sd of y", 1),
                 numericInput("rho1", "correlation", 0.5, min = -1, max = 1, step = 0.1)
        ),
        # inputs for the second class
        tabPanel("Class 2",
                 numericInput("n2", "number of points", 100, min = 1),
                 numericInput("mu2_x", "mean of x", 5),
                 numericInput("mu2_y", "mean of y", 1),
                 numericInput("sigma2_x", "sd of x", 1),
                 numericInput("sigma2_y", "sd of y", 1),
                 numericInput("rho2", "correlation", -0.3, min = -1, max = 1, step = 0.1)
        ),
        # inputs for the third class
        tabPanel("Class 3",
                 numericInput("n3", "number of points", 100, min = 1),
                 numericInput("mu3_x", "mean of x", 8),
                 numericInput("mu3_y", "mean of y", 8),
                 numericInput("sigma3_x", "sd of x", 1.5),
                 numericInput("sigma3_y", "sd of y", 1),
                 numericInput("rho3", "correlation", 0, min = -1, max = 1, step = 0.1)
        )
      ),

      # a button to kick things off
      actionButton("go", "Generate Data & Run k-means", class = "btn-success"),

      hr(), # just a little horizontal line to separate things

      h4("2. Control Animation"),
      p("use the slider to step through the algorithm. press the play button for an animation."),
      # the slider to control which step of the animation we're on
      # notice the animate=TRUE part, that's what gives us the play button!
      sliderInput("step_slider", "Algorithm Step:", min = 1, max = 1, value = 1, step = 1,
                  animate = animationOptions(interval = 800, loop = FALSE))
    ),

    # this is the main panel where the plot will show up
    mainPanel(
      plotOutput("cluster_plot", height = "600px")
    )
  )
)

# --- server part ---
# this is where all the calculations and plot generation happens
server <- function(input, output, session) {

  # a reactive value to hold our k-means results
  # we use this so we can access the results from different places
  kmeans_results <- reactiveValues(steps = NULL, data = NULL)

  # this part only runs when the 'go' button is clicked
  observeEvent(input$go, {

    # --- data simulation part ---
    # build the mean vectors and covariance matrices from user inputs

    # class 1
    mu1 <- c(input$mu1_x, input$mu1_y)
    cov1 <- matrix(c(input$sigma1_x^2, input$rho1 * input$sigma1_x * input$sigma1_y,
                     input$rho1 * input$sigma1_x * input$sigma1_y, input$sigma1_y^2), 2)
    data1 <- as.data.frame(mvrnorm(n = input$n1, mu = mu1, Sigma = cov1))

    # class 2
    mu2 <- c(input$mu2_x, input$mu2_y)
    cov2 <- matrix(c(input$sigma2_x^2, input$rho2 * input$sigma2_x * input$sigma2_y,
                     input$rho2 * input$sigma2_x * input$sigma2_y, input$sigma2_y^2), 2)
    data2 <- as.data.frame(mvrnorm(n = input$n2, mu = mu2, Sigma = cov2))

    # class 3
    mu3 <- c(input$mu3_x, input$mu3_y)
    cov3 <- matrix(c(input$sigma3_x^2, input$rho3 * input$sigma3_x * input$sigma3_y,
                     input$rho3 * input$sigma3_x * input$sigma3_y, input$sigma3_y^2), 2)
    data3 <- as.data.frame(mvrnorm(n = input$n3, mu = mu3, Sigma = cov3))

    # combine them all into one big dataframe
    colnames(data1) <- c("x", "y")
    colnames(data2) <- c("x", "y")
    colnames(data3) <- c("x", "y")
    sim_data <- rbind(data1, data2, data3)

    # store the data so we can use it for plotting
    kmeans_results$data <- sim_data

    # --- k-means algorithm part ---
    k <- 3 # we're looking for 3 clusters

    # randomly pick 3 points from our data to be the first centroids
    # set.seed makes sure the "random" start is the same every time you click for the same data
    set.seed(123)
    centroids <- sim_data[sample(nrow(sim_data), k), ]

    # this will be a list to store the state of the algorithm at each step
    steps <- list()

    # let's loop for a max of 20 iterations, just in case it doesn't converge
    for (i in 1:20) {

      # --- assignment step ---
      # for each point, find the closest centroid
      distances <- as.matrix(dist(rbind(centroids, sim_data)))
      distances <- distances[-(1:k), 1:k] # get only distances from points to centroids
      assignments <- apply(distances, 1, which.min)

      # store the current state (assignments and centroids) for our animation
      steps[[i]] <- list(assignments = assignments, centroids = centroids)

      # --- update step ---
      # calculate the new centroids by taking the mean of all points in each cluster
      new_centroids <- sim_data %>%
        mutate(cluster = assignments) %>%
        group_by(cluster) %>%
        summarise(x = mean(x), y = mean(y), .groups = 'drop') %>%
        # if a cluster has no points, it disappears, so we need to handle that
        # we'll just keep the old centroid position if that happens
        right_join(data.frame(cluster = 1:k), by = "cluster") %>%
        arrange(cluster) %>%
        select(x, y)

      # check if any cluster was empty and fill with old centroid value if needed
      for (cluster_num in 1:k) {
        if(is.na(new_centroids$x[cluster_num])) {
          new_centroids[cluster_num, ] <- centroids[cluster_num, ]
        }
      }

      # --- check for convergence ---
      # if the centroids didn't move, we're done!
      if (all(centroids == new_centroids)) {
        break
      }

      # if not done, update the centroids and go to the next iteration
      centroids <- new_centroids
    }

    # store all the steps of the algorithm
    kmeans_results$steps <- steps

    # now, let's update the slider so its max value is the number of steps it took
    updateSliderInput(session, "step_slider", max = length(steps), value = 1)
  })


  # --- plotting part ---
  # this creates the plot that the user sees
  output$cluster_plot <- renderPlot({

    # wait until we have some results before trying to plot
    req(kmeans_results$steps, kmeans_results$data)

    # get the current step from the slider
    current_step_num <- input$step_slider
    current_step <- kmeans_results$steps[[current_step_num]]

    # prepare the data for plotting by adding the cluster assignments
    plot_data <- kmeans_results$data %>%
      mutate(cluster = factor(current_step$assignments))

    # prepare the centroid data for plotting
    centroids_data <- current_step$centroids %>%
      mutate(cluster = factor(1:nrow(.)))

    # let's make the plot using ggplot
    ggplot(plot_data, aes(x = x, y = y)) +
      # draw the data points, colored by their assigned cluster
      geom_point(aes(color = cluster), alpha = 0.7) +
      # draw the centroids as big 'x' marks
      geom_point(data = centroids_data, aes(x = x, y = y, fill = cluster),
                 shape = 23, size = 8, color = "black") +
      # a few theme tweaks to make it look clean
      theme_minimal(base_size = 16) +
      labs(
        title = paste("k-means: Step", current_step_num),
        subtitle = "Points are assigned to the nearest centroid. Centroids are the mean of their assigned points.",
        x = "X value",
        y = "Y value",
        color = "Assigned Cluster",
        fill = "Centroid"
      ) +
      # using some nice colors
      scale_color_brewer(palette = "Set1") +
      scale_fill_brewer(palette = "Set1")
  })

}

# this line actually runs the app
shinyApp(ui, server)
