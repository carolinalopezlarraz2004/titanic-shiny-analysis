############################################################
# Reactive Web Application - Titanic Survival Analysis
# First Assignment - R Shiny
#
# This application performs an exploratory statistical
# analysis of the Titanic dataset using reactive programming.
# The goal is to dynamically visualize survival patterns
# across different categorical variables.
############################################################


# ----------------------------------------------------------
# 1. Load required libraries
# ----------------------------------------------------------

library(shiny)      # Provides the framework to build interactive web applications
library(ggplot2)    # Used for data visualization
library(dplyr)      # Used for data manipulation and summarization
library(datasets)   # Contains built-in datasets (including Titanic)
library(vcd)        # Provides mosaic plots for categorical association
library(reshape2)   # Used to reshape contingency tables into data frames


# ----------------------------------------------------------
# 2. Data preparation
# ----------------------------------------------------------

# Load the Titanic dataset (included in base R)
data(Titanic)

# Display the internal structure of Titanic
# This shows that it is not a regular data frame
# but a 4-dimensional contingency table
str(Titanic)

# Convert the contingency table into a standard data frame
# Each row now represents a combination of:
# Class, Sex, Age and Survived, plus a frequency count
df <- as.data.frame(Titanic)

# Show the first rows of the converted data frame
# This allows us to inspect the data manually
head(df)

# Check the structure again to confirm data types
# We observe that the first four variables are factors
# and Freq is numeric
str(df)

# Expand the frequency counts into individual observations
# This replicates each row according to its frequency
# After this step, each row represents one passenger
# The final dataset has 2201 rows
df_expanded <- df[rep(1:nrow(df), df$Freq), 1:4]


# ----------------------------------------------------------
# 3. User Interface (UI)
# ----------------------------------------------------------

# fluidPage defines a simple responsive layout
ui <- fluidPage(
  
  # Title displayed at the top of the application
  titlePanel("Titanic - Data Analysis of Survival"),
  
  # sidebarLayout creates two panels:
  # left (inputs) and right (outputs)
  sidebarLayout(
    
    # Sidebar contains input controls
    sidebarPanel(
      
      # Dropdown menu allowing the user to select
      # which variable to analyze dynamically
      selectInput(
        inputId = "xvar",                       # Internal ID used in server
        label = "Choose variable to explore:",  # Label shown in UI
        choices = c("Class", "Sex", "Age")      # Available categorical variables
      )
    ),
    
    # Main panel displays all dynamic outputs
    mainPanel(
      
      # Plot 1: Distribution of selected variable
      h3("1. How Is This Variable Distributed?"),
      plotOutput("dist_plot"),
      
      # Plot 2: Stacked survival counts
      h3("2. How Many Survived vs Did Not Survive?"),
      plotOutput("stacked_plot"),
      
      # Plot 3: Survival rate
      h3("3. What Is the Survival Rate in Each Group?"),
      plotOutput("rate_plot"),
      
      # Plot 4: Heatmap
      h3("4. Survival Counts Heatmap"),
      plotOutput("heatmap_plot"),
      
      # Plot 5: Interaction plot
      h3("5. Does Class and Sex Interact in Survival?"),
      plotOutput("interaction_plot"),
      
      # Plot 6: Logistic regression predicted probabilities
      h3("6. Model-Based Survival Probability (Logistic Regression)"),
      plotOutput("logistic_plot"),
      
      # Statistical output
      h3("7. Are These Differences Statistically Significant?"),
      verbatimTextOutput("stats_output")
    )
  )
)


# ----------------------------------------------------------
# 4. Server Logic
# ----------------------------------------------------------

# The server function defines how the app behaves
# It connects user inputs with dynamic outputs
server <- function(input, output) {
  
  
  # ----------------------------------------------------------
  # 4.1. Distribution plot
  # ----------------------------------------------------------
  
  output$dist_plot <- renderPlot({
    
    # Create a bar plot counting each category
    ggplot(df_expanded, aes(x = .data[[input$xvar]])) +
      
      # geom_bar automatically counts occurrences
      geom_bar(fill = "steelblue") +
      
      # Minimal theme for cleaner appearance
      theme_minimal() +
      
      # Axis labels
      labs(x = input$xvar,
           y = "Count")
  })
  
  
  # ----------------------------------------------------------
  # 4.2. Stacked survival counts
  # ----------------------------------------------------------
  
  output$stacked_plot <- renderPlot({
    
    # Bar plot grouped by survival outcome
    ggplot(df_expanded,
           aes(x = .data[[input$xvar]], fill = Survived)) +
      
      # position = "stack" shows absolute counts
      geom_bar(position = "stack") +
      
      theme_minimal() +
      
      labs(x = input$xvar,
           y = "Count")
  })
  
  
  # ----------------------------------------------------------
  # 4.3. Survival rate calculation
  # ----------------------------------------------------------
  
  output$rate_plot <- renderPlot({
    
    # Group data by selected variable
    survival_rates <- df_expanded %>%
      
      # group_by allows calculation per category
      group_by(.data[[input$xvar]]) %>%
      
      # Calculate mean survival (Yes = TRUE)
      summarise(rate = mean(Survived == "Yes"),
                .groups = "drop")
    
    # Create bar plot of survival rate
    ggplot(survival_rates,
           aes(x = .data[[input$xvar]], y = rate)) +
      
      geom_col(fill = "darkgreen") +
      
      # Add numeric labels above bars
      geom_text(aes(label = round(rate, 2)),
                vjust = -0.5) +
      
      # Limit y-axis to probability range
      coord_cartesian(ylim = c(0,1)) +
      
      theme_minimal() +
      
      labs(x = input$xvar,
           y = "Survival Rate")
  })
  
  
  # ----------------------------------------------------------
  # 4.4. Heatmap of counts
  # ----------------------------------------------------------
  
  output$heatmap_plot <- renderPlot({
    
    # Create contingency table
    tab <- table(df_expanded[[input$xvar]],
                 df_expanded$Survived)
    
    # Convert table into data frame format
    heat_data <- melt(tab)
    
    # Create heatmap using geom_tile
    ggplot(heat_data,
           aes(Var1, Var2, fill = value)) +
      
      geom_tile() +
      
      # Display numeric count inside each cell
      geom_text(aes(label = value)) +
      
      # Color gradient from white to red
      scale_fill_gradient(low = "white",
                          high = "red") +
      
      theme_minimal() +
      
      labs(x = input$xvar,
           y = "Survived")
  })
  
  
  # ----------------------------------------------------------
  # 4.5. Interaction plot
  # ----------------------------------------------------------
  
  output$interaction_plot <- renderPlot({
    
    # Calculate survival rate by Class and Sex
    interaction_data <- df_expanded %>%
      group_by(Class, Sex) %>%
      summarise(rate = mean(Survived == "Yes"),
                .groups = "drop")
    
    # Plot interaction lines
    ggplot(interaction_data,
           aes(x = Class,
               y = rate,
               color = Sex,
               group = Sex)) +
      
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      
      theme_minimal() +
      
      labs(y = "Survival Rate")
  })
  
  
  # ----------------------------------------------------------
  # 4.6. Logistic regression model
  # ----------------------------------------------------------
  
  logistic_model <- reactive({
  
  glm(Survived ~ Class + Sex + Age,
      data = df_expanded,
      family = binomial)
})
  
  output$logistic_plot <- renderPlot({
    
    # Use the reactive model
    model <- logistic_model()
    
    # Compute predicted survival probabilities
    predicted_probs <- predict(model,
                              type = "response")
    
    # Plot distribution of predicted probabilities
    ggplot(data.frame(pred = predicted_probs),
          aes(x = pred)) +
      
      geom_histogram(bins = 30,
                    fill = "purple") +
      
      theme_minimal() +
      
      labs(x = "Predicted Survival Probability",
          y = "Frequency")
  })
  
  
  # ----------------------------------------------------------
  # 4.7. Statistical tests
  # ----------------------------------------------------------
  
  output$stats_output <- renderPrint({
    
    # Build contingency table
    tab <- table(df_expanded[[input$xvar]],
                 df_expanded$Survived)
    
    # Perform Chi-square independence test
    chi <- chisq.test(tab)
    
    # Compute Cramér's V (effect size measure)
    n <- sum(tab)
    cramer_v <- sqrt(chi$statistic /
                     (n * (min(dim(tab)) - 1)))
    
    # Print results
    cat("Chi-square p-value:", format(chi$p.value, scientific = TRUE), "\n")
    cat("Cramér's V (effect size):",
        round(cramer_v, 3), "\n")
  })
}


# ----------------------------------------------------------
# 5. Run the application
# ----------------------------------------------------------

# Launch the Shiny app
shinyApp(ui, server)
