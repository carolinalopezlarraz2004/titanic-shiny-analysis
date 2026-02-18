############################################################
# Reactive Web Application - Titanic Survival Analysis
# First Assignment - R Shiny
#
# In this project I developed an interactive Shiny application
# to explore survival patterns in the Titanic dataset.
# The objective is to apply reactive programming concepts
# while performing exploratory statistical analysis.
############################################################


# ----------------------------------------------------------
# 1. Load required libraries
# ----------------------------------------------------------

# DT is used to display interactive data tables inside Shiny
# It allows pagination, sorting and searching
library(DT)

# shiny provides the framework to build interactive web apps
library(shiny)

# ggplot2 is used for all data visualizations in this app
# It provides a clean grammar-of-graphics approach
library(ggplot2)

# dplyr is used for data manipulation such as grouping
# and calculating summary statistics
library(dplyr)

# datasets contains built-in R datasets, including Titanic
# This avoids the need to import external CSV files
library(datasets)

# vcd is useful for categorical data visualization
# (even though mosaic plots are not used directly here)
library(vcd)

# reshape2 allows reshaping contingency tables
# It is used to convert tables into data frames for plotting
library(reshape2)


# ----------------------------------------------------------
# 2. Data preparation
# ----------------------------------------------------------

# Load the Titanic dataset (included in base R)
# This dataset is not a regular dataframe
# but a 4-dimensional contingency table
data(Titanic)

# Inspect the internal structure of Titanic
# This helps understand that it is structured as:
# Class x Sex x Age x Survived with frequency counts
str(Titanic)

# Convert the contingency table into a standard dataframe
# After this step, each row represents a combination of:
# Class, Sex, Age, Survived + the number of passengers (Freq)
df <- as.data.frame(Titanic)

# Display the first rows to manually inspect the transformation
# This confirms the structure visually
head(df)

# Check structure again to verify data types
# The first four variables are factors (categorical)
# Freq is numeric (count of passengers)
str(df)

# Expand the frequency counts into individual observations
# This replicates each row according to its Freq value
# After expansion, each row represents one passenger
# The final dataset contains 2201 rows
df_expanded <- df[rep(1:nrow(df), df$Freq), 1:4]


# ----------------------------------------------------------
# 3. User Interface (UI)
# ----------------------------------------------------------

# fluidPage creates a responsive layout
# It adapts automatically to different screen sizes
ui <- fluidPage(
  
  # Title displayed at the top of the app
  titlePanel("Titanic - Data Analysis of Survival"),
  
  # sidebarLayout splits the screen into:
  # - left panel (inputs)
  # - right panel (outputs)
  sidebarLayout(
    
    # Sidebar panel contains user input controls
    sidebarPanel(
      
      # Dropdown menu that allows the user
      # to dynamically choose which variable to analyze
      selectInput(
        inputId = "xvar",                       # internal reference in server
        label = "Choose variable to explore:",  # label shown in app
        choices = c("Class", "Sex", "Age")      # categorical variables available
      )
    ),
    
    # Main panel contains the visual outputs
    mainPanel(
      tabsetPanel(
        
        # First tab contains all statistical analysis plots
        tabPanel("Analysis",
                 
                 # 1. Distribution plot
                 h3("1. How Is This Variable Distributed?"),
                 plotOutput("dist_plot"),
                 
                 # 2. Stacked survival counts
                 h3("2. How Many Survived vs Did Not Survive?"),
                 plotOutput("stacked_plot"),
                 
                 # 3. Survival rate plot
                 h3("3. What Is the Survival Rate in Each Group?"),
                 plotOutput("rate_plot"),
                 
                 # 4. Heatmap
                 h3("4. Survival Counts Heatmap"),
                 plotOutput("heatmap_plot"),
                 
                 # 5. Interaction plot
                 h3("5. Does Class and Sex Interact in Survival?"),
                 plotOutput("interaction_plot"),
                 
                 # 6. Statistical test results
                 h3("6. Are These Differences Statistically Significant?"),
                 verbatimTextOutput("stats_output")
        ),
        
        # Second tab displays the full dataset
        tabPanel("Dataset",
                 h3("Expanded Titanic Dataset"),
                 DTOutput("data_table")  # interactive data table
        )
        
      )
    )
  )
)


# ----------------------------------------------------------
# 4. Server Logic
# ----------------------------------------------------------

# The server function defines how the application reacts
# It links user inputs to dynamic outputs
server <- function(input, output) {
  
  # Render interactive dataset table
  output$data_table <- renderDT({
    datatable(df_expanded,
              options = list(pageLength = 10)) # show 10 rows per page
  })
  
  
  # ----------------------------------------------------------
  # 4.1. Distribution plot
  # ----------------------------------------------------------
  
  output$dist_plot <- renderPlot({
    
    # Create bar plot counting frequency of selected variable
    ggplot(df_expanded, aes(x = .data[[input$xvar]])) +
      
      # geom_bar automatically counts observations
      geom_bar(fill = "steelblue") +
      
      # Minimal theme for clean visual style
      theme_minimal() +
      
      # Dynamic axis labels
      labs(x = input$xvar,
           y = "Count")
  })
  
  
  # ----------------------------------------------------------
  # 4.2. Stacked survival counts
  # ----------------------------------------------------------
  
  output$stacked_plot <- renderPlot({
    
    # Bar plot showing survival outcome distribution
    ggplot(df_expanded,
           aes(x = .data[[input$xvar]], fill = Survived)) +
      
      # Stack survival categories (Yes / No)
      geom_bar(position = "stack") +
      
      theme_minimal() +
      
      labs(x = input$xvar,
           y = "Count")
  })
  
  
  # ----------------------------------------------------------
  # 4.3. Survival rate calculation
  # ----------------------------------------------------------
  
  output$rate_plot <- renderPlot({
    
    # Group dataset by selected variable
    survival_rates <- df_expanded %>%
      
      # Calculate survival probability per category
      group_by(.data[[input$xvar]]) %>%
      summarise(rate = mean(Survived == "Yes"),
                .groups = "drop")
    
    # Plot survival rates
    ggplot(survival_rates,
           aes(x = .data[[input$xvar]], y = rate)) +
      
      geom_col(fill = "darkgreen") +
      
      # Add numeric labels above bars
      geom_text(aes(label = round(rate, 2)),
                vjust = -0.5) +
      
      # Keep y-axis between 0 and 1 (probability scale)
      coord_cartesian(ylim = c(0,1)) +
      
      theme_minimal() +
      
      labs(x = input$xvar,
           y = "Survival Rate")
  })
  
  
  # ----------------------------------------------------------
  # 4.4. Heatmap
  # ----------------------------------------------------------
  
  output$heatmap_plot <- renderPlot({
    
    # Build contingency table
    tab <- table(df_expanded[[input$xvar]],
                 df_expanded$Survived)
    
    # Convert table to dataframe for ggplot
    heat_data <- melt(tab)
    
    # Heatmap using tile geometry
    ggplot(heat_data,
           aes(Var1, Var2, fill = value)) +
      
      geom_tile() +
      
      # Add count labels inside tiles
      geom_text(aes(label = value)) +
      
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
  # 4.6. Statistical tests
  # ----------------------------------------------------------
  
  output$stats_output <- renderPrint({
    
    # Contingency table
    tab <- table(df_expanded[[input$xvar]],
                 df_expanded$Survived)
    
    # Chi-square test for independence
    chi <- chisq.test(tab)
    
    # Effect size calculation (Cramér's V)
    n <- sum(tab)
    cramer_v <- sqrt(chi$statistic /
                     (n * (min(dim(tab)) - 1)))
    
    # Print formatted results
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
