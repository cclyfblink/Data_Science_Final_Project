library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(dplyr)
library(usmap)
library(tidycensus)
library(cluster)    # For k-means clustering
library(factoextra) # For visualizing clusters
library(tidyverse)  # For data manipulation
library(usmap)      # For plotting US maps
library(here)       # For relative paths
library(datasets)   # For state name abbreviations
library(RColorBrewer) # For color palettes
library(mosaic) # For summary table
library(latex2exp)

# Define server logic
server <- function(input, output) {
  dataset <- reactive({
  read_csv(here("data/variables.csv"))
  })
  scaled_data <- reactive(
    read_csv(here("data/scaled_variables.csv"))
  )
  
  # Data description text
  output$data_description_text <- renderText({

    # Provide a overall brief description of our data
    "We used 2021's 5-year ACS data in a national-level. We explore ten tables including B01002, B11012, B14001, B17020,
    B18101, B19122, B22001, B23025, B28010, B29004. And Ten variables were created: 
    median_age, proportion_married_couple_families, proportion_school_enrolled, 
    proportion_poverty, proportion_disability_35to64, proportion_no_earners, proportion_received_food_stamps,
    proportion_not_in_labor_force, proportion_with_computing_devices, median_household_income. 
    In the case of proportional variables, such as proportion_poverty, 
    we construct them by dividing the number of units in interest by the total number. 
    In the case of a single-valued variable, such as median_age, 
    we extract the corresponding variable directly from the corresponding table."
  })
  
  # Each variable description text
  output$variable_description_text <- renderText({
    req(input$variable) # Require the input before proceeding
    data <- dataset()
    table_explored_list <- c('B01002', 'B11012', 'B14001', 'B17020', 'B18101',
                             'B19122', 'B22001', 'B23025', 'B28010', 'B29004')
    variable_chosen = c("median_age", "proportion_married_couple_families",
                        "proportion_school_enrolled", "proportion_poverty", "proportion_disability_35to64",
                        "proportion_no_earners", "proportion_received_food_stamps", 
                        "proportion_not_in_labor_force", "proportion_with_computing_devices",
                        "median_household_income")
    table_used <- table_explored_list[variable_chosen == input$variable]
    variable_creation <- c('median ages in each states',
                           'in married-couple families over total families',
                           'enrolled in school population over total population',
                           'in the past 12 months below poverty level population over total population',
                           '35-64 years old with disability population over 35-64 years old population',
                           'no earners families over total families',
                           'Household received food stamps in the past 12 months over total population',
                           'not in labor force population / total population',
                           'having one or more types of computing devices popultaion
                           over total popultaion',
                           'median household income for households with a citizen, voting-age householder ')
    created_text <- variable_creation[variable_chosen == input$variable]
    
    paste0('The variable name is "',  input$variable, '", and the table explored is "', table_used, 
           '". And the variable was created by ', created_text, '.')
  })
    
  
  # For histogram
  output$histPlot <- renderPlotly({
    req(input$variable) # Require the input before proceeding
    data <- dataset()
    gg <- ggplot(data, aes_string(x = input$variable)) + 
      geom_histogram(bins = 30, fill = "dodgerblue", color = "black") +
      labs(x = input$variable, y = "Count", title = paste0("Histogram of ", input$variable)) +
      theme_minimal()
    ggplotly(gg)
  })
  
  # For summary table
  output$summaryTable <- renderDataTable({
    req(input$variable) # Require the input before proceeding
    data <- dataset()
    # Use favstats method
    summary_table <- round(favstats(~ data[[input$variable]]), digits = 3)
    datatable(summary_table, options = list(scrollX = TRUE))
  })

  # For map visualization
  output$mapView <- renderPlotly({
    req(input$variable)
    # Map state names to state abbreviations
    data <- dataset() %>%
      mutate(state = c(state.abb, 'DC')[match(STATE, c(state.name, 'District of Columbia'))])
    
    # Aggregate input variable's data by state
    state_data <- data %>% 
      group_by(state) %>%
      mutate(Value = !!sym(input$variable))
    
    # Plot using usmap and ggplot2
    plot_usmap(regions = "states", data = state_data, values = "Value", labels = TRUE) +
      scale_fill_continuous(low = "white", high = "blue", na.value = "grey50") +
      theme(legend.position = "right")
  })
  
  # For map with colored clusters
  output$clusterMap <- renderPlotly({
    set.seed(123)
    data <- scaled_data()
    kmeans_result <- kmeans(data[, sapply(data, is.numeric)], centers = input$clusters)
    data$cluster <- as.factor(kmeans_result$cluster)
    # Map state names to state abbreviations
    data <- data %>%
      mutate(state = c(state.abb, 'DC')[match(STATE, c(state.name, 'District of Columbia'))])
    
    # Color states by cluster label
    state_data <- data %>% 
      group_by(state) %>%
      mutate(Cluster = cluster)
    
    # Plot using usmap and ggplot2
    plot_usmap(regions = "states", data = state_data, values = "Cluster", labels = TRUE) +
      scale_fill_brewer(palette = "Paired", name = "Cluster") +
      theme(legend.position = "right")
  })
  
  # Define wss function used for elbow plot
  wss <- function(k, data) {
    kmeans(data, k, nstart = 50)$tot.withinss
  }
  
  # For elbow plot
  output$elbowPlot <- renderPlotly({
    data <- scaled_data() # Ensure this function is defined in your server logic
    k_values <- 1:12
    wss_values <- map_dbl(k_values, wss, data = data[, sapply(data, is.numeric)])
    wss_values <- tibble(wss = wss_values, k = k_values)
    elbow <- wss_values %>% 
      ggplot(aes(x = k, y = wss)) +
      geom_point() + 
      geom_line() +
      scale_x_continuous(breaks = k_values) + # Ensure all integer values are shown
      labs(title = "Elbow Plot for Optimal k",
           x = "Number of Clusters (k)",
           y = "Total Within Sum of Squares (WSS)") + # Add axis titles and plot title
      theme_minimal()
    ggplotly(elbow)
  })
  
  # Search functionality for the summary table
  output$searchResults <- renderText({
    req(input$variable, input$search_stat)
    data <- dataset()
    chosen_variable <- input$variable
    stat_name <- input$search_stat
    
    # Assuming favstats returns a data frame with the statistic names as column names
    stats <- favstats(~ data[[chosen_variable]])
    # Retrieve the value of the requested statistic
    stat_value <- stats[[tolower(stat_name)]]
    
    if (!is.null(stat_value)) {
      paste(stat_name, "for", chosen_variable, "is", stat_value)
    } else {
      paste("Statistic", stat_name, "not found for", chosen_variable)
    }
  })
  
  # Observe event for when the search button is pressed
  observeEvent(input$search, {
    output$searchResults
  })
}

# Return server function
shinyServer(server)
