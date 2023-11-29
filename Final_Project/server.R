library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(dplyr)
library(tidycensus)
library(cluster)    # For k-means clustering
library(factoextra) # For visualizing clusters
library(tidyverse)  # For data manipulation
library(usmap)      # For plotting US maps
library(here)       # For relative paths
library(datasets)   # For state name abbreviations
library(RColorBrewer) # For color palettes


# Define server logic
server <- function(input, output) {
  dataset <- reactive({
  read_csv(here("data/variables.csv"))
})
  
  # For histogram
  output$histPlot <- renderPlotly({
    req(input$variable) # Require the input before proceeding
    data <- dataset()
    gg <- ggplot(data, aes_string(x = input$variable)) + 
      geom_histogram(bins = 30, fill = "blue") +
      theme_minimal()
    ggplotly(gg)
  })
  
  # For summary table
  output$summaryTable <- renderDataTable({
    req(input$variable) # Require the input before proceeding
    data <- dataset()
    summarise(data,
              Min = min(!!sym(input$variable), na.rm = TRUE),
              Q1 = quantile(!!sym(input$variable), 0.25, na.rm = TRUE),
              Median = median(!!sym(input$variable), na.rm = TRUE),
              Q3 = quantile(!!sym(input$variable), 0.75, na.rm = TRUE),
              Max = max(!!sym(input$variable), na.rm = TRUE),
              Mean = mean(!!sym(input$variable), na.rm = TRUE),
              SD = sd(!!sym(input$variable), na.rm = TRUE),
              Missing = sum(is.na(!!sym(input$variable)))
    )
  })

  # For map visualization
  output$mapView <- renderPlotly({
    req(input$variable)
    # Map state names to state abbreviations
    data <- dataset() %>%
      mutate(state = c(state.abb, 'DC', 'PR')[match(STATE, c(state.name, 'District of Columbia', 'Puerto Rico'))])
    
    # Aggregate input variable's data by state
    state_data <- data %>% 
      group_by(state) %>%
      mutate(Value = !!sym(input$variable))
    
    # Plot using usmap and ggplot2
    plot_usmap(regions = "states", data = state_data, values = "Value") +
      scale_fill_continuous(name = input$variable, low = "white", high = "blue", na.value = "grey50") +
      theme(legend.position = "right")
  })
  
  # For map with colored clusters
  output$clusterMap <- renderPlotly({
    req(input$variable)
    data <- dataset()
    kmeans_result <- kmeans(data[, sapply(data, is.numeric)], centers = input$clusters)
    data$cluster <- as.factor(kmeans_result$cluster)
    # Map state names to state abbreviations
    data <- data %>%
      mutate(state = c(state.abb, 'DC', 'PR')[match(STATE, c(state.name, 'District of Columbia', 'Puerto Rico'))])
    
    # Color states by cluster label
    state_data <- data %>% 
      group_by(state) %>%
      mutate(Cluster = cluster)
    
    # Plot using usmap and ggplot2
    plot_usmap(regions = "states", data = state_data, values = "Cluster") +
      scale_fill_brewer(palette = "Paired", name = "Cluster") +
      theme(legend.position = "right")
  })
  
  # Define wss function used for elbow plot
  wss <- function(k, data) {
  kmeans(data, k, nstart = 50)$tot.withinss
  }
  
  # For elbow plot
  output$elbowPlot <- renderPlotly({
    req(input$variable)
    data <- dataset()
    k_values <- 1:(input$clusters + 5)
    wss_values <- map_dbl(k_values, wss, data = data %>% select(input$variable))
    wss_values <- tibble(wss = wss_values,
                         k = k_values)
    elbow <- wss_values %>% 
      ggplot(aes(x = k, y = wss)) +
      geom_point() + 
      geom_line() +
      scale_x_continuous(breaks = k_values) + # Ensure all integer values are shown
      theme_minimal()
    ggplotly(elbow)
  })
  

}
# Return server function
shinyServer(server)
