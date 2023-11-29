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
  output$mapView <- renderPlot({
    req(input$variable)
    # Map state names to state abbreviations
    data <- dataset() %>%
      mutate(state = c(state.abb, 'DC', 'PR')[match(STATE, c(state.name, 'District of Columbia', 'Puerto Rico'))])
    
    # Data description text
    output$data_description_text <- renderText({
      # Provide a brief description of my data
      "We explore ten tables including B01002, B05002, B11012, B14001, B17020,
      B19122, B21001, B22001, B23025, B28010. \n
      Ten variables were created: \n\n
      median_age, proportion_foreign_born, proportion_married_couple_families, 
      proportion_school_enrolled, proportion_poverty, proportion_no_earners,
      proportion_veteran, proportion_received_food_stamps, 
      proportion_not_in_labor_force, proportion_with_computing_devices.\n\n
      1. median_age = median ages in each states; \n
      2. proportion_foreign_born = the number of people borned foreign / totol people;\n
      3. proportion_married_couple_families = in married-couple families / total families
      "
    })
    
    # Aggregate input variable's data by state
    state_data <- data %>% 
      group_by(state) %>%
      mutate(Value = !!sym(input$variable))
    
    # Plot using usmap and ggplot2
    plot_usmap(regions = "states", data = state_data, values = "Value") +
      scale_fill_continuous(name = input$variable, low = "white", high = "blue", na.value = "grey50") +
      theme(legend.position = "right")
  })
  
  # For k-means clustering visualization
  output$clusterPlot <- renderPlot({
    req(input$variable) 
    data <- dataset()
    kmeans_result <- kmeans(data[, sapply(data, is.numeric)], centers = input$clusters)
    fviz_cluster(kmeans_result, data = data[, sapply(data, is.numeric)])
  })
}

# Return server function
shinyServer(server)
