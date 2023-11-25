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
library(sf)         # For spatial data operations

# Define UI
ui <- fluidPage(
  titlePanel("Exploratory Data Analysis with Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select a variable:", choices = c("median_age", "proportion_foreign_born")),
      numericInput("clusters", "Number of Clusters:", value = 3, min = 2, max = 10)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Variables", plotlyOutput("histPlot"), dataTableOutput("summaryTable"), leafletOutput("mapView")),
        tabPanel("Unsupervised Learning", plotOutput("pcaPlot"), plotOutput("clusterPlot"))
      )
    )
  )
)

# Define server logic
# server <- function(input, output) {
 #  dataset <- reactive({
  #  data.frame(
  #    GEOID = 1:50,
  #    median_age = rnorm(50, mean = 35, sd = 10),
   #   proportion_foreign_born = runif(50, min = 0, max = 1)
   # )
  # })
  
  # Load spatial data for mapping 
 # spatial_data <- reactive({
    # st_read("得下载地图")
    
    # st_as_sf(data.frame(
    #   GEOID = 1:50,
    #  geometry = st_sfc(rep(st_point(c(0, 0)), 50))
    # ), crs = 4326)
  # })
  
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
  output$mapView <- renderLeaflet({
    req(input$variable) 
    data <- dataset()
    spatial <- spatial_data()
    leaflet(spatial) %>%
      addTiles() %>%
      addPolygons(fillColor = ~colorQuantile("Blues", data[[input$variable]])(spatial[[input$variable]]),
                  fillOpacity = 0.7, 
                  color = "#BDBDC3", 
                  weight = 1,
                  popup = ~paste(input$variable, data[[input$variable]])
      )
  })
  
  # For k-means clustering visualization
  output$clusterPlot <- renderPlot({
    req(input$variable) 
    data <- dataset()
    kmeans_result <- kmeans(data[, sapply(data, is.numeric)], centers = input$clusters)
    fviz_cluster(kmeans_result, data = data[, sapply(data, is.numeric)])
  })
}

  # For PCA visualization
  output$pcaPlot <- renderPlot({
   req(input$variable)
   data <- dataset()
    pca_result <- prcomp(data[, sapply(data, is.numeric)], scale. = TRUE)
    fviz_pca_ind(pca_result)
  })

# Return server function
shinyServer(server)
