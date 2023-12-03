# Load necessary libraries
library(plotly)
library(leaflet)
library(shiny)
library(DT)

# Define the sidebar panel
sidebar <- sidebarPanel(
  # Dropdown menu for variable selection
  selectInput("variable", 
              "Choose a variable:", 
              choices = c("median_age", "proportion_foreign_born", "proportion_married_couple_families",
                          "proportion_school_enrolled", "proportion_poverty", "proportion_no_earners",
                          "proportion_veteran", "proportion_received_food_stamps", 
                          "proportion_not_in_labor_force", "proportion_with_computing_devices")), 
  # Output text area for data description
  textOutput("data_description_text")
)

# Define the main panel
main <- mainPanel(
  tabsetPanel(
    # First tab for variables
    tabPanel("Variables",
             textOutput("variable_description_text"),
             plotlyOutput("histPlot"),
             dataTableOutput("summaryTable"),
             plotlyOutput("mapView")
    ),
    # Second tab for unsupervised learning
    tabPanel("Unsupervised Learning",
             # Slider input for number of clusters
             sliderInput("clusters", "Number of Clusters:", min = 2, value = 3, max = 12, step = 1),
             plotlyOutput("clusterMap"),
             plotlyOutput("elbowPlot")
    )
  )
)

# Define UI
ui <- fluidPage(
  titlePanel("ACS Data Exploratory Analysis"),
  sidebarLayout(sidebar, main)
)

# Return UI object
shinyUI(ui)