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
              choices = c("median_age", "proportion_disability_35to64", "proportion_married_couple_families",
                          "proportion_school_enrolled", "proportion_poverty", "proportion_no_earners",
                          "median_household_income", "proportion_received_food_stamps", 
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
             sliderInput("clusters", "Number of Clusters:", min = 2, value = 2, max = 12, step = 1),
             plotlyOutput("clusterMap"),
             plotlyOutput("elbowPlot"),
             # Add the updated descriptive text at the bottom with a line break
             HTML('<div style="margin-top: 20px;"><p>Drawing upon the elbow plot depicted beneath the map, 
             we ascertain that the ideal number of clusters, denoted as k, is 2.
                  <br>We\'ve chosen variables that reflect the overall quality of life in each state. 
                  While we can\'t definitively state which cluster leads a more affluent life, 
                  it\'s evident that they exhibit differences in numerous aspects concerning the chosen variables.</p></div>')
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