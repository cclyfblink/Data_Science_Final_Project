# Load necessary libraries
library(plotly)
library(leaflet)
library(shiny)
library(DT)

# Define the sidebar panel
sidebar <- sidebarPanel(
  # Output text area for data description
  textOutput("data_description_text")
)

# Define the main panel
main <- mainPanel(
  tabsetPanel(
    # First tab for variables
    tabPanel("Variables",
             # Dropdown menu for variable selection
             h2("Choose a Variable Here"),
             selectInput("variable", 
                         "Choose a variable:", 
                         choices = c("median_age", "proportion_disability_35to64", "proportion_married_couple_families",
                                     "proportion_school_enrolled", "proportion_poverty", "proportion_no_earners",
                                     "median_household_income", "proportion_received_food_stamps", 
                                     "proportion_not_in_labor_force", "proportion_with_computing_devices")), 
             
             h2("1. Data (Variable) Description"),  # add a title
             textOutput("variable_description_text"),
             
             h2("2. Histogram Plot"),  # add a title
             plotlyOutput("histPlot"),
             
             h2("3. Summary Table"),  # add a title
             dataTableOutput("summaryTable"),
             
             h2("4. US Map View"),  # add a title
             plotlyOutput("mapView")
    ),
    # Second tab for unsupervised learning
    tabPanel("Unsupervised Learning",
             # Slider input for number of clusters
             h2("Choose a K Value here"),  # add a title
             sliderInput("clusters", "Number of Clusters:", min = 2, value = 2, max = 12, step = 1),
             
             h2("1. Clustered US Map View"),  # add a title
             plotlyOutput("clusterMap"),
             
             h2("2. Elbow Plot"),  # add a title
             plotlyOutput("elbowPlot"),
             
             h2("3. Summary and Conclusion"),  # add a title
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