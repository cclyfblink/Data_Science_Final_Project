library(plotly)
library(leaflet)
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("ACS Data Exploratory Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a variable:", 
                  choices = c("median_age", "proportion_foreign_born", "proportion_married_couple_families",
                              "proportion_school_enrolled", "proportion_poverty", "proportion_no_earners",
                              "proportion_veteran", "proportion_received_food_stamps", 
                              "proportion_not_in_labor_force", "proportion_with_computing_devices"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Variables", 
                 textOutput("data_description_text"),
                 plotlyOutput("histPlot"),
                 dataTableOutput("summaryTable"),
                 plotlyOutput("mapView")
        ),
        tabPanel("Unsupervised Learning",
                 # Limit max number of clusters to 12 due to palette colors limitation
                 numericInput("clusters", "Number of Clusters:", min = 2, value = 3, max = 12, step = 1),
                 plotlyOutput("clusterMap"),
                 plotlyOutput("elbowPlot")
        )
      )
    )
  )
)

# Return UI object
shinyUI(ui)
