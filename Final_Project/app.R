library(shiny)
library(leaflet)
library(rgdal)
library(sf)
library(magrittr)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("South Bend Information"),
    
    # Main panel
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel(title = "Parks and School",
                             ), #end tabPanel Parks and School - Edith
                    
                    tabPanel(title = "Business",
                             ), # end tabPanel Business - Dana
                    
                    tabPanel(title = "Summary Analysis",
                             ) #end tabPanel Summary Analysis - Ankur
                    
        ) #tabsetPanel
           
        ) #mainPanel
    
)# end fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ## Edith
    
    ## Dana
    
    ## Ankur

    
} # end server

# Run the application 
shinyApp(ui = ui, server = server)
