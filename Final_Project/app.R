library(shiny)
library(leaflet)
library(rgdal)
library(sf)
library(ggmap)
library(magrittr)
library(tidyverse)

### Edith


############################################################################

### Dana
# Load in Business data
business_points <- read.csv("Business_Licenses_geocoded.csv", stringsAsFactors = F)

# Load in Abandoned Properties
abandoned_spatial <- st_read("Abandoned_Property_Parcels.shp", stringsAsFactors = F)

st_crs(abandoned_spatial)

# Convert bussiness to spatial data
business_spatial <- business_points %>% 
                    st_as_sf(coords = c("X","Y")) %>% 
                    st_set_crs(value = st_crs(abandoned_spatial))


############################################################################

### Ankur



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("South Bend Information"),
    
    # Main panel
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel(title = "Parks and School",
                             ), #end tabPanel Parks and School - Edith
                    
                    tabPanel(title = "Business and Abandoned Lot Map",
                             leafletOutput(outputId = "buss_map")), # end tabPanel Business - Dana
                    
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
