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
business_points <- read.csv("Business_Licenses_geocoded.csv", stringsAsFactors = F) %>% 
    # Filter out businesses that are physically in South Bend IN only
    filter(State == "IN")

# Load in Abandoned Properties
abandoned_spatial <- st_read("Abandoned_Property_Parcels.shp", stringsAsFactors = F)

st_crs(abandoned_spatial)

# Convert bussiness to spatial data
business_spatial <- business_points %>% 
                    st_as_sf(coords = c("X","Y")) %>% 
                    st_set_crs(value = st_crs(abandoned_spatial))

# Clean up zip_code
business_spatial$zip_code <- as.character(business_spatial$Zip_Code) %>%
    gsub('^([0-9]{5})([0-9]+)$', '\\1-\\2', .)


abandoned_spatial$zip_code <- as.character(abandoned_spatial$Zip_Code) %>% 
    gsub('^([0-9]{5})([0-9]+)$', '\\1-\\2', .)


# Create pop-up
business_spatial$popup <- paste("<b>", business_spatial$Business_N, "</b><br>",
                                "Type: ", business_spatial$Classifi_1, sep="") 

abandoned_spatial$popup <- paste('<b>', abandoned_spatial$Property_S, "</b><br>",
                                 "Structure Type: ", abandoned_spatial$Structures, sep="")

# Create color-palette
bus_pal <- colorFactor(palette = "BrBG", alph = T, domain = business_spatial$Classifi_1)
############################################################################

### Ankur



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("South Bend Information"),
    
    # Main panel
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel(title = "Parks and School"
                             ), #end tabPanel Parks and School - Edith
                    
                    tabPanel(title = "Business and Abandoned Lot Map",
                             leafletOutput(outputId = "bus_map")), # end tabPanel Business - Dana
                    
                    tabPanel(title = "Summary Analysis"
                            ) #end tabPanel Summary Analysis - Ankur
                    
        ) #tabsetPanel
           
        ) #mainPanel
    
)# end fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ## Edith
    
    #######################################################################
    
    ## Dana
    output$bus_map <- renderLeaflet({
        leaflet(business_points) %>% 
            
            addTiles() %>% 
            
            fitBounds(~min(X), ~min(Y), ~max(X), ~max(Y)) %>% 
          
            # User CartoDB.Position tile for easy view
            addProviderTiles((providers$CartoDB.Positron))  %>% 
    
            # Add markers for business
            addCircleMarkers(data = business_spatial,
                              popup = ~popup,
                              stroke = F,
                              fillOpacity = 0.8,
                              radius=3,
                              group = "Business") %>%
            
            # Add legend for business
            addLegend(data = business_spatial,
                       values = "Business",
                       opacity = 1,
                       group = "Business") %>%
            
            # Add outline for abandoned properties
            addPolygons(data = abandoned_spatial,
                         popup = ~popup,
                         opacity = 0.5,
                         fillOpacity = 0.5,
                         group = "Abandoned Property") # %>%
            
            # Add legend for abandoned properties
            addLegend(data = abandoned_spatial,
                       values = "Abandoned Property",
                       opacity = 0.5,
                       group = "Abandoned Property") %>%

            # Add layer control
            addLayersControl(overlayGroups = c("Business", "Abandoned Property"),
                              options = layersControlOptions(collapsed = F))
            
    
    
    }) #end buss_map
    ######################################################################
    ## Ankur

    
} # end server

# Run the application 
shinyApp(ui = ui, server = server)
