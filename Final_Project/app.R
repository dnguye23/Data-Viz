library(shiny)
library(leaflet)
library(rgdal)
library(sf)
library(ggmap)
library(magrittr)
library(tidyverse)
library(stm)

# Load Zip code file
zip_code <- st_read("SJCZipCodes_clip.shp")

# Review zip code data
glimpse(zip_code)

###########################################################################
### Edith

# # Load school data
# school_data <- st_read("School_Boundaries.shp")
# 
# # review school data
# glimpse(school_data)
# 
# # Load park data
# park_data <- read_csv("Parks_Locations_and_Features.csv")
# 
# # review park data
# glimpse(park_data)

# Load census data
census_data <- st_read("2010_CensusData.shp")

# review park data
glimpse(census_data)

############################################################################

### Dana
# Load in Abandoned Properties
abandoned_spatial <- st_read("Abandoned_Property_Parcels.shp")

# Remove geometry var
abandoned_nogeo <- st_set_geometry(abandoned_spatial, NULL)

# Load in Business data
business_points <- read.csv("Business_Licenses_geocoded.csv", stringsAsFactors = F) %>% 
    # Filter out businesses that are physically in South Bend IN only
    filter(State == "IN")

# Convert business to spatial data
business_spatial <- business_points %>% 
                    st_as_sf(coords = c("X","Y")) %>% 
                    st_set_crs(value = st_crs(abandoned_spatial))

# Clean up zip_code. Add "-" in between zip code
business_spatial$zip_code <- as.character(business_spatial$Zip_Code) %>%
                             gsub('^([0-9]{5})([0-9]+)$', '\\1-\\2', .)


abandoned_spatial$zip_code <- as.character(abandoned_spatial$Zip_Code) %>% 
                              gsub('^([0-9]{5})([0-9]+)$', '\\1-\\2', .)


# Create pop-up
business_spatial$popup <- paste("<b>", business_spatial$Business_N, "</b><br>",
                                "Type: ", business_spatial$Classifi_1, sep="") 

abandoned_spatial$popup <- paste('<b>', abandoned_spatial$Property_S, "</b><br>",
                                 "Structure Type: ", abandoned_spatial$Structures, sep="")


############################################################################

### Ankur



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("South Bend Information"),
    
    # Sidebar with for input 
    sidebarLayout(
        sidebarPanel(
            # TextInput for Zip Code 
            textInput(inputId = "zipcode",
                      label = "Zip Code",
                      value = '46601')
        ),
        
        # Main panel
        mainPanel(
            
            # Create tabs
            tabsetPanel(type = "tabs",
                        
                        tabPanel(title = "Parks and School"
                        ), #end tabPanel Parks and School - Edith
                        
                        tabPanel(title = "Business and Abandoned Lot Map",
                                 leafletOutput(outputId = "bus_map"),
                                 leafletOutput(outputId = "age_density")
                        ), # end tabPanel Business - Dana
                        
                        tabPanel(title = "Summary Analysis"
                        ) #end tabPanel Summary Analysis - Ankur
                        
            ) # end tabsetPanel
            
        ) # end mainPanel
        
    ) # end Sidebar 
    
    
)# end fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ## Edith
    
    #######################################################################
    
    ## Dana
    
    # Subset data based on zipcode input
    business_zip <- reactive({
        business_spatial %>% filter(zip_code %in% input$zipcode)
    })
    
    abandoned_zip <- reactive({
        abandoned_spatial %>% filter(zip_code %in% input$zipcode)
    })
    
    output$bus_map <- renderLeaflet({
        leaflet() %>% 
            
            addTiles() %>% 
          
            # User CartoDB.Position tile for easy view
            addProviderTiles(providers$CartoDB.Positron)  %>% 
    
            # Add markers for business
            addCircleMarkers(data = business_zip(),
                              popup = ~popup,
                              stroke = F,
                              fillOpacity = 0.8,
                              radius=3,
                              group = "Business") %>%
            
            # Add legend for business
            addLegend(data = business_zip(),
                       labels = "Businesess",
                       colors = 'blue',
                       opacity = 1,
                       group = "Business") %>%
            
            # Add outline for abandoned properties
            addPolygons(data = abandoned_zip(),
                         popup = ~popup,
                        color = 'red',
                         opacity = 0.5,
                         fillOpacity = 0.5,
                         group = "Abandoned Property") %>%
            
            # Add legend for abandoned properties
            addLegend(data = abandoned_zip(),
                       labels = "Abandoned Properties",
                       colors = 'red',
                       opacity = 0.5,
                       group = "Abandoned Property") %>%

            # Add layer control
            addLayersControl(overlayGroups = c("Business", "Abandoned Property"),
                              options = layersControlOptions(collapsed = F)) 
            
    }) #end bus_map
    
    
    ######################################################################
    ## Ankur

    
} # end server

# Run the application 
shinyApp(ui = ui, server = server)
