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
# Subset census data for age and gender only 
census_age_fm <- census_data %>% 
    select(SE_T003_01, SE_T003_02, starts_with("SE_T008"), -SE_T008_00, geometry)

# Joining census data and zip_code together
google.crs <- 3857

# Transform crs 
zip_code_google <- zip_code %>% st_transform(crs = google.crs)

st_crs(zip_code_google)

census_data_google <- census_age_fm %>% st_transform((crs=google.crs))

st_crs(census_data_google)

# Join zip_codes onto census_age_fm
ov <- st_join(x = census_data_google, y = zip_code_google %>% select(ZCTA5))

# Total age range and gender population by zip-code
pop_by_zip <- ov %>% st_set_geometry(NULL) %>% group_by(ZCTA5) %>% 
    summarise(across(.cols = starts_with("SE"), .fns = sum))

colnames(pop_by_zip) <- c("zipcode", "male", "female", "under_5", "5-9",
                          "10-14","15-17", "18-24", "25-34", "35-44","45-54",
                          "55-64", "65-74", "75-84", "over_84")
              
pop_fm_by_zip <- pop_by_zip %>% select(zipcode, male, female)
pop_age_by_zip <- pop_by_zip %>% select(-male, -female)

# Tidy pop data
pop_fm_tidy <- gather(pop_fm_by_zip, 
                   key = "gender", 
                   value = "population",
                   -zipcode)

pop_age_tidy <- gather(pop_age_by_zip,
                       key = "age_range",
                       value="population",
                       -zipcode)
# Test graph
fm_filter = pop_fm_tidy %>% filter(zipcode == 46365) %>% 
    mutate(prop = round(population/sum(.$population)*100,2))
    
    
    ggplot(., aes(x="", y = population , fill=gender)) + 
    geom_bar(stat="identity", width = 1, color="white") +
    coord_polar("y", start=0) +
    theme_void() +
    scale_fill_brewer(palette = "Pastel2") +
    geom_text(aes(y=ypos, label=gender), color="black", size = 6)
    

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
