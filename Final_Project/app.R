#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(shinythemes)
library(sf)
library(tidyverse)
library(dplyr)
library(DT)
library(rgdal)
library(sf)
library(ggmap)
library(magrittr)
library(tidyverse)
library(stm)


skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "blue"


# Load school data
schools <- st_read("School_Boundaries.shp")

schools$popup <- paste("<b>", schools$School, "</b><br>",
                       "Type: ", schools$SchoolType, "<br>")
# Load park data
parks <- read_csv("Parks_Locations_and_Features.csv")

parks <- parks %>% 
    st_as_sf(coords = c("Lon", "Lat")) %>%
    st_set_crs(value = 4326)

parks$popup <- paste("<b>", parks$Park_Name, "</b><br>",
                            "Type: ", parks$Park_Type, "<br>",
                            "Address: ",parks$Address, "<br>")
types <- c(unique(parks$Park_Type))

# review park data
#glimpse(park_data)

# Load census data
#census_data <- st_read("2010_CensusData.shp")

# review park data
#glimpse(census_data)


#<<<<<<< HEAD
############################################################################
#=======
# Load Zip code file
#zip_code <- st_read("SJCZipCodes_clip.shp")

# Review zip code data
#glimpse(zip_code)

#>>>>>>> 342992e17b018d97b1faa2d27653d606babb3f1a

### Dana
# Load in Abandoned Properties
abandoned_spatial <- st_read("Abandoned_Property_Parcels.shp")

# Remove geometry var
abandoned_nogeo <- st_set_geometry(abandoned_spatial, NULL)

# Load in Business data
business_points <- read.csv("Business_Licenses_geocoded.csv", stringsAsFactors = F) %>% 
  # Filter out businesses that are physically in South Bend IN only
  filter(State == "IN")

# Convert bussiness to spatial data
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

# Min/Max latitudes and longitude
min_lng = min(business_points$X)
max_lng = max(business_points$X)
min_lat = min(business_points$Y)
max_lat = max(business_points$Y)

############################################################################

### Ankur

header <- dashboardHeader(
  title = "Dashboard"
) # end header




filter_s <- selectInput(inputId = "zipcode", 
                        label = "ZipCode to Choose",
                        choices = park_data$Zip_Code, selected=46617
) # filter string


sidebar <- dashboardSidebar(
  filter_s,
  sidebarMenu(
    menuItem("Parks and Schools", tabName = "schools", icon = icon("map")
    ), #end menuItem Parks and School - Dana
    
    menuItem("Businesses", icon = icon("th"), tabName = "business"
    ), #end menuItem Businesses - Edith
    
    menuItem("Summary Data", icon = icon("table"),
             menuSubItem("Parks", tabName = "park_table"),
             menuSubItem("Businesses", tabName = "business_table")
    ), #end menuItem Summary Data - Ankur
    
    menuItem("About the Project", icon = icon("file-code-o"), tabName="about"
    ), # end menuItem About - Ankur
    
    menuItem("Template to Reuse", tabName = "temps", icon = icon("map")
    ) #end Templates to reuse
    
  ) # end sidebarmenu
) # end sidebar

body <- dashboardBody(
  tabItems(
    # Use the template from here for anything you want to add !!
    
    
    
    #### EDITH ##################
    
    tabItem("schools",
            fluidRow(
            box(
                title = "Schools and Parks",
                status = "primary",
                leafletOutput(outputId = "map")
                ), # end box
                selectInput(inputId = "stype",
                        label = "Select School Type:",
                        choices = c("Private", "Public")),
            
                selectInput(inputId = "ptype",
                        label = "Select Park Type:",
                        choices = types)
            ), # end fluidRow
            
        ),# end tabItem
                     
    # End parks and schools tab item Edith
    
    
    
    #### DANA ##################
    
    tabItem("business",
            fluidRow(
              box(
                title = "Businesses",
                status = "primary",
                leafletOutput(outputId = "bus_map")
              ), # end box
              tabBox(
                height = 300,
                tabPanel("Age Density",
                         leafletOutput(outputId = "age_density")
                ),
                tabPanel("View 2",
                         plotOutput("scatterY", height = 230)
                )
              )# end tabbox
            ), # end fluidRow
    ), # End business tab item Dana
    
    #### ANKUR ##################
    
    tabItem("park_table",
            fluidRow(
              box(
                title = "Park Table",
                status = "primary",
                DT::dataTableOutput("summarytable")
              ), # end box
              
            ), # end fluidRow
    ), # park table - Ankur
    
    tabItem("about",
            fluidRow(
              box(
                title = "About the Project",
                width = 4,
                background = "maroon",
                "This is South-Bend Dashboard. For the residents of south Bend and businesses, this dashboard will show the nearby parks and businesses around your neighborhood.
                        You can navigate through this dashboard, and feel free to let us know if you have any questions.
                        Project Team - Ankur, Dana and Edith"
              ), # end box
              
            ), # end fluidRow
    ),# About - Ankur
    
    tabItem("temps",
            fluidRow(
              box(
                title = "Distribution",
                status = "primary",
                plotOutput("plot1", height = 240),
                height = 300
              ),
              tabBox(
                height = 300,
                tabPanel("View 1",
                         plotOutput("scatter1", height = 230)
                ),
                tabPanel("View 2",
                         plotOutput("scatter2", height = 230)
                )
              )
            ),
            # Boxes with solid headers
            fluidRow(
              box(
                title = "Histogram control", width = 4, solidHeader = TRUE, status = "primary",
                sliderInput("count", "Count", min = 1, max = 500, value = 120)
              ),
              box(
                title = "Appearance",
                width = 4, solidHeader = TRUE,
                radioButtons("fill", "Fill", # inline = TRUE,
                             c(None = "none", Blue = "blue", Black = "black", red = "red")
                )
              ),
              box(
                title = "Scatterplot control",
                width = 4, solidHeader = TRUE, status = "warning",
                selectInput("spread", "Spread",
                            choices = c("0%" = 0, "20%" = 20, "40%" = 40, "60%" = 60, "80%" = 80, "100%" = 100),
                            selected = "60"
                )
              )
            )
    ) # end tab item temps
    
    
  )
)

ui <- shinyUI(
  dashboardPage(
    header,
    sidebar,
    body, skin=skin)
  
)

server <- function(input, output) {
  ## Edith
    
    dataSchool <- eventReactive(input$stype, {
                  return(schools[schools$SchoolType == input$stype,])
    })
    
    dataParks <- eventReactive(input$ptype, {
                 return(parks[parks$Park_Type == input$ptype,])
    })
    
    
    output$map <- renderLeaflet({
                leaflet(data = dataSchool()) %>%
                addTiles() %>%
                addPolygons(popup = ~popup) %>%
                addMarkers(data = dataParks(), popup = ~popup)
    })
  
  #######################################################################
  
  ## Dana
  
  # Subset data based on zipcode input
  business_zip <- reactive({
    business_spatial %>% filter(zip_code %in% input$zipcode)
  })
  
  abandoned_zip <- reactive({
    abandoned_spatial %>% filter(zip_code %in% input$zipcode)
  })
  
  park_zip <- reactive({
    park_data %>% filter(Zip_Code %in% input$zipcode)
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
  
  output$summarytable <- DT::renderDataTable({
    
    parks <- park_data %>%
      select(Park_Name, Park_Type, Zip_Code, Address, Lat, Lon) %>%
      filter(Zip_Code == input$zipcode)
    DT::datatable(parks)
  }) # end summarytable
  
  #### TEMPLATE REUSE - DELETE LATER
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    if (is.null(input$count) || is.null(input$fill))
      return()
    
    data <- histdata[seq(1, input$count)]
    color <- input$fill
    if (color == "none")
      color <- NULL
    hist(data, col = color, main = NULL)
  })
  
  output$scatter1 <- renderPlot({
    spread <- as.numeric(input$spread) / 100
    x <- rnorm(1000)
    y <- x + rnorm(1000) * spread
    plot(x, y, pch = ".", col = "blue")
  })
  
  output$scatter2 <- renderPlot({
    spread <- as.numeric(input$spread) / 100
    x <- rnorm(1000)
    y <- x + rnorm(1000) * spread
    plot(x, y, pch = ".", col = "red")
  })
  
  
  
} # end server




# Run the application 
shinyApp(ui = ui, server = server)

