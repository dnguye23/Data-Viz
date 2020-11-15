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
library(forcats)


skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "blue"

## Edith

# Load school data
school_data <- st_read("School_Boundaries.shp")

# Create popup
school_data$popup <- paste("<b>", school_data$School, "</b><br>",
                       "Type: ", school_data$SchoolType, "<br>")

# Load park data
park_data <- read_csv("Parks_Locations_and_Features.csv")


# create popup
park_data$popup <- paste("<b>", park_data$Park_Name, "</b><br>",
                         "Type: ", park_data$Park_Type, "<br>",
                         "Address: ",park_data$Address, "<br>")

# Load census data
census_data <- st_read("2010_CensusData.shp")


############################################################################
#=======
# Load Zip code file
zip_code <- st_read("SJCZipCodes_clip.shp")

# Review zip code data
# glimpse(zip_code)


### Dana

# Load in Abandoned Properties
abandoned_spatial <- st_read("Abandoned_Property_Parcels.shp")

# Load in Business data
business_points <- read.csv("Business_Licenses_geocoded.csv", stringsAsFactors = F) %>% 
  filter(State == "IN")# Filter out businesses that are physically in South Bend IN only

    ### Convert bussiness to spatial data
business_spatial <- business_points %>% 
  st_as_sf(coords = c("X","Y")) %>% 
  st_set_crs(value = st_crs(abandoned_spatial))

    ### Clean up zip_code. Add "-" in between zip code
business_spatial$Zip_Code <- as.character(business_spatial$Zip_Code) %>%
  gsub('^([0-9]{5})([0-9]+)$', '\\1', .) %>% as.integer(.)


abandoned_spatial$Zip_Code <- as.character(abandoned_spatial$Zip_Code) %>% 
  gsub('^([0-9]{5})([0-9]+)$', '\\1', .) %>% as.integer(.)

    ### Remove geometry data from the business and abandoned df
business_nogeo <- business_spatial %>% st_set_geometry(NULL)

abandoned_nogeo <- abandoned_spatial %>% st_set_geometry(NULL)

    ### Create pop-up
business_spatial$popup <- paste("<b>", business_spatial$Business_N, "</b><br>",
                                "Type: ", business_spatial$Classifi_1, sep="") 

abandoned_spatial$popup <- paste('<b>', abandoned_spatial$Property_S, "</b><br>",
                                 "Structure Type: ", abandoned_spatial$Structures, sep="")



# Subset census data for age and gender only 
census_age_fm <- census_data %>% 
    select(SE_T003_01, SE_T003_02, starts_with("SE_T008"), -SE_T008_00, geometry)

    ### Joining census data and zip_code together
google.crs <- 3857

    ### Transform crs 
zip_code_google <- zip_code %>% st_transform(crs = google.crs)

# st_crs(zip_code_google)

census_data_google <- census_age_fm %>% st_transform((crs=google.crs))

# st_crs(census_data_google)

    ### Join zip_codes onto census_age_fm
ov <- st_join(x = census_data_google, y = zip_code_google %>% select(ZCTA5))

    ### Total age range and gender population by zip-code
pop_by_zip <- ov %>% 
    st_set_geometry(NULL) %>% 
    group_by(ZCTA5) %>% 
    summarise(across(.cols = starts_with("SE"), .fns = sum))

colnames(pop_by_zip) <- c("zipcode", "male", "female", "under_5", "5-9",
                          "10-14","15-17", "18-24", "25-34", "35-44","45-54",
                          "55-64", "65-74", "75-84", "over_84")

    ### Create dataframes for gender and age separately 
pop_fm_by_zip <- pop_by_zip %>% select(zipcode, male, female)
pop_age_by_zip <- pop_by_zip %>% select(-male, -female)

    ### Tidy pop data
    # Gender data
pop_fm_tidy <- gather(pop_fm_by_zip, 
                      key = "gender", 
                      value = "population",
                      -zipcode)

    # Age data
pop_age_tidy <- gather(pop_age_by_zip,
                       key = "age_range",
                       value="population",
                       -zipcode)

age_level <- c("under_5", "5-9","10-14","15-17", "18-24", "25-34", 
               "35-44","45-54","55-64", "65-74", "75-84", "over_84")

pop_age_tidy$age_range <- factor(pop_age_tidy$age_range, levels = age_level)
                

#### NEED TO CREATE A LIST OF UNIQUE ZIP CODES FOR SCHOOLS, PARKS, BUSINESS AND ABANDONED LOTS 

zipcode_list = c(park_data$Zip_Code, business_spatial$Zip_Code, abandoned_spatial$Zip_Code) # Edith to add school_data$Zip_Code

unique_zip = unique(zipcode_list)

############################################################################

### Ankur

header <- dashboardHeader(
  title = "Dashboard"
) # end header


filter_s <- selectInput(inputId = "zipcode", 
                        label = "Choose a Zip Code",
                        choices = unique_zip, 
                        selected = 46617
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

            box(
                selectInput(inputId = "stype",
                        label = "Select School Type:",
                        choices = c("Private", "Public")),

                selectInput(inputId = "ptype",
                        label = "Select Park Type:",
                        choices = types),
               ),

            #column(12,
            #navbarPage(
             #  title = "Statistics",
              #tabPanel("Demographics")
              #),
               # end second box
            ),


            ), # end fluidRow

        #),# end tabItem

    # End parks and schools tab item Edith


    #### DANA ##################
    
    tabItem("business",
            fluidRow(
              box(
                title = "Map", width = 12,
                status = "primary",
                leafletOutput(outputId = "bus_map")
                
                 )  # end box
              ), # end fluidRow 1
            
            fluidRow(
              box(
                title = "Age Distribution", solidHeader = T, width = 6,
                radioButtons(inputId = "age_choice", label = "",
                             choices = c("Population Value" = "pop" , "Population Percentage" = "prop" ),
                             selected = "pop"),
                uiOutput(outputId = "age_plot_or_warning")
                ), # end box
              
              box(
                title = "Gender Distribution", solidHeader = T, width = 6,
                radioButtons(inputId = "fm_choice", label = "",
                             choices = c("Population Value" = "pop", "Population Percentage" = 'prop'),
                             selected = "pop"),
                uiOutput(outputId = 'gender_plot_or_warning')
              )# end box
            ) # end fluidRow 2
            
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
    
    ### TEMPLATE
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
#<<<<<<< HEAD
    
    # filter data based on school type selection
    dataSchool <- eventReactive(input$stype, {
                  return(school_data[school_data$SchoolType == input$stype,])
    })
    
    # filter data based on park type selection
   
    zipParks <- eventReactive(input$zipcode, {
                return(park_data[park_data$Zip_Code == input$zipcode,])
    })
    
    dataParks <- eventReactive(input$ptype, {
                 return(zipParks()%>%filter(Park_Type == input$ptype))
    })
    
    # output the school type and park type
    output$map <- renderLeaflet({
                leaflet(data = dataSchool()) %>%
                addTiles() %>%
                addPolygons(data = dataSchool(), popup = ~popup) %>%
                addMarkers(data = dataParks(), popup = ~popup)
    })
  
    park_zip <- reactive({
        park_data %>% filter(Zip_Code %in% input$zipcode)
    })

  #######################################################################
  
  ## Dana
    
  #### MAP for BUSINESS AND ABANDONED LOTS
  ## Subset business and abandoned data based on zip code input
  business_zip <- reactive({
    business_spatial %>% filter(Zip_Code == input$zipcode)
  })
  
  abandoned_zip <- reactive({
    abandoned_spatial %>% filter(Zip_Code == input$zipcode)
  })
  
  # Create map
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
                colors = 'steelblue',
                opacity = 1,
                group = "Business") %>%
      
      # Add outline for abandoned properties
      addPolygons(data = abandoned_zip(),
                  popup = ~popup,
                  color = '#CC6666',
                  opacity = 0.5,
                  fillOpacity = 0.5,
                  group = "Abandoned Property") %>%
      
      # Add legend for abandoned properties
      addLegend(data = abandoned_zip(),
                labels = "Abandoned Properties",
                colors = '#CC6666',
                opacity = 0.5,
                group = "Abandoned Property") %>%
      
      # Add layer control
      addLayersControl(overlayGroups = c("Business", "Abandoned Property"),
                       options = layersControlOptions(collapsed = F)) 
    
  }) #end bus_map

    #### BAR GRAPH FOR AGE DISTRIBUTION
    ## Subset age data based on zip code
    age_zip <- reactive({
      
      age_filter <- pop_age_tidy %>% 
        filter(zipcode == input$zipcode) %>% 
        mutate(prop = round(population/sum(.$population) * 100, 2))
      
      return(age_filter)
    }) #end age_zip
    
    ## Switch between population and proportion 
    age_graph <- reactive({
      switch(input$age_choice,
             
             pop = ggplot(age_zip(), aes(x = age_range, y = population)) +
                    geom_bar(stat = "identity", fill = "#9999CC") +
                    labs(x = "Age Range", y = "Population") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(size = 12),
                          axis.title.x = element_text(size = 18),
                          axis.text.y = element_text(size = 12),
                          axis.title.y = element_text(size = 18)), 
            
            prop = ggplot(age_zip(), aes(x = age_range, y = prop)) +
                    geom_bar(stat = "identity", fill = "#9999CC") +
                    labs(x = "Age Range", y = "Population %") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(size = 12),
                          axis.title.x = element_text(size = 18),
                          axis.text.y = element_text(size = 12),
                          axis.title.y = element_text(size = 18))
      ) # end switch
      
    }) # end age_graph
    
    ## Display age plot or warning
    output$age_plot <- renderPlot(age_graph())
    
    output$age_warning <- renderText({
      
      paste("There is no Age data for ", input$zipcode, ". Please select another zip code.")
      
    }) # end age_warning
    
    output$age_plot_or_warning <- renderUI({
      
      if(input$zipcode %in% pop_age_tidy$zipcode) {
        plotOutput("age_plot")
        
      }
      
      else{
        textOutput("age_warning")
      }
    })# end age_plot_or_waring
    
    
    #### PIE GRAPH FOR GENDER DISTRIBUTION 
    ## Subset gender data based on zip code
    fm_zip <- reactive({

      fm_filter <- pop_fm_tidy %>%
        filter(zipcode == input$zipcode) %>%
        mutate(prop = round(population/sum(.$population) * 100, 1))

      fm_filter$ypos_prop <- c(80,20)

      return(fm_filter)
    }) # end fm_zip

    ## Switch between population and proportion
    gender_graph <- reactive({
      switch(input$fm_choice,
             
             pop = ggplot(fm_zip(), aes(x = "", y = population, fill = gender)) +
                     geom_col(width = 1, color = "white") +
                     coord_polar("y", start = 0) +
                     ggrepel::geom_text_repel(aes(label = population), color = "navy", size = 6) +
                     scale_fill_brewer(palette = "Pastel2") +
                     theme_void() +
                     theme(legend.text = element_text(size = 15), legend.title = element_text(face = 'bold', size = 25)),

             prop =  ggplot(fm_zip(), aes(x = "", y = prop, fill = gender)) +
                     geom_bar(stat = "identity", width = 1, color = "white") +
                     coord_polar("y", start = 0) +
                     geom_text(aes(y = ypos_prop, label = paste0(prop, "%")), color = "navy", size = 6) +
                     scale_fill_brewer(palette = "Pastel2") +
                     theme_void() +
                     theme(legend.text = element_text(size = 15), legend.title = element_text(face = 'bold', size = 25))
             
      ) # end switch
    }) # end gender_graph
    
    ## Display gender plot or warning 
    
    output$gender_plot <- renderPlot(gender_graph())
    
    output$gender_warning <- renderText({
      
      paste("There is no Gender data for ", input$zipcode, ". Please select another zip code.")
      
    })# end gender_warning
    
    output$gender_plot_or_warning <- renderUI({
      
      if(input$zipcode %in% pop_fm_tidy$zipcode) {
        plotOutput("gender_plot")
      }
      
      else{
        textOutput("gender_warning")
      }
    })# end gender_plot_or_waring
    
  
  

  
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

