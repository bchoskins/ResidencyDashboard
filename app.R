#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(ggplot2)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(rgdal)
library(scales)
library(shinydashboard)
library(DT)
library(shinyjs)

dataScience <- read_excel("state_M2020_dl.xlsx")

dataScience$TOT_EMP <- round(as.numeric(dataScience$TOT_EMP), 2)

######### Anesthiasology Data ##########
anesthesia <- read.csv("residency_clean.csv")

anesthesia$state <-  sub(".*\\b([A-Z]{2}) \\d{5}.*", "\\1", anesthesia$Address)

# anesthesia2 <- right_join(dataScience, anesthesia, by = c("PRIM_STATE" = "state"))
# 
# dollar <- dollar_format(prefix = "$")
# 
# anesthesia2$A_MEDIAN <- dollar(anesthesia2$A_MEDIAN)

######### Family Med Data ###############

family <- read.csv("residency_clean_famMed.csv")

family$state <-  sub(".*\\b([A-Z]{2}) \\d{5}.*", "\\1", family$Address)

# family2 <- right_join(dataScience, family, by = c("PRIM_STATE" = "state"))
# 
# dollar <- dollar_format(prefix = "$")
# 
# family2$A_MEDIAN <- dollar(family2$A_MEDIAN)

######### Pyschiatry Data ###############

psych <- read.csv("residency_clean_psych.csv")

psych$state <-  sub(".*\\b([A-Z]{2}) \\d{5}.*", "\\1", psych$Address)

# psych2 <- right_join(dataScience, psych, by = c("PRIM_STATE" = "state"))
# 
# dollar <- dollar_format(prefix = "$")
# 
# psych2$A_MEDIAN <- dollar(psych2$A_MEDIAN)


##################### Begin UI ############################ 
ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Residency Site Selector"),
    
    dashboardSidebar( collapsed = TRUE,
        
        sidebarMenu( id = "tabs",
            ###### First tab Name
            menuItem("Home", tabName = "home", icon = icon("home")),
            
            ######  Second tab Name
            menuItem("Anesthesia", tabName = "anesthesiazSites", icon = icon("diagnoses")),
            
            ###### 3rd tab Name 
            menuItem("Family Med", tabName = "famMedSites", icon = icon("clinic-medical")),
            
            ###### 4th tab Name
            menuItem("Psychiatry", tabName = "psychSites", icon = icon("brain")),
            
            ###### 5th tab Name
            menuItem("Match Data", tabName = "matchData", icon = icon("file-pdf"))
            
        )
    ),
    
    ## Body content
    dashboardBody(
        useShinyjs(),
        tabItems(
            tabItem(tabName = "home",
                fluidPage(
                    # Application title
                    titlePanel("Welcome to your Residency Research Tool!"),

                    mainPanel(
                        h2("Things to consider: "),
                        HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/yQLD_HZPBwM" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                        h2("The Match Algorithm: "),
                        HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/mFh3JIaMGJo" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')    
                    )
                )
            ),
            tabItem(tabName = "anesthesiazSites",
                    fluidPage(
                        # Application title
                        titlePanel("Location of Anesthesia Residency Sites"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                    pickerInput("stateInputsAnesthesia", label = h3("State"), options = list(`actions-box` = TRUE), multiple = TRUE,
                                                choices = c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "FL", "GA", "IA", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
                                                            "ME", "MI", "MN", "MO", "MS", "NC", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR",
                                                            "RI", "SC", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV"
                                                ),
                                                selected = c(  "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "FL", "GA", "IA", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
                                                               "ME", "MI", "MN", "MO", "MS", "NC", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR",
                                                               "RI", "SC", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV"))
                            ),
                            mainPanel(
                                tabsetPanel( type = "tabs",
                                    tabPanel("Map", leafletOutput(outputId = "map_anesthesia", height = "95vh"),
                                       HTML('<a target="_blank" href="https://icons8.com/icon/aQm1MNQRwKAU/health">Health</a> icon by <a target="_blank" href="https://icons8.com">Icons8</a>')),
                                    tabPanel("Data", DTOutput('anesthesia_data'))
                                )
                            )
                        )
                    )
            ),
            tabItem(tabName = "famMedSites", 
                    fluidPage(
                        # Application title
                        titlePanel("Location of Family Medicine Residency Sites"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
                                pickerInput("stateInputsFamMed", label = h3("State"), options = list(`actions-box` = TRUE), multiple = TRUE,
                                            choices = c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "FL", "GA", "IA", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
                                                        "ME", "MI", "MN", "MO", "MS", "NC", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR",
                                                        "RI", "SC", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV"
                                            ),
                                            selected = c( "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "FL", "GA", "IA", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
                                                           "ME", "MI", "MN", "MO", "MS", "NC", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR",
                                                           "RI", "SC", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV"))
                            ),
                            mainPanel( 
                                tabsetPanel( type = "tabs",
                                    tabPanel("Map", leafletOutput(outputId = "map_famMed", height = "95vh"),
                                       HTML('<a target="_blank" href="https://icons8.com/icon/aQm1MNQRwKAU/health">Health</a> icon by <a target="_blank" href="https://icons8.com">Icons8</a>')),
                                    tabPanel("Data", DTOutput('famMed_data'))
                                )
                            )# end mainPanel
                        )
                    )
            ),
            tabItem(tabName = "psychSites", 
                    fluidPage(
                        # Application title
                        titlePanel("Location of Psychiatry Residency Sites"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
                                pickerInput("stateInputsPsych", label = h3("State"), options = list(`actions-box` = TRUE), multiple = TRUE,
                                            choices = c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "FL", "GA", "IA", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
                                                        "ME", "MI", "MN", "MO", "MS", "NC", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR",
                                                        "RI", "SC", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV"
                                            ),
                                            selected = c( "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "FL", "GA", "IA", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
                                                          "ME", "MI", "MN", "MO", "MS", "NC", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR",
                                                          "RI", "SC", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV"))
                            ),
                            mainPanel( 
                                tabsetPanel(type = "tabs",
                                    tabPanel("Map",leafletOutput(outputId = "map_psych", height = "95vh"),
                                       HTML('<a target="_blank" href="https://icons8.com/icon/aQm1MNQRwKAU/health">Health</a> icon by <a target="_blank" href="https://icons8.com">Icons8</a>')),
                                    tabPanel("Data", DTOutput('psych_data'))
                                )
                            )# end mainPanel
                        ),
                    )
            ),
            tabItem(tabName = "matchData",

                        mainPanel(
                            tabsetPanel(
                                tabPanel("Program Results", h1("Historical Match results by program from 2017-2021"),
                                         tags$iframe(style="height:550px; width:100%; scrolling=yes", 
                                                     src="https://www.nrmp.org/wp-content/uploads/2021/08/Program-Results_2017_2021.pdf"),
                                         h5("*Data from https://www.nrmp.org/match-data-analytics/residency-data-reports/")),
                                tabPanel("2021 Report", h1("2021 In-Depth Report"),
                                         h4("Page 58 begins the match results by program, ordered by state."),
                                         tags$iframe(style="height:525px; width:100%; scrolling=yes", 
                                                     src="https://www.nrmp.org/wp-content/uploads/2021/08/MRM-Results_and-Data_2021.pdf"),
                                         h5("*Data from https://www.nrmp.org/match-data-analytics/residency-data-reports/"))
                            )
                        )
            )
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    initial_lat = 37.0902
    initial_lng = -95.7129
    initial_zoom = 3
    
    health <- makeIcon("icons8-health-64.png",
                       iconWidth = 35, iconHeight = 45,
                       iconAnchorX = 22, iconAnchorY = 41)
    
    observeEvent(input$tabs, {
        shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")
    }, ignoreInit = TRUE)
    
    ############ Anesthisia Server ###############
    output$map_anesthesia <- renderLeaflet({
        
        leaflet(anesthesia) %>%
            addTiles() %>%
            setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
            addMarkers(~lon, ~lat, icon = health, popup = ~paste("<b>",Program.Name,"<br>",
                                                  "Phone: ",Phone,"<br>",
                                                  "Email: ",Email,""))

        
    })
    
    selectedStatesAnesthesia <- reactive({
        print(input$stateInputsAnesthesia)
        return(anesthesia[anesthesia$state %in% input$stateInputsAnesthesia, ] )
    })
    
    selectedStatesAnesthesia_DT <- reactive({
        return(anesthesia[anesthesia$state %in% input$stateInputsAnesthesia, c(1,3:5,8)] )
    })
    
    observe({
        print(selectedStatesAnesthesia)
        leafletProxy("map_anesthesia", data = selectedStatesAnesthesia()) %>%
            clearMarkers() %>%
            addMarkers(~lon, ~lat, icon = health, popup = ~paste("<b>",Program.Name,"<br>",
                                                  "Phone: ",Phone,"<br>",
                                                  "Email: ",Email,"")) 
    })
    
    output$anesthesia_data = renderDT(
        #anesthesia[,c(1,3:5,8)], options = list(lengthChange = FALSE)
        {selectedStatesAnesthesia_DT()}, options = list(lengthChange = FALSE)
    )
    
    ############ Family Med Server ###############
    output$map_famMed <- renderLeaflet({
        
        leaflet(family) %>%
            addTiles() %>%
            setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
            addMarkers(~lon, ~lat, icon = health, popup = ~paste("<b>",Program.Name,"<br>",
                                                  "Phone: ",Phone,"<br>",
                                                  "Email: ",Email,""))
    })
    
    selectedStatesFamilyMed <- reactive({
        print(input$stateInputsFamMed)
        return(family[family$state  %in% input$stateInputsFamMed, ] )
    })
    
    selectedStatesFamilyMed_DT <- reactive({
        print(input$stateInputsFamMed)
        return(family[family$state  %in% input$stateInputsFamMed, c(1,3:5,8)] )
    })
    
    observe({
        print(selectedStatesFamilyMed)
        leafletProxy("map_famMed", data = selectedStatesFamilyMed()) %>%
            clearMarkers() %>%
            addMarkers(~lon, ~lat, icon = health, popup = ~paste("<b>",Program.Name,"<br>",
                                                  "Phone: ",Phone,"<br>",
                                                  "Email: ",Email,"")) 
    })
    
    output$famMed_data = renderDT(
        {selectedStatesFamilyMed_DT()}, options = list(lengthChange = FALSE)
    )
    
    ############ Psychiatry Server ###############
    output$map_psych <- renderLeaflet({
        
        leaflet(psych) %>%
            addTiles() %>%
            setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
            addMarkers(~lon, ~lat, icon = health, popup = ~paste("<b>",Program.Name,"<br>",
                                                  "Phone: ",Phone,"<br>",
                                                  "Email: ",Email,""))
        
    })
    
    selectedStatesPsych <- reactive({
        print(input$stateInputsFamMed)
        return(psych[psych$state  %in% input$stateInputsPsych, ] )
    })
    
    selectedStatesPsych_DT <- reactive({
        print(input$stateInputsFamMed)
        return(psych[psych$state  %in% input$stateInputsPsych, c(1,3:5,8)] )
    })
    
    observe({
        print(selectedStatesPsych)
        leafletProxy("map_psych", data = selectedStatesPsych()) %>%
            clearMarkers() %>%
            addMarkers(~lon, ~lat, icon = health, popup = ~paste("<b>",Program.Name,"<br>",
                                                  "Phone: ",Phone,"<br>",
                                                  "Email: ",Email,"")) 
    })
    
    output$psych_data = renderDT(
        selectedStatesPsych_DT(), options = list(lengthChange = FALSE)
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
