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

dataScience <- read_excel("state_M2020_dl.xlsx")

dataScience$TOT_EMP <- round(as.numeric(dataScience$TOT_EMP), 2)

######### Anesthiasology Data ##########
anesthesia <- read.csv("residency_clean.csv")

anesthesia$state <-  sub(".*\\b([A-Z]{2}) \\d{5}.*", "\\1", anesthesia$Address)

anesthesia2 <- right_join(dataScience, anesthesia, by = c("PRIM_STATE" = "state"))

dollar <- dollar_format(prefix = "$")

anesthesia2$A_MEDIAN <- dollar(anesthesia2$A_MEDIAN)

######### Family Med Data ###############

family <- read.csv("residency_clean_famMed.csv")

family$state <-  sub(".*\\b([A-Z]{2}) \\d{5}.*", "\\1", family$Address)

family2 <- right_join(dataScience, family, by = c("PRIM_STATE" = "state"))

dollar <- dollar_format(prefix = "$")

family2$A_MEDIAN <- dollar(family2$A_MEDIAN)

######### Pyschiatry Data ###############

psych <- read.csv("residency_clean_psych.csv")

psych$state <-  sub(".*\\b([A-Z]{2}) \\d{5}.*", "\\1", psych$Address)

psych2 <- right_join(dataScience, psych, by = c("PRIM_STATE" = "state"))

dollar <- dollar_format(prefix = "$")

psych2$A_MEDIAN <- dollar(psych2$A_MEDIAN)

ui <- dashboardPage(
  
  dashboardHeader(title = "Residency Site Selector"),
  
  dashboardSidebar(    
    
    sidebarMenu(
      ###### First tab Name
      menuItem("Home", tabName = "home", icon = icon("home")),
      
      ######  Second tab Name
      menuItem("Anesthesia", tabName = "anesthesiazSites", icon = icon("diagnoses")),
      
      ###### 3rd tab Name 
      menuItem("Family Med", tabName = "famMedSites", icon = icon("clinic-medical")),
      
      ###### 4th tab Name
      menuItem("Psychiatry", tabName = "psychSites", icon = icon("brain"))
      
      
    )
    
  ),
  
  ## Body content
  dashboardBody(
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
                
                # Sidebar with a slider input for number of bins 
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
                  mainPanel( leafletOutput(outputId = "map_anesthesia", height = "95vh"),
                             HTML('<a target="_blank" href="https://icons8.com/icon/aQm1MNQRwKAU/health">Health</a> icon by <a target="_blank" href="https://icons8.com">Icons8</a>')
                  ),
                ),
                p(),
                DTOutput('anesthesia_data')
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
                  mainPanel( leafletOutput(outputId = "map_famMed", height = "95vh"),
                             HTML('<a target="_blank" href="https://icons8.com/icon/aQm1MNQRwKAU/health">Health</a> icon by <a target="_blank" href="https://icons8.com">Icons8</a>')
                             
                  )# end mainPanel
                ),
                p(),
                DTOutput('famMed_data')
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
                  mainPanel( leafletOutput(outputId = "map_psych", height = "95vh"),
                             HTML('<a target="_blank" href="https://icons8.com/icon/aQm1MNQRwKAU/health">Health</a> icon by <a target="_blank" href="https://icons8.com">Icons8</a>')
                             
                  )# end mainPanel
                ),
                p(),
                DTOutput('psych_data')
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
  
  
  #health <- makeIcon("https://img.icons8.com/external-line-colors-royyan-wijaya/64/000000/external-health-medical-stuff-line-colors-royyan-wijaya-4.png", 18, 18)
  health <- makeIcon("icons8-health-64.png",
                     iconWidth = 35, iconHeight = 45,
                     iconAnchorX = 22, iconAnchorY = 41)
  
  ############ Anesthisia Server ###############
  output$map_anesthesia <- renderLeaflet({
    
    leaflet(anesthesia2) %>%
      addTiles() %>%
      setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
      addMarkers(~lon, ~lat, icon = health, popup = ~paste("<b>",Program.Name,"<br>",
                                                           "Phone: ",Phone,"<br>",
                                                           "Email: ",Email,"<br>",
                                                           "DS Jobs (State): ", TOT_EMP, "<br>",
                                                           "Median DS Income (State): ", A_MEDIAN,""))
    
    
  })
  
  selectedStatesAnesthesia <- reactive({
    print(input$stateInputsAnesthesia)
    return(anesthesia2[anesthesia2$PRIM_STATE %in% input$stateInputsAnesthesia, ] )
  })
  
  observe({
    print(selectedStatesAnesthesia)
    leafletProxy("map_anesthesia", data = selectedStatesAnesthesia()) %>%
      clearMarkers() %>%
      addMarkers(~lon, ~lat, icon = health, popup = ~paste("<b>",Program.Name,"<br>",
                                                           "Phone: ",Phone,"<br>",
                                                           "Email: ",Email,"<br>",
                                                           "DS Jobs (State): ", TOT_EMP, "<br>",
                                                           "Median DS Income (State): ", A_MEDIAN,"")) 
  })
  
  output$anesthesia_data = renderDT(
    anesthesia[,c(1,3:5,8)], options = list(lengthChange = FALSE)
  )
  
  ############ Family Med Server ###############
  output$map_famMed <- renderLeaflet({
    
    leaflet(family2) %>%
      addTiles() %>%
      setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
      addMarkers(~lon, ~lat, icon = health, popup = ~paste("<b>",Program.Name,"<br>",
                                                           "Phone: ",Phone,"<br>",
                                                           "Email: ",Email,"<br>",
                                                           "DS Jobs (State): ", TOT_EMP, "<br>",
                                                           "Median DS Income (State): ", A_MEDIAN,""))
  })
  
  selectedStatesFamilyMed <- reactive({
    print(input$stateInputsFamMed)
    return(family2[family2$PRIM_STATE %in% input$stateInputsFamMed, ] )
  })
  
  observe({
    print(selectedStatesFamilyMed)
    leafletProxy("map_famMed", data = selectedStatesFamilyMed()) %>%
      clearMarkers() %>%
      addMarkers(~lon, ~lat, icon = health, popup = ~paste("<b>",Program.Name,"<br>",
                                                           "Phone: ",Phone,"<br>",
                                                           "Email: ",Email,"<br>",
                                                           "DS Jobs (State): ", TOT_EMP, "<br>",
                                                           "Median DS Income (State): ", A_MEDIAN,"")) 
  })
  
  output$famMed_data = renderDT(
    family[,c(1,3:5,8)], options = list(lengthChange = FALSE)
  )
  
  ############ Psychiatry Server ###############
  output$map_psych <- renderLeaflet({
    
    leaflet(psych2) %>%
      addTiles() %>%
      setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
      addMarkers(~lon, ~lat, icon = health, popup = ~paste("<b>",Program.Name,"<br>",
                                                           "Phone: ",Phone,"<br>",
                                                           "Email: ",Email,"<br>",
                                                           "DS Jobs (State): ", TOT_EMP, "<br>",
                                                           "Median DS Income (State): ", A_MEDIAN,""))
    
  })
  
  selectedStatesPsych <- reactive({
    print(input$stateInputsFamMed)
    return(psych2[psych2$PRIM_STATE %in% input$stateInputsPsych, ] )
  })
  
  observe({
    print(selectedStatesPsych)
    leafletProxy("map_psych", data = selectedStatesPsych()) %>%
      clearMarkers() %>%
      addMarkers(~lon, ~lat, icon = health, popup = ~paste("<b>",Program.Name,"<br>",
                                                           "Phone: ",Phone,"<br>",
                                                           "Email: ",Email,"<br>",
                                                           "DS Jobs (State): ", TOT_EMP, "<br>",
                                                           "Median DS Income (State): ", A_MEDIAN,"")) 
  })
  
  output$psych_data = renderDT(
    psych[,c(1,3:5,8)], options = list(lengthChange = FALSE)
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
