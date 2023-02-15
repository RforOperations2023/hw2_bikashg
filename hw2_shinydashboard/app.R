library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

# Load and clean data ----------------------------------------------

country_profile <- read.csv('selected_country.csv')

country_profile <- country_profile %>%
  mutate(Region = as.factor(Region))

# Avoid plotly issues ----------------------------------------------
pdf(NULL)


# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Country Profile Dashboard",

                          # Drop down menu with hard coded values ------------------------------
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "Interested in International Development, apply here", 
                                                        icon = icon("users"))
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 90, color = "green",
                                                "Previous Application")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Stanford University",
                                         message = HTML("You are in the next round"),
                                         icon = icon("exclamation-circle"))
                          )
)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
    
    # Inputs: select variables to plot ----------------------------------------------
    selectInput("worldSelect",
                "Homeworld:",
                choices = sort(unique(country_profile$Region)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("SouthernAsia", "WesternAfrica")),
    
    # Birth year Selection ----------------------------------------------
    sliderInput("birthSelect",
                "GDP Per capita:",
                min = min(country_profile$GDP.per.capita..current.US.., na.rm = T),
                max = max(country_profile$GDP.per.capita..current.US.., na.rm = T),
                value = c(min(country_profile$GDP.per.capita..current.US.., na.rm = T), 
                          max(country_profile$GDP.per.capita..current.US.., na.rm = T)),
                step = 100)
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("mass"),
            valueBoxOutput("height")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Mass", plotlyOutput("plot_mass")),
                   tabPanel("Height", plotlyOutput("plot_height")))
          )
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Selected Character Stats", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  swInput <- reactive({
    country_sub <- country_profile %>%
      
      # Slider Filter ----------------------------------------------
    filter(GDP.per.capita..current.US.. >= input$birthSelect[1] & GDP.per.capita..current.US.. <= input$birthSelect[2])
    
    # Homeworld Filter ----------------------------------------------
    if (length(input$worldSelect) > 0 ) {
      country_sub <- subset(country_profile, Region %in% input$worldSelect)
    }
    
    # Return dataframe ----------------------------------------------
    return(country_sub)
  })
  
  # Reactive melted data ----------------------------------------------
  mwInput <- reactive({
    swInput() %>%
      melt(id = "country")
  })
  
  # A plot showing the mass of characters -----------------------------
  output$plot_mass <- renderPlotly({
    dat <- subset(mwInput(), variable == "Population.density..per.km2..2017.")
    
    # Generate Plot ----------------------------------------------
    ggplot(data = dat, aes(x = country, y = as.numeric(value), fill = country)) + geom_bar(stat = "identity")
  })
  
  # A plot showing the height of characters -----------------------------------
  output$plot_height <- renderPlotly({
    dat <- subset(mwInput(),  variable == "Seats.held.by.women.in.national.parliaments..")
    ggplot(data = dat, aes(x = country, y = as.numeric(value), fill = country)) + geom_bar(stat = "identity")
  })
  
  # Data table of characters ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(swInput(), select = c(country, Region, Population.density..per.km2..2017.))
  })
  
  # Mass mean info box ----------------------------------------------
  output$mass <- renderInfoBox({
    sw <- swInput()
    num <- round(mean(country_profile$Food.production.index..2004.2006.100., na.rm = T), 2)
    
    infoBox("Avg Mass", value = num, subtitle = paste(nrow(sw), "characters"), icon = icon("balance-scale"), color = "purple")
  })
  
  # Height mean value box ----------------------------------------------
  output$height <- renderValueBox({
    sw <- swInput()
    num <- round(mean(country_profile$Seats.held.by.women.in.national.parliaments.., na.rm = T), 2)
    
    valueBox(subtitle = "Avg Height", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)

