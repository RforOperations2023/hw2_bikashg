library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

# Next steps:
# 1. Appropriate Ids: output and input
# 2. Figure out why info box do not buzz 
# 3. Learn about scatter plot, pie chart, and heat map that you can generate
# 4. Use 1st tab to provide info and include images
# 5. Learn more about reactivity 
# 6. Learn from slides, cheatsheet, and other resources online 
# 7. Choose better indicators that can be more informative
# 

# Load and clean data ----------------------------------------------

country_profile <- read.csv('selected_country.csv')

country_profile <- country_profile %>%
  mutate(Region = as.factor(Region),
         country = as.factor(country))

# Avoid plotly issues ----------------------------------------------
pdf(NULL)


# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Country Profile Dashboard",

                          # Drop down menu with hard coded values ------------------------------
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "Saci Suta approved your request", 
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
                "Geographical Regions:",
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
          
          # What is this?
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("mass"),
            valueBoxOutput("height")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Food Production", plotlyOutput("plot_mass")),
                   tabPanel("Women Leaders", plotlyOutput("plot_height")))
          )
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Selected Country Stats", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  swInput <- reactive({
    country_sub <- country_profile %>%
      
    # GDP Per Capita Filter ----------------------------------------------
    filter(GDP.per.capita..current.US.. >= input$birthSelect[1] & GDP.per.capita..current.US.. <= input$birthSelect[2])
    
    # Region Filter ----------------------------------------------
    if (length(input$worldSelect) > 0 ) {
      country_sub <- subset(country_profile, Region %in% input$worldSelect)
    }
    
    # Return dataframe ----------------------------------------------
    return(country_sub)
  })
  
  # Reactive melted data ----------------------------------------------
  mwInput <- reactive({
    swInput() %>%
      melt(id = "country") # should I be melting by country? 
  })
  
  # A plot showing the mass of characters -----------------------------
  output$plot_mass <- renderPlotly({
    dat <- subset(mwInput(), variable == "Food.production.index..2004.2006.100.")
    
    # Generate Plot ----------------------------------------------
    ggplot(data = dat, aes(x = country, y = as.numeric(value), fill = country)) + geom_bar(stat = "identity") + 
      labs(y = 'Food Production Index', x = 'Country') + theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
  })
  
  # A plot showing seats held by women in national parliament -----------------------------------
  output$plot_height <- renderPlotly({
    dat <- subset(mwInput(),  variable == "Seats.held.by.women.in.national.parliaments..")
    
    ggplot(data = dat, aes(x = country, y = as.numeric(value), fill = country)) + geom_bar(stat = "identity") + 
      labs(y = 'Seats Held by Women in National Parliament', x = 'Country') + 
      theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
  })
  
  # Data table of characters ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(swInput(), select = c(country, Region, Population.density..per.km2..2017.))
  })
  
  #
  # Mass mean info box ----------------------------------------------
  output$mass <- renderInfoBox({
    sw <- swInput()
    num <- round(mean(sw$Food.production.index..2004.2006.100., na.rm = T), 2)
    
    infoBox("Avg Food Production", value = num, subtitle = paste(nrow(sw), "characters"), 
            icon = icon("balance-scale"), color = "purple")
  })
  
  # Height mean value box ----------------------------------------------
  output$height <- renderValueBox({
    sw <- swInput()
    num <- round(mean(sw$Seats.held.by.women.in.national.parliaments.., na.rm = T), 2)
    
    valueBox(subtitle = "Women Leaders Number", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)

