library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

# Load and clean data ----------------------------------------------

country_profile <- read.csv('selected_country.csv')

country_profile <- country_profile %>%
  mutate(Region = as.factor(Region),
         country = as.factor(country))

# Avoid plotly issues ----------------------------------------------
pdf(NULL)


# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Country Profile Dashboard", titleWidth = 350,

                          # Drop down menu with hard coded values ------------------------------
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = " Saci Suta approved your request", 
                                                        icon = icon("users")),
                                       notificationItem(text = " US Census 2023 Unveiled",
                                                        icon = icon("table")),
                                       notificationItem(text = " Coup in Britain",
                                                        icon = icon("newspaper"),
                                                        status = "warning")
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 90, color = "green",
                                                "Data Cleaning"),
                                       taskItem(value = 50, color = "aqua",
                                                "Data Analysis"),
                                       taskItem(value = 32, color ="red",
                                                "Overall Progress")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Stanford University",
                                         message = HTML("You are in the next round"),
                                         icon = icon("medal")),
                                       messageItem(
                                         from = "AI Lab",
                                         message = "ChatGPT and Academia",
                                         icon = icon("book")
                                       ),
                                       messageItem(
                                         from = 'Consequential Podcast',
                                         message = 'New Episode in',
                                         icon = icon("bullhorn")
                                       )
                          )
)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(width=350,
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Data Viz", icon = icon("chart-column"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", 
             badgeColor = "green"),
    menuItem("Source Code", icon=icon("code"), 
             href="https://github.com/RforOperations2023/hw2_bikashg"),
    
    
    # GDP per Capita Range Selection ----------------------------------------------
    sliderInput("gdp",
                "Choose range for GDP Per capita:",
                min = min(country_profile$GDP.per.capita..current.US.., na.rm = T),
                max = max(country_profile$GDP.per.capita..current.US.., na.rm = T),
                value = c(min(country_profile$GDP.per.capita..current.US.., na.rm = T), 
                          max(country_profile$GDP.per.capita..current.US.., na.rm = T)),
                step = 100),
    
    # Inputs: Narrow down or expand your geographical regions -------------------------
    selectInput("region",
                "Select geographical regions from the dropdown menu below after choosing the GDP per capita range above: ",
                choices = sort(unique(country_profile$Region)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c()),
                #selected = c("SouthernAsia", "WesternAfrica")),
    
    hr(),
    
    # Input x-variable: selecting an input for x-axis (scatter plot)
    selectInput("x_var", "Select a socio-economic indicator for x-axis", 
                choices = c("Health Expenditure Share of GDP" = "Health..Total.expenditure....of.GDP.", 
                            "Doctors per 1000 People" = "Health..Physicians..per.1000.pop..",
                            "Education Expenditure Share of GDP" = "Education..Government.expenditure....of.GDP.", 
                            "Internet Users per 100 People" = "Individuals.using.the.Internet..per.100.inhabitants.",
                            "Energy Supply per Capita" = "Energy supply per capita",
                            "Fertility Rate" = "Fertility.rate..total..live.births.per.woman.",
                            "Female Labor Participation" = "Female Labor Paricipation"
                )),
    
    # Input y-variable: selecting an input for y-axis (scatter plot)
    selectInput("y_var", "Select a socio-economic indicator for y-axis", 
                choices = c("Infant Mortality per 1000 live births" = "Infant.mortality.rate..per.1000.live.births",
                            "GDP per Capita" = "GDP.per.capita..current.US..",
                            "Women Representation in Parliament" = "Seats.held.by.women.in.national.parliaments..",
                            "International Trade Balance" = "International.trade..Balance..million.US..",
                            "Food Production Index" = "Food.production.index..2004.2006.100."))
    
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("food"),
            valueBoxOutput("female"),
            valueBoxOutput('health')
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Data Visualization",
                   width = 12,
                   tabPanel("Scatterplot", plotlyOutput("scatterplot")),
                   tabPanel("Food Production", plotlyOutput("plot_food")),
                   tabPanel("Women Leaders", plotlyOutput("plot_female")),
                   tabPanel("Population Pie-chart", plotlyOutput("piechart")))
          )
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Selected Country Statistics", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body, skin='purple')

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  swInput <- reactive({
    country_sub <- country_profile %>%
      
    # GDP Per Capita Filter ----------------------------------------------
    filter(`GDP.per.capita..current.US..` >= input$gdp[1],
           `GDP.per.capita..current.US..` <= input$gdp[2])
    
    # Region Filter ----------------------------------------------
    if (length(input$region) > 0 ) {
      country_sub <- subset(country_profile, Region %in% input$region)
    }
    
    # Return dataframe ----------------------------------------------
    return(country_sub)
  })
  
  # Reactive melted data ----------------------------------------------
  mwInput <- reactive({
    swInput() %>%
      melt(id = "country") 
  })
  
  
  # A plot showing a scatter plot
  output$scatterplot <- renderPlotly({
    print(summary(swInput()[input$x_var]))
    print(summary(swInput()[input$y_var]))
    ggplot(swInput(), aes_string(x = input$x_var, y = input$y_var)) +
      geom_point() + labs(x = str_replace_all(input$x_var, "[.]", " "),
           y = str_replace_all(input$y_var, "[.]", " ")) 
  })
  
  # A plot showing the food production -----------------------------
  output$plot_food <- renderPlotly({
    dat <- subset(mwInput(), variable == "Food.production.index..2004.2006.100.")
    
    # Generate Plot ----------------------------------------------
    ggplot(data = dat, aes(x = country, y = as.numeric(value), fill = country)) + 
      geom_bar(stat = "identity") + labs(y = 'Food Production Index', x = 'Country') + 
      theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))

  })
  
  # A plot showing seats held by women in national parliament -----------------------------------
  output$plot_female <- renderPlotly({
    dat <- subset(mwInput(),  variable == "Seats.held.by.women.in.national.parliaments..")
    
    ggplot(data = dat, aes(x = country, y = as.numeric(value), fill = country)) + geom_bar(stat = "identity") + 
      labs(y = 'Seats Held by Women in National Parliament', x = 'Country') + 
      theme(axis.text.x=element_text(angle=45, hjust=1, vjust=0.5))
  })

  
  # Reactive pie chart based on input values
  output$piechart <- renderPlotly({
    pie_data <- swInput() %>%
      group_by(Region) %>%
      summarise(n = n(), avg_pop = mean(`Population.in.thousands..2017.`, na.rm=TRUE))
    
    plot_ly(pie_data, labels = ~Region, values = ~avg_pop, type = "pie")
    
  })
  
  
  # Data table of country ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(swInput(), select = c(country, 
                                 Region, 
                                 Population.in.thousands..2017.,
                                 International.trade..Balance..million.US..,
                                 Unemployment....of.labour.force.,
                                 Seats.held.by.women.in.national.parliaments..
                                 ))
  })
  
  
  # food mean info box ----------------------------------------------
  output$food <- renderInfoBox({
    sw <- swInput()
    num <- round(mean(sw$Food.production.index..2004.2006.100., na.rm = T), 2)
    
    infoBox("Food Production Standing", value = num, subtitle = "Base Index = 100, Base Years: 2004-2006", 
            icon = icon("balance-scale"), color = "purple")
  })
  
  # female mean value box ----------------------------------------------
  output$female <- renderValueBox({
    sw <- swInput()
    num <- round(mean(sw$Seats.held.by.women.in.national.parliaments.., na.rm = T), 2)
    
    valueBox(subtitle = "Women Leader in Parliament (%)", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
  
  # health output box ---------------------------------------------------
  output$health <- renderValueBox({
    sw <- swInput()
    num <- round(mean(sw$Health..Total.expenditure....of.GDP., na.rm = T), 2)
    
    valueBox(subtitle = "Health Expenditure Share of GDP (%)", value = num, 
             icon = icon("sort-numeric-asc"), color = "blue")
  })
  
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)

