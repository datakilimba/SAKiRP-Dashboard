library(shiny)
library(shinydashboard)
library(dygraphs)

source("Market Survey.R")

## Load global data yeah
load_data()

sidebar = dashboardSidebar(
    sidebarMenu(id = "menu",
                menuItem("Home", icon = icon("home"), tabName = "home"),
                menuItem("Market Data", tabName = "market", icon = icon("chart-line")),
                menuItem("Sunflower", icon = icon("seedling"), tabName = "sunflower")
    )
    ,
    conditionalPanel(
        condition = "input.menu == 'market'",
        selectInput("district", label = "District",
                    choices = c("",unique(market_dat_long$district)), selected = "Kigoma"),
        uiOutput("ward_choice"),
        selectInput("product", label = "Product",
                    choices = c("",unique(market_dat_long$item)),
                    selected = "Local Yellow Market Price")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "home",
                h2("Home tab content")
        ),
        
        tabItem(tabName = "market",
                fluidRow(
                    column(10, dygraphOutput("regionPlot"))
                )
        ),
        
        tabItem(tabName = "sunflower",
                h2("Sunflower tab content")
        )
    )
)

ui = dashboardPage(
    dashboardHeader(title = "SAKiRP Dashboard"),
    sidebar,
    body
)

server = function(input, output, session) { 
    
    output$ward_choice <- renderUI({
        selectInput(inputId="ward",
                    label="Ward", 
                    choices = unique(ward_dat 
                                     [ward_dat$district==input$district, 
                                         "ward"]))
    })
    
    
    ward = eventReactive(input$product,{
        if(is.null(input$ward)){
            return("Bitale")
        }else{
            input$ward
        }
    })
    
    district = eventReactive(input$product,{
        input$district
    })
    
    output$regionPlot = renderDygraph({
        get_region_dygraph(product = input$product ,dstrct =  district(),
                           wrd = ward())
    })
    
}

shinyApp(ui, server)
