library(shiny)
library(shinydashboard)
library(dygraphs)

source("Market Survey.R")

load_data()

ui <- dashboardPage(
    dashboardHeader(title = "SAKiRP Dashboard"),
    dashboardSidebar(
        selectInput("district", label = "District", 
                    choices = c("",unique(market_dat_long$district)), selected = "Kigoma"),
        uiOutput("ward_choice"),
        selectInput("product", label = "Product",
                    choices = c("",unique(market_dat_long$item)),
                    selected = "Local Yellow Market Price")
    ),
    dashboardBody(
        fluidRow(
            column(10, dygraphOutput("regionPlot"))
        )
    )
)

server <- function(input, output,session) { 
    
    output$ward_choice <- renderUI({
        selectInput(inputId="ward",
                    label="Select Ward", 
                    choices = unique(ward_dat 
                                     [ward_dat$district==input$district, 
                                         "ward"]))
    })
    
    # values = reactiveValues()
    # 
    # observeEvent(input$product,{
    #     values$product = if(input$product==""){
    #         "Local Yellow Market Price"
    #     }else{
    #         input$product
    #     }
    #     
    #     values$district = input$district
    #     
    #     values$ward = if(input$ward==""){
    #         "Bitale"
    #     }else{
    #         input$ward
    #     }
    #     print(paste0("Product: ",values$product))
    # })
    # 
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
    
    
    # ward = reactive(if(is.null(input$ward)){
    #     return("Bitale")
    # }else{input$ward}) 
    # 
    # district = reactive(input$district)
    # prod = reactive(input$product)
    
    output$regionPlot = renderDygraph({
        get_region_dygraph(product = input$product ,dstrct =  district(),
                           wrd = ward())
    })
    
    
    
}

shinyApp(ui, server)
