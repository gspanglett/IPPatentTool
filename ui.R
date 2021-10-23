#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Title
    titlePanel("DEMO - IP Patent Tool"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(

            tags$h1("Search Input"),
            br(),
            searchInput(
                inputId = "search", label = "Enter your text",
                placeholder = "A placeholder",
                btnSearch = icon("search"),
                btnReset = icon("remove"),
                width = "450px"
            ),
            p("Allow up to 60 seconds for the tool to load the initial dataset ..."),
            br(),
            verbatimTextOutput(outputId = "res"),
            br(),
            uiOutput("CPCTableInput"),
            br(),
            verbatimTextOutput(outputId = "var"),
            br(),
            uiOutput("CPCSubTableInput"),
            br(),
            verbatimTextOutput(outputId = "subvar"),
            br(),
            DTOutput("CPCTable"),
            br(),
            DTOutput("CPCSubTable")
            
            
        ),
        
        
        mainPanel(

            
            br(),
            #DTOutput("CPCTable"),
            DTOutput("filteredCPCTable"),
            
            br(),
            #DTOutput("CPCSubTable"),
            DTOutput("filteredCPCSubTable"),
            
            br(),
            DTOutput("filteredPatentTable"),
            
            
            
        )
    )
))
