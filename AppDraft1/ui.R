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
            
            tabsetPanel(type = "tabs",
                tabPanel("Data Filtering",

            
                    br(),
                    #DTOutput("CPCTable"),
                    DTOutput("filteredCPCTable"),
                    
                    br(),
                    #DTOutput("CPCSubTable"),
                    DTOutput("filteredCPCSubTable"),
                    
                    br(),
                    DTOutput("filteredPatentTable"),
                    
                    br(),
                    uiOutput("PatentTableInput"),
                    br(),
                    verbatimTextOutput(outputId = "patentvar")
                ),
                tabPanel("Geolocation",
                         
                         br(),
                         leafletOutput("CPCMap"),
                         br(),
                         column(5, wellPanel(
                             h4("CPC Subgroup Map"),
                             leafletOutput("SubCPCMap", width="100%")
                             
                         )),
                         column(5, wellPanel(
                             h4("Patent Geolocation Map"),
                             leafletOutput("PatentMap", width="100%")
                             
                             
                         )),
                         br()
                ),
                tabPanel("CPC Subgroup Network",
                         
                         br(),
                         visNetworkOutput("SubCPCNetwork", width = "100%", height="900px"),
                         br()
                ),
                tabPanel("Patent Network and Data",
                         
                         br(),
                         visNetworkOutput("PatentNetwork"),
                         br(),
                         DTOutput("PatentNetworkNodeTable"),
                         br(),
                         DTOutput("PatentNetworkEdgeTable"),
                         br()
                )
            )
            
        )
    )
))
