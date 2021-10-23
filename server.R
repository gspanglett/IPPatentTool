#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(DT)
# Define server logic required to draw a histogram



# LOAD DATASET

pv_res <- readRDS("pv_res.rds")
data <- pv_res$data$patents


Patent_data <- reactive({
    data %>% rownames_to_column() %>%  ##dplyr's awkward way to preserve rownames
        column_to_rownames()
})





shinyServer(function(input, output) {
    
    # CREATE REACTIVE DATA SOURCE FROM PATENT DATA UNNESTED BY CPCs
    

    Patent_data <- reactive({
        filter(data, grepl(input$search, patent_abstract)) %>% unnest(cpcs)# %>% select(17,19)
    })
    

    # USED TO CREATE SELECTIVEINPUT FIELD TO SELECT CPC GROUP FOR FILTERING
    
    output$CPCTableInput <- renderUI({
        df = Patent_data() %>% select(27)
        selectizeInput("vars", "CPC Group", choices=distinct(df),multiple = TRUE)
    })
    

    # FILTER REACTIVE TABLE OF SELECTED CPC GROUPS - APPEARS IN MAIN PANEL
    
    output$filteredCPCTable <- DT::renderDataTable({
        df = Patent_data() %>% select(27,28)
        distinct(df[df$cpc_group_id  %in%  input$vars,])
        
    })
    
    
    
    # USED TO CREATE SELECTIVEINPUT FIELD TO SELECT CPC SUBGROUP FOR FILTERING - CHANGES AS CPC GROUPS ARE SELECTED
    
    
    output$CPCSubTableInput <- renderUI({
        df = Patent_data()
        selectizeInput("subvars", "CPC Subroup", choices=distinct(df[df$cpc_group_id  %in%  input$vars,c(32)]),multiple = TRUE)
    })
    

    
    # FILTER REACTIVE TABLE OF SELECTED CPC SUBGROUPS - APPEARS IN MAIN PANEL

    output$filteredCPCSubTable <- DT::renderDataTable({
        df = Patent_data()
        distinct(df[df$cpc_group_id  %in%  input$vars & df$cpc_subgroup_id %in% input$subvars,c(32,33)])
        
    })
    
    
 
    
    
    # FILTER REACTIVE TABLE OF PATENTS - APPEARS IN MAIN PANEL - BECOMES THE BASE DATASET FOR GRAPHS AND VISUALIZATIONS
    
    output$filteredPatentTable <- DT::renderDataTable({
        df = Patent_data() 
        df[df$cpc_group_id  %in%  input$vars & df$cpc_subgroup_id %in% input$subvars,c(17,3,19,27,32),by=df$patent_number] %>% 
            group_by(patent_number,patent_date,patent_title) %>%
            summarise(cpc = paste(cpc_group_id, collapse = ", ", cpcsub = paste(cpc_subgroup_id, collapse = ", ")))
        
    })
    

    
        
    
    # THE REMAINING ITEMS ARE FOR POPULATING THE INPUTS AND REFERENCE TABLES IN THE SIDEBAR

    output$CPCTable <- DT::renderDataTable({
        df = Patent_data() %>% select(27,28)
        datatable(
            distinct(df),
            selection = list(mode = "multiple"),
            caption = "Filtered CPC Table"
        )
    })
    

    
    output$CPCSubTable <- DT::renderDataTable({
        df = Patent_data() %>% select(32,33)
        datatable(
            distinct(df),
            selection = list(mode = "multiple"),
            caption = "Filtered CPC Subgroup Table"
        )
    })
    
    
    output$PatentTable <- DT::renderDataTable({
        df = Patent_data() %>% select(17,3,19)
        datatable(
            distinct(df),
            selection = list(mode = "multiple"),
            caption = "Filtered Patent Table"
        )
    })
    

    
    output$res <- renderPrint({
        input$search
    })
    
    output$var <- renderPrint({
        input$vars
    })
    
    output$subvar <- renderPrint({
        input$subvars
    })
    

    # TEMP MSG AT LOADING
    
    output$loadingTime <- renderText({
        invalidateLater(30000, session)
        paste("The current time is", Sys.time())
    })
    
    
})

