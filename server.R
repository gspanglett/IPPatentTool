# Copyright 2020-2021, Sarah Crothers <scrot021@uottawa.ca> and Gareth Spanglett <gspan042@uottawa.ca>
# Authors:  Gareth Spanglett, Sarah Crothers
# GNU General Public License v3.0
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(leaflet)
library(visNetwork)
library(tidyverse)
library(igraph)
library(htmltools)
library(patentsview)
library(datasets)
library(DT)
library(knitr)
library(magrittr)

# Define server logic required to draw a histogram



# LOAD DATASET

pv_res <- readRDS("pv_res.rds")
data <- pv_res$data$patents


Patent_data <- reactive({
    data %>% rownames_to_column() %>%  ##dplyr's awkward way to preserve rownames
        column_to_rownames()
})


pat_title <- function(title, number) {
  temp_title <- str_wrap(title)
  i <- gsub("\\n", "<br>", temp_title)
  paste0('<a href="https://patents.google.com/patent/US', number, '">', i, '</a>')
}




shinyServer(function(input, output) {
    
    # CREATE REACTIVE DATA SOURCE FROM PATENT DATA UNNESTED BY CPCs
    

    Patent_data <- reactive({
        filter(data, grepl(input$search, patent_abstract))# %>% unnest(cpcs) %>% select(17,19)
    })
    

    # USED TO CREATE SELECTIVEINPUT FIELD TO SELECT CPC GROUP FOR FILTERING
    
    output$CPCTableInput <- renderUI({
        df = Patent_data() %>% unnest(cpcs) %>% select(27)
        selectizeInput("vars", "CPC Group", choices=distinct(df),multiple = TRUE)
    })
    

    # FILTER REACTIVE TABLE OF SELECTED CPC GROUPS - APPEARS IN MAIN PANEL
    
    output$filteredCPCTable <- DT::renderDataTable({
        df = Patent_data() %>% unnest(cpcs) %>% select(27,28)
        distinct(df[df$cpc_group_id  %in%  input$vars,])
        
    })
    
    
    
    # USED TO CREATE SELECTIVEINPUT FIELD TO SELECT CPC SUBGROUP FOR FILTERING - CHANGES AS CPC GROUPS ARE SELECTED
    
    
    output$CPCSubTableInput <- renderUI({
        df = Patent_data() %>% unnest(cpcs)
        selectizeInput("subvars", "CPC Subroup", choices=distinct(df[df$cpc_group_id  %in%  input$vars,c(32)]),multiple = TRUE)
    })
    

    
    # FILTER REACTIVE TABLE OF SELECTED CPC SUBGROUPS - APPEARS IN MAIN PANEL

    output$filteredCPCSubTable <- DT::renderDataTable({
        df = Patent_data() %>% unnest(cpcs)
        distinct(df[df$cpc_group_id  %in%  input$vars & df$cpc_subgroup_id %in% input$subvars,c(32,33)])
        
    })
    
    
 
    
    
    # FILTER REACTIVE TABLE OF PATENTS - APPEARS IN MAIN PANEL - BECOMES THE BASE DATASET FOR GRAPHS AND VISUALIZATIONS
    
    output$filteredPatentTable <- DT::renderDataTable({
        df = Patent_data()  %>% unnest(cpcs)
        df[df$cpc_group_id  %in%  input$vars & df$cpc_subgroup_id %in% input$subvars,c(17,3,19,27,32),by=df$patent_number] %>% 
            group_by(patent_number,patent_date,patent_title) %>%
            summarise(cpc = paste(cpc_group_id, collapse = ", ", cpcsub = paste(cpc_subgroup_id, collapse = ", ")))
        
    })
    

    # USED TO CREATE SELECTIVEINPUT FIELD TO SELECT PATENTS FOR FILTERING - CHANGES AS CPC SUBGROUPS ARE SELECTED
    
    
    output$PatentTableInput <- renderUI({
        df = Patent_data() %>% unnest(cpcs)
        selectizeInput("patentvars", "Patents", choices=distinct(df[df$cpc_group_id  %in%  input$vars & df$cpc_subgroup_id %in% input$subvars,c(17)]),multiple = TRUE)
    })
    
    
    
    
    # GET DATASET FOR CITATION NETWORK
    
    output$SubCPCNetwork <- renderVisNetwork({
        
        # Write a query to pull patents assigned to the CPC code of "Y10S707/933"
        query <- qry_funs$eq(cpc_subgroup_id = c(input$subvars))
        
        # Create a list of fields to pull from the API
        fields <- c(
            "patent_number", 
            "patent_title",
            "cited_patent_number", # Which patents do these patents cite?
            "citedby_patent_number" # Which patents cite them?
        )
        
        # Send a request to the API
        res <- search_pv(query, fields = fields, all_pages = TRUE)
        
        # Unnest the data found in the list columns
        res_lst <- unnest_pv_data(res$data, pk = "patent_number")
        
        pat_title <- function(title, number) {
            temp_title <- str_wrap(title)
            i <- gsub("\\n", "<br>", temp_title)
            paste0('<a href="https://patents.google.com/patent/US', number, '">', i, '</a>')
        }
        
        edges <-
            res_lst$cited_patents %>%
            semi_join(x = ., y = ., by = c("cited_patent_number" = "patent_number")) %>%
            set_colnames(c("from", "to"))
        
        nodes <-
            res_lst$patents %>%
            mutate(
                id = patent_number,
                label = patent_number,
                title = pat_title(patent_title, patent_number)
            )

        graph <- graph.data.frame(edges, directed = T)
        degree_value <- degree(graph)
        nodes$value <- degree_value[match(nodes$id, names(degree_value))]
        
        visNetwork(
            nodes = nodes, edges = edges, # height = "100%", width = "100%",
            main = "Citations among patents in selected CPC subgroups"
        ) %>%
            visEdges(arrows = list(to = list(enabled = TRUE))) %>%
            visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) %>%
            visIgraphLayout()
        
        
        
        
    })
    

    
    output$PatentNetwork <- renderVisNetwork({
        
        # Write a query to pull patents assigned to the CPC code of "Y10S707/933"
        p1 <- Patent_data() %>% filter(patent_number %in% input$patentvars) %>% select(patent_number)
        
        p3 <- p1$patent_number
        
        query <- qry_funs$eq(patent_number = c(p3))
        
        
        
        
        # Create a list of fields to pull from the API
        fields <- c(
            "patent_number", 
            "patent_title",
            "cited_patent_number", # Which patents do these patents cite?
            "citedby_patent_number" # Which patents cite them?
        )
        
        # Send a request to the API
        res <- search_pv(query, fields = fields, all_pages = TRUE)
        
        # Unnest the data found in the list columns
        res_lst <- unnest_pv_data(res$data, pk = "patent_number")
        res_lst2 <- lapply(res_lst, function(x) x[x$patent_number %in% p3, ])
        
        rel_pats <-
            res_lst2$cited_patents %>%
            rbind(setNames(res_lst2$citedby_patents, names(.))) %>% 
            select(-patent_number) %>%
            rename(patent_number = cited_patent_number) %>%
            bind_rows(data.frame(patent_number = p3)) %>% 
            distinct() %>%
            filter(!is.na(patent_number))
        
        # Look up which patents the relevant patents cite
        rel_pats_res <- search_pv(
            query = list(patent_number = rel_pats$patent_number),
            fields =  c("cited_patent_number", "patent_number", "patent_title"), 
            all_pages = TRUE, method = "POST"
        )
        
        rel_pats_lst <- unnest_pv_data(rel_pats_res$data, "patent_number")
        
        cited_pats <-
            rel_pats_lst$cited_patents %>%
            filter(!is.na(cited_patent_number))
        
        full_network <- 
            cited_pats %>%
            do({
                .$ind <- 
                    group_by(., patent_number) %>% 
                    group_indices()
                group_by(., patent_number) %>%  
                    mutate(sqrt_num_cited = sqrt(n()))
            }) %>%
            inner_join(x = ., y = ., by = "cited_patent_number") %>%
            filter(ind.x > ind.y) %>%
            group_by(patent_number.x, patent_number.y) %>% 
            mutate(cosine_sim = n() / (sqrt_num_cited.x * sqrt_num_cited.y)) %>% 
            ungroup() %>%
            select(matches("patent_number\\.|cosine_sim")) %>%
            distinct()
   
        
        output$PatentNetworkTable <- DT::renderDataTable({
           datatable(full_network, caption = "Network consine similiarity measure")
        })
        
        pat_title <- function(title, number) {
            temp_title <- str_wrap(title)
            i <- gsub("\\n", "<br>", temp_title)
            paste0('<a href="https://patents.google.com/patent/US', number, '">', i, '</a>')
        }
        
             
        edges <- 
            full_network %>%
            filter(cosine_sim >= .1) %>% 
            rename(from = patent_number.x, to = patent_number.y, value = cosine_sim) %>%
            mutate(title = paste("Cosine similarity =", as.character(round(value, 3))))
        
        nodes <-
            rel_pats_lst$patents %>%
            rename(id = patent_number) %>%
            mutate(
                # the 3 patents of interest will be represented as blue nodes, all others
                # will be yellow
                color = ifelse(id %in% p3, "#97C2FC", "#DDCC77"), 
                label = id,
                title = pat_title(patent_title, id)
            )
        

        
        output$PatentNetworkNodeTable <- DT::renderDataTable({
            datatable(nodes %>% select(id, patent_title, value), caption = "Nodes")
        })
        
        output$PatentNetworkEdgeTable <- DT::renderDataTable({
            datatable(edges %>% select(from, to, title), caption = "Edges")
        })
        
        
        
                
        graph <- graph.data.frame(edges, directed = T)
        degree_value <- degree(graph)
        nodes$value <- degree_value[match(nodes$id, names(degree_value))]

        
        
        visNetwork(
            nodes = nodes, edges = edges, height = "1000px", width = "100%",
            main = "Network of selected patents (in blue) and related cited and citing patents"
        ) %>%
            visEdges(color = list(color = "#343434")) %>%
            visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) %>%
            visIgraphLayout()

                
        
    })
    
    
    
    
    
    
    
        
    # TAKE PATENTVARS, PULL FROM DATA, AND MAP USING LEAFLET        
    
    output$CPCMap <- renderLeaflet(({
        data <-
            Patent_data() %>%
            unnest(assignees) %>%
            unnest (cpcs) %>%
            filter(cpc_group_id %in% input$vars) %>%
            select(assignee_id, assignee_organization, patent_number,
                   assignee_longitude, assignee_latitude) %>%
            group_by_at(vars(-matches("pat"))) %>%
            mutate(num_pats = n()) %>%
            ungroup() %>%
            select(-patent_number) %>%
            distinct() %>%
            mutate(popup = paste0("<font color='Black'>",
                                  htmlEscape(assignee_organization), "<br><br>Patents:",
                                  num_pats, "</font>")) %>%
            mutate_at(vars(matches("_l")), as.numeric) %>%
            filter(!is.na(assignee_id))
        
        leaflet(data) %>%
            addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
            addCircleMarkers(lng = ~assignee_longitude, lat = ~assignee_latitude,
                             popup = ~popup, ~sqrt(num_pats), color = "yellow")
        
    }))
    
    
    
    output$SubCPCMap <- renderLeaflet(({
        data <-
            Patent_data() %>%
            unnest(assignees) %>%
            unnest (cpcs) %>%
            filter(cpc_subgroup_id %in% input$subvars) %>%
            select(assignee_id, assignee_organization, patent_number,
                   assignee_longitude, assignee_latitude) %>%
            group_by_at(vars(-matches("pat"))) %>%
            mutate(num_pats = n()) %>%
            ungroup() %>%
            select(-patent_number) %>%
            distinct() %>%
            mutate(popup = paste0("<font color='Black'>",
                                  htmlEscape(assignee_organization), "<br><br>Patents:",
                                  num_pats, "</font>")) %>%
            mutate_at(vars(matches("_l")), as.numeric) %>%
            filter(!is.na(assignee_id))
        
        leaflet(data) %>%
            addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
            addCircleMarkers(lng = ~assignee_longitude, lat = ~assignee_latitude,
                             popup = ~popup, ~sqrt(num_pats), color = "yellow")
        
    }))
    
    
    
    
    
    
    
       
    
    output$PatentMap <- renderLeaflet(({
        data <-
            Patent_data() %>%
            unnest(assignees) %>%
            filter(patent_number %in% input$patentvars) %>%
            select(assignee_id, assignee_organization, patent_number,
                   assignee_longitude, assignee_latitude) %>%
            group_by_at(vars(-matches("pat"))) %>%
            mutate(num_pats = n()) %>%
            ungroup() %>%
            select(-patent_number) %>%
            distinct() %>%
            mutate(popup = paste0("<font color='Black'>",
                                  htmlEscape(assignee_organization), "<br><br>Patents:",
                                  num_pats, "</font>")) %>%
            mutate_at(vars(matches("_l")), as.numeric) %>%
            filter(!is.na(assignee_id))
        
        leaflet(data) %>%
            addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
            addCircleMarkers(lng = ~assignee_longitude, lat = ~assignee_latitude,
                             popup = ~popup, ~sqrt(num_pats), color = "yellow")
        
    }))
    
    
        
    
    # THE REMAINING ITEMS ARE FOR POPULATING THE INPUTS AND REFERENCE TABLES IN THE SIDEBAR

    output$CPCTable <- DT::renderDataTable({
        df = Patent_data() %>% unnest(cpcs) %>% select(27,28)
        datatable(
            distinct(df),
            selection = list(mode = "multiple"),
            caption = "Filtered CPC Table"
        )
    })
    

    
    output$CPCSubTable <- DT::renderDataTable({
        df = Patent_data() %>% unnest(cpcs) %>% select(32,33)
        datatable(
            distinct(df),
            selection = list(mode = "multiple"),
            caption = "Filtered CPC Subgroup Table"
        )
    })
    
    
    output$PatentTable <- DT::renderDataTable({
        df = Patent_data() %>% unnest(cpcs) %>% select(17,3,19)
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
    
    output$patentvar <- renderPrint({
        input$patentvars
    })
    
    
    

    
    
    
    
    
    
    
    # TEMP MSG AT LOADING
    
    output$loadingTime <- renderText({
        invalidateLater(30000, session)
        paste("The current time is", Sys.time())
    })
    
    
})

