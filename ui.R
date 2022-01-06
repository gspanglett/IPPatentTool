# Copyright 2021, Sarah Crothers and Gareth Spanglett
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
