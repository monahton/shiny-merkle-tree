# ----------------------------
# Description: Demonstration of the User Intefrace capable to verify hardware configuration
# Author: Vladimir Zhbanko
# Date: 2019-06-05
# ----------------------------
# 
library(tidyverse)
library(shiny)
library(DT)
library(magrittr)
library(shinyjs)
library(V8)

# code to provide colors to the fields in shiny see https://stackoverflow.com/q/50313540/5499595
jsCode <- '
shinyjs.backgroundCol = function(params) {
var defaultParams = {
id : null,
col : "red"
};
params = shinyjs.getParams(params, defaultParams);
var el = $("#" + params.id);
el.css("background-color", params.col);
}'


shinyUI(fluidPage(theme = "bootstrap.css",
                  useShinyjs(),
                  extendShinyjs(text = jsCode),
                  
                  
                  # Adding Logo
                  fluidRow(column(2, img(height = 100, width  = 158, src = "logo.jpg")),
                           # Application title
                           column(10, titlePanel("User Intefrace to verify Hardware Configuration"))),
                  hr(),
                  
                  #### ***************************************************************
                  ####        USER INPUT
                  #### ***************************************************************
                  # # User General Inputs about property ==
                  # fluidRow(column(8,  tableOutput("table"))),
                  
                  # a horizontal rule on the page
                  tags$hr(),
                  #textOutput(outputId, container = if (inline) span else div,
                  #           inline = FALSE)
                  # Page Row 3 == User's specific Inputs about property  ==
                           # User Enters Parts Installed on Group 1
                  fluidRow(column(2, textInput(inputId = "Prt1G1", value = "", label = "Part1 Group1", placeholder = "Enter Part Code")),
                           column(2, textInput(inputId = "Prt2G1", value = "", label = "Part2 Group1", placeholder = "Enter Part Code")), 
                           column(2, textInput(inputId = "Prt3G1", value = "", label = "Part3 Group1", placeholder = "Enter Part Code")),
                           column(2, textInput(inputId = "Prt4G1", value = "", label = "Part4 Group1", placeholder = "Enter Part Code"))),
                  fluidRow(column(2, textInput(inputId = "Hsh1G1", value = "", label = "Signature P1G1", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "Hsh2G1", value = "", label = "Signature P2G1", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "Hsh3G1", value = "", label = "Signature P3G1", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "Hsh4G1", value = "", label = "Signature P4G1", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "actG1", value = "", label = "Actual Signature G1", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "reqG1", value = "", label = "Required Signature G1", placeholder = "Automatic Field"))),
                  hr(),
                           # User Enters Parts Installed on Group 2
                  fluidRow(column(2, textInput(inputId = "Prt1G2", value = "", label = "Part1 Group2", placeholder = "Enter Part Code")),
                           column(2, textInput(inputId = "Prt2G2", value = "", label = "Part2 Group2", placeholder = "Enter Part Code")),
                           column(2, textInput(inputId = "Prt3G2", value = "", label = "Part3 Group2", placeholder = "Enter Part Code")),
                           column(2, textInput(inputId = "Prt4G2", value = "", label = "Part4 Group2", placeholder = "Enter Part Code"))),
                  fluidRow(column(2, textInput(inputId = "Hsh1G2", value = "", label = "Signature P1G2", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "Hsh2G2", value = "", label = "Signature P2G2", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "Hsh3G2", value = "", label = "Signature P3G2", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "Hsh4G2", value = "", label = "Signature P4G2", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "actG2", value = "", label = "Actual Signature G2", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "reqG2", value = "", label = "Required Signature G2", placeholder = "Automatic Field"))),
                  hr(),
                           # User Enters Parts Installed on Group 3
                  fluidRow(column(2, textInput(inputId = "Prt1G3", value = "", label = "Part1 Group3", placeholder = "Enter Part Code")), 
                           column(2, textInput(inputId = "Prt2G3", value = "", label = "Part2 Group3", placeholder = "Enter Part Code")),
                           column(2, textInput(inputId = "Prt3G3", value = "", label = "Part3 Group3", placeholder = "Enter Part Code")),
                           column(2, textInput(inputId = "Prt4G3", value = "", label = "Part4 Group3", placeholder = "Enter Part Code"))),
                  fluidRow(column(2, textInput(inputId = "Hsh1G3", value = "", label = "Signature P1G3", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "Hsh2G3", value = "", label = "Signature P2G3", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "Hsh3G3", value = "", label = "Signature P3G3", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "Hsh4G3", value = "", label = "Signature P4G3", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "actG3", value = "", label = "Actual Signature G3", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "reqG3", value = "", label = "Required Signature G3", placeholder = "Automatic Field"))),
                  
                  # a horizontal rule on the page
                  tags$hr(),

                  # buttons and output for the entire unit
                           # button to bring required configuration (parts and signatures) via API
                  fluidRow(column(1, actionButton("conf", "Config", icon = icon("drafting-compass"))),
                           # button to generate hashed values of single parts, groups and units 
                           column(2, actionButton("sign", "Compare Congituration", icon = icon("stethoscope"))),
                           # button to save obtained check to the table
                           column(1, actionButton("submit", "Store", icon = icon("sign-in"))),
                           # button to eraze all contents
                           column(1, actionButton("reset", "Reset", icon = icon("refresh"))),
                           column(2, offset = 8, textInput(inputId = "actU", value = "", label = "Actual Signature Unit", placeholder = "Automatic Field")),
                           column(2, textInput(inputId = "reqU", value = "", label = "Reqrd Signature Unit", placeholder = "Automatic Field"))),
                  
                  # a horizontal rule on the page
                  tags$hr(),
                  #### ***************************************************************
                  ####        DATA OUTPUT
                  #### ***************************************************************
                  # arrange content using panels 
                  tabsetPanel(
                    tabPanel("Data Table",
                             # Page Row 3 == Last results view as interactive data table ==
                             fluidRow(column(12,  DT::dataTableOutput("responses", width = 400)),
                                      # add horizontal line
                                      tags$hr(),
                                      
                                      # action button aim: when user click save information to csv file        
                                      fluidRow(column(1, downloadButton('downloadTable', 'Download')))
                             )         
                    ),  
                    tabPanel("Help", 
                               #### ***************************************************************
                               ####        DASHBOARD
                               #### ***************************************************************
                               navlistPanel(widths = c(2, 8),
                                            
                                            tabPanel("1. About", 
                                                     wellPanel(
                                                         # text info
                                                         "Use this app to verify configuration",
                                                         hr(),
                                                         "button 'Config' - to bring required configuration (optional to speed up demo)",
                                                         hr(),
                                                         "button 'Check' - to generate hashed values of parts",
                                                         hr(),
                                                         "button 'Compare' - to compare results by bringing hashed values via API",
                                                         hr(),
                                                         "button 'Store' - to save obtained check to the table",
                                                         hr(),
                                                        "button to eraze all contents"
                                                         
                                                     )          ),
                                            tabPanel("2. Contact", 
                                                     wellPanel(
                                                         # Copyright
                                                         titlePanel("Created by:"),
                                                         "(C) 2019 Vladimir Zhbanko",
                                                         "vladimir.zhbanko@gmail.com"
                                                     )
                                            )
                               ))
                      
                      
                      
                  )
                  
                  
                  ))