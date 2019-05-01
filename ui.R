library(shiny)
library(plotly)
library(shinyjs)

fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  titlePanel(
    "Alcohol Brief Interventions - Dashboard"
  ),
  
  mainPanel(
    width = 12,
    tabsetPanel(
      type = "tabs",
      id = "tabsetPanel",
      tabPanel("Contents", icon = icon("info-circle"), br(), htmlOutput("contents")),
      
      navbarMenu("ABIs Delivered",
                 icon = icon("chart-bar"),
                 tabPanel("Total ABIs Delivered", br(), {selectInput(
                   inputId = "board",
                   label = "Select NHS Board(s)",
                   choices = list("All" = "All", "Ayrshire & Arran" = "Ayrshire & Arran", "Borders" = "Borders",
                                  "Dumfries & Galloway" = "Dumfries & Galloway",
                                  "Fife" = "Fife", "Forth Valley" = "Forth Valley",
                                  "Grampian" = "Grampian", "Greater Glasgow & Clyde" = "Greater Glasgow & Clyde",
                                  "Highland" = "Highland", "Lanarkshire" = "Lanarkshire",
                                  "Lothian" = "Lothian", "Orkney" = "Orkney",
                                  "Shetland" = "Shetland", "Tayside" = "Tayside",
                                  "Western Isles" = "Western Isles", "Scotland" = "Scotland"),
                   selected = "All"
                 )}, plotlyOutput("plot1"), br(), htmlOutput("text1"), 
                          br(), actionButton("actB1", "Hide / Show Table"), downloadButton("downloadData1", "Download Data"), shinyjs::hidden(DT::dataTableOutput("table1"))),
                 tabPanel("ABIs Delivered vs Standard", br(), {selectInput(
                   inputId = "board2",
                   label = "Select NHS Board(s)",
                   choices = list("All" = "All", "Ayrshire & Arran" = "Ayrshire & Arran", "Borders" = "Borders",
                                  "Dumfries & Galloway" = "Dumfries & Galloway",
                                  "Fife" = "Fife", "Forth Valley" = "Forth Valley",
                                  "Grampian" = "Grampian", "Greater Glasgow & Clyde" = "Greater Glasgow & Clyde",
                                  "Highland" = "Highland", "Lanarkshire" = "Lanarkshire",
                                  "Lothian" = "Lothian", "Orkney" = "Orkney",
                                  "Shetland" = "Shetland", "Tayside" = "Tayside",
                                  "Western Isles" = "Western Isles", "Scotland" = "Scotland"),
                   selected = "All"
                 )}, plotlyOutput("plot2"), br(), htmlOutput("text2"), 
                          br(), actionButton("actB2", "Hide / Show Table"), downloadButton("downloadData2", "Download Data"), shinyjs::hidden(DT::dataTableOutput("table2")))),

      navbarMenu("ABIs by Setting",
                 icon = icon("chart-bar"),
                 tabPanel("Priority & Wider Settings", br(), plotlyOutput("plot3"), br(), htmlOutput("text3"), 
                          br(), actionButton("actB3", "Hide / Show Table"), downloadButton("downloadData3", "Download Data"), shinyjs::hidden(DT::dataTableOutput("table3"))),
                 tabPanel("All Settings", plotlyOutput("plot4"), br(), htmlOutput("text4"), 
                          br(), actionButton("actB4", "Hide / Show Table"), downloadButton("downloadData4", "Download Data"), shinyjs::hidden(DT::dataTableOutput("table4"))),
                 tabPanel("Wider Settings", br(), fluidRow(column(width = 6, plotlyOutput("plot5")), 
                                                           column(width = 6, plotlyOutput("plot6"))),
                          br(), actionButton("actB5", "Hide / Show Table"), downloadButton("downloadData5", "Download Data"), shinyjs::hidden(DT::dataTableOutput("table5_6"))),
                 tabPanel("Criminal Justice Settings", br(), fluidRow(column(width = 6, plotlyOutput("plot7")),
                                                                      column(width = 6, plotlyOutput("plot8"))),
                          br(), actionButton("actB6", "Hide / Show Table"), downloadButton("downloadData6", "Download Data"), DT::dataTableOutput("table7_8"))),
      
      tabPanel("Glossary", icon = icon("question-circle"), br(), htmlOutput("glossary"))
    ),
    
    hr(),
    
    div(class="footer", style="text-align:right;",
        HTML("<a href='http://isdscotland.org/' target='_blank'><img src='logos.gif' alt = 'ISD & NHS logos' height = '49px' width = '103px' align = 'left' style='margin-top:-15px;'></a>"),
        HTML("<p>Contact: <a href='mailto:NSS.isdsubstancemisuse@nhs.net'>NSS.isdsubstancemisuse@nhs.net</a></p>")
    )
  )
)