library(shiny)
library(plotly)
library(shinyjs)

fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  titlePanel(
    "Alcohol Brief Interventions"
  ),
  
  mainPanel(
    width = 12,
    tabsetPanel(
      type = "tabs",
      id = "tabsetPanel",
      tabPanel("Contents", icon = icon("info-circle"), 
               br(), htmlOutput("contents")),
      
      tabPanel("Total ABIs Delivered", icon = icon("chart-line"), br(), div({selectInput(
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
      )}, style="display: inline-block; vertical-align:top;"), 
      div(downloadButton("downloadData1", "Download Data"), style="display: inline-block; padding-top: 25px;"), 
      div(downloadButton("downloadGlossary1", "Download Glossary"), style="display: inline-block; padding-top: 25px;"), 
      div(actionButton("cc1", "Help", icon("question-circle")), style="display: inline-block; padding-top: 25px;"),
      {shinyjs::hidden(div(id = "chc1", 
                           HTML("<p>You can view data for a specific <strong>NHS Board</strong> or <strong>Scotland</strong> by selecting the appropriate 
                                choice from the drop down box. To download the selected data as a CSV file, click the <strong>Download Data</strong> button.
                                If any of the terminology used in this dashboard is unclear, a glossary can be downloaded via the <strong>Download Glossary</strong> button.</p>"),
                           p("At the top-right corner of the chart, you will see a toolbar with a", tags$b(icon("camera"), "Download plot as a png"), "button, which allows to save 
                             an image of the chart (not available in Internet Explorer). Categories can be shown/hidden by clicking on labels in the legend to the right of the chart."), 
                           HTML("<p>In order to view a table of your data selection in the browser, please click on the <strong>Show/Hide Table</strong> button underneath the chart.</p>")))},
      plotlyOutput("plot1"), br(), htmlOutput("text1"), br(), actionButton("actB1", "Show / Hide Table"), shinyjs::hidden(DT::dataTableOutput("table1"))),
      
      tabPanel("ABIs Delivered compared to Standard", icon = icon("chart-bar"), br(), div({selectInput(
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
      )}, style="display: inline-block; vertical-align:top;"), 
      div(downloadButton("downloadData2", "Download Data"), style="display: inline-block; padding-top: 25px;"), 
      div(downloadButton("downloadGlossary2", "Download Glossary"), style="display: inline-block; padding-top: 25px;"), 
      div(actionButton("cc2", "Help", icon("question-circle")), style="display: inline-block; padding-top: 25px;"),
      {shinyjs::hidden(div(id = "chc2", 
                           HTML("<p>You can view data for a specific <strong>NHS Board</strong> or <strong>Scotland</strong> by selecting the appropriate 
                                choice from the drop down box. To download the selected data as a CSV file, click the <strong>Download Data</strong> button.
                                If any of the terminology used in this dashboard is unclear, a glossary can be downloaded via the <strong>Download Glossary</strong> button.</p>"),
                           p("At the top-right corner of the chart, you will see a toolbar with a", tags$b(icon("camera"), "Download plot as a png"), "button, which allows to save 
                             an image of the chart (not available in Internet Explorer). Categories can be shown/hidden by clicking on labels in the legend to the right of the chart."), 
                           HTML("<p>In order to view a table of your data selection in the browser, please click on the <strong>Show/Hide Table</strong> button underneath the chart.</p>")))},
      plotlyOutput("plot2"), br(), htmlOutput("text2"), br(), actionButton("actB2", "Show / Hide Table"), shinyjs::hidden(DT::dataTableOutput("table2"))),

      navbarMenu("ABIs by Setting",
                 icon = icon("chart-bar"),
                 tabPanel("Priority & Wider Settings", br(), downloadButton("downloadData3", "Download Data"), downloadButton("downloadGlossary3", "Download Glossary"),
                          actionButton("cc3", "Help", icon("question-circle")),
                          {shinyjs::hidden(div(id = "chc3", br(), 
                                               HTML("<p>To download the data as a CSV file, click the <strong>Download Data</strong> button.
                                                    If any of the terminology used in this dashboard is unclear, a glossary can be downloaded via the <strong>Download Glossary</strong> button.</p>"),
                                               p("At the top-right corner of the chart, you will see a toolbar with a", tags$b(icon("camera"), "Download plot as a png"), "button, which allows to save 
                                                 an image of the chart (not available in Internet Explorer). Categories can be shown/hidden by clicking on labels in the legend at the top of the chart."), 
                                               HTML("<p>In order to view a table of the data in the browser, please click on the <strong>Show/Hide Table</strong> button underneath the chart.</p>")))}, 
                          br(), plotlyOutput("plot3"), br(), htmlOutput("text3"), br(), actionButton("actB3", "Show / Hide Table"), shinyjs::hidden(DT::dataTableOutput("table3"))),
                 tabPanel("All Settings", br(), downloadButton("downloadData4", "Download Data"), downloadButton("downloadGlossary4", "Download Glossary"), 
                          actionButton("cc4", "Help", icon("question-circle")),
                          {shinyjs::hidden(div(id = "chc4", br(), 
                                               HTML("<p>To download the data as a CSV file, click the <strong>Download Data</strong> button.
                                                    If any of the terminology used in this dashboard is unclear, a glossary can be downloaded via the <strong>Download Glossary</strong> button.</p>"),
                                               p("At the top-right corner of the chart, you will see a toolbar with a", tags$b(icon("camera"), "Download plot as a png"), "button, which allows to save 
                                                 an image of the chart (not available in Internet Explorer). Categories can be shown/hidden by clicking on labels in the legend at the top of the chart."), 
                                               HTML("<p>In order to view a table of the data in the browser, please click on the <strong>Show/Hide Table</strong> button underneath the chart.</p>")))}, 
                          br(), plotlyOutput("plot4"), br(), htmlOutput("text4"), br(), actionButton("actB4", "Show / Hide Table"), shinyjs::hidden(DT::dataTableOutput("table4"))),
                 tabPanel("Wider Settings", br(), downloadButton("downloadData5", "Download Data"), downloadButton("downloadGlossary5", "Download Glossary"), 
                          actionButton("cc5", "Help", icon("question-circle")),
                          {shinyjs::hidden(div(id = "chc5", br(), 
                                               HTML("<p>To download the data as a CSV file, click the <strong>Download Data</strong> button.
                                                    If any of the terminology used in this dashboard is unclear, a glossary can be downloaded via the <strong>Download Glossary</strong> button.</p>"),
                                               p("At the top-right corner of the chart, you will see a toolbar with a", tags$b(icon("camera"), "Download plot as a png"), "button, which allows to save 
                                                 an image of the chart (not available in Internet Explorer). Categories can be shown/hidden by clicking on labels in the legend at the top of the chart."), 
                                               HTML("<p>In order to view a table of the data in the browser, please click on the <strong>Show/Hide Table</strong> button underneath the chart.</p>")))},
                          fluidRow(column(width = 6, plotlyOutput("plot5")), 
                                   column(width = 6, plotlyOutput("plot6"))),
                          htmlOutput("text5_6"), br(), actionButton("actB5", "Show / Hide Table"), shinyjs::hidden(DT::dataTableOutput("table5_6"))),
                 tabPanel("Criminal Justice Settings", br(), downloadButton("downloadData6", "Download Data"), downloadButton("downloadGlossary6", "Download Glossary"), 
                          actionButton("cc6", "Help", icon("question-circle")),
                          {shinyjs::hidden(div(id = "chc6", br(), 
                                               HTML("<p>To download the data as a CSV file, click the <strong>Download Data</strong> button.
                                                    If any of the terminology used in this dashboard is unclear, a glossary can be downloaded via the <strong>Download Glossary</strong> button.</p>"),
                                               p("At the top-right corner of the chart, you will see a toolbar with a", tags$b(icon("camera"), "Download plot as a png"), "button, which allows to save 
                                                 an image of the chart (not available in Internet Explorer). Categories can be shown/hidden by clicking on labels in the legend at the top of the chart."), 
                                               HTML("<p>In order to view a table of the data in the browser, please click on the <strong>Show/Hide Table</strong> button underneath the chart.</p>")))},
                          fluidRow(column(width = 6, plotlyOutput("plot7")),
                                   column(width = 6, plotlyOutput("plot8"))),
                          htmlOutput("text7_8"), br(), actionButton("actB6", "Show / Hide Table"), shinyjs::hidden(DT::dataTableOutput("table7_8")), br(), hr(), plotlyOutput("plot9"), 
                          htmlOutput("text9"), br(), actionButton("actB7", "Show / Hide Table"), DT::dataTableOutput("table9")))
    ),
    
    hr(),
    
    div(class="footer", style="text-align:right;",
        HTML("<a href='http://isdscotland.org/' target='_blank'><img src='logos.gif' alt = 'ISD & NHS logos' height = '49px' width = '103px' align = 'left' style='margin-top:-15px;'></a>"),
        HTML("<p>Contact: <a href='mailto:NSS.isdsubstancemisuse@nhs.net'>NSS.isdsubstancemisuse@nhs.net</a></p>")
    )
  )
)