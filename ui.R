library(shiny)
library(plotly)
library(shinyjs)
library(rintrojs)

fluidPage(
  
  useShinyjs(),
  introjsUI(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  introBox(
    titlePanel(
    "Alcohol Brief Interventions"
    ),
  data.step = 1,
  data.intro = "Welcome to the <strong>Alcohol Brief Intervention</strong> dashboard.
  This dashboard allows you to visualise and explore the delivery of ABIs in Scotland."
  ),
  
  mainPanel(
    width = 12,
    tabsetPanel(
      type = "tabs",
      id = "tabsetPanel",
      tabPanel(introBox("Contents", icon = icon("info-circle"), data.step = 2, data.intro = "You can navigate the dashboard by clicking on the different tabs, e.g. <strong>Contents</strong>, <strong>Total ABIs Delivered</strong> etc."), 
               br(), htmlOutput("contents")),
      
      tabPanel("Total ABIs Delivered", icon = icon("chart-line"), br(), div({introBox(selectInput(
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
      ), data.step = 3, data.intro = "For <strong>Total ABIs Delivered</strong> and <strong>ABIs Delivered vs Standard</strong> you can view NHS Board specific data by clicking on the drop down menu and selecting the name of the desired NHS Board.")}, style="display: inline-block; vertical-align:top;"), 
      div(introBox(downloadButton("downloadData1", "Download Data"), data.step = 4, data.intro = "If you wish to download the data based on your selection as an Excel workbook you can do so by clicking the <strong>Download Data</strong> button."), style="display: inline-block; padding-top: 25px;"), 
      div(introBox(downloadButton("downloadGlossary1", "Download Glossary"), data.step = 7, data.intro = "If you are unsure about the terminology used in this dashboard, download the glossary by clicking on the <strong>Download Glossary</strong> button."), style="display: inline-block; padding-top: 25px;"), 
      div(introBox(actionButton("cc1", "Chart Controls"), data.step = 6, data.intro = "6.	In order to learn about the chart controls press the <strong>Chart Control</strong> button."), style="display: inline-block; padding-top: 25px;"),
      {shinyjs::hidden(div(id = "chc1", p("At the top-right corner of the chart, you will see a toolbar with four buttons:"), tags$ul(
        tags$li(icon("camera"), tags$b("Download plot as a png"), "- save an image of the chart (not available in Internet Explorer)."),
        tags$li(icon("search"), tags$b("Zoom"), "- click and drag within the chart area to focus on a specific part."),
        tags$li(icon("move", lib = "glyphicon"), tags$b("Pan"), "- click and move the mouse in any direction to modify the chart axes."),
        tags$li(icon("home"), tags$b("Reset axes"), "- click this button to return the axes to their default range.")
      ), p("Categories can be shown/hidden by clicking on labels in the legend to the right of the chart.")))},
      plotlyOutput("plot1"), br(), htmlOutput("text1"), br(), introBox(actionButton("actB1", "Show / Hide Table"), data.step = 5, data.intro = "Alternatively, you can view the data table in the browser by clicking <strong>Show / Hide Table</strong>."), shinyjs::hidden(DT::dataTableOutput("table1"))),
      
      tabPanel("ABIs Delivered vs Standard", icon = icon("chart-bar"), br(), div({selectInput(
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
      div(actionButton("cc2", "Chart Controls"), style="display: inline-block; padding-top: 25px;"),
      {shinyjs::hidden(div(id = "chc2", p("At the top-right corner of the chart, you will see a toolbar with four buttons:"), tags$ul(
        tags$li(icon("camera"), tags$b("Download plot as a png"), "- save an image of the chart (not available in Internet Explorer)."),
        tags$li(icon("search"), tags$b("Zoom"), "- click and drag within the chart area to focus on a specific part."),
        tags$li(icon("move", lib = "glyphicon"), tags$b("Pan"), "- click and move the mouse in any direction to modify the chart axes."),
        tags$li(icon("home"), tags$b("Reset axes"), "- click this button to return the axes to their default range.")
      ), p("Categories can be shown/hidden by clicking on labels in the legend to the right of the chart.")))},
      plotlyOutput("plot2"), br(), htmlOutput("text2"), br(), actionButton("actB2", "Show / Hide Table"), shinyjs::hidden(DT::dataTableOutput("table2"))),

      navbarMenu("ABIs by Setting",
                 icon = icon("chart-bar"),
                 tabPanel("Priority & Wider Settings", br(), downloadButton("downloadData3", "Download Data"), downloadButton("downloadGlossary3", "Download Glossary"),
                          actionButton("cc3", "Chart Controls"),
                          {shinyjs::hidden(div(id = "chc3", br(), p("At the top-right corner of the chart, you will see a toolbar with four buttons:"), tags$ul(
                            tags$li(icon("camera"), tags$b("Download plot as a png"), "- save an image of the chart (not available in Internet Explorer)."),
                            tags$li(icon("search"), tags$b("Zoom"), "- click and drag within the chart area to focus on a specific part."),
                            tags$li(icon("move", lib = "glyphicon"), tags$b("Pan"), "- click and move the mouse in any direction to modify the chart axes."),
                            tags$li(icon("home"), tags$b("Reset axes"), "- click this button to return the axes to their default range.")
                          ), p("Categories can be shown/hidden by clicking on labels in the legend to the right of the chart.")))}, 
                          br(), plotlyOutput("plot3"), br(), htmlOutput("text3"), br(), actionButton("actB3", "Show / Hide Table"), shinyjs::hidden(DT::dataTableOutput("table3"))),
                 tabPanel("All Settings", br(), downloadButton("downloadData4", "Download Data"), downloadButton("downloadGlossary4", "Download Glossary"), 
                          actionButton("cc4", "Chart Controls"),
                          {shinyjs::hidden(div(id = "chc4", br(), p("At the top-right corner of the chart, you will see a toolbar with four buttons:"), tags$ul(
                            tags$li(icon("camera"), tags$b("Download plot as a png"), "- save an image of the chart (not available in Internet Explorer)."),
                            tags$li(icon("search"), tags$b("Zoom"), "- click and drag within the chart area to focus on a specific part."),
                            tags$li(icon("move", lib = "glyphicon"), tags$b("Pan"), "- click and move the mouse in any direction to modify the chart axes."),
                            tags$li(icon("home"), tags$b("Reset axes"), "- click this button to return the axes to their default range.")
                          ), p("Categories can be shown/hidden by clicking on labels in the legend to the right of the chart.")))}, 
                          br(), plotlyOutput("plot4"), br(), htmlOutput("text4"), br(), actionButton("actB4", "Show / Hide Table"), shinyjs::hidden(DT::dataTableOutput("table4"))),
                 tabPanel("Wider Settings", br(), downloadButton("downloadData5", "Download Data"), downloadButton("downloadGlossary5", "Download Glossary"), 
                          actionButton("cc5", "Chart Controls"),
                          {shinyjs::hidden(div(id = "chc5", br(), p("At the top-right corner of the chart, you will see a toolbar with four buttons:"), tags$ul(
                            tags$li(icon("camera"), tags$b("Download plot as a png"), "- save an image of the chart (not available in Internet Explorer)."),
                            tags$li(icon("search"), tags$b("Zoom"), "- click and drag within the chart area to focus on a specific part."),
                            tags$li(icon("move", lib = "glyphicon"), tags$b("Pan"), "- click and move the mouse in any direction to modify the chart axes."),
                            tags$li(icon("home"), tags$b("Reset axes"), "- click this button to return the axes to their default range.")
                          ), p("Categories can be shown/hidden by clicking on labels in the legend to the right of the chart.")))},
                          fluidRow(column(width = 6, plotlyOutput("plot5")), 
                                   column(width = 6, plotlyOutput("plot6"))),
                          br(), actionButton("actB5", "Show / Hide Table"), shinyjs::hidden(DT::dataTableOutput("table5_6"))),
                 tabPanel("Criminal Justice Settings", br(), downloadButton("downloadData6", "Download Data"), downloadButton("downloadGlossary6", "Download Glossary"), 
                          actionButton("cc6", "Chart Controls"),
                          {shinyjs::hidden(div(id = "chc6", br(), p("At the top-right corner of the chart, you will see a toolbar with four buttons:"), tags$ul(
                            tags$li(icon("camera"), tags$b("Download plot as a png"), "- save an image of the chart (not available in Internet Explorer)."),
                            tags$li(icon("search"), tags$b("Zoom"), "- click and drag within the chart area to focus on a specific part."),
                            tags$li(icon("move", lib = "glyphicon"), tags$b("Pan"), "- click and move the mouse in any direction to modify the chart axes."),
                            tags$li(icon("home"), tags$b("Reset axes"), "- click this button to return the axes to their default range.")
                          ), p("Categories can be shown/hidden by clicking on labels in the legend to the right of the chart.")))},
                          fluidRow(column(width = 6, plotlyOutput("plot7")),
                                   column(width = 6, plotlyOutput("plot8"))),
                          br(), actionButton("actB6", "Show / Hide Table"), shinyjs::hidden(DT::dataTableOutput("table7_8")), br(), hr(), plotlyOutput("plot9"), br(), 
                          actionButton("actB7", "Show / Hide Table"), DT::dataTableOutput("table9"))),
      
      tabPanel("Help", value = "help", icon = icon("question-circle"), br(), htmlOutput("contentsForTour"))
    ),
    
    hr(),
    
    div(class="footer", style="text-align:right;",
        HTML("<a href='http://isdscotland.org/' target='_blank'><img src='logos.gif' alt = 'ISD & NHS logos' height = '49px' width = '103px' align = 'left' style='margin-top:-15px;'></a>"),
        introBox(HTML("<p>Contact: <a href='mailto:NSS.isdsubstancemisuse@nhs.net'>NSS.isdsubstancemisuse@nhs.net</a></p>"), data.step = 8, 
                 data.intro = "If you have any further questions relating to the data, or experience problems using this dashboard, please get in touch.")
    )
  )
)