# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(plotly)
library(scales)
library(shinyjs)

##################################################

ABIs <- readRDS("Resources/Data/TidyABIs.rds")
Data2 <- readRDS("Resources/Data/Data2.rds")
Data3 <- readRDS("Resources/Data/Data3.rds")
Data4 <- readRDS("Resources/Data/Data4.rds")
Data5 <- readRDS("Resources/Data/Data5.rds")
Data6 <- readRDS("Resources/Data/Data6.rds")
Data7 <- readRDS("Resources/Data/Data7.rds")

##################################################

# Colour-blind friendly colours, recommended by Tina Fu, http://mkweb.bcgsc.ca/biovis2012/color-blindness-palette.png
colours15 <- c("#000000", "#004949", "#009292",
               "#FF6DB6", "#FFB6DB", "#490092",
               "#006DDB", "#B66DFF", "#6DB6FF",
               "#B6DBFF", "#920000", "#924900",
               "#DB6D00", "#24FF24", "#FFFF6D")

# Blues as per Chart & Dashboard guidance

bluesISD <- c("#004785", "#00a2e5", "#4c7ea9", "#99daf5", "#4cbeed")


shinyServer(function(input, output, session) {
  
  output$contents <- renderUI({list(
    HTML("<p>This dashboard shows national summary information relating to <strong title='Alcohol Brief Interventions are consultations which aim to help individuals cut down their drinking habits to sensible levels.'
         style='border-bottom: 1px dotted black;'>Alcohol Brief Interventions</strong> (ABIs) in Scotland.
         It visualises and explores the delivery of ABIs in the context of NHS Scotland's individual Health Boards
         and their respective delivery targets, Priority Settings, Wider Settings and Criminal Justice Settings.
         The dashboard covers ABIs for the financial years 2008/09 to 2018/19.</p>

         <p>In order to navigate the dashboard, please click on the tabs above (e.g. <em>Total ABIs Delivered</em>).</p>

         <p>For more information on the latest ABI publication, please view one of the following:</p>
          <ul>
            <li><a href='https://www.isdscotland.org/Health-Topics/Drugs-and-Alcohol-Misuse/Publications/2019-06-25/2019-06-25-AlcoholBriefInterventions-Report.pdf' target='_blank'>Full Report</a></li>
            <li><a href='https://www.isdscotland.org/Health-Topics/Drugs-and-Alcohol-Misuse/Publications/2019-06-25/2019-06-25-AlcoholBriefInterventions-Summary.pdf' target='_blank'>Summary Report</a></li>
            <li><a href='http://isdscotland.org/Health-Topics/Drugs-and-Alcohol-Misuse/Publications/2019-06-25/2019-06-25-AlcoholBriefInterventions-Tables.xlsx'>Download Data Tables (Excel Workbook)</a></li>
          </ul>
         
         <p>The general structure of the dashboard is in line with the accompanying <em>Excel workbook</em>, i.e.:</p>"), 
    tags$ul(
      tags$li(actionLink("tab1", "Total ABIs Delivered:"), "Total number of ABIs delivered in comparison with LDP standard, by NHS Board"),
      tags$li(actionLink("tab2", "ABIs Delivered compared to Standard:"), "ABIs delivered against standard, by NHS board; financial year 2018/19"),
      tags$li(actionLink("tab3", "Priority & Wider Settings:"), "Number of ABIs delivered across Scotland split by Priority and Wider Settings"),
      tags$li(actionLink("tab4", "All Settings:"), "Number and percentage of ABIs delivered within each Setting; by NHS Board"),
      tags$li(actionLink("tab5_6", "Wider Settings:"), "Number and percentage of ABIs delivered in Wider Settings in Scotland"),
      tags$li(actionLink("tab7_8", "Criminal Justice Settings:"), "Number and percentage of ABIs delivered in Criminal Justice Settings in Scotland and by NHS Board")),
    HTML("<br>
         <p>If you experience any problems using this dashboard or have further questions relating to the data, please contact us at: <a href='mailto:NSS.isdsubstancemisuse@nhs.net'>NSS.isdsubstancemisuse@nhs.net</a></p>
         <br>
         <p><strong>Source:</strong> ISD Scotland</p>
         <p><strong>Updated:</strong> June 2019</p>")
  )})

  output$plot1 <- renderPlotly({
    
    style(ggplotly(
      if (req(input$board) == "All") {
        
        ABIs %>%
          ggplot(aes(x = FY, y = Delivered, color = Board, group = Board, text = paste0("Financial Year: ", FY,
                                                                                        "<br>ABIs Delivered: ", Delivered,
                                                                                        "<br>NHS Board: ", Board))) +
            geom_line(size = 0.5) +
            scale_y_log10() +
            scale_color_manual(values = colours15) +
            theme_minimal() +
            scale_x_discrete(expand = c(0.03,0.03)) +
            labs(title = "Total number of ABIs delivered across all NHS Boards (2008/09 - 2018/19)", x = "Financial Year", y = "ABIs delivered") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                  axis.text.y = element_text(size = 8),
                  axis.title = element_text(size = 9, face = "bold"),
                  title = element_text(size = 10, face = "bold"),
                  legend.text = element_text(size = 8))
      } else if (req(input$board) == "Scotland") {
        ABIs %>%
          group_by(FY) %>%
          summarise(SumDelivered = sum(Delivered)) %>%
          ggplot(aes(x = FY, y = SumDelivered, group = 1, text = paste0("Financial Year: ", FY,
                                                                        "<br>ABIs Delivered: ", SumDelivered,
                                                                        "<br>Scotland"))) +
            geom_line(size = 1, color="#004785") + 
            theme_minimal() +
            scale_x_discrete(expand = c(0.03,0.03)) +
            labs(title = "Total number of ABIs delivered on national level (2008/09 - 2018/19)", x = "Financial Year", y = "ABIs delivered") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                  axis.text.y = element_text(size = 8),
                  axis.title = element_text(size = 9, face = "bold"),
                  title = element_text(size = 10, face = "bold"),
                  legend.text = element_text(size = 8))
      } else {
        ABIs %>%
          filter(Board == input$board) %>%
          ggplot(aes(x = FY, y = Delivered, group = Board, text = paste0("Financial Year: ", FY,
                                                                         "<br>ABIs Delivered: ", Delivered,
                                                                         "<br>NHS Board: ", Board))) +
          geom_line(size = 1, color="#004785") +
          theme_minimal() +
          scale_x_discrete(expand = c(0.03,0.03)) +
          labs(title = paste0("Total number of ABIs delivered in ", input$board, " (2008/09 - 2018/19)"), x = "Financial Year", y = "ABIs delivered") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                axis.text.y = element_text(size = 8),
                axis.title = element_text(size = 9, face = "bold"),
                title = element_text(size = 10, face = "bold"),
                legend.text = element_text(size = 8))
      }, tooltip = "text"
    ), mode = "line+markers") %>%
      layout(margin = list(t=75),
             xaxis = list(fixedrange=TRUE), 
             yaxis= list(fixedrange=TRUE)) %>%
      config(displayModeBar = TRUE,
             modeBarButtons = list(list("toImage")),
             displaylogo=FALSE, collaborate=FALSE, editable=FALSE)
  })
  
  output$text1 <- renderUI({
    
    del18_19 <- numeric()
    del17_18 <- numeric()
    tar18_19 <- numeric()
    tar17_18 <- numeric()
    
    if (req(input$board) == "All") {
      list(
        h5("For more information on individual NHS Board or national level, please make the appropriate selection from", tags$strong("Select NHS Board(s)."))
      )
    } else if (req(input$board) == "Scotland") {
      ABIs %>%
        group_by(FY) %>%
        summarise(Delivered = sum(Delivered), Target = sum(Target), PercentOfTarget = Delivered/Target*100) %>%
        filter(FY == "2018/19") %>%
        select(Delivered) -> del18_19
      ABIs %>%
        group_by(FY) %>%
        summarise(Delivered = sum(Delivered), Target = sum(Target), PercentOfTarget = Delivered/Target*100) %>%
        filter(FY == "2017/18") %>%
        select(Delivered) -> del17_18
      ABIs %>%
        group_by(FY) %>%
        summarise(Delivered = sum(Delivered), Target = sum(Target), PercentOfTarget = Delivered/Target*100) %>%
        filter(FY == "2018/19") %>%
        select(Target) -> tar18_19
      ABIs %>%
        group_by(FY) %>%
        summarise(Delivered = sum(Delivered), Target = sum(Target), PercentOfTarget = Delivered/Target*100) %>%
        filter(FY == "2017/18") %>%
        select(Target) -> tar17_18
      
      list(
        tags$h4("Key Points"),
        tags$ul(tags$li(paste0(input$board, " has ", ifelse(del18_19 > tar18_19, "exceeded", "missed"), " its target by ", round(abs((del18_19/tar18_19*100)-100), digits = 1), "% in FY 2018/19.")),
                tags$li(paste0("In comparison to FY 2017/18, ", input$board, " has delivered ", ifelse(del18_19/tar18_19 > del17_18/tar17_18, "more", "less"), " ABIs.")))
      )
      
    } else {
      ABIs %>%
        filter(Board == input$board, FY == "2018/19") %>%
        select(Delivered) -> del18_19
      ABIs %>%
        filter(Board == input$board, FY == "2018/19") %>%
        select(Target) -> tar18_19
      ABIs %>%
        filter(Board == input$board, FY == "2017/18") %>%
        select(Delivered) -> del17_18
      ABIs %>%
        filter(Board == input$board, FY == "2017/18") %>%
        select(Target) -> tar17_18
      
      list(
        tags$h4("Key Points"),
        tags$ul(tags$li(paste0(input$board, " have ", ifelse(del18_19 > tar18_19, "exceeded", "missed"), " their target by ", round(abs((del18_19/tar18_19*100)-100), digits = 1), "% in FY 2018/19.")),
                tags$li(paste0("In comparison to FY 2017/18, ", input$board, " have delivered ", ifelse(del18_19/tar18_19 > del17_18/tar17_18, "more", "less"), " ABIs.")))
      )
      
    }
    
  })
  
  output$table1 <- DT::renderDataTable(DT::datatable({
    if (req(input$board) == "All") {
      #Do nothing
    } else if (req(input$board) == "Scotland") {
      ABIs %>%
        group_by(FY) %>%
        summarise(Delivered = sum(Delivered), Target = sum(Target), `Percent of Target` = round(Delivered/Target*100, digits = 1)) %>%
        arrange(desc(FY)) %>% 
        rename(`Financial Year` = FY)
    } else {
      ABIs %>%
        filter(Board == input$board) %>%
        arrange(desc(FY)) %>% 
        rename(`Financial Year` = FY)
    }},
    options = list(dom = "tif", paging = FALSE), rownames = F))
  
  output$plot2 <- renderPlotly({
    
    Data2ForPlot <- data.frame(Data2$`Total delivered as % of standard`, 
                               Data2$`Delivered in priority settings as % of standard`,
                               Data2$`NHS Board`)
    names(Data2ForPlot) <- c("Total delivered as % of standard", 
               "Delivered in priority settings as % of standard",
               "NHS Board")
    
    Data2ForPlot <- melt(Data2ForPlot, id.vars = "NHS Board")
    
    ggplotly(
      if (input$board2 == "All") {
        Data2ForPlot %>%
          ggplot(aes(x = `NHS Board`, y = value, fill = variable, text = paste0("NHS Board: ", `NHS Board`,
                                                                               "<br>Value: ", value, "%",
                                                                               "<br>Variable: ", variable))) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_fill_manual(values = bluesISD) +
            geom_hline(yintercept = 100, size = 0.6) +
            annotate("text", x = 15.3, y = 106, label = "100%", size = 3) +
            geom_hline(yintercept = 80, size = 0.6, color="#0066ff") +
            annotate("text", x = 15.3, y = 86, label = "80%", size = 3, color = "#0066ff") +
            theme_minimal() +
            labs(title = "ABIs delivered overall and in priority settings; as % of standard; 2018/19", x = "NHS Board", y = "Percent [%]") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                axis.text.y = element_text(size = 8),
                axis.title = element_text(size = 9, face = "bold"),
                title = element_text(size = 10, face = "bold"),
                legend.text = element_text(size = 8),
                legend.title = element_blank())
      } else {
        Data2ForPlot %>%
          filter(`NHS Board` == input$board2) %>%
          ggplot(aes(x = `NHS Board`, y = value, fill = variable, text = paste0("NHS Board: ", `NHS Board`,
                                                                                "<br>Value: ", value, "%",
                                                                                "<br>Variable: ", variable))) +
            geom_bar(stat = "identity", position = "dodge", width = 0.5) +
            scale_fill_manual(values = bluesISD) +
            geom_hline(yintercept = 100, size = 0.6) +
            annotate("text", x = 1.46, y = 102.5, label = "100%", size = 3) +
            geom_hline(yintercept = 80, size = 0.6, color = "#0066ff") +
            annotate("text", x = 1.46, y = 82.5, label = "80%", size = 3, color = "#0066ff") +
            theme_minimal() +
            labs(title = "ABIs delivered overall and in priority settings; as % of standard; 2018/19", x = "NHS Board", y = "Percent [%]") +
            theme(axis.text.x = element_text(size = 8),
                axis.text.y = element_text(size = 8),
                axis.title = element_text(size = 9, face = "bold"),
                title = element_text(size = 10, face = "bold"),
                legend.text = element_text(size = 8),
                legend.title = element_blank())
      }, tooltip = "text"
    ) %>%  
      layout(margin = list(t=75),
                 legend = list(orientation = "h",
                               xanchor = "center",
                               x = 0.475,
                               y = 100),
                 xaxis = list(fixedrange=TRUE), 
                 yaxis= list(fixedrange=TRUE)) %>%
      config(displayModeBar = TRUE,
             modeBarButtons = list(list("toImage")),
             displaylogo=FALSE, collaborate=FALSE, editable=FALSE)
  })
  
  output$text2 <- renderUI({
    deliveredTotal <- numeric()
    primaryDelivered <- numeric()
    
    if (input$board2 == "All") {
      list(
        h5("For more information on individual NHS Board or national level, please make the appropriate selection from", tags$strong("Select NHS Board(s)."))
      )
    } else {
      Data2 %>%
        filter(`NHS Board` == input$board2) %>%
        select(`Total delivered as % of standard`) -> deliveredTotal
      
      Data2 %>%
        filter(`NHS Board` == input$board2) %>%
        select(`Delivered in priority settings as % of standard`) -> primaryDelivered
      
      list(
        tags$h4("Key Point:"),
        tags$ul(
          tags$li(paste0("Out of ", round(deliveredTotal, digits = 1), "% of ABIs delivered overall, ", input$board2, ifelse(input$board2 == "Scotland", " has ", " have "), "delivered ", round(primaryDelivered, digits = 1), "% of ABIs in Priority Settings."))
        )
      )
    }
  })
  
  output$table2 <- DT::renderDataTable(DT::datatable({
    if (input$board2 == "All") {
      Data2 %>%
        arrange(`NHS Board`) 
    } else {
      Data2 %>%
        filter(`NHS Board` == input$board2)
    }
  },
    options = list(dom = "tif", paging = FALSE), rownames = F))
  
  output$plot3 <- renderPlotly({
    
    Data3ForPlot <- data.frame(Data3$`FY`, 
                               Data3$`Priority Settings`,
                               Data3$`Wider Settings`)
    names(Data3ForPlot) <- c("Financial Year", 
                             "Priority Settings",
                             "Wider Settings")
    
    Data3ForPlot <- melt(Data3ForPlot, id.vars = "Financial Year")
    
    ggplotly(
    Data3ForPlot %>%
      ggplot(aes(x = `Financial Year`, y = value, fill = variable, text = paste0("Financial Year: ", `Financial Year`,
                                                                                 "<br>Value: ", value,
                                                                                 "<br>Variable: ", variable))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = bluesISD) +
      theme_minimal() +
      labs(title = "ABIs delivered across Scotland split by Priority and Wider Settings", x = "Financial Year", y = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            axis.text.y = element_text(size = 8),
            axis.title = element_text(size = 9, face = "bold"),
            title = element_text(size = 10, face = "bold"),
            legend.position = c(0.9,0.9),
            legend.text = element_text(size = 8),
            legend.title = element_blank())
    , tooltip = "text") %>% layout(margin = list(t=75), 
                 legend = list(orientation = "h",
                               xanchor = "center",
                               x = 0.475,
                               y = 100),
                 xaxis = list(fixedrange=TRUE), 
                 yaxis= list(fixedrange=TRUE)) %>%
      config(displayModeBar = TRUE,
             modeBarButtons = list(list("toImage")),
             displaylogo=FALSE, collaborate=FALSE, editable=FALSE)
  })
  
  output$text3 <- renderText({
    HTML("<h4>Key Point:</h4>
         <p>In 2018/19, 52,383 ABIs were delivered in priority settings nationwide, representing 85.8% of 
          the number of ABIs specified in the LDP standard (61,081). This is 5.8 percentage points more than the 
          figure for priority settings set out in the standard (48,865).</p>
         ")
  })
  
  output$table3 <- DT::renderDataTable(DT::datatable({
    Data3 %>%
      arrange(desc(FY)) %>%
      rename(`Financial Year` = FY)
  },
    options = list(dom = "tif", paging = FALSE), rownames = F))
  
  output$plot4 <- renderPlotly({
    Data4ForPlot <- data.frame(Data4$`NHS Board`,
                               Data4$`Primary Care [%]`, 
                               Data4$`A&E [%]`,
                               Data4$`Antenatal [%]`,
                               Data4$`Wider Settings [%]`)
    names(Data4ForPlot) <- c("NHS Board", 
                             "Primary Care",
                             "A&E",
                             "Antenatal",
                             "Wider Settings")
    
    Data4ForPlot <- melt(Data4ForPlot, id.vars = "NHS Board")
    
    ggplotly(
      Data4ForPlot %>%
        ggplot(aes(x = `NHS Board`, y = value, fill = variable, text = paste0("NHS Board: ", `NHS Board`,
                                                                              "<br>Value: ", value, "%",
                                                                              "<br>Variable: ", variable))) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = bluesISD) +
        theme_minimal() +
        labs(title = "Percentage of ABIs delivered within each setting; by NHS Board (FY 2018/19)", x = "NHS Board", y = "Percent [%]") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
              axis.text.y = element_text(size = 8),
              axis.title = element_text(size = 9, face = "bold"),
              title = element_text(size = 10, face = "bold"),
              legend.text = element_text(size = 8),
              legend.title = element_blank())
    , tooltip = "text") %>% 
      layout(margin = list(t=75), 
                 legend = list(orientation = "h",
                               xanchor = "center",
                               x = 0.475, 
                               y = 100),
                 xaxis = list(fixedrange=TRUE), 
                 yaxis= list(fixedrange=TRUE)) %>%
      config(displayModeBar = TRUE,
             modeBarButtons = list(list("toImage")),
             displaylogo=FALSE, collaborate=FALSE, editable=FALSE)
  })
  
  output$text4 <- renderText({
    HTML("
         <h4>Key Point:</h4>
         <p>Of the 80,575 ABIs delivered in 2018/19 on a national level, 49.3% were delivered in Primary Care, 13.6% in Accident & 
         Emergency, 2.1% in Antenatal Settings and 35.0% in Wider Settings.</p>
         ")
  })
  
  output$table4 <- DT::renderDataTable(DT::datatable({
    Data4 %>%
      arrange(`NHS Board`)
  },
    options = list(dom = "tif", paging = FALSE), rownames = F))
  
  observe(output$plot5 <- renderPlotly({
    Data5ForPlot <- Data5 %>%
                      filter(`Wider setting` != "Total") %>%
                      select(1:4) %>%
                      melt(id.vars = "Wider setting")
    
    xlabels <- c("Community", "Criminal\nJustice", 
                 "NHS Professionals\nother than doctor\nor nurse",
                 "NHS Settings", "Other")
    ggplotly(
      Data5ForPlot %>%
        ggplot(aes(x = `Wider setting`, y = value, fill = variable, text = paste0("Wider Setting: ", `Wider setting`,
                                                                                  "<br>ABIs Delivered: ", value,
                                                                                  "<br>Financial Year: ", variable))) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_x_discrete(labels = xlabels) +
        scale_fill_manual(values = bluesISD) +
        theme_minimal() +
        labs(title = "ABIs delivered in Wider Settings", x = "Settings", y = "") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
              axis.text.y = element_text(size = 8),
              axis.title = element_text(size = 9, face = "bold"),
              title = element_text(size = 10, face = "bold"),
              legend.text = element_text(size = 8),
              legend.title = element_blank())
    , tooltip = "text") %>% layout(margin = list(t=75),
                 legend = list(orientation = "h",
                               xanchor = "center",
                               x = 0.45,
                               y = 100),
                 xaxis = list(fixedrange=TRUE), 
                 yaxis= list(fixedrange=TRUE)) %>%
      config(displayModeBar = TRUE,
             modeBarButtons = list(list("toImage")),
             displaylogo=FALSE, collaborate=FALSE, editable=FALSE)
  }))
  
  output$plot6 <- renderPlotly({
    Data6ForPlot <- Data5 %>%
      filter(`Wider setting` != "Total") %>%
      select(1,5:7) %>%
      melt(id.vars = "Wider setting")
    
    xlabels <- c("Community", "Criminal\nJustice", 
                 "NHS Professionals\nother than doctor\nor nurse",
                 "NHS Settings", "Other")
    
    ggplotly(
      Data6ForPlot %>%
        ggplot(aes(x = `Wider setting`, y = value, fill = variable, text = paste0("Wider Setting: ", `Wider setting`,
                                                                                  "<br>Percent of ABIs Delivered: ", value, "%",
                                                                                  "<br>Financial Year: ", variable))) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_x_discrete(labels = xlabels) +
        scale_fill_manual(values = bluesISD, labels = c("2016/17", "2017/18", "2018/19")) +
        theme_minimal() +
        labs(title = "ABIs delivered in Wider Settings [%]", x = "Settings", y = "") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
              axis.text.y = element_text(size = 8),
              axis.title = element_text(size = 9, face = "bold"),
              title = element_text(size = 10, face = "bold"),
              legend.text = element_text(size = 8),
              legend.title = element_blank())
    , tooltip = "text") %>% layout(margin = list(t=75),
                 legend = list(orientation = "h",
                               xanchor = "center",
                               x = 0.45,
                               y = 100),
                 xaxis = list(fixedrange=TRUE), 
                 yaxis= list(fixedrange=TRUE)) %>%
      config(displayModeBar = TRUE,
             modeBarButtons = list(list("toImage")),
             displaylogo=FALSE, collaborate=FALSE, editable=FALSE)
  })
  
  output$text5_6 <- renderUI({
    p("Please note: NHS Grampian provided aggregate figures for Wider Settings. 
      Therefore ABIs delivered in NHS Grampian in Wider Settings are counted in the 'Other' category.", style = "font-size: 9px; text-align: center;")
  })
  
  output$table5_6 <- DT::renderDataTable(DT::datatable({
    Data5
  },
    options = list(dom = "tif", paging = FALSE), rownames = F))
  
  output$plot7 <- renderPlotly({
    Data7ForPlot <- Data6 %>%
      filter(`Criminal Justice Setting` != "Total") %>%
      select(1:4) %>%
      melt(id.vars = "Criminal Justice Setting")
    
    xlabels <- c("Custody\nSuites", "Police", "Prisons", "Social\nwork")
    
    ggplotly(
      Data7ForPlot %>%
        ggplot(aes(x = `Criminal Justice Setting`, y = value, fill = variable, text = paste0("Criminal Justice Setting: ", `Criminal Justice Setting`,
                                                                                             "<br>ABIs Delivered: ", value,
                                                                                             "<br>Financial Year: ", variable))) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_x_discrete(labels = xlabels) +
        scale_fill_manual(values = bluesISD) +
        theme_minimal() +
        labs(title = "ABIs in Criminal Justice Settings", x = "Criminal Justice Settings", y = "") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
              axis.text.y = element_text(size = 8),
              axis.title = element_text(size = 9, face = "bold"),
              title = element_text(size = 10, face = "bold"),
              legend.text = element_text(size = 8),
              legend.title = element_blank())
    , tooltip = "text") %>% layout(margin = list(t=75),
                 legend = list(orientation = "h",
                               xanchor = "center",
                               x = 0.45,
                               y = 100),
                 xaxis = list(fixedrange=TRUE), 
                 yaxis= list(fixedrange=TRUE)) %>%
      config(displayModeBar = TRUE,
             modeBarButtons = list(list("toImage")),
             displaylogo=FALSE, collaborate=FALSE, editable=FALSE)
  })
  
  output$plot8 <- renderPlotly({
    Data8ForPlot <- Data6 %>%
      filter(`Criminal Justice Setting` != "Total") %>%
      select(1, 5:7) %>%
      melt(id.vars = "Criminal Justice Setting")
    
    xlabels <- c("Custody\nSuites", "Police", "Prisons", "Social\nwork")
    
    ggplotly(
      Data8ForPlot %>%
        ggplot(aes(x = `Criminal Justice Setting`, y = value, fill = variable, text = paste0("Criminal Jusice Setting: ", `Criminal Justice Setting`,
                                                                                             "<br>Percent of ABIs Delivered: ", value, "%",
                                                                                             "<br>Financial Year: ", variable))) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_x_discrete(labels = xlabels) +
        scale_fill_manual(values = bluesISD, labels = c("2016/17", "2017/18", "2018/19")) +
        theme_minimal() +
        labs(title = "ABIs in Criminal Justice Settings [%]", x = "Criminal Justice Settings", y = "") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
              axis.text.y = element_text(size = 8),
              axis.title = element_text(size = 9, face = "bold"),
              title = element_text(size = 10, face = "bold"),
              legend.text = element_text(size = 8),
              legend.title = element_blank())
    , tooltip = "text") %>% layout(margin = list(t=75),
                 legend = list(orientation = "h",
                               xanchor = "center",
                               x = 0.45,
                               y = 100),
                 xaxis = list(fixedrange=TRUE), 
                 yaxis= list(fixedrange=TRUE)) %>%
      config(displayModeBar = TRUE,
             modeBarButtons = list(list("toImage")),
             displaylogo=FALSE, collaborate=FALSE, editable=FALSE)
  })
  
  output$text7_8 <- renderUI({
    p("Please note: NHS Grampian provided only aggregate figures for Wider Settings. 
      Therefore, NHS Grampian was omitted for Criminal Justice figures.", style = "font-size: 9px; text-align: center;")
  })
  
  output$table7_8 <- DT::renderDataTable(DT::datatable({
    Data6
  },
    options = list(dom = "tif", paging = FALSE), rownames = F))
  
  output$plot9 <- renderPlotly({
    DataForPlot9 <- Data7 %>% 
      filter(`NHS Board` != "Total") %>% 
      select(-Total) %>% 
      melt(id.vars = "NHS Board")
      
    ggplotly(
      DataForPlot9 %>% 
        ggplot(aes(x = `NHS Board`, y = value, fill = variable, text = paste0("NHS Board: ", `NHS Board`,
                                                                              "<br>ABIs Delivered: ", value,
                                                                              "<br>Criminal Justice Setting: ", variable))) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = bluesISD) +
        theme_minimal() +
        labs(title = "ABIs delivered in Criminal Justice settings; by NHS Board for 2018/19", x = "NHS Board", y = "") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
              axis.text.y = element_text(size = 8),
              axis.title = element_text(size = 9, face = "bold"),
              title = element_text(size = 10, face = "bold"),
              legend.text = element_text(size = 8),
              legend.title = element_blank())
    , tooltip = "text") %>% layout(margin = list(t=75),
            legend = list(orientation = "h",
                          xanchor = "center",
                          x = 0.475,
                          y = 100),
            xaxis = list(fixedrange=TRUE), 
            yaxis= list(fixedrange=TRUE)) %>%
      config(displayModeBar = TRUE,
             modeBarButtons = list(list("toImage")),
             displaylogo=FALSE, collaborate=FALSE, editable=FALSE)
  })
  
  output$text9 <- renderUI({
    p("Please note: NHS Grampian provided only aggregate figures for Wider Settings. 
      Therefore, NHS Grampian was omitted for Criminal Justice figures. 
      Also, there are no prisons in NHS Borders, NHS Fife, NHS Orkney, NHS Shetland, NHS Western Isles.", style = "font-size: 9px; text-align: center;")
  })
  
  output$table9 <- DT::renderDataTable(DT::datatable({
    Data7
  }, 
    options = list(dom = "tif", paging = FALSE), rownames = F))
  
  # Download handlers
  
  selection1 <-  reactive({if (req(input$board) == "All") {
    ABIs %>%
      arrange(desc(FY), Board)
  } else if (req(input$board) == "Scotland") {
    ABIs %>%
      group_by(FY) %>%
      summarise(Delivered = sum(Delivered), Target = sum(Target), `Percent of Target` = round(Delivered/Target*100, digits = 0)) %>%
      arrange(desc(FY)) 
  } else {
    ABIs %>%
      filter(Board == input$board) %>%
      arrange(desc(FY)) 
  }})
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste(input$board, "-TotalABIsDelivered", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(selection1(), file, row.names = F)
    }
  )
  
  selection2 <- reactive({
    if (input$board2 == "All") {
      Data2 %>%
        arrange(`NHS Board`) 
    } else {
      Data2 %>%
        filter(`NHS Board` == input$board2)
    }
  })
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste0(input$board2, "-ABIs-Vs-Standard", ".csv")
    },
    content = function(file) {
      write.csv(selection2(), file, row.names = F)
    }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = "Priority&WiderSettings.csv",
    content = function(file) {
      cont <- Data3 %>%
        arrange(desc(FY)) %>%
        rename(`Financial Year` = FY)
      write.csv(cont, file, row.names = F)
    }
  )
  
  output$downloadData4 <- downloadHandler(
    filename = "AllSettings.csv",
    content = function(file) {
      cont <- Data4 %>%
        arrange(`NHS Board`)
      write.csv(cont, file, row.names = F)
    }
  )
  
  output$downloadData5 <- downloadHandler(
    filename = "WiderSettings.csv",
    content = function(file) {
      write.csv(Data5, file, row.names = F)
    }
  )
  
  output$downloadData6 <- downloadHandler(
    filename = "CriminalJusticeSettings.csv",
    content = function(file) {
      write.csv(Data6, file, row.names = F)
    }
  )
  
  output$downloadGlossary1 <- downloadHandler(
    filename = "ABI-Glossary.pdf",
    content = function(file) {
      file.copy("www/Glossary-ABIs.pdf", file)
    }
  )
  
  output$downloadGlossary2 <- downloadHandler(
    filename = "ABI-Glossary.pdf",
    content = function(file) {
      file.copy("www/Glossary-ABIs.pdf", file)
    }
  )
  
  output$downloadGlossary3 <- downloadHandler(
    filename = "ABI-Glossary.pdf",
    content = function(file) {
      file.copy("www/Glossary-ABIs.pdf", file)
    }
  )
  
  output$downloadGlossary4 <- downloadHandler(
    filename = "ABI-Glossary.pdf",
    content = function(file) {
      file.copy("www/Glossary-ABIs.pdf", file)
    }
  )
  
  output$downloadGlossary5 <- downloadHandler(
    filename = "ABI-Glossary.pdf",
    content = function(file) {
      file.copy("www/Glossary-ABIs.pdf", file)
    }
  )
  
  output$downloadGlossary6 <- downloadHandler(
    filename = "ABI-Glossary.pdf",
    content = function(file) {
      file.copy("www/Glossary-ABIs.pdf", file)
    }
  )
  
  # Event Observers
  
  observeEvent({input$tab1
               input$tab2
               input$tab3
               input$tab4
               input$tab5
               input$tab6},
               {{updateTabItems(session, "sidebarMenuTab", "tab1")}
               {updateTabItems(session, "sidebarMenuTab", "tab2")}
               {updateTabItems(session, "sidebarMenuTab", "tab3")}
               {updateTabItems(session, "sidebarMenuTab", "tab4")}
               {updateTabItems(session, "sidebarMenuTab", "tab5")}
               {updateTabItems(session, "sidebarMenuTab", "tab2")}}
              )

  observe({
    toggleState("board", input$sidebarMenuTab == "tab1" | input$sidebarMenuTab == "tab2")
  })
  
  observeEvent(input$actB1, {shinyjs::toggle("table1")})
  
  observeEvent(input$actB2, {toggle("table2")})
  
  observeEvent(input$actB3, {toggle("table3")})
  
  observeEvent(input$actB4, {toggle("table4")})
  
  observeEvent(input$actB5, {toggle("table5_6")})
  
  observeEvent(input$actB6, {toggle("table7_8")})
  
  observeEvent(input$actB7, {toggle("table9")})
  
  observeEvent(input$cc1, {toggle("chc1")})
  
  observeEvent(input$cc2, {toggle("chc2")})
  
  observeEvent(input$cc3, {toggle("chc3")})
  
  observeEvent(input$cc4, {toggle("chc4")})
  
  observeEvent(input$cc5, {toggle("chc5")})
  
  observeEvent(input$cc6, {toggle("chc6")})
  
  # Navigation from Contents page
  
  observeEvent(input$tab1, {updateTabsetPanel(session, "tabsetPanel", "Total ABIs Delivered")})
  
  observeEvent(input$tab2, {updateTabsetPanel(session, "tabsetPanel", "ABIs Delivered compared to Standard")})
  
  observeEvent(input$tab3, {updateTabsetPanel(session, "tabsetPanel", "Priority & Wider Settings")})
  
  observeEvent(input$tab4, {updateTabsetPanel(session, "tabsetPanel", "All Settings")})
  
  observeEvent(input$tab5_6, {updateTabsetPanel(session, "tabsetPanel", "Wider Settings")})
  
  observeEvent(input$tab7_8, {updateTabsetPanel(session, "tabsetPanel", "Criminal Justice Settings")})

})
