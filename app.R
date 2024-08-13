library(shiny)
library(dataquieR)
library(bslib)
library(readxl)
library(tidyverse)
library(plotly)
library(echarts4r)
library(DT)


ui <- page_navbar(
  title = "Transfer Office",
  theme = bs_theme(bootswatch = "minty"),
  bg = "#78c2ad",
   nav_panel(title = "General", style = "background-color: #e9ebeb;",
    fluidRow(
      column(3,
             uiOutput("requests"),
             uiOutput("provision"),
             uiOutput("archiving"),
             uiOutput("institutions"),
             uiOutput("department"),
             uiOutput("uac")
      ), 
      column(9, 
             card(
               card_header("Type of utilisation"),
               echarts4rOutput("use", height = "100%", width = "100%"),
               style = "height: 90vh; width: 100%; background-color: #e9ebeb;",
               full_screen = TRUE
             )
      )
    )
  ),
  nav_menu(title = "Integrity",
           nav_panel(title = "Structural data set error", style = "background-color: #e9ebeb;",
                     tabsetPanel(  
                       tabPanel(title = "Duplicates",
                                fluidRow(
                                  column(4, 
                                         uiOutput("duplicates")
                                  )
                                )
                       )
                     )
           ),
           nav_panel(title = "Value format error", style = "background-color: #e9ebeb;",
                     tabsetPanel(
                       tabPanel(title = "Data type mismatch",
                                fluidRow(
                                  column(12, 
                                         plotlyOutput("integritymatrix", height = "80vh")
                                  )
                                )
                       )
                     )
           )
  ),
  nav_menu(title = "Completeness",
           nav_panel(title = "Crude Missingness", style = "background-color: #e9ebeb;",
                     tabsetPanel(
                       tabPanel(title = "Missing Values - Unit missingness",
                                fluidPage(
                                  uiOutput("unit.miss"),
                                )
                       ),
                       tabPanel(title = "Missing Values - Item missingness",
                                fluidPage(
                                  plotOutput("item.miss", height = "85vh"),
                                )
                       )
                     )
           )
  ),
  nav_menu(title = "Consistency",
           nav_panel(title = "Range and value violations", style = "background-color: #e9ebeb;",
                     tabsetPanel(
                       tabPanel(title = "Inadmissible time-date values - Date of submission",
                                fluidPage(
                                  plotOutput("inadmissible.submission", height = "85vh"),
                                )
                       ),
                       tabPanel(title = "Inadmissible time-date values - Data provision",
                                fluidPage(
                                  plotOutput("inadmissible.provision", height = "85vh"),
                                ),
                       ),
                       
                       tabPanel(title = "Inadmissible time-date values - Data archiving",
                                fluidPage(
                                  plotOutput("inadmissible.archiving", height = "85vh"),
                                ),
                       ),
                       
                       tabPanel(title = "Inadmissible categorical values",
                                fluidPage(
                                  plotlyOutput("inadmissible.cat", height = "85vh"),
                                  DT::dataTableOutput("inadmissible.cat.table")
                                ))
                       
                     )
           ),
           nav_panel(title = "Contradictions", style = "background-color: #e9ebeb;",
                     fluidPage(
                       plotOutput("contradictions", height = "85vh"),
                     )
                     
                     
           )
  ),
  nav_spacer(),
  nav_item(
    actionButton(inputId="show",
                 label="", 
                 style=HTML("background-color: transparent;
                        border: none;"),
                 icon = icon("circle-info",lib = "font-awesome"))
  )
)

server <- function(input, output) { 
  
  observeEvent(input$show, {  
    showModal(modalDialog(  
      title = "What is the Transfer Office?",
      "The Transfer Office is a centralised, independent entity forms part of a Data Integration Centre (DIC),
      which is responsible for ensuring the secure transfer of data across all locations, while also taking into account 
      the relevant local regulations pertaining to data sharing, such as those relating to data protection and 
      ethical standards. The Transfer Office coordinates data usage requests and acts as an interface between the DIC, 
      scientists and the FDPG.",
      easyClose = TRUE,  
      footer = NULL  
    ))  
  })  
  
  
  
  #load data
  data = read_excel("data.xlsx")
  
  data$applicants = as.character(data$applicants)
  
  #load meta data
  prep_load_workbook_like_file("meta_dqa_transferoffice.xlsx")
  items <- prep_get_data_frame("item_level")
  checks= prep_get_data_frame("checks")
  
  
  # Amount of requests
  sparkline <- 
    data %>% 
    dplyr::group_by(date_of_submission) %>% 
    dplyr::summarise(Count = n()) %>% 
    drop_na() %>% 
    plot_ly() %>%
    add_lines(
      x = ~date_of_submission, y = ~Count,
      color = I("white"), span = I(1),
      fill = 'tozeroy', alpha = 0.2
    ) %>%
    layout(
      xaxis = list(visible = TRUE, showgrid = F, title = ""),
      yaxis = list(visible = FALSE, showgrid = F, title = "", showticklabels = FALSE),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>%
    config(displayModeBar = F) %>%
    htmlwidgets::onRender("
    function(el) {
      var valueBox = el.closest('.bslib-value-box');
      if (valueBox) {
        valueBox.addEventListener('bslib.card', function(ev) {
          var isFullScreen = ev.detail.fullScreen;
          var update = {
            'xaxis.visible': isFullScreen,
            'yaxis.visible': isFullScreen,
            'yaxis.showticklabels': isFullScreen
          };
          Plotly.relayout(el, update);
        });
      }
    }
  ")
  
  
  output$requests <- renderUI({
    bslib::value_box(
      value = nrow(data),
      title = "Requests",
      theme = "bg-gradient-blue-pink",
      showcase = sparkline,
      full_screen = TRUE,
      style = "height: calc(15vh - 0.9rem);"
      )
    
  })
  
  sparkline2 <- 
    data %>% 
    dplyr::group_by(data_provision) %>% 
    dplyr::summarise(Count = n()) %>% 
    drop_na() %>% 
    plot_ly() %>%
    add_lines(
      x = ~data_provision, y = ~Count,
      color = I("white"), span = I(1),
      fill = 'tozeroy', alpha = 0.2
    ) %>%
    layout(
      xaxis = list(visible = TRUE, showgrid = F, title = ""),
      yaxis = list(visible = FALSE, showgrid = F, title = "", showticklabels = FALSE),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>%
    config(displayModeBar = F) %>%
    htmlwidgets::onRender("
    function(el) {
      var valueBox = el.closest('.bslib-value-box');
      if (valueBox) {
        valueBox.addEventListener('bslib.card', function(ev) {
          var isFullScreen = ev.detail.fullScreen;
          var update = {
            'xaxis.visible': isFullScreen,
            'yaxis.visible': isFullScreen,
            'yaxis.showticklabels': isFullScreen
          };
          Plotly.relayout(el, update);
        });
      }
    }
  ")
  
  
  # Amount of data provisions
  output$provision <- renderUI({
    data %>%
      dplyr::select(data_provision) %>% 
      drop_na() %>% 
      nrow() %>% 
      bslib::value_box(
        title = "Data provision",
        theme = "bg-gradient-blue-pink",
        showcase = sparkline2,
        full_screen = TRUE,
        style = "height: calc(15vh - 0.9rem);"
      )
  })
  
  
  # Amount of Intitution
  output$institutions <- renderUI({
    bslib::value_box(
      value = n_distinct(data$institution),
      title = "Institutions",
      theme = "bg-gradient-blue-green",
      showcase_layout = "left center",
      style = "height: calc(15vh - 1rem);"
    )
  })
  
  # Amount of Fachbereiche
  output$department <- renderUI({
    bslib::value_box(
      value = n_distinct(data$department),
      title = "Departments",
      theme = "bg-gradient-blue-cyan",
      showcase_layout = "left center",
      full_screen = TRUE,
      style = "height: calc(15vh - 0.9rem);"
    )
  })
  
  sparkline_archiving <- 
    data %>% 
    dplyr::group_by(archiving) %>% 
    dplyr::summarise(Count = n()) %>% 
    drop_na() %>% 
    plot_ly() %>%
    add_lines(
      x = ~archiving, y = ~Count,
      color = I("white"), span = I(1),
      fill = 'tozeroy', alpha = 0.2
    ) %>%
    layout(
      xaxis = list(visible = TRUE, showgrid = F, title = ""),
      yaxis = list(visible = FALSE, showgrid = F, title = "", showticklabels = FALSE),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>%
    config(displayModeBar = F) %>%
    htmlwidgets::onRender("
    function(el) {
      var valueBox = el.closest('.bslib-value-box');
      if (valueBox) {
        valueBox.addEventListener('bslib.card', function(ev) {
          var isFullScreen = ev.detail.fullScreen;
          var update = {
            'xaxis.visible': isFullScreen,
            'yaxis.visible': isFullScreen,
            'yaxis.showticklabels': isFullScreen
          };
          Plotly.relayout(el, update);
        });
      }
    }
  ")
  
  
  # Amount of data provisions
  output$archiving <- renderUI({
    data %>%
      dplyr::select(archiving) %>% 
      drop_na() %>% 
      nrow() %>% 
      bslib::value_box(
        title = "Archiving",
        theme = "bg-gradient-blue-pink",
        showcase = sparkline_archiving,
        full_screen = TRUE,
        style = "height: calc(15vh - 0.9rem);"
      )
  })
  
  pie_uac <- 
    data %>% 
    dplyr::group_by(uac_decision) %>% 
    dplyr::summarise(Count = n()) %>% 
    drop_na() %>% 
    plot_ly() %>%
    add_pie(
      labels = ~uac_decision, value = ~Count,
      color = I("white"), span = I(1),
      fill = 'tozeroy', alpha = 0.2,
      showlegend = FALSE
    ) %>%
    layout(
      xaxis = list(visible = FALSE, showgrid = F, title = ""),
      yaxis = list(visible = FALSE, showgrid = F, title = "", showticklabels = FALSE),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>%
    config(displayModeBar = F) %>%
    htmlwidgets::onRender("
    function(el) {
      var valueBox = el.closest('.bslib-value-box');
      if (valueBox) {
        valueBox.addEventListener('bslib.card', function(ev) {
          var isFullScreen = ev.detail.fullScreen;
          var update = {
            'xaxis.visible': isFullScreen,
            'yaxis.visible': isFullScreen,
            'yaxis.showticklabels': isFullScreen
          };
          Plotly.relayout(el, update);
        });
      }
    }
  ")
  
  
  # Amount of data provisions
  output$uac <- renderUI({
    data %>%
      dplyr::select(uac_decision) %>% 
      drop_na() %>% 
      nrow() %>% 
      bslib::value_box(
        title = "UAC decisions",
        theme = "bg-gradient-blue-pink",
        showcase = pie_uac,
        full_screen = TRUE,
        style = "height: calc(15vh - 0.9rem);"
      )
  })
  
  # Get the counts of each unique value
  df_utilisation <- as.data.frame(table(data$utilisation))
  
  output$use <- renderEcharts4r({
    
    e_charts(data=df_utilisation, Var1) |> 
      #e_color(colors) |> 
      e_pie(Freq, 
            label = list(
              show = TRUE,
              position = "outside", 
              formatter = "{b}",
              textStyle = list(
                fontSize = 18,    # Adjust the font size as needed
                fontWeight = "bold"
              )
            ),
            labelLine = list(show = TRUE)) |> 
      e_pie(Freq,
            label = list(
              show = TRUE,
              position = "inside", 
              formatter = htmlwidgets::JS("function(params) {
            return params.value.toFixed(0);
          }"),
          textStyle = list(
            fontSize = 18,    # Adjust the font size as needed
            fontWeight = "lightbold"
          )
            ),
          radius = c("40%", "70%"),
          itemStyle = list(borderWidth = 1)) |> 
      e_legend(show = FALSE)
  })
  
  # Integrity (Dimension) ----
  ## Structural data set error (Domain) ----
  ### Duplictes (Indicator) ----
  num_duplicates.id =
    data %>% 
    dplyr::select(id) %>% 
    na.omit() %>% 
    duplicated() %>% 
    sum()
  
  # show duplicates and count how often they occur
  dup.id =
    data %>%
    dplyr::select(id) %>% 
    na.omit() %>%
    group_by(id) %>%
    summarise(duplicates = sum(duplicated(id))) %>% 
    dplyr::filter(duplicates > 0) %>% 
    as.data.frame()
  
  # Concatenate duplicated IDs into a single string
  dup_ids_str <- paste(dup.id$id, collapse = ", ")
  
  output$duplicates = renderUI({
    
    if (num_duplicates.id == 0) {
      bslib::value_box(value = paste(num_duplicates.id, " duplicated IDs"), 
                       title = "No duplicated IDs",
                       theme_color = "succeSss")
    } else {
      bslib::value_box(value = paste("Duplicated IDs: ", dup_ids_str), 
                       title = paste(num_duplicates.id, " duplicated IDs"),
                       theme_color = "danger")
    }
  })
  
  ## Value format error ----
  ### Data type mismatch ----
  integritymatrix  <- int_datatype_matrix(study_data = data, 
                                        meta_data = items, 
                                        label_col = LABEL, 
                                        split_segments = FALSE)
  
  output$integritymatrix  <- renderPlotly({
    integritymatrix$SummaryPlot %>% 
      ggplotly() %>% 
      layout(title = list(text = "Applicability Check"),
             margin = list(t=50)
      )
  })
  
  
  # Completeness ----
  ## Crude Missingness ----
  ### Missing values ----
  #### Unit Missingness ----
  unit_miss <- com_unit_missingness(
    study_data = data,
    meta_data = items,
    id_vars = "ID",
    label_col = "LABEL"
  )
  
  output$unit.miss <- renderUI({
    
    if (sum(unit_miss$FlaggedStudyData$Unit_missing) == 0 ) {
      bslib::value_box(value = unit_miss$SummaryData$N, 
                       title = "No unit missingness",
                       theme_color = "succeSss")
    } else{
      bslib::value_box(value = paste(unit_miss$SummaryData$N,"Requests with no entries:",
                                     paste(subset(unit_miss$FlaggedStudyData,Unit_missing > 0)$ID, collapse = ", ")), 
                       title = paste(unit_miss$SummaryData$X.,"%", "Unit missingness"),
                       theme_color = "danger")
    }
    
  })
  
  #### Item Missingess ----
  item_miss <- com_item_missingness(
    study_data = data,
    meta_data = items,
    label_col = "LABEL",
    show_causes = TRUE,
    include_sysmiss = TRUE,
    threshold_value = 0
  )
  
  output$item.miss <- renderPlot({
    item_miss$SummaryPlot
  })
  
  # Consistency ----
  ## Range and value violations ----
  ### Inadmissible time-date values----
  limit_deviations <- con_limit_deviations(label_col  = "LABEL",
                                           study_data = data,
                                           meta_data  = items,
                                           limits     = "HARD_LIMITS")
  
  
  output$inadmissible.submission = renderPlot({
    limit_deviations$SummaryPlotList$`Date of submission`
  })
  
  output$inadmissible.provision = renderPlot({
    limit_deviations$SummaryPlotList$`Data provision`
  })
  
  
  output$inadmissible.archiving = renderPlot({
    limit_deviations$SummaryPlotList$Archiving
  })
  
  ### Inadmissible Categorical values----
  IAVCatAll <- con_inadmissible_categorical(
    study_data = data,
    meta_data = items,
    label_col = LABEL,
    threshold = 0
  )
  
  output$inadmissible.cat = renderPlotly({
    plot_ly(data = IAVCatAll$SummaryTable, 
            y = ~Variables,
            x = ~NUM_con_rvv_icat,
            type = 'bar',
            
            hovertemplate = paste(
              "Non-Matching: ",IAVCatAll$SummaryData$NON_MATCHING_N_PER_CATEGORY
            ),
            text = ~paste0(NUM_con_rvv_icat, "(", PCT_con_rvv_icat, "%)"))%>% 
      layout(xaxis=list(title="Inadmissible categorical values"))
    
  })
  
  
  ##Contradictions ----
  contradictions <- con_contradictions_redcap(
    study_data = data, 
    meta_data = items, 
    label_col = "LABEL",
    threshold_value = 0, 
    meta_data_cross_item = checks
  )
  
  output$contradictions = renderPlot({
    contradictions$SummaryPlot +
      geom_text(aes(label = paste0("n=",contradictions$SummaryTable$NUM_con_con), hjust = -0.2, vjust = -1))
  })
  
} 

shinyApp(ui, server)
