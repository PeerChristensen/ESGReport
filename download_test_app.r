library(shiny)
library(bs4Dash)
library(tidyverse)
library(shinyjs)
library(jsonlite)
library(shinyWidgets)
library(glue)
library(lubridate)
library(rlist)

source("utils.R")
source("generate_indicator_ui.R")
#source("generate_report.R")

# -------------------------------------------
# indicator selection and data for inputs
indicator_info <- load_indicator_info()
indicator_info_df <- indicator_info %>% as_tibble()
indicator_texts <- indicator_info$indicator_text
indicator_types <- indicator_info$indicator_types %>% unique()
indicator_themes <- indicator_info$indicator_themes
theme_order <- indicator_themes %>% unique()
#themes_list <- read_lines("data/themes_list")
n_indicators <- length(indicator_texts) 
indicators <- split(indicator_texts,indicator_themes)[theme_order]

# see notes
indicators$BIODIVERSITET <- list(indicators$BIODIVERSITET)

# -------------------------------------------
# UI
ui <- dashboardPage(title = "ESGreen Tool Report",fullscreen = TRUE,
                    dashboardHeader(
                      title = dashboardBrand(
                        title = "ESGreen Tool Report", color = "primary"
                      ),
                      controlbarIcon = icon("th")
                    ),   
                    dashboardSidebar(
                      sidebarUserPanel(
                        image = NULL,
                        name = textOutput("welcome")
                      ),
                      sidebarMenu(
                        menuItem("Ny rapport", tabName = "new_report", icon = icon("file"),
                                 menuSubItem(text = "Standard",tabName = "standard")
                                 ,
                                 menuSubItem(text = "Vælg indikatorer",tabName = "choose")
                        )
                      )
                    ),
                    footer = dashboardFooter(left = "Kontakt, copyright info etc.."),
                    dashboardBody(
                      useShinyjs(),
                      tags$head(includeCSS("www/style.css")),
                      tabItems(
                        tabItem(tabName = "choose",
                                box(title = "Vælg indikatorer",
                                    id = "select_indicators_box",
                                    pickerInput(
                                      inputId = "select_indicators",
                                      label = NULL, 
                                      choices = indicators,
                                      options = list(
                                        `actions-box` = TRUE,
                                        `select-all-text` = "Vælg alle",
                                        `deselect-all-text` = "Fravælg alle",
                                        `none-selected-text` = "Intet valgt",
                                        `selected-text-format` = "count",
                                        `count-selected-text` = "{0} spørgsmål valgt"), 
                                      multiple = TRUE),
                                    actionButton("gen_indicators", "Udfyld rapport")
                                ),
                                #uiOutput("report_ui"),
                                box(title = "Hent rapport",
                                    p("Titel"),
                                    textInput("report_title",label=NULL, value = "ESG Rapport"),
                                    p("Kernefortælling (valgfri)"),
                                    textAreaInput("report_intro", label=NULL),
                                    p("Fakta om bedriften (valgfri)"),
                                    textAreaInput("report_facts", label=NULL),
                                    downloadButton("generate_report", "Download PDF")
                                ),
                                uiOutput("indicators"),
                                tableOutput('show_inputs')
                        ),
                        tabItem(tabName = "standard",
                                uiOutput("indicators_std")
                        ),
                        tabItem(tabName = "data",
                                box(title = "Dine data", width=11,
                                    dataTableOutput('userdata')
                                )
                        )
                      )
                    )
)

# -------------------------------------------
# SERVER

server <- shinyServer(function(input, output, session) {
  
  observeEvent(input$reset, {
    session$reload()
  })
  
  # Generate indicators
  observeEvent(input$gen_indicators, {
    output$indicators <- renderUI({
      generate_indicators(selected_indicators = input$select_indicators)
    })
    shinyjs::hide(id = "select_indicators_box")
  })
  
  output$indicators_std <- renderUI({
    # currently selects all indicators
    generate_indicators(selected_indicators = indicator_texts)
  })
  
  #Generate report
  output$generate_report <- downloadHandler(
    filename =  "new_report.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "ESGReport.Rmd")
      tempCSS <- file.path(tempdir(), "report_style.css")
      file.copy("ESGReport.Rmd", tempReport, overwrite = TRUE)
      file.copy("www/report_style.css", tempCSS, overwrite = TRUE)
      params <-   reactiveValuesToList(input)
      #params <-   input
      html_fn <- rmarkdown::render(tempReport,  params = list("params"=params,
                                                              "indicators_df"=indicator_info_df),
                                   envir = new.env(parent = globalenv()))

      pagedown::chrome_print(html_fn, file)
    }
  )
  
  AllInputs <- reactive({
    myvalues <- NULL
    for(i in 1:length(names(input))){
      myvalues <- as.data.frame(rbind(myvalues,(cbind(names(input)[i],input[[names(input)[i]]]))))
    }
    names(myvalues) <- c("input_name","input_value")
    myvalues <- myvalues %>%
      filter(!is.na(as.numeric(substring(input_name, 1, 1))) |
               substring(input_name, 1,3) == "ini" |
               substring(input_name, 1,6) == "report")
    myvalues
  })

  output$show_inputs <- renderTable({
    AllInputs()
  })
 
})


shinyApp(ui, server)