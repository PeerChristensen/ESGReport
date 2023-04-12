library(shiny)
library(bs4Dash)
library(tidyverse)
library(shinyjs)
library(jsonlite)
library(shinyWidgets)
library(glue)
library(lubridate)
library(rlist)
library(shinycssloaders)

source("src/utils.R")
source("src/generate_indicator_ui.R")
source("src/generate_indicator_ui_std.R")
source("src/generate_report.R")
source("src/controlbar.R")
source("src/new_report_elements.R")
source("src/select_indicators.R")

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

curr_year <- year(now())
years <- c(curr_year,curr_year-1,curr_year-2)

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
                        menuItem("Hjem", tabName = "home", icon = icon("home")
                        ),
                        menuItem("Ny rapport", tabName = "new_report", icon = icon("file")
                        ),
                        menuItem("Gemte rapporter", tabName = "prev_reports", icon = icon("save"),
                                 menuSubItem(text = "22/2/2022",tabName = "prev_1")
                                 ,
                                 menuSubItem(text = "1/1/2022",tabName = "prev_2")
                        )
                      )
                    ),
                    footer = dashboardFooter(left = "Kontakt, copyright info etc.."),
                    controlbar =   dashboardControlbar(
                      id = "controlbar", collapsed = T, pinned =T,
                      uiOutput("controlbar_items")
                      ),                 
                    dashboardBody(
                      useShinyjs(),
                      tags$head(includeCSS("www/style.css")),
                      tabItems(
                        tabItem(tabName = "home",
                                box(width = 9,
                                    title = "Sådan bruger du ESGreen Tool Report",
                                    p("I menuen til venstre kan du under 'Ny rapport' vælge at udfylde en standard ESG-rapport eller selv vælge hvilke indikatorer der skal indgå i din rapport. Du kan også tilgå og redigere i tidligere rapporter."),
                                    p("Menuen til højre hjælper dig til at navigere i værktøjet. Når du har udfyldt en rapport kan du her klikke 'Hent rapport' for at downloade din rapport."),
                                    collapsible = FALSE
                                )
                        ),
                        tabItem(tabName = "new_report",
                                uiOutput("report_type"),
                                uiOutput("select_indicators"),
                                uiOutput("indicators"),
                                uiOutput("indicators_std"),
                                
                                uiOutput("report_ui")

                        )
                      )
                    )
)

# -------------------------------------------
# SERVER

server <- function(input, output, session) {
  
  values <- reactiveValues(n_indicators = n_indicators)
  
  observeEvent(input$reset, {
    session$reload()
  })
  
  # Greet user
  output$welcome <- renderText({ paste0("Hej Niels", input$select_user)})
  
  # new report tab item selected
  # select report type
  output$report_type <- renderUI({ select_report_type() })
  
  # show control bar
  # hide report type selection
  # show next ui step
  observeEvent(input$select_report_type_btn, {
    updateControlbar(id = "controlbar", session = session)
    output$controlbar_items <- renderUI({ show_controlbar_items() })
    
    hide(id = "report_type")
    
    if (input$select_report_type == "Vælg indikatorer") {
      
      output$select_indicators <- renderUI({ select_indicators(indicators, years) })
      
    } else if (input$select_report_type == "Standardrapport") {
      output$indicators_std <- renderUI({ generate_indicators_std(indicators) })
    }
  })
  
  # Generate custom indicators
  observeEvent(input$gen_indicators, {
    output$indicators <- renderUI({
      generate_indicators(selected_indicators = input$select_indicators,
                          selected_years = input$select_years)
    })
    hide(id = "select_indicators_box")
  })
  
  # output$indicators_std <- renderUI({
  #   # currently selects all indicators
  #   generate_indicators_std(selected_indicators = indicator_texts)
  # })
  
  
    # generate report
  observeEvent(input$complete, {
    output$report_ui <- renderUI({ get_report_ui()})
    hide("indicators")
    hide("indicators_std")
    
    #output$report_ui_std <- renderUI({ get_report_ui()})
  }
  )
  
  output$generate_report <- downloadHandler(
    filename =  "ESGreenToolReport.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "ESGReport.Rmd")
      tempCSS <- file.path(tempdir(), "report_style.css")
      file.copy("templates/ESGReport.Rmd", tempReport, overwrite = TRUE)
      file.copy("www/report_style.css", tempCSS, overwrite = TRUE)
      params <-   reactiveValuesToList(input)
      html_fn <- rmarkdown::render(tempReport,  
                                   params = list(
                                     "params" = params,
                                     "indicators_df" = indicator_info_df
                                   ),
                                   envir = new.env(parent = globalenv()))
      
      pagedown::chrome_print(html_fn, file)
    }
  )
  
  output$generate_report_html <- downloadHandler(
    filename =  "ESGreenToolReport.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "ESGReport_html.Rmd")
      tempCSS <- file.path(tempdir(), "report_style.css")
      file.copy("templates/ESGReport_html.Rmd", tempReport, overwrite = TRUE)
      file.copy("www/report_style.css", tempCSS, overwrite = TRUE)
      params <-   reactiveValuesToList(input)
      
      render_markdown <- function(){
        rmarkdown::render(tempReport,
                          output_file = file,
                          params = list(
                            "params" = params,
                            "indicators_df" = indicator_info_df
                          ),
                          envir = new.env(parent = globalenv())
        )}
      render_markdown()
    }
  )
  
}

shinyApp(ui, server)