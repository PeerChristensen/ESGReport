library(shiny)
library(bs4Dash)
library(tidyverse)
library(shinyjs)
library(jsonlite)
library(shinyWidgets)
library(glue)
library(lubridate)

source("utils.R")
source("generate_indicator_ui.R")
source("generate_report.R")

# -------------------------------------------
# indicator selection and data for inputs
indicator_info <- load_indicator_info()
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
# User data
data <- read_csv("data/data.csv")
users <- data %>% distinct(AgroID) %>% pull(AgroID)
users <- append(users,'admin')
user_data <- read_csv("data/user_data.csv")

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
      menuItem("Ny rapport", tabName = "new_report", icon = icon("file"),
               menuSubItem(text = "Standard",tabName = "standard")
               ,
               menuSubItem(text = "Vælg indikatorer",tabName = "choose")
               ),
      menuItem("Gemte kladder", tabName = "drafts", icon = icon("save")
               ),
      menuItem("Tidl. rapporter", tabName = "prev_reports", icon = icon("file"),
               menuSubItem(text = "22/2/2022",tabName = "prev_1")
               ,
               menuSubItem(text = "1/1/2022",tabName = "prev_2")
               ),
      menuItem("Data", tabName = "data", icon = icon("database")
      )
    )
  ),
  footer = dashboardFooter(left = "Kontakt, copyright info etc.."),
  controlbar = dashboardControlbar(
    id = "controlbar",collapsed = F, pinned =T,
    column(12,
           div(style="text-align: center;margin-top: 25px;",
               actionButton("save", "Gem kladde", width="125px")
           )
    ),
    column(12,
           div(style="text-align: center;margin-top: 25px;",
               actionButton("reset", "Start forfra",width="125px")
               )
           )
    ),
  dashboardBody(
    useShinyjs(),
    tags$head(includeCSS("www/style.css")),
    tabItems(
      tabItem(tabName = "home",
              box(title = "Sådan bruger du ESGreen Tool Report",
                  p("text"),
                  p("text"),
                  collapsible = FALSE
                  )
              ),
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
              uiOutput("report_ui"),
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

server <- function(input, output, session) {
    
    observeEvent(input$reset, {
      session$reload()
      })
    
    values <- reactiveValues(n_indicators = n_indicators)
    
    # Greet user
    output$welcome <- renderText({ paste0("Hej ", input$select_user)})
    
    # Show user data
    output$userdata <- renderDataTable({
      user_data %>% filter(AgroID == input$select_user)
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
    
    output$report_ui <- renderUI({ get_report_UI()
    })
    
    AllInputs <- reactive({
      myvalues <- NULL
      for(i in 1:length(names(input))){
        myvalues <- as.data.frame(rbind(myvalues,(cbind(names(input)[i],input[[names(input)[i]]]))))
      }
      names(myvalues) <- c("input_name","input_value")
      myvalues <- myvalues %>% 
        filter(!is.na(as.numeric(substring(input_name, 1, 1))) | substring(input_name, 1,3) == "ini")
    })
    
    output$show_inputs <- renderTable({
      AllInputs()
    })
    
}
  
shinyApp(ui, server)