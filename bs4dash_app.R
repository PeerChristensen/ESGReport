library(shiny)
library(bs4Dash)
library(tidyverse)
library(shinyjs)
library(jsonlite)
library(shinyWidgets)
library(glue)

source("utils.R")

# -------------------------------------------
# indicator selection and data for inputs
indicator_info <- load_indicator_info()
indicator_texts <- indicator_info$indicator_text
indicator_types <- indicator_info$indicator_types %>% unique()
indicator_themes <- indicator_info$indicator_themes %>% unique()
n_indicators <- length(indicator_texts) 
indicators <- split(indicator_texts,indicator_themes)

# -------------------------------------------
# User data
data <- read_csv("data.csv")
users <- data %>% distinct(AgroID) %>% pull(AgroID)
users <- append(users,'admin')
user_data <- read_csv("user_data.csv")

# -------------------------------------------
# UI
ui <- dashboardPage(title = "ESGreen Tool Report",fullscreen = TRUE,
  dashboardHeader(title = "ESGreen Tool Report"),    

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
  #controlbar = dashboardControlbar(id = "controlbar",collapsed = FALSE),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              box(title = "Sådan bruger du ESGreen Tool Report",
                  p("text"),
                  p("text"),
                  collapsible = FALSE
                  ),
              box(title = "Vælg bruger",
                  selectInput("select_user", label = NULL,
                              choices = users)
                  ),
              uiOutput("new_indicator"),
              ),
      tabItem(tabName = "choose",
              box(title = "Vælg indikatorer",
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
              uiOutput("indicators")
              ),
      tabItem(tabName = "data",
              box(title = "Dine data",width=11,
                  dataTableOutput('userdata')
                  )
              )
      )
    )
  )

# -------------------------------------------
# SERVER

server <- function(input, output, session) {
    
    values <- reactiveValues(n_indicators = n_indicators)
    
    output$welcome <- renderText({ paste0("Hej ", input$select_user)})
    
    output$userdata <- renderDataTable({
      user_data %>% filter(AgroID == input$select_user)
    })
    
    observeEvent(input$gen_indicators, {
      output$indicators <- generate_indicators(input$select_indicators)
    })
    
    new_indicator_ui <- reactive({
      
      if (req(input$select_user) == "admin") {
        box(title = "Tilføj indikator",
            textAreaInput("new_indicator_text", "Tekst"),
            selectInput("new_indicator_theme", "Tema", choices = indicator_themes),
            selectInput("new_indicator_type", "Type", choices = indicator_types),
            uiOutput("new_indicator_ui_choices"),
            actionButton("create_indicator","Send")
            )
        }
    })
    
    output$new_indicator <- renderUI({ new_indicator_ui()})
    
    new_indicator_ui_choices <- reactive({
      if (req(input$new_indicator_type) == "kategorisk") {
            textAreaInput("new_indicator_choices", "Valgmuligheder", placeholder = "Separér værdier med komma")
      }
      else if (req(input$new_indicator_type) == "skala") {
        numericRangeInput("range", "Vælg interval", min=1,max=10, value=c(1,5))
      }
    })
    
    output$new_indicator_ui_choices <- renderUI({ new_indicator_ui_choices()})
    
    observeEvent(input$create_indicator, {
      
      create_new_indicator(
        n_indicators = values$n_indicators,
        new_indicator_text = input$new_indicator_text,
        new_indicator_type = input$new_indicator_type,
        new_indicator_theme = input$new_indicator_theme,
        new_indicator_choices = input$new_indicator_choices
      )
      
      values$n_indicators = values$n_indicators+1
    })
  }
  
shinyApp(ui, server)