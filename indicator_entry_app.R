library(shiny)
library(bs4Dash)
library(tidyverse)
library(shinyjs)
library(jsonlite)
library(shinyWidgets)
library(glue)

source("utils.R")

n_indicators <- length(list.files("indicators"))
indicator_themes <- read_lines("data/themes_list")

indicator_types <- c("Numerisk", "Kategorisk", "Skala")

ui = dashboardPage(
    title = "ESG Indikatorer",
    header = dashboardHeader(title = "ESG indikatorer"),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem("Ny indikator", tabName = "new_indicator"),
        menuItem("Aktivér / Deaktivér", tabName = "update_status")
      )
    ),
    body = dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        tabItem("new_indicator", uiOutput("new")
                ),
        tabItem("update_status",
                box("Opdatér indikator status",
                    uiOutput("update")
                    )
                )
        )
    )
)

server = function(input, output) {
  
  values <- reactiveValues(n_indicators = n_indicators)
    
    # ------------------------------------------
    # new indicator
    
    # main ui
    new_indicator_ui <- reactive({
      
        box(title = "Tilføj indikator",
            textAreaInput("new_indicator_text", "Tekst"),
            selectInput("new_indicator_theme", "Tema", choices = indicator_themes),
            selectInput("new_indicator_type", "Type", choices = indicator_types),
            textInput("new_indicator_unit", "Enhed"),
            uiOutput("new_indicator_ui_choices"),
            selectInput("new_indicator_status", "Status", choices = c("Aktiv", "Inaktiv")),
            actionButton("create_indicator","Send")
        )
    })
    
    # choices ui
    new_indicator_ui_choices <- reactive({
      if (req(input$new_indicator_type) == "Kategorisk") {

        textAreaInput("new_indicator_choices", "Valgmuligheder", placeholder = "Separér værdier med komma")
        
      }
      else if (req(input$new_indicator_type) == "Skala") {
        fluidRow(
          numericInput("new_indicator_scale_min", "Min", value = 1),
          numericInput("new_indicator_scale_max", "Max", value = 5)
        )
        }
    })
    
    output$new_indicator_ui_choices <- renderUI({ new_indicator_ui_choices()})
    
    observeEvent(input$create_indicator, {
      
      shinyjs::reset()

      create_new_indicator(
        n_indicators = values$n_indicators,
        new_indicator_text = input$new_indicator_text,
        new_indicator_type = input$new_indicator_type,
        new_indicator_unit = input$new_indicator_unit,
        new_indicator_theme = input$new_indicator_theme,
        new_indicator_choices = input$new_indicator_choices,
        new_indicator_scale_min = input$new_indicator_scale_min,
        new_indicator_scale_max = input$new_indicator_scale_max,
        new_indicator_status = input$new_indicator_status
      )
      
      values$n_indicators = values$n_indicators + 1
    })
    
    output$new <- renderUI({ new_indicator_ui()})
    
  # ------------------------------------------
  # update indicator
  
    output$update <- renderUI({
      
      
    })
}

shinyApp(ui,server)