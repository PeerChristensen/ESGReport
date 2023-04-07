
library(shiny)
library(bs4Dash)
library(jsonlite)
library(tidyverse)
library(glue)

# functions
get_ui_for_indicator <- function(indicatorID) {
  
  json <- jsonlite::fromJSON(glue::glue("indicators/{indicatorID}.json"))
  indicatorID <- json %>% pull(indicatorID)
  indicatorText <- json %>% pull(indicatorText)
  indicatorType <- json %>% pull(indicatorType)
  indicatorUnit <- json %>% pull(indicatorUnit)
  indicatorTheme <- json %>% pull(indicatorTheme)
  indicatorStatus <- json %>% pull(indicatorStatus)
  
  out <- list(indicatorID = indicatorID,
              indicatorText = indicatorText,
              indicatorType = indicatorType, 
              indicatorUnit = indicatorUnit,
              indicatorTheme = indicatorTheme,
              indicatorStatus = indicatorStatus)
  
  if (indicatorType == "Skala") {
    indicatorScaleMin <- json %>% pull(indicatorScaleMin)
    indicatorScaleMax <- json %>% pull(indicatorScaleMax)
    out[["indicatorScaleMin"]] = indicatorScaleMin
    out[["indicatorScaleMax"]] = indicatorScaleMax
  } else if (indicatorType == "Kategorisk") {
    indicatorChoices <- json %>% pull(indicatorChoices) %>% str_split(",", simplify = T)
    out[["indicatorChoices"]] = c(indicatorChoices)
  }
  out
}

create_indicator_ui <- function() {
  
  indicator_info <- get_ui_for_indicator("01")
  if (indicator_info$indicatorType == "Numerisk") {
      
      column(12,
             hr(),
              p(glue("{indicator_info$indicatorText}"),style = "font-size: 20px;"),
              p(glue("Enhed: {indicator_info$indicatorUnit}", id = "unit"),
              fluidRow(
                column(3, numericInput(glue("{indicator_info$indicatorID}"),"Mål 2023", value=0)),
                column(3, numericInput(glue("{indicator_info$indicatorID}"), "2022", value = 0)),
                column(3, numericInput(glue("{indicator_info$indicatorID}"), "2021", value = 0))
              )
      )
      )
    
  }
}

# ui ------------------------------
ui = dashboardPage(
  title = "Basic Dashboard",
  header = dashboardHeader(),
  sidebar = dashboardSidebar(),
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  body = dashboardBody(
    tags$head(includeCSS("www/style.css")),
    uiOutput("test")
  )
)


server <- function(input, output) {
  
  output$a <- renderUI({
    create_indicator_ui()
  })
  
  output$b <- renderUI({
    create_indicator_ui()
  })
  
  output$test <- renderUI({
    accordion(id="x",
      accordionItem(title="KLIMA",
        uiOutput("a"),
        uiOutput("b"),
      ),
      accordionItem(title="BIODIVERSITET",
                    numericInput("t","T", value=0),
                    hr(),
                    selectInput("s","S", choices=indicatorChoices),
                    br(),
                    hr()
      )
    )
  })
}

shinyApp(ui,server)
