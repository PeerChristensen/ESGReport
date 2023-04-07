
# code for admin ui to create new indicators
uiOutput("new_indicator"),


# Create new indicator
new_indicator_ui <- reactive({
  
  if (req(input$select_user) == "admin") {
    box(title = "Tilføj indikator",
        textAreaInput("new_indicator_text", "Tekst"),
        selectInput("new_indicator_theme", "Tema", choices = themes_list),
        selectInput("new_indicator_type", "Type", choices = indicator_types),
        uiOutput("new_indicator_ui_choices"),
        actionButton("create_indicator","Send")
    )
  }
})

output$new_indicator <- renderUI({ new_indicator_ui() })

new_indicator_ui_choices <- reactive({
  if (req(input$new_indicator_type) == "Kategorisk") {
    textAreaInput("new_indicator_choices", "Valgmuligheder", placeholder = "Separér værdier med komma")
  }
  else if (req(input$new_indicator_type) == "Skala") {
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