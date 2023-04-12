

select_indicators <- function(indicators, years) {
  
  box(width = 9,
      title = "Vælg indikatorer",
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
      p("Vælg årstal til udfyldning"),
      pickerInput(
        inputId = "select_years",
        label = NULL, 
        choices = years,
        options = list(
          `actions-box` = TRUE,
          `select-all-text` = "Vælg alle",
          `deselect-all-text` = "Fravælg alle",
          `none-selected-text` = "Intet valgt"
        ), 
        multiple = TRUE),
      actionButton("gen_indicators", "Udfyld rapport")
  )
}