

select_report_type <- function() {
  
  box(
    width = 9,
    title = "Din ESG-rapport",
    id = "select_report_type_box",
    p("Vælg indikatorer eller udfyld en standardrapport"),
    selectInput(
      "select_report_type", 
      label=NULL,
      choices = c("Vælg indikatorer", "Standardrapport")
      ),
    actionButton("select_report_type_btn", label= "Gå videre")
    )
}