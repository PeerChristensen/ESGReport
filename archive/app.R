library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "ESGreen Tool Report"),
  dashboardSidebar(
    menuItem("Ny rapport", tabName = "new_report"),
    menuItem("Gemte rapporter", tabName = "prev_reports")
  ),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)