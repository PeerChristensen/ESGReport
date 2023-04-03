library(bs4Dash)
library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(type = 'text/css', "p {font-size: 18px;}#unit{font-size: 14px;}")
  ),
  br(),
  # eksempel 1
  hr(),
  column(
    6,
    h4("Numerisk input 1"),
    br(),
    p("Hvad er din omsætning?"),
    p("DKK", id = "unit"),
    fluidRow(
      column(3,
             numericInput("input1", "Mål 2023", value = 0)),
      column(3,
             numericInput("input2", "2022", value = 0)),
      column(3,
             numericInput("input3", "2021", value = 0))
    ),
    # eksempel 2
    hr(),
    fluidRow(column(12,
                    h4("Numerisk input 2"),
                    br())),
    fluidRow(column(6,
           p("Vi forebygger arbejdsulykker"),
           p("1-5 point", id = "unit")),
    column(
      2,
      numericInput(
        "input4",
        "Mål 2023",
        value = 1,
        min = 1,
        max = 5
      ),
      numericInput(
        "input5",
        "2022",
        value = 1,
        min = 1,
        max = 5
      ),
      numericInput(
        "input6",
        "2022",
        value = 1,
        min = 1,
        max = 5
      )
      )
    ),
  # eksempel 3
  hr(),
  fluidRow(
    h4("Numerisk input 3"),
    br(),
    p("Afkastningsgraden"),
    p("%", id = "unit"),
    sliderInput(
      "input7",
      "Mål 2023",
      value = 50,
      min = 0,
      max = 100
    ),
    sliderInput(
      "input8",
      "2022",
      value = 0,
      min = 0,
      max = 100
    ),
           sliderInput(
             "input9",
             "2022",
             value = 100,
             min = 0,
             max = 100
           )
  )
),

# column 2
column(6,
       # eksempel 1
       column(12,h4("Kategorisk input 1"),
       br()),
       column(6,
       p("Udarbejdelse af sprøjtejournal")),
       column(2,
       selectInput("input10","Mål 2023",choices=c("ja","nej"))),
       column(2,
       selectInput("input11","2022",choices=c("ja","nej"))),
       column(2,
       selectInput("input12","2021",choices=c("ja","nej")))
       ,
       # eksempel 2
       column(12,
              hr(),
              h4("Kategorisk input 2"),
              br()),
       column(6,
              p("Udarbejdelse af sprøjtejournal")),
       column(2,
              checkboxInput("input13","Mål 2023", value=F)),
       column(2,
              checkboxInput("input14","2022",value=F)),
       column(2,
              checkboxInput("input15","2021",value=F))
       ,
       # eksempel 3
       column(12,
              hr(),
              h4("Kategorisk input 3"),
              br()),
       column(6,
              p("Udarbejdelse af sprøjtejournal")),
       column(2,
              radioButtons("input16","Mål 2023", choices=c("ja","nej"),
                           selected=character(0))),
       column(2,
              radioButtons("input17","2022",choices=c("ja","nej"),
                           selected=character(0))),
       column(2,
              radioButtons("input18","2021",choices=c("ja","nej"),
                           selected=character(0)))
       )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
}

# Run the application
shinyApp(ui = ui, server = server)
