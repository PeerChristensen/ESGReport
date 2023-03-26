library(shiny)
library(bs4Dash)
library(shinyjs)
library(jsonlite)
library(shinyWidgets)
library(glue)

questions_df <- read_csv("questions.csv")
questions <- questions_df %>% pull(questionText)
question_types <- questions_df %>% distinct(inputType) %>% pull(inputType)
n_questions <- nrow(questions_df)
# group questions
A <- questions[1:2]
B <- questions[3:5]
questions <- list("A" = A, "B" = B)
choices <- jsonlite::fromJSON("choices.json")
data <- read_csv("data.csv")
users <- data %>% distinct(AgroID) %>% pull(AgroID)
users <- append(users,'admin')
user_data <- read_csv("user_data.csv")

ui <- dashboardPage(title = "ESGreen Tool Report",
  dashboardHeader(title = "ESGreen Tool Report"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Hjem", tabName = "home", icon = icon("home")),
      menuItem("Ny rapport", tabName = "new_report", icon = icon("file"),
               menuSubItem(text = "Standard",tabName = "standard")
               ,
               menuSubItem(text = "Vælg indhold",tabName = "choose")
      ),
      menuItem("Gemte rapporter", tabName = "prev_reports", icon = icon("save"),
               menuSubItem(text = "22/2/2022",tabName = "prev_1")
               ,
               menuSubItem(text = "1/1/2022",tabName = "prev_2")
      ),
      menuItem("Data", tabName = "data", icon = icon("database")
      )
    )
  ),
  #dashboardControlbar(id = "controlbar",collapsed = FALSE),
  #dashboardFooter(id = "footer",left = "Copyright etc.."),
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
              uiOutput("new_question"),
              
              ),
      tabItem(tabName = "choose",
              box(title = "Vælg spørgsmål",
                  pickerInput(
                    inputId = "select_questions",
                    label = NULL, 
                    choices = questions,
                    options = list(
                      `actions-box` = TRUE,
                      `select-all-text` = "Vælg alle",
                      `deselect-all-text` = "Fravælg alle",
                      `none-selected-text` = "Intet valgt",
                      `selected-text-format` = "count",
                      `count-selected-text` = "{0} spørgsmål valgt"), 
                    multiple = TRUE),
                  actionButton("gen_questions", "Udfyld rapport")
                  ),
              uiOutput("questions")
              ),
      tabItem(tabName = "data",
              box(title = "Dine data",width=11,
                  dataTableOutput('userdata')
                  )
              )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    source("utils.R")
    
    output$userdata <- renderDataTable({
      user_data %>% filter(AgroID == input$select_user)
    })
    
    observeEvent(input$gen_questions, {
      output$questions <- generate_questions(input$select_questions)
    })
    
    new_question_ui <- reactive({
      
      if (req(input$select_user) == "admin") {
        box(title = "Tilføj spørgsmål",
            textAreaInput("new_question_text", "Tekst"),
            selectInput("new_question_type", "Type", choices = question_types),
            textAreaInput("new_question_choices", "Valgmuligheder", placeholder = "Separér værdier med komma"),
            actionButton("create_question","Send")
        )
      }
    })
    
    output$new_question <- renderUI({ new_question_ui()})
    
    observeEvent(input$create_question, {
      
      create_new_question(    n_questions = n_questions,
                              new_question_text = input$new_question_text,
                              new_question_type = input$new_question_type,
                              new_question_choices = input$new_question_choices)
      
    })
  
    
  }
  
  shinyApp(ui, server)