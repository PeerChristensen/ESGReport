
load_question_info <- function() {
  
  question_files <- list.files("questions", full.names = T)
  question_info <- map(question_files,fromJSON) 
  question_texts <- map(question_info,pull,questionText) %>% unlist()
  question_types <- map(question_info,pull,inputType) %>% unlist()
  question_themes <- map(question_info, pull,questionTheme) %>% unlist()
  list(questions=question_texts,question_types=question_types,question_themes=question_themes)
}

create_new_question <- function(
    n_questions,
    new_question_text,
    new_question_type,
    new_question_theme,
    new_question_choices) {
 
  question_id = paste0("q",n_questions+1)
  tibble(questionID = question_id,
         questionText = paste0(question_id, ": ", new_question_text),
         questionType = new_question_type,
         questionTheme = new_question_theme,
         questionChoices = list(new_question_choices)) %>%
    write_json(glue("questions/{question_id}.json"))
  }

# update this function to return a UI for questions to fill in
generate_questions <- function(selected_questions) {
  
  renderUI(
    box(title = "Spørgsmål til udfyldning",
        p("placeholder")
        )
    )
}

create_menu_items <- function(x) {
  
  renderMenu({
    menu_list <- lapply(
      unique(items$dataset),
      function(x) {
        sub_menu_list = lapply(
          items[items$dataset == x,]$experiment,
          function(y) {
            menuSubItem(y, tabName = str_replace_all(paste0("ExpID_", x, "_", y)," ","")
            )
          }
        )
        menuItem(text = x, do.call(tagList, sub_menu_list))
      }
    )
    sidebarMenu(menuItem("Hjem", tabName = "home"),
                menu_list
                )
  })
}