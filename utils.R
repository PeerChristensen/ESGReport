
create_new_question <- function(n_questions,
                                new_question_text,
                                new_question_type,
                                new_question_choices) {
  
  question_id = "q6"#paste0("q",n_questions+1)
  tibble(questionID = question_id,
         questionText = new_question_text,
         inputType = new_question_type,
         choices = list(new_question_choices)) %>%
    write_json(glue("questions/q6.json"))
  }

# update this function to return a UI for questions to fill in
generate_questions <- function(questions) {
  
  renderUI(
    box(title = "test",
        p(
          print(length(questions))
          )
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