
load_question_info <- function() {
  
  question_files <- list.files("questions", full.names = T)
  question_info <- map(question_files,fromJSON) 
  question_texts <- map(question_info,pull,questionText) %>% unlist()
  question_types <- map(question_info,pull,questionType) %>% unlist()
  question_themes <- map(question_info, pull,questionTheme) %>% unlist()
  list(question_texts=question_texts,question_types=question_types,question_themes=question_themes)
}

create_new_indicator <- function(
    n_indicators,
    new_indicator_text,
    new_indicator_type,
    new_indicator_unit,
    new_indicator_theme,
    new_indicator_choices,
    new_indicator_scale_min,
    new_indicator_scale_max,
    new_indicator_active) {
 
  indicator_id = n_indicators + 1
  tibble(indicatorID = indicator_id,
         indicatorText = paste0(indicator_id, ": ", new_indicator_text),
         indicatorType = new_indicator_type,
         indicatorUnit = new_indicator_unit,
         indicatorTheme = new_indicator_theme,
         indicatorChoices = list(new_indicator_choices),
         indicatorScaleMin = new_indicator_scale_min,
         indicatorScaleMax = new_indicator_scale_max,
         indicatorActive = new_indicator_active) %>%
    write_json(glue("indicators/{indicator_id}.json"))
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