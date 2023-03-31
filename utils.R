
load_indicator_info <- function() {
  
  indicator_files <- list.files("indicators", full.names = T)
  indicator_info <- map(indicator_files,jsonlite::fromJSON) 
  indicator_texts <- map(indicator_info,pull,indicatorText) %>% unlist()
  indicator_types <- map(indicator_info,pull,indicatorType) %>% unlist()
  indicator_themes <- map(indicator_info, pull,indicatorTheme) %>% unlist()
  list(indicator_texts=indicator_texts,indicator_types=indicator_types,indicator_themes=indicator_themes)
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
    new_indicator_status) {
 
  indicator_id = n_indicators + 1
  tibble(indicatorID = indicator_id,
         indicatorText = paste0(indicator_id, ": ", new_indicator_text),
         indicatorType = new_indicator_type,
         indicatorUnit = new_indicator_unit,
         indicatorTheme = new_indicator_theme,
         indicatorChoices = list(new_indicator_choices),
         indicatorScaleMin = new_indicator_scale_min,
         indicatorScaleMax = new_indicator_scale_max,
         indicatorStatus = new_indicator_status) %>%
    mutate(indicatorScaleMin = ifelse(indicatorType == "Skala",indicatorScaleMin,NA),
           indicatorScaleMax = ifelse(indicatorType == "Skala",indicatorScaleMax,NA),
           indicatorChoices = ifelse(indicatorType == "Kategorisk", indicatorChoices,NA)) %>%
    write_json(glue("indicators/{indicator_id}.json"))
  }

# update this function to return a UI for indicators to fill in
generate_indicators <- function(selected_indicators) {
  
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