
# update this function to return a UI for indicators to fill in
# 1. get list of indicatorIDs
# 2. add a "0" to single digit file names
# 3. read files
# 4. create accordionitem for each theme in selection

# MAIN FUNCTION
generate_indicators <- function(selected_indicators) {
  
  ids <- map(selected_indicators,str_split,":", simplify=T) %>% 
    map(1) %>% 
    unlist()
  indicator_info <- load_indicators(ids)
  klima <- indicator_info %>% keep(~.$indicatorTheme == "KLIMA")
  bio <- indicator_info %>% keep(~.$indicatorTheme == "BIODIVERSITET")
  vand <- indicator_info %>% keep(~.$indicatorTheme == "VANDMILJØ OG LUFTKVALITET")
  mark <- indicator_info %>% keep(~.$indicatorTheme == "MARKJORDENS FRUGTBARHED")
  husdyr <- indicator_info %>% keep(~.$indicatorTheme == "HUSDYRENES SUNDHED OG VELFÆRD")
  ressource <- indicator_info %>% keep(~.$indicatorTheme == "OPTIMAL RESSOURCEANVENDELSE")
  oekonomi <- indicator_info %>% keep(~.$indicatorTheme == "ØKONOMISK ROBUSTHED")
  ledelse <- indicator_info %>% keep(~.$indicatorTheme == "LEDELSE")
  arbejde <- indicator_info %>% keep(~.$indicatorTheme == "ARBEJDSFORHOLD")
  
  ui_list <- map(indicator_info, create_ui_inputs)
}

load_indicators <- function(ids) {
  
  # prefix single-valued IDs with "0" to read files
  for (i in 1:length(ids)) {
    if (nchar(ids[i]) == 1) {
      ids[i] = paste0("0",ids[i])
    }
  }
  
  selected_indicator_info <- map(ids, get_input_for_indicator_ui)
  
}

get_input_for_indicator_ui <- function(indicatorID) {
  
  json <- jsonlite::fromJSON(glue::glue("indicators/{indicatorID}.json"))
  indicatorID <- json %>% pull(indicatorID)
  indicatorText <- json %>% pull(indicatorText)
  indicatorType <- json %>% pull(indicatorType)
  indicatorUnit <- json %>% pull(indicatorUnit)
  indicatorTheme <- json %>% pull(indicatorTheme)
  indicatorStatus <- json %>% pull(indicatorStatus)
  
  out <- list(indicatorID = indicatorID,
              indicatorText = indicatorText,
              indicatorType = indicatorType, 
              indicatorUnit = indicatorUnit,
              indicatorTheme = indicatorTheme,
              indicatorStatus = indicatorStatus)
  
  if (indicatorType == "Skala") {
    indicatorScaleMin <- json %>% pull(indicatorScaleMin)
    indicatorScaleMax <- json %>% pull(indicatorScaleMax)
    out[["indicatorScaleMin"]] = indicatorScaleMin
    out[["indicatorScaleMax"]] = indicatorScaleMax
  } else if (indicatorType == "Kategorisk") {
    indicatorChoices <- json %>% pull(indicatorChoices) %>% str_split(",", simplify = T)
    out[["indicatorChoices"]] = c(indicatorChoices)
  }
  out
}


create_ui_inputs <- function(indicator) {
  
  if (indicator['indicatorType'] == "Numerisk") {
    
    ui <- column(12,
           hr(),
           p(indicator['indicatorText'],style = "font-size: 20px;"),
           p(indicator['indicatorUnit'], id = "unit"),
             fluidRow(
               column(3, numericInput(glue("{indicator['indicatorID']}_2023"),"Mål 2023", value=0)),
               column(3, numericInput(glue("{indicator['indicatorID']}_2022"), "2022", value = 0)),
               column(3, numericInput(glue("{indicator['indicatorID']}_2021"), "2021", value = 0))
             )
          )
  }
  
  else if (indicator['indicatorType'] == "Kategorisk") {
    
    ui <- column(12,
                 hr(),
                 p(indicator['indicatorText'],style = "font-size: 20px;"),
                 p(indicator['indicatorUnit'], id = "unit"),
                 fluidRow(
                   column(3, selectInput(glue("{indicator['indicatorID']}_2023"),"Mål 2023", choices = indicator["indicatorChoices"])),
                   column(3, selectInput(glue("{indicator['indicatorID']}_2022"), "2022", choices = indicator["indicatorChoices"])),
                   column(3, selectInput(glue("{indicator['indicatorID']}_2021"), "2021", choices = indicator["indicatorChoices"]))
                 )
    )
  }
  
  else if (indicator['indicatorType'] == "Skala") {
    
    ui <- column(12,
                 hr(),
                 p(indicator['indicatorText'],style = "font-size: 20px;"),
                 p(indicator['indicatorUnit'], id = "unit"),
                 fluidRow(
                   column(3, numericInput(glue("{indicator['indicatorID']}_2023"),"Mål 2023", 
                                          min = indicator['indicatorScaleMin'],
                                          max = indicator['indicatorScaleMax'],
                                          value = 1)),
                   column(3, numericInput(glue("{indicator['indicatorID']}_2022"),"Mål 2022", 
                                          min = indicator['indicatorScaleMin'],
                                          max = indicator['indicatorScaleMax'],
                                          value = 1)),
                   column(3, numericInput(glue("{indicator['indicatorID']}_2021"),"Mål 2021", 
                                          min = indicator['indicatorScaleMin'],
                                          max = indicator['indicatorScaleMax'],
                                          value = 1))
                   )
    )
  }
}
