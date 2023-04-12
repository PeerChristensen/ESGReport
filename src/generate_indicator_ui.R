
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
  klima <- indicator_info %>% keep(~.$indicatorTheme == "KLIMA") %>% map(create_ui_inputs) 
  klima_text_input <- create_text_inputs("KLIMA")
  klima <- list.append(klima,klima_text_input)
  
  bio <- indicator_info %>% keep(~.$indicatorTheme == "BIODIVERSITET") %>% map(create_ui_inputs)
  bio_text_input <- create_text_inputs("BIODIVERSITET")
  bio <- list.append(bio,bio_text_input)
  
  vand <- indicator_info %>% keep(~.$indicatorTheme == "VANDMILJØ OG LUFTKVALITET") %>% map(create_ui_inputs)
  vand_text_input <- create_text_inputs("VANDMILJØ OG LUFTKVALITET")
  vand <- list.append(vand,vand_text_input)
  
  mark <- indicator_info %>% keep(~.$indicatorTheme == "MARKJORDENS FRUGTBARHED") %>% map(create_ui_inputs)
  mark_text_input <- create_text_inputs("MARKJORDENS FRUGTBARHED")
  mark <- list.append(mark,mark_text_input)
  
  husdyr <- indicator_info %>% keep(~.$indicatorTheme == "HUSDYRENES SUNDHED OG VELFÆRD") %>% map(create_ui_inputs)
  husdyr_text_input <- create_text_inputs("HUSDYRENES SUNDHED OG VELFÆRD")
  husdyr <- list.append(husdyr,husdyr_text_input)
  
  ressource <- indicator_info %>% keep(~.$indicatorTheme == "OPTIMAL RESSOURCEANVENDELSE") %>% map(create_ui_inputs)
  ressource_text_input <- create_text_inputs("OPTIMAL RESSOURCEANVENDELSE")
  ressource <- list.append(ressource,ressource_text_input)
  
  oekonomi <- indicator_info %>% keep(~.$indicatorTheme == "ØKONOMISK ROBUSTHED") %>% map(create_ui_inputs)
  oekonomi_text_input <- create_text_inputs("ØKONOMISK ROBUSTHED")
  oekonomi <- list.append(oekonomi,oekonomi_text_input)
  
  ledelse <- indicator_info %>% keep(~.$indicatorTheme == "LEDELSE") %>% map(create_ui_inputs)
  ledelse_text_input <- create_text_inputs("LEDELSE")
  ledelse <- list.append(ledelse,ledelse_text_input)
  
  arbejde <- indicator_info %>% keep(~.$indicatorTheme == "ARBEJDSFORHOLD") %>% map(create_ui_inputs)
  arbejde_text_input <- create_text_inputs("ARBEJDSFORHOLD")
  arbejde <- list.append(arbejde,arbejde_text_input)
  
  ui_list <- list("KLIMA" = klima,
                  "BIODIVERSITET" = bio, 
                  "VANDMILJØ OG LUFTKVALITET" = vand, 
                  "MARKJORDENS FRUGTBARHED" = mark, 
                  "HUSDYRENES SUNDHED OG VELFÆRD" = husdyr, 
                  "OPTIMAL RESSOURCEANVENDELSE" = ressource, 
                  "ØKONOMISK ROBUSTHED" = oekonomi, 
                  "LEDELSE" = ledelse, 
                  "ARBEJDSFORHOLD" = arbejde)
  
  accordion(id="accordion", width = 9,
            accordionItem(title = "KLIMA", klima),
            accordionItem(title = "BIODIVERSITET", bio),
            accordionItem(title = "VANDMILJØ OG LUFTKVALITET", vand),
            accordionItem(title = "MARKJORDENS FRUGTBARHED", mark),
            accordionItem(title = "HUSDYRENES SUNDHED OG VELFÆRD", husdyr),
            accordionItem(title = "OPTIMAL RESSOURCEANVENDELSE", ressource),
            accordionItem(title = "ØKONOMISK ROBUSTHED", oekonomi),
            accordionItem(title = "LEDELSE", ledelse),
            accordionItem(title = "ARBEJDSFORHOLD", arbejde)
  )
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
  
  year <- year(now())
  
  if (indicator['indicatorType'] == "Numerisk") {
    
    ui <- column(12,
           hr(),
           p(indicator['indicatorText'],style = "font-size: 20px;"),
           p(indicator['indicatorUnit'], id = "unit"),
             fluidRow(
               column(3, numericInput(glue("{indicator['indicatorID']}_{year}"),glue("Mål {year}"), value=12345)), # change back to NA
               column(3, numericInput(glue("{indicator['indicatorID']}_{year-1}"),glue("{year-1}"), value = 12345)),
               column(3, numericInput(glue("{indicator['indicatorID']}_{year-2}"), glue("{year-2}"), value = 12345))
             )
          )
  }
  
  else if (indicator['indicatorType'] == "Kategorisk") {
    
    ui <- column(12,
                 hr(),
                 p(indicator['indicatorText'],style = "font-size: 20px;"),
                 p(indicator['indicatorUnit'], id = "unit"),
                 fluidRow(
                   column(3, selectInput(glue("{indicator['indicatorID']}_{year}"),glue("Mål {year}"), 
                                         choices = c("",unlist(indicator["indicatorChoices"],use.names=F)),selected="ja",selectize=T)),
                   column(3, selectInput(glue("{indicator['indicatorID']}_{year-1}"), glue("{year-1}"), 
                                         choices = c("",unlist(indicator["indicatorChoices"],use.names=F)),selected="Nej",selectize=T)),
                   column(3, selectInput(glue("{indicator['indicatorID']}_{year-2}"), glue("{year-2}"), 
                                         choices = c("",unlist(indicator["indicatorChoices"],use.names=F)),selected="Ja",selectize=T)),
                   style='padding-bottom:75px;'
                 )
    )
  }
  
  else if (indicator['indicatorType'] == "Skala") {
    
    ui <- column(12,
                 hr(),
                 p(indicator['indicatorText'],style = "font-size: 20px;"),
                 p(indicator['indicatorUnit'], id = "unit"),
                 fluidRow(
                   column(3, numericInput(glue("{indicator['indicatorID']}_{year}"),glue("Mål {year}"), 
                                          min = indicator['indicatorScaleMin'],
                                          max = indicator['indicatorScaleMax'],
                                          value = 3)), # back to NA
                   column(3, numericInput(glue("{indicator['indicatorID']}_2022"),glue("{year-1}"), 
                                          min = indicator['indicatorScaleMin'],
                                          max = indicator['indicatorScaleMax'],
                                          value = 3)),
                   column(3, numericInput(glue("{indicator['indicatorID']}_2021"),glue("{year-2}"), 
                                          min = indicator['indicatorScaleMin'],
                                          max = indicator['indicatorScaleMax'],
                                          value = 3))
                   )
    )
  }
}

create_text_inputs <- function(theme) {
  
  inputs <- column(12,
    hr(),
    p("Iværksatte tiltag",style = "font-size: 20px;"),
    textAreaInput(inputId = glue("initiatives_curr_{theme}"), label=NULL,
                  placeholder = "Separér tiltag med '-'  (Eksempel: - Tiltag 1 - Tiltag 2..)"
                  )
    ,
    br(),
    p("Fremtidige tiltag",style = "font-size: 20px;"),
    textAreaInput(inputId = glue("initiatives_fut_{theme}"), label=NULL,
                  placeholder = "Separér tiltag med '-'  (Eksempel: - Tiltag 1 - Tiltag 2..)"
                  )
    )
}
