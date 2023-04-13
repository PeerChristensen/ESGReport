

# MAIN FUNCTION
generate_indicators_std <- function(selected_indicators) {
  
  ids <- map(selected_indicators,str_split,":", simplify=T) %>% 
    map(1) %>% 
    unlist()
  
  indicator_info <- load_indicators_std(ids)
  klima <- indicator_info %>% keep(~.$indicatorTheme == "KLIMA") %>% map(create_ui_inputs_std) 
  klima_text_input <- create_text_inputs_std("KLIMA")
  klima <- list.append(klima,klima_text_input)
  
  bio <- indicator_info %>% keep(~.$indicatorTheme == "BIODIVERSITET") %>% map(create_ui_inputs_std)
  bio_text_input <- create_text_inputs_std("BIODIVERSITET")
  bio <- list.append(bio,bio_text_input)
  
  vand <- indicator_info %>% keep(~.$indicatorTheme == "VANDMILJØ OG LUFTKVALITET") %>% map(create_ui_inputs_std)
  vand_text_input <- create_text_inputs_std("VANDMILJØ OG LUFTKVALITET")
  vand <- list.append(vand,vand_text_input)
  
  mark <- indicator_info %>% keep(~.$indicatorTheme == "MARKJORDENS FRUGTBARHED") %>% map(create_ui_inputs_std)
  mark_text_input <- create_text_inputs_std("MARKJORDENS FRUGTBARHED")
  mark <- list.append(mark,mark_text_input)
  
  husdyr <- indicator_info %>% keep(~.$indicatorTheme == "HUSDYRENES SUNDHED OG VELFÆRD") %>% map(create_ui_inputs_std)
  husdyr_text_input <- create_text_inputs_std("HUSDYRENES SUNDHED OG VELFÆRD")
  husdyr <- list.append(husdyr,husdyr_text_input)
  
  ressource <- indicator_info %>% keep(~.$indicatorTheme == "OPTIMAL RESSOURCEANVENDELSE") %>% map(create_ui_inputs_std)
  ressource_text_input <- create_text_inputs_std("OPTIMAL RESSOURCEANVENDELSE")
  ressource <- list.append(ressource,ressource_text_input)
  
  oekonomi <- indicator_info %>% keep(~.$indicatorTheme == "ØKONOMISK ROBUSTHED") %>% map(create_ui_inputs_std)
  oekonomi_text_input <- create_text_inputs_std("ØKONOMISK ROBUSTHED")
  oekonomi <- list.append(oekonomi,oekonomi_text_input)
  
  ledelse <- indicator_info %>% keep(~.$indicatorTheme == "LEDELSE") %>% map(create_ui_inputs_std)
  ledelse_text_input <- create_text_inputs_std("LEDELSE")
  ledelse <- list.append(ledelse,ledelse_text_input)
  
  arbejde <- indicator_info %>% keep(~.$indicatorTheme == "ARBEJDSFORHOLD") %>% map(create_ui_inputs_std)
  arbejde_text_input <- create_text_inputs_std("ARBEJDSFORHOLD")
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
            accordionItem(title = "KLIMA", klima, collapsed = FALSE),
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

load_indicators_std <- function(ids) {
  
  # prefix single-valued IDs with "0" to read files
  for (i in 1:length(ids)) {
    if (nchar(ids[i]) == 1) {
      ids[i] = paste0("0",ids[i])
    }
  }
  
  selected_indicator_info <- map(ids, get_input_for_indicator_ui_std)
  
}

get_input_for_indicator_ui_std <- function(indicatorID) {
  
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


create_ui_inputs_std <- function(indicator) {
  
  inputs <- NULL
  curr_year <- year(now())
  years <- as.character(c(curr_year,curr_year-1,curr_year-2))
  
  for (i in years) {
    
    # numeric
    if (indicator['indicatorType'] == "Numerisk") {
      if (i == curr_year) {
        inputs[[i]] <- column(3, numericInput(glue("{indicator['indicatorID']}_{i}"),glue("Mål {i}"), value=12345))
      } else {
        inputs[[i]] <- column(3, numericInput(glue("{indicator['indicatorID']}_{i}"),glue("{i}"), value=12345))
      }
      
      # categorical
    } else if (indicator['indicatorType'] == "Kategorisk") {
      if (i == curr_year) {
        inputs[[i]] <- column(3, selectInput(glue("{indicator['indicatorID']}_{i}"),glue("Mål {i}"), 
                                             choices = c("",unlist(indicator["indicatorChoices"],use.names=F)),selected="ja",selectize=T))
      } else {
        inputs[[i]] <- column(3, selectInput(glue("{indicator['indicatorID']}_{i}"),glue("{i}"), 
                                             choices = c("",unlist(indicator["indicatorChoices"],use.names=F)),selected="ja",selectize=T))
      }
      # scale
    } else if (indicator['indicatorType'] == "Skala") {
      if (i == curr_year) {
        inputs[[i]] <- column(3, numericInput(glue("{indicator['indicatorID']}_{i}"),glue("Mål {i}"), 
                                              min = indicator['indicatorScaleMin'],
                                              max = indicator['indicatorScaleMax'],
                                              value = 3)) # NA
      } else {
        inputs[[i]] <- column(3, numericInput(glue("{indicator['indicatorID']}_{i}"),glue("{i}"), 
                                              min = indicator['indicatorScaleMin'],
                                              max = indicator['indicatorScaleMax'],
                                              value = 3)) # NA
      }
    }
  }
  
  ui <- column(12,
               hr(),
               p(indicator['indicatorText'],style = "font-size: 20px;"),
               p(indicator['indicatorUnit'], id = "unit"),
               fluidRow(inputs)
  )
}

create_text_inputs_std <- function(theme) {
  
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
