load_indicator_info <- function() {
  
  indicator_files <- list.files("indicators", full.names = T)
  indicator_info <- map(indicator_files,jsonlite::fromJSON) %>% 
    keep(~.$indicatorStatus == "Aktiv")
  indicator_ids <- map(indicator_info,pull,indicatorID) %>% unlist()
  indicator_texts <- map(indicator_info,pull,indicatorText) %>% unlist()
  indicator_types <- map(indicator_info,pull,indicatorType) %>% unlist()
  indicator_units <- map(indicator_info,pull,indicatorUnit) %>% unlist()
  indicator_themes <- map(indicator_info, pull,indicatorTheme) %>% unlist()
  indicator_status <- map(indicator_info, pull,indicatorStatus) %>% unlist()
  list(indicator_ids=indicator_ids,
       indicator_texts=indicator_texts,
       indicator_types=indicator_types,
       indicator_units=indicator_units,
       indicator_themes=indicator_themes,
       indicator_status=indicator_status)
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