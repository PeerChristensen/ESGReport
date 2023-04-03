
source("utils.R")

indicator_info <- load_indicator_info()
indicators <- indicator_info$indicator_texts
indicator_types <- indicator_info$indicator_types
indicator_themes <- indicator_info$indicator_themes

tibble(indicators,indicator_types,indicator_themes) %>% view()

s = c("1","10")
for (i in s) {
  if (nchar(i) == 1) {
    i = paste0("0",i)
    }
  print(i)
  }

