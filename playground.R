
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


format_html_list <- function(char){
  
  seps <- c("<li>", "</li>")
  html_wrapper <- c("<ul>", "</ul>")
  items <- str_split(char, "-",simplify=T) %>% str_trim()
  items <- items[nchar(items)>0]
  bullets <- map(items, ~paste0(seps[1], .x, seps[2], collapse = ""))
  bullets <- paste(bullets,collapse = "")

  html_list <- paste0(html_wrapper[1], bullets, html_wrapper[2])
  
  return(html_list)
}

