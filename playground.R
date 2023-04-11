
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



indicator_info <- load_indicator_info()
indicator_info_df <- indicator_info %>% as_tibble() %>%
  rename(indicatorID = indicator_ids) %>%
  mutate(indicatorID = as.character(indicatorID)) %>%
  rename(indicatorText = indicator_texts,
         indicatorUnit = indicator_units,
         indicatorTheme = indicator_themes
         )

inputs <-  read_csv("input_data2.csv") %>%
  rename(input_name = ind, input_value=values) %>%
  filter(input_value != "", 
         !is.na(input_value),
         input_value != "TRUE",
         input_value != "FALSE",
         !grepl('select|tabs|sidebar|accordion', input_name)
  ) %>%
  separate(input_name,sep = "_", into="indicatorID", remove = FALSE) %>%
  left_join(indicator_info_df, by = "indicatorID", keep=T) %>%
  as_tibble()

write_csv(inputs, "input_data3.csv")




df <- read_csv("input_data.csv") %>%
  mutate(indicatorTheme = ifelse(is.na(indicatorTheme),input_name,indicatorTheme),
         indicatorTheme = str_replace(indicatorTheme, "[a-z+_]*", "")) %>%
  select(-indicatorID.y) %>%
  rename(indicatorID = indicatorID.x)

themes_list <- df %>% pull(indicatorTheme) %>% unique()

initiatives_df <- df %>%
  filter(is.na(indicatorUnit))

df <- df %>%
  select(input_name,Indikator = indicatorText, 
         Enhed = indicatorUnit, 
         Svar = input_value, indicatorTheme) %>%
  drop_na()

df <- df %>% mutate(year = str_sub(input_name,-4,-1)) %>%
  arrange(desc(year)) %>%
  pivot_wider(names_from = year, 
              values_from = Svar, 
              id_cols = c(Indikator,Enhed, indicatorTheme),
              names_sort=F)


