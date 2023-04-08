
library(tidyverse)

questions_df <- read_csv("questions.csv") %>% select(-choice)
questions_ids <- questions_df %>% pull(questionID)

questions_df %>% 
  slice(1) %>% 
  mutate(k=list(k)) %>%
  write_json(glue::glue("questions/{questionID}.json"))

a <- read_json("questions/q1.json") 
glimpse(a)
a
a$k

id = "q3"
text = "blabla"
type="numeric"
choices = c("a", "b")
tibble(id=id,text=text,type=type, choices=list(choices)) %>%
  toJSON(pretty = T)


questions_df <- read_csv("questions.csv")
choices <- list("","",c("Jylland","Fyn", "Sjælland"),"","")
questions_df$choices <- choices

for (row in 1:nrow(questions_df)) {
  data = questions_df %>% slice(row)
  question_id <- data %>% pull(questionID)
  write_json(data, glue::glue("questions/{question_id}.json"))
}

indicator_files <- list.files("indicators", full.names = T)
indicators <- map(indicator_files,jsonlite::fromJSON)
indicators <-map(indicators, ~.x %>% 
                 mutate(indicatorID = as.character(indicatorID)))


for (i in 1:length(indicators)) {
  
  indicators[[i]] %>%write_json(glue::glue("indicators2/{i}.json"))
  }


# dataframe for handling output
indicator_files <- list.files("indicators", full.names = T)
indicators <- map(indicator_files,jsonlite::fromJSON)
indicators <- map_df(indicators, 
                     select,indicatorID, indicatorText, indicatorTheme) %>%
  mutate(indicatorID = as.character(indicatorID))

mylist <- list("4_2023" = 5,"4_2022" = 5, "33_2002"=3)
df <- mylist %>% 
  stack() %>% 
  rename(input_name = ind, input_value=values) %>%
  mutate(indicatorID = str_split(input_name, "_", simplify = F, n=1)) %>%
  left_join(indicators, by = "indicatorID") %>%
  as_tibble()

df = data.frame( id = I(c("abcd_123ABC","abc_5234NHYK")), x = c(1.0,2.0) )

## separate knowing you will have three components
df %>% separate(id, sep = "_", into="w", remove = FALSE) 
