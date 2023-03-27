
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


questions_df <- read_csv("questions.csv") %>% select(-choice)
choices <- list("","",c("Jylland","Fyn", "Sjælland"),"","")
questions_df$choices <- choices

for (row in 1:nrow(questions_df)) {
  data = questions_df %>% slice(row)
  question_id <- data %>% pull(questionID)
  write_json(data, glue::glue("questions/{question_id}.json"))
}

question_files <- list.files("questions", full.names = T)
questions <- map(question_files,fromJSON) %>%
  map(pull,questionText) %>% 
  unlist()


