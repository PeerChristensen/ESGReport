
source("utils.R")

question_info <- load_question_info()
questions <- question_info$questions
question_types <- question_info$question_types
question_themes <- question_info$question_themes


create_grouped_questions <- function(grp,value) {
  df <- tibble(grp,value)
  out <- split(df$value,a$grp)
  }
create_grouped_questions(question_themes,question_types)
