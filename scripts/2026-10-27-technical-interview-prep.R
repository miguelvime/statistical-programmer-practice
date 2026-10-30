library(rio)
library(skimr)
library(janitor)
library(lubridate)

raw_cms<-import("formaciones/statistical-programmer/statistical-programmer-practice/data/Timely_and_Effective_Care-State.csv")

glimpse(raw_cms)

# All variables are character, including dates and integuers.

skimr::skim(raw_cms)

raw_cms |> head(10)
# clean names
# Pivot longer test, score.
# Variable types.

clean_names_cms <- raw_cms |> 
  janitor::clean_names()

 
clean_cms<- clean_names_cms |> 
  mutate(
    score = case_when(
      score == "Not Available" ~ NA,
      TRUE ~ as.integer(score),
    )
  )


ggplot(data = clean_names_cms,mapping = aes(
  x = condition,
)) +
  geom_histogram(stat = "count") 

# APRENDIZAJES
# - Manejo de NA y formato d efechas
clean_cms <- raw_cms |>
  clean_names() |>
  mutate(
    score = na_if(score, "Not Available"),
    score = as.numeric(score),
    start_date = mdy(start_date),
    end_date = mdy(end_date)
  )
# - Busca output pequeño: one measure, one scale
ed_time <- clean_cms |>
  filter(measure_id == "OP_18b", !is.na(score))

ggplot(ed_time, aes(x = score, y = reorder(state, score))) +
  geom_col()