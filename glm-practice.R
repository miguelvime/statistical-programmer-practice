
melanoma_data <- boot::melanoma

library(tidyverse)
library(finalfit)
library(ggplot2)

melanoma_data |> glimpse()
melanoma_data |> ff_glimpse()

melanoma_data_recoded <- melanoma_data |>
  mutate(
    sex_factor = factor(sex) |> 
      fct_recode(
        "Female" = "0",
        "Male" = "1"
      ),
    ulcer_factor = factor(ulcer) |> 
      fct_recode(
        "Present" = "1",
        "Absent" = "0"
      ),
    status_factor = factor (status) |> 
      fct_recode(
        "Died melanoma" = "1",
        "Alieve" = "2",
        "Died - other" = "3") |> 
      fct_relevel("Alive"),
    t_stage_factor = thickness |>
      cut(
        breaks = c(0, 1.0, 2.0, 4.0, max(thickness,na.rm = TRUE)),
        include.lowest = TRUE
      ) |>
      fct_recode(
        "T1" = "[0,1]",
        "T2" = "(1,2]",
        "T3" = "(2,4]",
        "T4" = "(4,17.4]"
      ) 
  )

melanoma_data_recoded |> 
  ggplot(mapping = aes(
    x = time/365
  )) +
  geom_histogram() +
  facet_wrap(~status_factor)
  
