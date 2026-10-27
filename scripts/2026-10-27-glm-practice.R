
melanoma_data <- boot::melanoma

library(tidyverse)
library(finalfit)
library(ggplot2)
library (gtsummary)
library(broom)

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
        "Alive" = "2",
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


# 5 year mortality of melanoma

melanoma_mort_5yr <- melanoma_data_recoded |> 
  mutate(
    mort_5yr = 
      if_else((time/365) < 5 & status == 1, "Yes", "No") |> 
      fct_relevel("No")
  )
  
# Association between tumour ulceration and mort_5yr

melanoma_mort_5yr |> 
  ggplot(mapping = aes(
    x = ulcer_factor,
    fill = ulcer_factor
  )) +
  stat_count() + 
  facet_wrap(~mort_5yr,
              labeller = labeller(mort_5yr = c(
                "Yes" = "Died of melanoma within 5 years",
                "No" = "Did not die of melanoma within 5 years"
              )))

# The patients who died of melanoma within 5 years had proportionally more ulcers than those who did not.
# Ulcers can be a risk factor for death to happen within 5 years

# Fit model

fit1 <- glm(mort_5yr ~ ulcer_factor, data=melanoma_mort_5yr, family = binomial)

tidy(fit1, conf.int = TRUE, exponentiate = TRUE)

or_table<-tbl_regression(
  fit1,
  exponentiate = TRUE,
  label = list(ulcer_factor ~ "Ulcer")) |>  
  modify_caption("**Odds of dying of melanoma within 5 years**") |>
  modify_source_note("Unadjusted logistic regression (n = 205).")
or_table
# OR for those with ulcer is 6.68 [3.18 - 15.2] p-value <0.001. So it seems like it's associated with 5yr mortality