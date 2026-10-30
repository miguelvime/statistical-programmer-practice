
# ============================
#        DEDUPLICATION
# ============================

# Source: https://www.epirhandbook.com/en/new_pages/deduplication.html cap 15 De-duplication

library(janitor)
library(dplyr)

obs <- data.frame(
  recordID  = c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),
  personID  = c(1,1,2,2,3,2,4,5,6,7,2,1,3,3,4,5,5,7,8),
  name      = c("adam", "adam", "amrish", "amrish", "mariah", "amrish", "nikhil", "brian", "smita", "raquel", "amrish",
                "adam", "mariah", "mariah", "nikhil", "brian", "brian", "raquel", "natalie"),
  date      = c("1/1/2020", "1/1/2020", "2/1/2020", "2/1/2020", "5/1/2020", "5/1/2020", "5/1/2020", "5/1/2020", "5/1/2020","5/1/2020", "2/1/2020",
                "5/1/2020", "6/1/2020", "6/1/2020", "6/1/2020", "6/1/2020", "7/1/2020", "7/1/2020", "7/1/2020"),
  time      = c("09:00", "09:00", "14:20", "14:20", "12:00", "16:10", "13:01", "15:20", "14:20", "12:30", "10:24",
                "09:40", "07:25", "08:32", "15:36", "15:31", "07:59", "11:13", "17:12"),
  encounter = c(1,1,1,1,1,3,1,1,1,1,2,
                2,2,3,2,2,3,2,1),
  purpose   = c("contact", "contact", "contact", "contact", "case", "case", "contact", "contact", "contact", "contact", "contact",
                "case", "contact", "contact", "contact", "contact", "case", "contact", "case"),
  symptoms_ever = c(NA, NA, "No", "No", "No", "Yes", "Yes", "No", "Yes", NA, "Yes",
                    "No", "No", "No", "Yes", "Yes", "No","No", "No")) %>% 
  mutate(date = as.Date(date, format = "%d/%m/%Y"))

obs |> tabyl(name,purpose)
obs |> get_dupes()
obs |> get_dupes(-recordID) 
obs |> get_dupes(name,purpose)

obs_dedup <- obs |> 
  distinct(across(-recordID))
obs_dedup

stopifnot(nrow(obs_dedup) == 17)

x <- c(1, 1, 2, NA, NA, 4, 5, 4, 4, 1, 2)
duplicated(x)
x[duplicated(x)]

unique(x)
unique (x) == x[duplicated(x)]

obs %>% 
  group_by(name) %>%       
  slice_max(date,          
            n = 1,         
            with_ties = F) # Ante empate se queda solo con 1. Si fuera TRUE quedaría con todas

# Learned: - data |> janitor::get_dupes(cols); data |> janitor::get_dupes(-cols) 
         # - distinct(across());distinct(across(-col));
         # - data |> group_by(col) |>  slice_max(col2,n=int,with_ties = F)


# ============================
#           JOINS
# ============================

# Source: https://www.epirhandbook.com/en/new_pages/deduplication.html cap 14 Joining data

library(readxl)
library(dplyr)

linelist_mini <- linelist_cleaned |> 
  select(case_id, date_onset, hospital) |> 
  head(10)

# Make the hospital information data frame
hosp_info = data.frame(
  hosp_name     = c("central hospital", "military", "military", "port", "St. Mark's", "ignace", "sisters"),
  catchment_pop = c(1950280, 40500, 10000, 50280, 12000, 5000, 4200),
  level         = c("Tertiary", "Secondary", "Primary", "Secondary", "Secondary", "Primary", "Primary")
)

# Exercise, clean df and join them

# Tasks:
  # Different names
  # hospital is the joincol

hosp_info_clean<-hosp_info |> 
  rename(hospital = hosp_name) |> 
  mutate(
    hospital = case_when(
      hospital == "central hospital" ~ "Other", # losing info!
      hospital == "military" ~ "Military Hospital",
      hospital == "port" ~ "Port Hospital",
      hospital == "St. Mark's" ~ "St. Mark's Maternity Hospital (SMMH)",
      hospital == "ignace" ~ "Other", # losing info!
      hospital == "sisters"~ "Other"  # losing info!
    )
  )

unique(linelist_mini$hospital)
unique(hosp_info_clean$hospital)

linelist_joined <- linelist_mini |> 
  left_join(hosp_info_clean, relationship = "many-to-many")

# Now correction with the book: 
  # - Don't lose info! It's better TRUE ~ hosp_name
  # - Use str_to_upper() to standarize characters and strings if necessary.
  # - The argument by= c(col1,col2) other option. With this there's no need of changing column names

hosp_info_clean <- hosp_info |> 
  mutate(
    hosp_name = case_when(
      hosp_name == "military"          ~ "Military Hospital",
      hosp_name == "port"              ~ "Port Hospital",
      hosp_name == "St. Mark's"        ~ "St. Mark's Maternity Hospital (SMMH)",
      hosp_name == "central hospital"  ~ "Central Hospital",
      TRUE                             ~ hosp_name
      )
    )

# ------------------------------
#        PIVOTING DATA
# ------------------------------

#  Source: https://www.epirhandbook.com/en/new_pages/deduplication.html cap 12 Pivoting data

# Tidy Standards -> column as variables
library(dplyr)
library(tidyr)

# To identify columns for pivoting, see similarities between them like malaria_rdt_{differences} or ~year1 ; ~year2; ~year3

df_long <- malaria_facility_count_data |> 
  pivot_longer(
    cols = c(`malaria_rdt_0-4`, `malaria_rdt_5-14`, `malaria_rdt_15`, `malaria_tot`)
  )

df_long

# For pivot wider can use count(), and with column n
table_wide <- 
  df_wide %>% 
  pivot_wider(
    id_cols = age_cat,
    names_from = gender,
    values_from = n
  )
