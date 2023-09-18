# description -------------------------------------------------------------

# packages ----------------------------------------------------------------

library(tidyverse)

# functions ---------------------------------------------------------------

source("utils/get_variable_info.R")
source("utils/count_unnest.R")

is_binary <- function(x) {
  all(x %in% c(0, 1))
}

# read data ---------------------------------------------------------------

## summer data -------

survey_summer <- read_delim("data-raw/Extent_of_open_burning_questionnaire_final_v2_-_all_versions_-_False_-_2023-09-13-13-10-26.csv",
                            delim = ";")

summer_codebook_names <- openxlsx::read.xlsx("data-raw/aBe2R3mpMFc8xxcjmqo9WU_summer_codebook.xlsx",
                                             sheet = 1) |>
  as_tibble()

summer_codebook_choices <- openxlsx::read.xlsx("data-raw/aBe2R3mpMFc8xxcjmqo9WU_summer_codebook.xlsx",
                                               sheet = 2) |>
  as_tibble()

characterisation_summer <- read_delim("data-raw/2022-11-30_waste_composition_summer.csv",
                                      delim = ",")

dates_summer <- read_delim("data-raw/collection_date_summer.csv",
                           delim = ",")


## winter data -------

survey_winter <- read_delim("data-raw/Open_Waste__Burning-Winter_Study_-_all_versions_-_False_-_2023-09-13-13-07-27.csv",
                            delim= ";")


winter_codebook_names <- openxlsx::read.xlsx("data-raw/aGbAArFX6NMPdokPWTuF6W_winter_codebook.xlsx",
                                             sheet = 1) |>
  as_tibble()

winter_codebook_choices <- openxlsx::read.xlsx("data-raw/aGbAArFX6NMPdokPWTuF6W_winter_codebook.xlsx",
                                               sheet = 2) |>
  as_tibble()

characterisation_winter <- read_delim("data-raw/waste_composition_winter.csv",
                                      delim = ",")
dates_winter <- read_delim("data-raw/collection_date_winter.csv",
                           delim = ",")

# data manipulation survey-----------------------------------------------------

## winter data -----------

### winter codebook --------

winter_codebook_names_tidy <- winter_codebook_names |>
  select(question = label, var_name = name, type) |>
  separate(col = "type", into = c("col1", "col2"), sep = " ") |>
  mutate(col2 = case_when(
    is.na(col2) == TRUE ~ col1,
    TRUE ~ col2
  )) |>
  rename(
    question_type = col1,
    list_name = col2
  ) |>
  # sections can be removed as they only indicate the start of a group
  filter(!question_type %in% c("begin_group",
                               "end_group",
                               "begin_repeat",
                               "end_repeat",
                               "calculate",
                               "note")) |>
  filter(!var_name %in% c("female_age", "male_age"))

# join survey questions with variable names and list names

winter_codebook <- winter_codebook_names_tidy |>
  left_join(winter_codebook_choices,
            relationship = "many-to-many") |>
  rename(value = name)

winter_codebook_small <- winter_codebook |>
  select(var_name, list_name, value, label)

### winter survey -------------


survey_winter_labelled <- survey_winter |>
  select(id = `_index`,
         today,
         burn_time,
         cooking_specify,
         interviewer, # Interviewer
         locat,
         gender, # gen_interviwee
         wmanagement,
         price,
         price_per,
         religion,
         occupation,
         income,
         males,
         females,
         daily_waste,
         wastecat,
         contains("disposal"), -disposal,
         burn_often,
         contains("why_burn"), -why_burn,
         cooking,
         cook_place,
         times_cook,
         vehicles,
         ti,
         # repeat_time2,
         timee,
         # repeat_time3,
         waste) |>
  pivot_longer(cols = !id:cooking_specify) |>
  # separates responses for multiple choice questions
  separate(col = name, into = c("var_name", "name"), sep = "/") |>
  # removes responses that are 0, as it was not selected in multiple choice
  # but keeps responses were 0 is "no"
  filter((var_name %in% c('gender', 'waste')) | (value != 0)) |>
  filter(!is.na(value)) |>
  mutate(value = case_when(
    is.na(name) == FALSE ~ as.numeric(name),
    TRUE ~ value
  )) |>
  mutate(value = as.character(value)) |>
  select(-name) |>
  left_join(winter_codebook_small) |>
  mutate(label = case_when(
    is.na(label) == TRUE ~ value,
    TRUE ~ label
  )) |>
  select(-list_name, -value) |>
  pivot_wider(names_from = var_name,
              values_from = label,
              values_fn = list) |>
  arrange(id)

## identify column names that can be unnested because each vector is length 1
## or 0

var_names_unnest_winter <- survey_winter_labelled %>%
  reframe(across(where(is.list), ~map_int(., length))) %>%
  select(where(is_binary)) |>
  names()

## final data manipulation

winter_survey <- survey_winter_labelled |>
  # turn income column into numeric vectors
  mutate(income = map(income, as.numeric)) |>
  mutate(females = map(females, as.numeric)) |>
  mutate(males = map(males, as.numeric)) |>
  mutate(daily_waste = map(daily_waste, as.numeric)) |>
  mutate(price = map(price, as.numeric)) |>
  mutate(settlement_type = case_when(
    locat %in% c("Sunnyside", "Nyambadwe", "Namiwawa", "Naperi") ~ "formal",
    locat %in% c("Ndirande", "Kachere", "Chirimba", "Bangwe") ~ "informal"
  )) |>
  mutate(burners = case_when(
    map_lgl(disposal, ~"Burning" %in% .) ~ "Yes",
    TRUE ~ "No"
  )) |>
  unnest(cols = all_of(var_names_unnest_winter), keep_empty = TRUE) |>
  mutate(season = "winter")

#unnest(cols = all_of(var_names_unnest_winter))
## summer data --------------

### summer codebook --------------

summer_codebook_names_tidy <- summer_codebook_names |>
  select(question = label, var_name = name, type) |>
  separate(col = "type", into = c("col1", "col2"), sep = " ") |>
  mutate(col2 = case_when(
    is.na(col2) == TRUE ~ col1,
    TRUE ~ col2
  )) |>
  rename(
    question_type = col1,
    list_name = col2
  ) |>
  # sections can be removed as they only indicate the start of a group
  filter(!question_type %in% c("begin_group",
                               "end_group",
                               "begin_repeat",
                               "end_repeat",
                               "calculate",
                               "note")) |>
  # removed, as they are stored in separate spreadsheet and not relevant
  # for full dataset. could be published separately to learn more about
  # the ages of people in the household.
  # b1: has not data (C1.if a burner who doesn’t sort, where are the mixed waste burned?)
  filter(!var_name %in% c("age0", "age1", "b1", "x1"))  |>
  # remove Section E_Perceptions_of_waste_burning from data entirely
  # data isn't used in analysis
  filter(!var_name %in% c("harzard",
                          "ppe",
                          "ppe_list",
                          "ppe_used",
                          "health_effect",
                          "effect_name",
                          "why_effect1",
                          "burning_ban",
                          "enforcer",
                          "ban",
                          "municipal_vs_household")) |> #
  # remove Section: Environmental_aspects
  filter(!var_name %in% c("rank_policies", "rank"))

# join survey questions with variable names and list names

summer_codebook <- summer_codebook_names_tidy |>
  left_join(summer_codebook_choices,
            relationship = "many-to-many") |>
  rename(value = name)

summer_codebook_small <- summer_codebook |>
  select(var_name, list_name, value, label)

### summer survey ----------------

survey_summer_labelled

survey_summer |>
  select(id = `_index`,
         today,
         Interviewer,
         locat,
         burners,
         gen_interviwee,
         position,
         ownership,
         managing_waste,
         worker_fee,
         pay_mode,
         edu_level,
         ethinicity,
         religion,
         married,
         occupation,
         income,
         males_no,
         females_no,
         waste_generation,
         contains("waste_cat"), -waste_cat,
         sort_org_inorg,
         contains("c1/"), -c1,
         contains("d1/"), -d1,
         contains("remaining"), -remaining_mixed, -remaining_mixed_001, -remaining_mixed_002,
         contains("ccc/"), -ccc,
         contains("e1/"), -e1,
         contains("g1/"), -g1,
         contains("w1/"), -w1,
         burn_often,
         burn_time,
         burn_week,
         burn_often_001,
         burn_time_001,
         burn_week_001,
         container,
         distance,
         skips_often,
         contains("skips_often_why/"), -skips_often_why,
         Kerb,
         col,
         empty_pay_cost,
         kerb_sometimes_why,
         agreement) |>
  pivot_longer(cols = !id:today) |>
  # separates responses for multiple choice questions
  separate(col = name, into = c("var_name", "name"), sep = "/") |>
  # removes responses that are 0, as it was not selected in multiple choice
  filter((var_name %in% c('gen_interviwee',
                          'burners',
                          'ownership',
                          'sort_org_inorg',
                          'container',
                          'Kerb',
                          'col',
                          'agreement')) | (value != 0)) |>
  filter(!is.na(value)) |>
  mutate(value = case_when(
    is.na(name) == FALSE ~ as.numeric(name),
    TRUE ~ value
  )) |>
  mutate(value = as.character(value)) |>
  select(-name) |>
  left_join(summer_codebook_small) |>
  mutate(label = case_when(
    is.na(label) == TRUE ~ value,
    TRUE ~ label
  )) |>
  select(-list_name, -value) |>
  pivot_wider(names_from = var_name,
              values_from = label,
              values_fn = list)

## identify column names that can be unnested because each vector is length 1
## or 0

var_names_unnest_summer <- survey_summer_labelled %>%
  reframe(across(where(is.list), ~map_int(., length))) %>%
  select(where(is_binary)) |>
  names()

## final data manipulation

summer_survey <- survey_summer_labelled |>
  # fix issue with ccc variable
  mutate(ccc = map(ccc, ~replace(., . == "Rubish pit", "Rubbish pit"))) |>
  # turn income column into numeric vectors
  mutate(income = map(income, as.numeric)) |>
  mutate(females_no = map(females_no, as.numeric)) |>
  mutate(males_no = map(males_no, as.numeric)) |>
  mutate(waste_generation = map(waste_generation, as.numeric)) |>
  mutate(worker_fee = map(worker_fee, as.numeric)) |>
  mutate(settlement_type = case_when(
    locat %in% c("Sunnyside", "Nyambadwe", "Namiwawa", "Naperi") ~ "formal",
    locat %in% c("Ndirande", "Kachere", "Chirimba", "Bangwe") ~ "informal"
  )) |>
  unnest(cols = all_of(var_names_unnest_summer), keep_empty = TRUE) |>
  mutate(season = "summer")


# data manipulation characterisation --------------------------------------

## winter data ---------------

winter_characterisation <- characterisation_winter |>
  arrange(hh_identifier) |>
  rename(id = hh_identifier) |>
  left_join(dates_winter) |>
  mutate(season = "winter") |>
  relocate(date_collect, .before = today) |>
  rename(characterisation_date = today)

# export data -------------------------------------------------------------

summer_characterisation <- characterisation_summer |>
  arrange(hh_identifier) |>
  rename(id = hh_identifier) |>
  left_join(dates_summer) |>
  mutate(season = "summer") |>
  relocate(date_collect, .before = characterisation_date) |>
  filter(!is.na(id))


# combine all data into one resource --------------------------------------

winter_survey_join <- winter_survey |>
  select(id,
         season,
         settlement_name = locat,
         settlement_type,
         date_survey = today,
         #wmanagement,
         occupation,
         males,
         females,
         daily_waste,
         burn_often,
         # burn_time,
         # cooking, cooking_specify, cook_place, times_cook, vehicles, ti,
         waste
  )


winter_characterisation_join <-  winter_characterisation |>
  mutate(settlement_type = case_when(
    settlement_name %in% c("Sunnyside", "Nyambadwe", "Namiwawa", "Naperi") ~ "formal",
    settlement_name %in% c("Ndirande", "Kachere", "Chirimba", "Bangwe") ~ "informal"
  )) |>
  select(id,
         date_collect,
         date_characterisation = characterisation_date,
         settlement_name,
         settlement_type,
         transparent_bag,
         wood_leaves,
         cardboard_paper,
         plastic_bottles,
         plastic_bags,
         other_plastics,
         clothes_textiles,
         rubber,
         others
  ) |>
  filter(!id %in% c(219, 226, 229))

winter_survey |>
  left_join(winter_characterisation_join) |>
  select(id, waste, date_characterisation) |>
  filter(waste == "No") |>
  filter(!is.na(date_characterisation))

winter_survey_bind <- winter_survey_join |>
  left_join(winter_characterisation_join)


winter_characterisation |>
  count(id, sort = TRUE)

summer_characterisation_join <- summer_characterisation |>
  mutate(settlement_type = case_when(
    settlement_name %in% c("Sunnyside", "Nyambadwe", "Namiwawa", "Naperi") ~ "formal",
    settlement_name %in% c("Ndirande", "Kachere", "Chirimba", "Bangwe") ~ "informal"
  )) |>
  select(id,
         season,
         date_collect,
         date_characterisation = characterisation_date,
         settlement_name,
         settlement_type,
         transparent_bag,
         wood_leaves,
         cardboard_paper,
         plastic_bottles,
         plastic_bags,
         other_plastics,
         clothes_textiles,
         rubber,
         others) |>
  mutate(plastic_bags = case_when(
    plastic_bags == "p.248" ~ "0.248",
    TRUE ~ plastic_bags)) |>
  mutate(plastic_bags = as.numeric(plastic_bags))

summer_survey_join <- summer_survey |>
  select(id,
         season,
         settlement_name = locat,
         settlement_type,
         date_survey = today,
         #wmanagement = managing_waste, # needs adaptation
         occupation,
         males = males_no,
         females = females_no,
         daily_waste = waste_generation,
         burn_often, # need adaptation
         # burn_time
         waste = agreement
  )

summer_survey_join |>
  left_join(summer_characterisation_join) |>
  select(id, waste, date_characterisation) |>
  filter(waste == "No") |>
  filter(!is.na(date_characterisation))



summer_survery_bind <- summer_survey_join |>
  left_join(summer_characterisation_join)


wasteburningblantyre <- summer_survery_bind |>
  bind_rows(winter_survey_bind) |>
  mutate(males = case_when(
    is.na(males) == TRUE ~ 0,
    TRUE ~ males
  )) |>
  mutate(females = case_when(
    is.na(females) == TRUE ~ 0,
    TRUE ~ females
  )) |>
  # select(-waste) |>
  relocate(c(date_collect, date_characterisation), .after = date_survey)


## code to prepare `DATASET` dataset goes here

usethis::use_data(summer_survey,
                  winter_survey,
                  summer_characterisation,
                  winter_characterisation,
                  wasteburningblantyre,
                  summer_codebook,
                  summer_codebook_names_tidy,
                  winter_codebook,
                  winter_codebook_names_tidy,
                  overwrite = TRUE)




# additional processing ---------------------------------------------------

# Specify values for directory and file_name

directories <- c(rep("data/", 5))

file_names <- c("summer_survey.rda",
                "winter_survey.rda",
                "summer_characterisation.rda",
                "winter_characterisation.rda",
                "wasteburningblantyre.rda")

dictionary <- get_variable_info(data = list(summer_survey,
                                            winter_survey,
                                            summer_characterisation,
                                            winter_characterisation,
                                            wasteburningblantyre),
                                directory = directories,
                                file_name = file_names)
dictionary |>
  write_csv("data-raw/dictionary.csv")

dictionary |>
  openxlsx::write.xlsx("data-raw/dictionary.xlsx")

## --------- cffr

library(cffr)

packageVersion("cffr")

# Hard code doi
# doi <- "10.5281/zenodo.6470427"

# creates CFF with all author roles
mod_cff <- cff_create("DESCRIPTION",
                      dependencies = FALSE,
                      keys = list(#"doi" = doi,
                        "date-released" = Sys.Date()))

# writes the CFF file
cff_write(mod_cff)

# Now write a CITATION file from the CITATION.cff file
# Use inst/CITATION instead (the default if not provided)
path_cit <- file.path("inst/CITATION")

write_citation("CITATION.cff", file = path_cit)



