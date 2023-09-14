
library(wasteburningblantyre)
source("data-raw/data_processing.R")

# -------------------------------------------------------------------------

# id cleared

summer_codebook_names_tidy



winter_codebook_names_tidy

# wmagement = managing_waste
# (selected, but needs adjustment)

# lesson: taking questions out is fine, but
# do not change the response values between
# questionnaires for the same question

summer_survey |>
  count_unnest(position)

summer_survey |>
  count_unnest(managing_waste)

winter_survey |>
  count_unnest(wmanagement)

# price = worker_fee
## worker_fee (variable not useful)
## reported in two ways
## informal households keep waste in two bags
## when they are full, someone collects it and disposed in rivers
## formal areas, there is curpside collection, reporting figures per month
## informal per bag collection
## worker_fee NAs unclear

summer_survey |>
  count_unnest(pay_mode)

summer_survey |>
  count_unnest(worker_fee)

winter_survey |>
  count_unnest(price_per)

# religion = religion
# did not have equal response categories in both
# winter has ATR and Other.

summer_survey |>
  count_unnest(religion)

winter_survey |>
  count_unnest(religion)

# occupation = occupation
# we can assume that "casual labour" stands for workers
# 8: casual labour

summer_survey |>
  count_unnest(occupation)

winter_survey |>
  count_unnest(occupation)

summer_survey |>
  count(gen_interviwee, occupation)

summer_survey |>
  count(occupation, managing_waste)

# income = income
# In summer: How much do you earn per month?
# In winter: Household monthly income

summer_survey |>
  count_unnest(income) |>
  arrange(desc(income))

winter_survey |>
  count_unnest(income)


summer_survey |>
  select(id, income, season) |>
  bind_rows(
    winter_survey |>
      select(id, income, season)
  ) |>
  ggplot(aes(x = income, fill = season)) +
  geom_histogram() +
  scale_x_log10(labels = scales::label_log())

# no of males & females
# males = males_no (selected)

# daily_waste = waste_generation

summer_codebook |>
  filter(var_name == "waste_generation") |>
  select(question) |>
  unique() |>
  pull(question)

winter_codebook |>
  filter(var_name == "daily_waste") |>
  select(question) |>
  unique() |>
  pull(question)

# waste_generation - How many 5 Liter bags
# 10 & 12 & 31 are probably data entry mistakes

summer_survey |>
  count_unnest(waste_generation, sort = FALSE) |>
  arrange()

# daily_waste
# extremely high numberss
winter_survey |>
  count_unnest(daily_waste, sort = FALSE)

# wastecat = waste_cat
# summer: multiple choice
# winter: select one
# can't be combined as they are different questions
# additionally remduced the number of response values

summer_codebook |>
  filter(var_name == "waste_cat") |>
  select(question) |>
  unique() |>
  pull(question)

summer_codebook |>
  filter(var_name == "waste_cat")  |>
  select(var_name, value, label)

winter_codebook |>
  filter(var_name == "wastecat") |>
  select(question) |>
  unique() |>
  pull(question)

winter_codebook |>
  filter(var_name == "wastecat")  |>
  select(var_name, value, label)

# disposal
# summer survey does not have that specific question

winter_codebook |>
  filter(var_name == "disposal") |>
  select(question) |>
  unique() |>
  pull(question)

winter_survey |>
  count_unnest(disposal)

summer_codebook_names_tidy |>
  filter(str_detect(question, "disposal"))

# burn_often = burn_often
# has different response categories

summer_survey |>
  count_unnest(burn_often)

summer_codebook |>
  filter(var_name == "burn_often")  |>
  select(var_name, value, label)

winter_survey |>
  count_unnest(burn_often)

winter_codebook |>
  filter(var_name == "burn_often")  |>
  select(var_name, value, label)

# burn_time = burn_time
# actual time (clock) vs category

summer_survey |>
  count_unnest(burn_time)

summer_codebook |>
  filter(var_name == "burn_time")  |>
  select(var_name, value, label)

winter_survey |>
  count_unnest(burn_time)

winter_codebook |>
  filter(var_name == "burn_time")  |>
  select(var_name, value, label)

