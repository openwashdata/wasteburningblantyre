
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

# price = worker_fee (selected)

summer_survey |>
  count_unnest(pay_mode)

summer_survey |>
  count_unnest(worker_fee)

winter_survey |>
  count_unnest(price_per)
