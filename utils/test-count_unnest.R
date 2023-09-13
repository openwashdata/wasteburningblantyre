
# -------------------------------------------------------------------------




# -------------------------------------------------------------------------

library(tidyverse)
library(testthat)
source(here::here("utils/count_unnest.R"))

# -------------------------------------------------------------------------

test_that("count_unnest works with list columns", {
  df <- tibble::tibble(
    group = c("A", "A", "B", "B", "C"),
    values = list(1:2, 3:4, 5:6, 7:8, 9:10)
  )
  result <- count_unnest(df, values)
  
  expect_equal(nrow(result), 10)
  expect_equal(sum(result$n), 10)
})

test_that("count_unnest works with non-list columns", {
  df <- tibble::tibble(
    group = c("A", "A", "B", "B", "C"),
    values = c(1, 2, 3, 4, 5)
  )
  result <- count_unnest(df, values)
  
  expect_equal(nrow(result), 5)
  expect_equal(sum(result$n), 5)
})


# -------------------------------------------------------------------------

testthat::test_file("utils/test-count_unnest.R")

