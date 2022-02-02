library(dplyr)
library(maps)

setwd(system.file("extdata", package = "far2"))

test_that("fars_read() works correctly", {
  expect_is(fars_read("accident_2015.csv"), "tbl_df")
})
