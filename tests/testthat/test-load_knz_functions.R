# Load the testthat package
library(testthat)

# Test for `load_knz_ppt`
test_that("load_knz_ppt returns a tibble", {
  skip_on_cran() # Skip on CRAN as it depends on external resources

  result <- load_knz_ppt()
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0) # Ensure the tibble is not empty
  expect_gt(ncol(result), 0) # Ensure the tibble has columns
})

test_that("load_knz_ppt handles errors gracefully", {
  mockery::stub(load_knz_ppt, "EDIutils::list_data_package_revisions", function(...) stop("Connection error"))
  expect_error(load_knz_ppt(), "Connection error")
})

# Test for `load_knz_weather`
test_that("load_knz_weather returns a tibble", {
  skip_on_cran()
  result <- load_knz_weather()
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_gt(ncol(result), 0)
})

test_that("load_knz_weather handles errors gracefully", {
  mockery::stub(load_knz_weather, "EDIutils::list_data_package_revisions", function(...) stop("Connection error"))
  expect_error(load_knz_weather(), "Connection error")
})

# Test for `load_knz_weirs`
test_that("load_knz_weirs returns a tibble", {
  skip("Takes a long time to run, skipping.")
  skip_on_cran()
  result <- load_knz_weirs()
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_gt(ncol(result), 0)
})

test_that("load_knz_weirs combines data correctly", {
  skip_on_cran()
  result <- load_knz_weirs()
  # Ensure combined data includes expected columns
  expected_cols <- c("DataCode", "RecType", "RecYear", "RecMonth", "RecDay", "Watershed",
                     "DayofYear", "RecHour", "Discharge", "Sheight", "CorrectedSheight",
                     "Height", "LogFlag", "QualFlag")
  expect_true(all(expected_cols %in% colnames(result)))
})

test_that("load_knz_weirs handles errors gracefully", {
  mockery::stub(load_knz_weirs, "EDIutils::list_data_package_revisions", function(...) stop("Connection error"))
  expect_error(load_knz_weirs(), "Connection error")
})
