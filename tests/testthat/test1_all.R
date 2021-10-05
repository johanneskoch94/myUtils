context("Testing all functions")

test_string <- c("test1_2020-03-12_12.12.05",
                 "../data/test1_2020-03-12_12.12.05",
                 "../data/test1_2020-03-12_12.12.05/fulldata.gdx")

# Test the rm_timestamp function.
test_that("rm_timestamp", {
  my_string <- rm_timestamp(test_string)

  result_string <- c("test1", "../data/test1", "../data/test1/fulldata.gdx")
  expect_identical(my_string, result_string)
})


# Test the get_REMIND_run_names function.
test_that("get_REMIND_run_names", {
  my_string <- get_REMIND_run_names(test_string)

  result_string <- c("test1", "test1", "test1")
  expect_identical(my_string, result_string)
})


# Test the dir_with_timestamp function.
test_that("dir_with_timestamp", {
  sink("/dev/null")
  my_string1 <- dir_with_timestamp(path="../data", remove_timestamp = T)
  my_string2 <- dir_with_timestamp(path="../data", only_most_recent = T)
  my_string3 <- dir_with_timestamp(path="../data",  remove_timestamp = T, only_most_recent = T)
  my_string4 <- dir_with_timestamp(path="../data",  pattern = "test2", full.names = TRUE)
  sink()

  result_string1 <- c("test1", "test1", "test2", "test3")
  result_string2 <- c("test1_2020-04-12_12.12.05", "test2", "test3_2020-03-12_12.12.05")
  result_string3 <- c("test1", "test2", "test3")
  result_string4 <- dir("../data", pattern = "test2", full.names = T)
  expect_identical(my_string1, result_string1)
  expect_identical(my_string2, result_string2)
  expect_identical(my_string3, result_string3)
  expect_identical(my_string4, result_string4)
})
