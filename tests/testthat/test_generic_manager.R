context("Generic Manager")

test_that("initialization and parameter setting", {
  TEST_DIRECTORY <- test_path("test_results")
  generic_manager <- GenericManager$new(results_dir = TEST_DIRECTORY, attr1 = 22)
  expect_equal(generic_manager$get_attribute("results_dir"), TEST_DIRECTORY)
  expect_equal(generic_manager$get_attribute("attr1"), 22)
  expect_error(generic_manager$sample_data <- "dummy", "Sample data should be a data frame")
  generic_manager$sample_data <- as.matrix(data.frame(a = 1:4, b = 5:8))
  expect_is(generic_manager$sample_data, "data.frame")
  expect_silent(generic_manager$sample_data <- NULL)
  expect_error(
    generic_manager$generators <- "dummy",
    "Generators must be a list of Generator or inherited class objects"
  )
  expect_error(
    generic_manager$generators <- list(gen1 = "dummy", gen2 = 1:5),
    "Generators must be a list of Generator or inherited class objects"
  )
  generic_manager$generators <- list(gen1 = Generator$new(), gen2 = Generator$new())
  expect_named(generic_manager$generators, c("gen1", "gen2"))
  expect_is(generic_manager$generators$gen1, "Generator")
  expect_silent(generic_manager$generators <- NULL)
})

test_that("sample message and filename generation", {
  generic_manager <- GenericManager$new()
  # No sample data or filename attributes
  expect_equal(generic_manager$get_message_sample("testing %s message", 1), "testing sample 1 message")
  expect_equal(generic_manager$get_results_filename(2), "sample_2_results")
  # With sample data and filename attributes
  generic_manager$sample_data <- as.matrix(data.frame(attr1 = 3:4, attr2 = 5:6))
  generic_manager$results_filename_attributes <- "pre"
  expect_equal(generic_manager$get_message_sample("testing %s message", 1), "testing sample 1 message")
  expect_equal(generic_manager$get_results_filename(2), "pre_2_results")
  generic_manager$results_filename_attributes <- c("pre", "attr1")
  expect_equal(generic_manager$get_message_sample("testing %s message", 1), "testing attr1 3 message")
  expect_equal(generic_manager$get_results_filename(2), "pre_attr1_4_results")
  generic_manager$results_filename_attributes <- c("pre", "attr1", "attr2", "post")
  expect_equal(generic_manager$get_message_sample("testing %s message", 1), "testing attr1 3 attr2 5 message")
  expect_equal(generic_manager$get_results_filename(2), "pre_attr1_4_attr2_6_post")
  generic_manager$results_filename_attributes <- c("pre", "post")
  expect_equal(generic_manager$get_message_sample("testing %s message", 1), "testing sample 1 message")
  expect_equal(generic_manager$get_results_filename(2), "pre_2_post")
  generic_manager$results_filename_attributes <- c("attr2", "post")
  expect_equal(generic_manager$get_message_sample("testing %s message", 1), "testing attr2 5 message")
  expect_equal(generic_manager$get_results_filename(2), "attr2_6_post")
  expect_equal(generic_manager$results_ext, ".RData")
})
