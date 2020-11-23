context("Simulator Reference")

test_that("attributes", {
  test_class <- SimulatorReference$new()
  test_class$attached$attr1 <- "test1"
  test_class$attached$attr2 <- c(11, 22, 33)
  test_class$results$attr1 <- "test2"
  test_class$results$attr2 <- list(a = 1, b = 2)
  expect_equal(test_class$attached, list(attr1 = "test1", attr2 = c(11, 22, 33)))
  expect_equal(test_class$results, list(attr1 = "test2", attr2 = list(a = 1, b = 2)))
})
