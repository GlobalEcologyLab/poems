context("Model Simulator (class)")

test_that("initialization and parameter setting", {
  TEST_DIRECTORY <- test_path("test_inputs")
  # Default initialization
  model_simulator <- ModelSimulator$new(sample_id = 11, attach1 = 33, attach2 = 55)
  expect_null(model_simulator$simulation_function)
  expect_equal(model_simulator$sample_id, 11)
  expect_equal(model_simulator$attached, list(attach1 = 33, attach2 = 55))
  expect_equal(model_simulator$get_attribute("sample_id"), 11)
  expect_equal(model_simulator$get_attribute("attach1"), 33)
  # Set the simulation function
  expect_error(
    model_simulator$simulation_function <- file.path(TEST_DIRECTORY, "test_not_a_function.R"),
    paste("Could not assign function", file.path(test_path("test_inputs"), "test_not_a_function.R"))
  )
  expect_error(
    model_simulator$simulation_function <- file.path(TEST_DIRECTORY, "test_invalid_code.R"),
    "Error loading function from file"
  )
  expect_error(
    model_simulator$simulation_function <- file.path(TEST_DIRECTORY, "no_such_file.R"),
    paste("Could not assign function", file.path(test_path("test_inputs"), "no_such_file.R"))
  )
  expect_silent(model_simulator$simulation_function <- file.path(TEST_DIRECTORY, "test_function_2.R"))
  expect_error(model_simulator$simulation_function <- "dummy", "Could not assign function dummy")
  expect_silent(model_simulator$simulation_function <- "max")
  expect_silent(model_simulator$simulation_function <- function(x) x)
  # Set the simulation model
  expect_error(
    model_simulator$simulation_model <- "dummy",
    "Model must be a SimulationModel or inherited class object"
  )
  expect_silent(model_simulator$simulation_model <- SimulationModel$new())
})

test_that("run the simulator", {
  model_simulator <- ModelSimulator$new()
  coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))
  region <- Region$new(coordinates = coordinates, use_raster = FALSE)
  # Missing function and model
  model_simulator$simulation_function <- NULL
  expect_error(model_simulator$run(), "The simulation function needs to be set before the simulation can be run")
  model_simulator$simulation_function <- "max"
  expect_error(model_simulator$run(), "The simulation model needs to be set before the simulation can be run")
  model_simulator$simulation_model <- SimulationModel$new()
  # Incomplete model
  expect_equal(
    model_simulator$run(),
    list(
      successful = FALSE,
      message = "Model %s attributes are incomplete: region, time_steps, results_selection"
    )
  )
  model_simulator$simulation_model$set_attributes(region = region, time_steps = 10, results_selection = "any")
  # Error in simulation
  expect_equal(
    model_simulator$run()[c("successful", "message")],
    list(
      successful = FALSE,
      message = "Model %s simulation ran unsuccessfully with errors"
    )
  )
  expect_match(model_simulator$run()$errors, "Error")
  # Warning in simulation
  model_simulator$simulation_function <- function(sim_model) {
    warning("test")
    sim_model$time_steps
  }
  expect_equal(
    model_simulator$run()[c("successful", "message")],
    list(
      successful = TRUE,
      message = "Model %s simulation ran successfully with warnings"
    )
  )
  expect_match(model_simulator$run()$warnings, "Warning")
  # Ok simulation
  model_simulator$simulation_function <- function(sim_model) {
    sim_model$time_steps * 5
  }
  expect_equal(
    model_simulator$run(),
    list(
      successful = TRUE,
      message = "Model %s simulation ran successfully"
    )
  )
  expect_equal(model_simulator$results, 50)
})
