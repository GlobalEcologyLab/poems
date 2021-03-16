context("Simulation Manager")

test_that("initialization and parameter setting", {
  # Default initialization
  sim_manager <- SimulationManager$new()
  expect_is(sim_manager$model_simulator, "ModelSimulator")
  expect_null(sim_manager$model_simulator$simulation_function)
  sim_manager$model_template <- PopulationModel$new()
  expect_true(is.function(sim_manager$model_simulator$simulation_function))
  sim_manager <- SimulationManager$new(model_template = PopulationModel$new())
  expect_true(is.function(sim_manager$model_simulator$simulation_function))
  # Invalid attributes
  expect_error(sim_manager$model_template <- "dummy",
               "Model template must be a SimulationModel or inherited class object")
  expect_error(sim_manager$nested_model <- "dummy",
               "Nested model must be a SimulationModel or inherited class object")
  expect_error(sim_manager$model_simulator <- "dummy",
               "Model simulator must be a ModelSimulator or inherited class object")
  expect_silent(sim_manager$model_template <- NULL)
  expect_silent(sim_manager$nested_model <- NULL)
  expect_silent(sim_manager$model_simulator <- NULL)
})

test_that("attempt run with incomplete attributes", {
  TEST_DIRECTORY <- test_path("test_results")
  sim_manager <- SimulationManager$new()
  # No model template and samples
  sim_manager$model_simulator <- NULL
  expect_error(sim_manager$run(), "No model samples to run")
  sim_manager$model_template <- SimulationModel$new(model_attributes = c("time_steps", "attr1", "attr2"))
  sim_manager$sample_data <- data.frame() # empty
  expect_error(sim_manager$run(), "No model samples to run")
  sim_manager$sample_data <- data.frame(attr1 = 3:4, attr2 = 5:6)
  # No model simulator
  expect_error(sim_manager$run(), "The model simulator has not been set")
  sim_manager$model_simulator <- ModelSimulator$new()
  sim_manager$model_simulator$simulation_function <- NULL
  expect_error(sim_manager$run(), "The model simulator function has not been set")
  sim_manager$model_simulator$simulation_function <- "max"
  # No results output directory
  expect_error(sim_manager$run(), "No output directory set for results")
  sim_manager$results_dir <- "invalid_dir"
  expect_error(sim_manager$run(), "Could not find results directory invalid_dir")
  sim_manager$results_dir <- TEST_DIRECTORY
  # With incomplete model
  expect_null(sim_manager$nested_model)
  generator <- Generator$new(generative_requirements = list(attr3 = "function"),
                             inputs = c("attr2"), outputs = c("attr3"))
  generator$function_templates <- list(attr3 = list(function_def = function(params) return(params$a + 2),
                                                    call_params = c("attr2")))
  sim_manager$generators <- list(gen3 = generator)
  expect_error(sim_manager$run(), "Model attributes are incomplete: time_steps")
  expect_is(sim_manager$nested_model, "SimulationModel")
  expect_equal(sim_manager$nested_model$attached, list(sample_model_names = c("attr1", "attr2"),
                                                       sample_generative_names = list("attr3")))
  # Set model sample
  model_clone <- sim_manager$nested_model$clone()
  sim_manager$set_model_sample(model_clone, 1)
  expect_equal(model_clone$get_attributes(), list(attr1 = 3, attr2 = 5, sample_model_names = c("attr1", "attr2"),
                                                  sample_generative_names = list("attr3"), attr3 = 7))
})

test_that("run with complete attributes", {
  sim_manager <- SimulationManager$new(model_template = SimulationModel$new(time_steps = 10,
                                                                            model_attributes = c("time_steps", "attr1", "attr2")),
                                       sample_data = data.frame(attr1 = 3:4, attr2 = 5:6),
                                       parallel_cores = 2,
                                       results_dir = tempdir())
  sim_manager$model_simulator$simulation_function <- function (model) model$get_attributes(c("attr1", "attr2", "attr3"))
  generator <- Generator$new(generative_requirements = list(attr3 = "function"),
                             inputs = c("attr2"), outputs = c("attr3"))
  generator$function_templates <- list(attr3 = list(function_def = function(params) return(params$a + 2),
                                                    call_params = c("attr2")))
  sim_manager$generators <- list(gen3 = generator)
  run_output <- sim_manager$run()
  expect_named(run_output, c("summary", "failed_indices", "warning_indices", "full_log"))
  expect_equal(run_output$summary, "2 of 2 sample models ran and saved results successfully")
  expect_length(run_output$failed_indices, 0)
  expect_equal(run_output$full_log, list(list(successful = TRUE,
                                              message = "Model sample 1 simulation ran successfully and the results were saved"),
                                         list(successful = TRUE,
                                              message = "Model sample 2 simulation ran successfully and the results were saved")))
  expect_true(all(c("sample_1_results.RData", "sample_2_results.RData", "simulation_log.txt") %in% list.files(sim_manager$results_dir)))
  expect_equal(readRDS(file.path(sim_manager$results_dir, "sample_1_results.RData")), list(attr1 = 3, attr2 = 5, attr3 = 7))
  expect_equal(readRDS(file.path(sim_manager$results_dir, "sample_2_results.RData")), list(attr1 = 4, attr2 = 6, attr3 = 8))
})
