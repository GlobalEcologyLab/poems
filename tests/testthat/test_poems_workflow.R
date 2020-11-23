context("Poems Population Model Workflow")

test_that("workflow integration", {
  model_template <- PopulationModel$new(time_steps = 10, populations = 1, initial_abundance = 20,
                                        stage_matrix = 1.5, demographic_stochasticity = FALSE,
                                        harvest = list(rate = 0,
                                                       function(params) round(params$stage_abundance*(1 - params$rate))),
                                        attribute_aliases = list(harvest_rate = "harvest$rate"),
                                        results_selection = c("abundance", "harvested"))
  capacity_gen <- Generator$new(inputs = c("habitat_suit", "capacity_max"), outputs = c("carrying_capacity"),
                                generative_requirements = list(carrying_capacity = "function"))
  capacity_gen$add_function_template("carrying_capacity",
                                     function_def = function(params) round(params$habitat_suit*params$capacity_max),
                                     call_params = c("habitat_suit", "capacity_max"))
  lhs_gen <- LatinHypercubeSampler$new()
  lhs_gen$set_uniform_parameter("habitat_suit", lower = 0.1, upper = 1, decimals = 2)
  lhs_gen$set_uniform_parameter("capacity_max", lower = 80, upper = 120, decimals = 0)
  lhs_gen$set_uniform_parameter("harvest_rate", lower = 0.1, upper = 0.3, decimals = 2)
  sample_data <- lhs_gen$generate_samples(number = 10, random_seed = 123)
  sim_manager <- SimulationManager$new(sample_data = sample_data, model_template = model_template,
                                       generators = list(capacity_gen), parallel_cores = 2, results_dir = tempdir())
  run_output <- sim_manager$run()
  expect_length(run_output$failed_indices, 0)
  results_manager <- ResultsManager$new(simulation_manager = sim_manager,
                                        simulation_results = PopulationResults$new(),
                                        summary_metrics = c("final_n", "total_h"),
                                        summary_functions = list(final_n = function(results) results$abundance[10],
                                                                 total_h = function(results) sum(results$harvested)),
                                        parallel_cores = 2)
  gen_output <- results_manager$generate()
  expect_length(gen_output$failed_indices, 0)
  expect_named(results_manager$summary_metric_data, c("index", "final_n", "total_h"))
  validator <- Validator$new(simulation_parameters = sample_data,
                             simulation_summary_metrics = results_manager$summary_metric_data[-1],
                             observed_metric_targets = c(final_n = 35, total_h = 80))
  expect_warning(test <- utils::capture.output(validator$run(tolerance = 0.3)), "Validation function generated warning")
  expect_named(validator$selected_simulations, c("index", "weight"))
  expect_equal(validator$selected_simulations$index[order(validator$selected_simulations$weight, decreasing = TRUE)],
               c(8, 10, 3)) # best 3
})
