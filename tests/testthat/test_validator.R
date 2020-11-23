context("Validator")

test_that("initialization and parameter setting", {
  TEST_DIRECTORY <- test_path("test_inputs")
  # Default initialization
  validator <- Validator$new()
  # Attempt to set invalid parameters
  expect_error(validator$simulation_parameters <- 1:10,
               "The simulation parameters must be a data frame (or matrix)", fixed = TRUE)
  expect_error(validator$simulation_summary_metrics <- 1:10,
               "The simulation summary metrics must be a data frame (or matrix)", fixed = TRUE)
  # Consistency checks and column name matching
  validator$simulation_parameters <- file.path(TEST_DIRECTORY, "test_simulation_parameters.RData")
  expect_error(validator$simulation_summary_metrics <- array(1:16, c(8, 2)),
               "The simulation summary metrics must be have the same number of rows as the existing simulation parameters")
  validator$simulation_summary_metrics <- file.path(TEST_DIRECTORY, "test_simulation_summary_metrics.csv")
  expect_error(validator$simulation_parameters <- array(1:36, c(12, 3)),
               "The simulation parameters must be have the same number of rows as the existing simulation summary metrics")
  expect_error(validator$observed_metric_targets <- c(5, 0, 0, 1),
               "The observed metric targets must be have the same number of metrics as the existing simulation summary metrics (columns)",
               fixed = TRUE)
  expect_silent(validator$observed_metric_targets <- NULL)
  named_simulation_summary_metrics <- validator$simulation_summary_metrics
  # Name consistency
  expect_error(validator$observed_metric_targets <- list(x1 = 0.5, x2 = 0, x3 = 1),
               "The observed target metric names must correspond to those in the existing simulation summary metrics")
  expect_silent(validator$simulation_summary_metrics <- NULL)
  validator$observed_metric_targets <- list(x1 = 0.5, x2 = 0, x3 = 1)
  expect_error(validator$simulation_summary_metrics <- named_simulation_summary_metrics,
               "The simulation summary metric names must correspond to those in the existing observed target metrics")
  # Re-order existing/assigned targets to match assigned/existing summary metrics
  validator$observed_metric_targets <- list(m3 = 1, m1 = 0.5, m2 = 0)
  validator$simulation_summary_metrics <- named_simulation_summary_metrics
  expect_equal(validator$observed_metric_targets, unlist(list(m1 = 0.5, m2 = 0, m3 = 1)))
  validator$observed_metric_targets <- list(m3 = 1, m1 = 0.5, m2 = 0)
  expect_equal(validator$observed_metric_targets, unlist(list(m1 = 0.5, m2 = 0, m3 = 1)))
  # Name existing/assigned targets via assigned/existing summary metrics
  validator$simulation_summary_metrics <- NULL
  validator$observed_metric_targets <- c(0.5, 0, 1)
  validator$simulation_summary_metrics <- named_simulation_summary_metrics
  expect_named(validator$observed_metric_targets, c("m1", "m2", "m3"))
  validator$observed_metric_targets <- c(0.5, 0, 1)
  expect_named(validator$observed_metric_targets, c("m1", "m2", "m3"))
  # Name existing/assigned summary metrics via assigned/existing targets
  unnamed_simulation_summary_metrics <- named_simulation_summary_metrics
  names(unnamed_simulation_summary_metrics) <- NULL
  validator$observed_metric_targets <- NULL
  validator$simulation_summary_metrics <- unnamed_simulation_summary_metrics
  validator$observed_metric_targets <- list(m1 = 0.5, m2 = 0, m3 = 1)
  expect_named(validator$simulation_summary_metrics, c("m1", "m2", "m3"))
  validator$simulation_summary_metrics <- unnamed_simulation_summary_metrics
  expect_named(validator$simulation_summary_metrics, c("m1", "m2", "m3"))
  # No metric names given
  validator$simulation_summary_metrics <- NULL
  validator$observed_metric_targets <- NULL
  validator$simulation_summary_metrics <- unnamed_simulation_summary_metrics
  expect_error(validator$observed_metric_targets <- c(0.5, 0, 1), "Metrics have not been named")
  validator$simulation_summary_metrics <- NULL
  validator$observed_metric_targets <- c(0.5, 0, 1)
  expect_error(validator$simulation_summary_metrics <- unnamed_simulation_summary_metrics, "Metrics have not been named")
  # Non-finite replacements for simulation summary metrics validity and consistency
  validator$observed_metric_targets <- NULL
  validator$simulation_summary_metrics <- named_simulation_summary_metrics
  expect_error(validator$non_finite_replacements <- 1:2, "Non-finite replacements should be a list")
  expect_error(validator$non_finite_replacements <- list(m1 = 1, m2 = "dummy"), "Could not assign function dummy")
  expect_error(validator$non_finite_replacements <- list(m1 = 1, m2 = "min", m3 = file.path(TEST_DIRECTORY, "no_such_file.R")),
               paste("Could not assign function", file.path(TEST_DIRECTORY, "no_such_file.R")))
  expect_error(validator$non_finite_replacements <- list(m1 = 1, m2 = "min", m3 = file.path(TEST_DIRECTORY, "test_not_a_function.R")),
               paste("Could not assign function", file.path(TEST_DIRECTORY, "test_not_a_function.R")))
  validator$simulation_summary_metrics <- unnamed_simulation_summary_metrics
  expect_warning(validator$non_finite_replacements <- list(m1 = 1, m2 = min, m3 = file.path(TEST_DIRECTORY, "test_abc_function.R")),
                 "Ignoring replacements not found in summary metrics: m1, m2, m3")
  validator$simulation_summary_metrics <- named_simulation_summary_metrics
  expect_warning(validator$non_finite_replacements <- list(m0 = 0, m1 = 1, m2 = min, m3 = file.path(TEST_DIRECTORY, "test_abc_function.R")),
                 "Ignoring replacements not found in summary metrics: m0")
  expect_named(validator$non_finite_replacements, c("m1", "m2", "m3"))
  expect_equal(validator$non_finite_replacements[c("m1", "m2")], list(m1 = 1, m2 = "min"))
  expect_is(validator$non_finite_replacements$m3, "function")
  # Output directory
  expect_error(validator$output_dir <- file.path(TEST_DIRECTORY, "no_such_directory"),
               paste("Could not find output directory", file.path(TEST_DIRECTORY, "no_such_directory")))
  expect_silent(validator$output_dir <- NULL)
})

test_that("resolve non-finite metrics", {
  TEST_DIRECTORY <- test_path("test_inputs")
  validator <- Validator$new()
  validator$simulation_summary_metrics <- file.path(TEST_DIRECTORY, "test_simulation_summary_metrics.csv")
  # Combination 1 (use NAs)
  validator$simulation_summary_metrics$m1[3:5] <- NA
  validator$simulation_summary_metrics$m2[5:7] <- Inf
  validator$simulation_summary_metrics$m3[7:9] <- NaN
  validator$non_finite_replacements <- list(m1 = 1, m2 = min, m3 = file.path(TEST_DIRECTORY, "test_abc_function.R"))
  validator$resolve_nonfinite_metrics()
  expect_equal(validator$simulation_summary_metrics$m1[3:5], c(1, 1, 1))
  expect_equal(validator$simulation_summary_metrics$m2[5:7],
               rep(min(validator$simulation_summary_metrics$m2, na.rm = TRUE), 3))
  expect_equal(validator$simulation_summary_metrics$m3[7:9],
               rep(min(validator$simulation_summary_metrics$m3, na.rm = TRUE), 3))
  # Combination 2 (no NAs)
  validator$simulation_summary_metrics$m1[3:5] <- NA
  validator$simulation_summary_metrics$m2[5:7] <- NaN
  validator$simulation_summary_metrics$m3[7:9] <- c(-Inf, -Inf, Inf)
  source(file.path(TEST_DIRECTORY, "test_abc_function.R"))
  validator$non_finite_replacements <- list(m1 = "max", m2 = "test_abc_function",
                                            m3 = function(values) {return((values[which(is.infinite(values))] > 0)*2 - 1)})
  validator$resolve_nonfinite_metrics(use_nas = FALSE)
  expect_equal(validator$simulation_summary_metrics$m1[3:5],
               rep(max(validator$simulation_summary_metrics$m1, na.rm = TRUE), 3))
  expect_equal(validator$simulation_summary_metrics$m2[5:7],
               rep(min(validator$simulation_summary_metrics$m2, na.rm = TRUE), 3))
  expect_equal(validator$simulation_summary_metrics$m3[7:9], c(-1, -1, 1))
})

test_that("center and scale inputs", {
  TEST_DIRECTORY <- test_path("test_inputs")
  validator <- Validator$new()
  validator$simulation_parameters <- file.path(TEST_DIRECTORY, "test_simulation_parameters.RData")
  validator$simulation_summary_metrics <- file.path(TEST_DIRECTORY, "test_simulation_summary_metrics.csv")
  validator$observed_metric_targets <- list(m1 = 0.5, m2 = 0, m3 = 1)
  orig_simulation_parameters <- validator$simulation_parameters
  orig_simulation_summary_metrics <- validator$simulation_summary_metrics
  orig_observed_metric_targets <- validator$observed_metric_targets
  # Center and scale
  validator$center_scale_inputs()
  scaled_parameters <- scale(orig_simulation_parameters)
  expect_equal(validator$simulation_parameters, data.frame(scaled_parameters[,]))
  scaled_metrics <- scale(rbind(orig_simulation_summary_metrics, orig_observed_metric_targets))
  expect_equal(validator$simulation_summary_metrics, data.frame(scaled_metrics[-nrow(scaled_metrics),]))
  expect_equal(validator$observed_metric_targets, scaled_metrics[nrow(scaled_metrics),])
  expect_equal(validator$input_center_scale_values,
               list(simulation_parameters = list(center = attr(scaled_parameters, "scaled:center"), scale = attr(scaled_parameters, "scaled:scale")),
                    simulation_summary_metrics = list(center = attr(scaled_metrics, "scaled:center"), scale = attr(scaled_metrics, "scaled:scale")),
                    observed_metric_targets = list(center = attr(scaled_metrics, "scaled:center"), scale = attr(scaled_metrics, "scaled:scale"))))
  # Avoids repeated centering/scaling
  validator$center_scale_inputs()
  expect_equal(validator$input_center_scale_values,
               list(simulation_parameters = list(center = attr(scaled_parameters, "scaled:center"), scale = attr(scaled_parameters, "scaled:scale")),
                    simulation_summary_metrics = list(center = attr(scaled_metrics, "scaled:center"), scale = attr(scaled_metrics, "scaled:scale")),
                    observed_metric_targets = list(center = attr(scaled_metrics, "scaled:center"), scale = attr(scaled_metrics, "scaled:scale"))))
  # Clears centering/scaling when attributes re-assigned
  validator$simulation_parameters <- orig_simulation_parameters
  validator$simulation_summary_metrics <- orig_simulation_summary_metrics
  validator$observed_metric_targets <- NULL
  expect_equal(unname(validator$input_center_scale_values), list())
})

test_that("run attempts with missing/unresolved parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  validator <- Validator$new()
  # Missing parameters
  expect_error(validator$run(), paste("Validator run requires parameters to be set first:",
                                      "simulation_parameters, simulation_summary_metrics, observed_metric_targets"))
  # Set parameters
  validator$simulation_parameters <- file.path(TEST_DIRECTORY, "test_simulation_parameters.RData")
  validator$simulation_summary_metrics <- file.path(TEST_DIRECTORY, "test_simulation_summary_metrics.csv")
  validator$observed_metric_targets <- list(m1 = 0.5, m2 = 0, m3 = 1)
  # Run attempts with unresolvable non-finite simulation summary metric values
  validator$simulation_summary_metrics$m1[3:5] <- NA
  validator$simulation_summary_metrics$m2[5:7] <- Inf
  validator$simulation_summary_metrics$m3[7:9] <- NaN
  expect_error(validator$run(), paste("Non-finite simulation summary metric values need to be resolved",
                                      "(via appropriate replacements) for: m1, m2, m3"), fixed = TRUE)
  # with inappropriate functions
  validator$non_finite_replacements <- list(m1 = 1, m2 = min, m3 = "choose")
  expect_error(validator$run(), "Could not apply non-finite replacement for metric m3")
  validator$non_finite_replacements <- list(m1 = 1, m2 = min, m3 = match)
  expect_error(validator$run(), "Could not apply non-finite replacement for metric m3")
  # Attempt to generate diagnostics
  expect_error(validator$generate_diagnostics(),
               "Diagnostics generation requires the validation function to be run and the output directory to be set first")
  expect_error(validator$generate_diagnostics(output_dir = TEST_DIRECTORY),
               "Diagnostics generation requires the validation function to be run first")
})

test_that("run validator and generate diagnostics", {
  TEST_DIRECTORY <- test_path("test_inputs")
  validator <- Validator$new(output_dir = tempdir())
  validator$simulation_parameters <- file.path(TEST_DIRECTORY, "test_simulation_parameters.RData")
  validator$simulation_summary_metrics <- file.path(TEST_DIRECTORY, "test_simulation_summary_metrics.csv")
  validator$observed_metric_targets <- list(m1 = 0.5, m2 = 0, m3 = 1)
  expect_warning(expect_error(test <- utils::capture.output(validator$run(tol = 0.1)), "Validation function failed"),
                 "Validation function generated warning")
  expect_equal(validator$random_indices, 1:10) # no seed
  # Also set random seed
  validator$random_seed <- 1234
  expect_warning(test <- utils::capture.output(validator$run(tol = 0.2)), "Validation function generated warning")
  last_random_indices <- validator$random_indices
  # Generate diagnostics
  validator$generate_diagnostics()
  expect_true(file.exists(file.path(validator$output_dir, "validation_diagnostics.pdf")))
  expect_warning(test <- utils::capture.output(validator$run(tol = 0.1, method = "rejection", output_diagnostics = TRUE)),
                 "ABC diagnostics can only be generated with ABC method neuralnet or loclinear")
  expect_error(validator$generate_diagnostics(),
               "Validation diagnostics can only be generated when ABC methods neuralnet or loclinear were utilized")
  # Selected simulations
  expect_warning(test <- utils::capture.output(validator$run(tol = 0.4)), "Validation function generated warning")
  expect_equal(validator$random_indices, last_random_indices)
  expect_named(validator$selected_simulations, c("index", "weight"))
  expect_length(validator$selected_simulations$index, 4)
  expect_true(all(validator$selected_simulations$index %in% 1:10))
  expect_true(all(validator$selected_simulations$weight >= 0) && all(validator$selected_simulations$weight <= 1))
  # Re-assign validation call function
  expect_error(validator$validation_call_function <- "dummy",
               "The validation call function must be a function definition")
  validator$validation_call_function <- function(observed_metric_targets, simulation_parameters,
                                                 simulation_summary_metrics, tolerance, method, ...) {
    return(c("Empty test validation function return object", list(...)))
  }
  validator$run(extra_attr = 11)
  expect_equal(validator$validator_return_object, list("Empty test validation function return object",
                                                       extra_attr = 11))
  expect_error(validator$generate_diagnostics(), "Validation diagnostics generation failed")
})
