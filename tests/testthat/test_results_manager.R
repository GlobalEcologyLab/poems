context("Results Manager")

test_that("initialization and parameter setting", {
  TEST_DIRECTORY <- test_path("test_inputs")
  # Default initialization
  results_manager <- ResultsManager$new()
  # Attempt to set invalid simulation results
  expect_error(
    results_manager$simulation_results <- "dummy",
    "The simulation results must be a object of a class inherited from the SimulationResults class"
  )
  expect_silent(results_manager$simulation_results <- NULL)
  # Attempted to set invalid generation functions/values
  expect_error(results_manager$summary_functions <- "dummy", "Summary metric functions should be a list")
  expect_error(results_manager$summary_functions <- list(time_steps = "all$time_steps"),
    "Could not assign function all$time_steps",
    fixed = TRUE
  )
  expect_error(results_manager$summary_functions <- list(test = "dummy"), "Could not assign function dummy")
  expect_error(
    results_manager$summary_functions <- list(test = file.path(TEST_DIRECTORY, "test_not_a_function.R")),
    paste("Could not assign function", file.path(TEST_DIRECTORY, "test_not_a_function.R"))
  )
  expect_error(
    results_manager$summary_functions <- list(test = file.path(TEST_DIRECTORY, "no_such_file.R")),
    paste("Could not assign function", file.path(TEST_DIRECTORY, "no_such_file.R"))
  )
  expect_silent(results_manager$summary_functions <- NULL)
  # Attempted to set invalid result attachment functions
  expect_error(results_manager$result_attachment_functions <- "dummy", "Result attachment functions should be a list")
  expect_error(results_manager$result_attachment_functions <- list(test = "dummy"), "Could not assign function dummy")
  expect_error(
    results_manager$result_attachment_functions <- list(test = file.path(TEST_DIRECTORY, "test_not_a_function.R")),
    paste("Could not assign function", file.path(TEST_DIRECTORY, "test_not_a_function.R"))
  )
  expect_error(
    results_manager$result_attachment_functions <- list(test = file.path(TEST_DIRECTORY, "no_such_file.R")),
    paste("Could not assign function", file.path(TEST_DIRECTORY, "no_such_file.R"))
  )
  expect_silent(results_manager$result_attachment_functions <- NULL)
  # Set simulation results
  results_manager$simulation_results <- SimulationResults$new(time_steps = 10)
  results_manager$summary_metrics <- "dummy"
  # Set valid functions/values (with simulation results)
  results_manager$summary_functions <- list(
    constant = 22, time_steps = "all$time_steps",
    bad_test = file.path(TEST_DIRECTORY, "test_function_2.R"),
    total_n = file.path(TEST_DIRECTORY, "test_metrics_function.R"),
    min_results = "min", max_results = max,
    abundance = function(results) {
      return(results$all$get_attribute("abundance"))
    }
  )
  expect_named(results_manager$summary_functions, c(
    "constant", "time_steps", "bad_test", "total_n", "min_results",
    "max_results", "abundance"
  ))
  expect_equal(results_manager$summary_functions$constant, 22)
  expect_equal(
    results_manager$summary_functions[c("time_steps", "min_results", "max_results")],
    list(time_steps = "all$time_steps", min_results = "min", max_results = "max")
  )
  expect_is(results_manager$summary_functions$bad_test, "function")
  expect_is(results_manager$summary_functions$total_n, "function")
  expect_is(results_manager$summary_functions$abundance, "function")
  expect_equal(results_manager$summary_metrics, c(
    "dummy", "constant", "time_steps", "bad_test", "total_n", "min_results",
    "max_results", "abundance"
  ))
  # Initialize via simulation manager
  expect_error(
    results_manager <- ResultsManager$new(simulation_manager = "dummy"),
    "Simulation manager must be a GenericManager or inherited class object"
  )
  sim_manager <- SimulationManager$new(
    sample_data = data.frame(a = 1:3, b = 4:6),
    generators = list(Generator$new()),
    parallel_cores = 4,
    results_dir = test_path("test_results"),
    results_filename_attributes = "test"
  )
  results_manager <- ResultsManager$new(simulation_manager = sim_manager)
  expect_equal(results_manager$sample_data, data.frame(a = 1:3, b = 4:6))
  expect_is(results_manager$generators[[1]], "Generator")
  expect_equal(results_manager$parallel_cores, 4)
  expect_equal(results_manager$results_dir, test_path("test_results"))
  expect_equal(results_manager$results_filename_attributes, "test")
})

test_that("attempt generation with incomplete/invalid attributes", {
  TEST_DIRECTORY <- test_path("test_results")
  results_manager <- ResultsManager$new()
  # Attempt to run with incomplete attributes
  expect_error(
    results_manager$generate(),
    paste(
      "Summary metrics generation requires parameters to be set first:",
      "sample_data, simulation_results, summary_metrics, summary_matrices, summary_functions"
    )
  )
  results_manager$sample_data <- data.frame(a = 1:3, b = 4:6)
  results_manager$simulation_results <- SimulationResults$new(time_steps = 10)
  results_manager$summary_functions <- list(constant = 22, time_steps = "all$time_steps")
  # Attempt to run with a(n) missing/invalid results directory
  expect_error(results_manager$generate(), "No directory set for reading results")
  expect_error(results_manager$generate(results_dir = "G:\\no_such_drive\\"),
    "Could find the results directory G:\\no_such_drive\\",
    fixed = TRUE
  )
  # Attempt to run with result files not matched via results_filename_attributes
  results_manager$results_dir <- TEST_DIRECTORY
  results_manager$results_filename_attributes <- c("dummy")
  expect_error(results_manager$generate(),
    paste0(
      "Could not find (first) results file ", file.path(TEST_DIRECTORY, "dummy_1_results.RData"),
      ". Ensure results_filename_attributes is set to match the filename pattern"
    ),
    fixed = TRUE
  )
})

test_that("calculate summaries", {
  simulation_results <- SimulationResults$new(
    time_steps = 10, default = "all$abundance",
    results = file.path(test_path("test_results"), "sample_1_results.RData")
  )
  results_manager <- ResultsManager$new(simulation_results = simulation_results)
  results_manager$summary_functions <- list(
    constant = 22, time_steps = "all$time_steps",
    bad_test = file.path(test_path("test_inputs"), "test_function_2.R"),
    total_n = file.path(test_path("test_inputs"), "test_metrics_function.R"),
    min_results = "min", max_results = max,
    abundance = function(results) {
      return(results$all$get_attribute("abundance"))
    },
    warn = function(results) {
      warning("test")
      11
    }
  )
  # Include errors and warnings
  calculated_summaries <- results_manager$calculate_summaries(simulation_results, 1)
  expect_named(calculated_summaries, c("successful", "message", "errors", "warnings", "summary_metric_data"))
  expect_false(calculated_summaries$successful)
  expect_equal(calculated_summaries$message, "Summaries calculated with errors and warnings for sample 1")
  expect_match(calculated_summaries$errors[1], "Error encountered setting metric bad_test")
  expect_match(calculated_summaries$errors[2], "Metric function defined for abundance produces multiple values")
  expect_match(calculated_summaries$warnings[1], "Warning encountered setting metric warn")
  expect_equal(
    calculated_summaries$summary_metric_data,
    data.frame(
      index = 1, constant = 22, time_steps = 10, bad_test = NA,
      total_n = sum(simulation_results$attached$abundance),
      min_results = min(simulation_results$all$attached$abundance),
      max_results = max(simulation_results$all$attached$abundance),
      abundance = NA, warn = 11
    )
  )
  # No errors and warnings
  simulation_results$error_messages <- NULL
  simulation_results$warning_messages <- NULL
  results_manager$summary_metrics <- c("constant", "time_steps", "total_n", "min_results", "max_results")
  calculated_summaries <- results_manager$calculate_summaries(simulation_results, 1)
  expect_equal(
    calculated_summaries,
    list(
      successful = TRUE, message = "Summaries calculated for sample 1",
      summary_metric_data = data.frame(
        index = 1, constant = 22, time_steps = 10,
        total_n = sum(simulation_results$attached$abundance),
        min_results = min(simulation_results$all$attached$abundance),
        max_results = max(simulation_results$all$attached$abundance)
      )
    )
  )
  # Include summary matrix
  results_manager$summary_metrics <- c("constant", "time_steps")
  results_manager$summary_matrices <- "abundance"
  calculated_summaries <- results_manager$calculate_summaries(simulation_results, 1)
  expect_equal(
    calculated_summaries,
    list(
      successful = TRUE, message = "Summaries calculated for sample 1",
      summary_metric_data = data.frame(index = 1, constant = 22, time_steps = 10),
      summary_matrix_list = list(abundance = simulation_results$all$attached$abundance)
    )
  )
})

test_that("calculate result attachments", {
  simulation_results <- SimulationResults$new(
    time_steps = 10, default = "all$abundance",
    results = file.path(test_path("test_results"), "sample_1_results.RData")
  )
  results_manager <- ResultsManager$new(simulation_results = simulation_results)
  results_manager$result_attachment_functions <- list(
    bad_test = file.path(test_path("test_inputs"), "test_function_2.R"),
    total_n = file.path(test_path("test_inputs"), "test_metrics_function.R"),
    min_results = "min", max_results = max,
    abund_all = function(results) {
      return(results$all$get_attribute("abundance"))
    },
    warn = function(results) {
      warning("test")
      11
    }
  )
  results_manager$calculate_result_attachments(simulation_results)
  expect_named(simulation_results$attached, c("abundance", "total_n", "min_results", "max_results", "abund_all", "warn"))
  expect_equal(
    simulation_results$attached[c("total_n", "min_results", "max_results", "abund_all", "warn")],
    list(
      total_n = sum(simulation_results$attached$abundance),
      min_results = min(simulation_results$all$attached$abundance),
      max_results = max(simulation_results$all$attached$abundance),
      abund_all = simulation_results$all$attached$abundance,
      warn = 11
    )
  )
  expect_match(simulation_results$error_messages[1], "Error encountered attaching bad_test to results")
  expect_match(simulation_results$warning_messages[1], "Warning encountered attaching warn to results")
})

test_that("summary generation", {
  invisible(file.copy(test_path("test_results"), tempdir(), recursive = TRUE))
  TEST_DIRECTORY <- file.path(tempdir(), "test_results")
  abundance <- list(
    SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_1_results.RData"))$attached$abundance,
    SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_2_results.RData"))$attached$abundance,
    SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_3_results.RData"))$attached$abundance
  )
  abundance_all <- lapply(abundance, colSums)
  results_manager <- ResultsManager$new(
    simulation_results = SimulationResults$new(time_steps = 10, default = "all$abundance"),
    results_dir = TEST_DIRECTORY
  )
  results_manager$summary_matrices <- "abundance"
  results_manager$summary_functions <- list(
    constant = 22, time_steps = "all$time_steps",
    total_n = file.path(test_path("test_inputs"), "test_metrics_function.R"),
    min_results = "min", max_results = max,
    abundance = function(results) {
      return(results$all$get_attribute("abundance"))
    }
  )
  results_manager$sample_data <- data.frame(sample = 1:4)
  # Generation
  generation_output <- results_manager$generate()
  expect_named(generation_output, c("summary", "failed_indices", "warning_indices", "full_log"))
  expect_equal(
    generation_output[c("summary", "failed_indices", "warning_indices")],
    list(
      summary = "3 of 4 summary metrics/matrices generated from sample results successfully",
      failed_indices = 4, warning_indices = NULL
    )
  )
  expect_length(generation_output[["full_log"]], 4)
  for (i in 1:3) {
    expect_true(generation_output$full_log[[i]][["successful"]])
    expect_equal(generation_output$full_log[[i]][["message"]], sprintf("Summaries calculated for sample %s", i))
    expect_named(
      generation_output$full_log[[i]][["summary_metric_data"]],
      c("index", "constant", "time_steps", "total_n", "min_results", "max_results")
    )
  }
  expect_false(generation_output$full_log[[4]][["successful"]])
  expect_equal(generation_output$full_log[[4]][["message"]], "Could not find result file for sample 4")
  expect_equal(generation_output$full_log[[4]][["errors"]], "Result file not found")
  expect_equal(
    generation_output$full_log[[4]][["summary_metric_data"]],
    data.frame(index = 4, constant = NA, time_steps = NA, total_n = NA, min_results = NA, max_results = NA)
  )
  expect_equal(
    results_manager$summary_metric_data,
    data.frame(
      index = 1:4, constant = c(22, 22, 22, NA), time_steps = c(10, 10, 10, NA),
      total_n = c(unlist(lapply(abundance_all, sum)), NA),
      min_results = c(unlist(lapply(abundance_all, min)), NA),
      max_results = c(unlist(lapply(abundance_all, max)), NA)
    )
  )
  expect_equal(
    results_manager$summary_matrix_list,
    list(abundance = matrix(c(unlist(abundance_all), rep(NA, 10)), nrow = 4, ncol = 10, byrow = TRUE))
  )
  expect_true(file.exists(file.path(results_manager$results_dir, "generation_log.txt")))
  # Include simple (function) generator and calculate occupancy via attachment
  generator <- Generator$new(
    generative_requirements = list(occup_mask = "function"),
    inputs = c("a"), outputs = c("occup_mask")
  )
  generator$function_templates <- list(occup_mask = list(
    function_def = function(params) {
      m <- rep(1, 5)
      m[params$a] <- 0
      m
    },
    call_params = c("a")
  ))
  results_manager$generators <- list(gen1 = generator)
  results_manager$sample_data <- data.frame(sample = 1:4, a = 1:4)
  results_manager$simulation_results$attribute_aliases <- list(occup_mask = "occupancy_mask")
  results_manager$result_attachment_functions <- list(gen_mask = function(r) {
    return(r$get_attribute("occup_mask"))
  })
  results_manager$summary_matrices <- c("abundance", "occupancy", "mask")
  results_manager$summary_functions$occupancy <- function(r) {
    return(colSums(+(r$get_attribute("abundance") > 0)))
  }
  results_manager$summary_functions$mask <- function(r) {
    return(r$get_attribute("gen_mask"))
  }
  generation_output <- results_manager$generate()
  expect_equal(
    generation_output[c("summary", "failed_indices", "warning_indices")],
    list(
      summary = "3 of 4 summary metrics/matrices generated from sample results successfully",
      failed_indices = 4, warning_indices = NULL
    )
  ) # same as last time
  abundance_masked <- list()
  masks <- list()
  for (i in 1:3) {
    masks[[i]] <- generator$function_templates$occup_mask$function_def(list(a = i))
    abundance_masked[[i]] <- abundance[[i]] * masks[[i]]
  }
  abundance_all_masked <- lapply(abundance_masked, colSums)
  expect_equal(
    results_manager$summary_metric_data,
    data.frame(
      index = 1:4, constant = c(22, 22, 22, NA), time_steps = c(10, 10, 10, NA),
      total_n = c(unlist(lapply(abundance_all_masked, sum)), NA),
      min_results = c(unlist(lapply(abundance_all_masked, min)), NA),
      max_results = c(unlist(lapply(abundance_all_masked, max)), NA)
    )
  )
  expect_equal(
    results_manager$summary_matrix_list,
    list(
      abundance = matrix(c(unlist(abundance_all_masked), rep(NA, 10)), nrow = 4, ncol = 10, byrow = TRUE),
      occupancy = matrix(c(unlist(lapply(abundance_masked, function(a) colSums(a > 0))), rep(NA, 10)),
        nrow = 4, ncol = 10, byrow = TRUE
      ),
      mask = matrix(c(unlist(masks), rep(NA, 5)), nrow = 4, ncol = 5, byrow = TRUE)
    )
  )
})

test_that("calculate summary weighted averages", {
  TEST_DIRECTORY <- test_path("test_results")
  abundance <- list(
    SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_1_results.RData"))$attached$abundance,
    SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_2_results.RData"))$attached$abundance,
    SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_3_results.RData"))$attached$abundance
  )
  abundance_all <- lapply(abundance, colSums)
  results_manager <- ResultsManager$new()
  expect_error(results_manager$calculate_summary_weighted_averages(), "No summary matrices have been generated")
  results_manager$summary_matrix_list <- list(abundance = matrix(c(unlist(abundance_all), rep(NA, 10)),
    nrow = 4, ncol = 10, byrow = TRUE
  ))
  expect_error(results_manager$calculate_summary_weighted_averages(), "Sample data with a weight column is required")
  results_manager$sample_data <- data.frame(sample = 1:4, a = 1:4)
  expect_error(results_manager$calculate_summary_weighted_averages(), "Sample data with a weight column is required")
  results_manager$sample_data$weight <- c(3, 4, 5, 6)
  # Ignore NAs
  expect_silent(results_manager$calculate_summary_weighted_averages())
  expect_equal(
    results_manager$summary_matrix_weighted_averages,
    list(abundance = colSums(results_manager$summary_matrix_list$abundance * results_manager$sample_data$weight /
      sum(results_manager$sample_data$weight), na.rm = TRUE))
  )
  # NA replacements
  expect_error(
    results_manager$calculate_summary_weighted_averages(na_replacements = "dummy"),
    "NA replacements must be a list of values or functions"
  )
  expect_silent(results_manager$calculate_summary_weighted_averages(na_replacements = list(abundance = 100)))
  expect_equal(
    results_manager$summary_matrix_weighted_averages,
    list(abundance = colSums(rbind(results_manager$summary_matrix_list$abundance[1:3, ], rep(100, 10))
    * results_manager$sample_data$weight / sum(results_manager$sample_data$weight)))
  )
  na_function <- function(a) {
    a[which(is.na(a))] <- apply(a, 2, min, na.rm = NA)
    a
  }
  expect_silent(results_manager$calculate_summary_weighted_averages(na_replacements = list(abundance = na_function)))
  expect_equal(
    results_manager$summary_matrix_weighted_averages,
    list(abundance = colSums(results_manager$summary_matrix_list$abundance[c(1:3, 1), ]
    * results_manager$sample_data$weight / sum(results_manager$sample_data$weight)))
  )
})


test_that("summary generation via raster results", {
  invisible(file.copy(test_path("test_results"), tempdir(), recursive = TRUE))
  TEST_DIRECTORY <- file.path(tempdir(), "test_results", "raster")
  finite_indices <- c(1, 3, 5, 8, 9)
  abundance <- list(
    SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_1_raster_results.RData"))$attached$abundance[finite_indices],
    SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_2_raster_results.RData"))$attached$abundance[finite_indices],
    SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_3_raster_results.RData"))$attached$abundance[finite_indices]
  )
  abundance_all <- lapply(abundance, colSums)
  results_manager <- ResultsManager$new(
    simulation_results = SimulationResults$new(time_steps = 10),
    sample_data = data.frame(sample = 1:3),
    results_filename_attributes = c("sample", "raster_results"),
    results_dir = TEST_DIRECTORY
  )
  results_manager$summary_matrices <- "abundance"
  results_manager$summary_functions <- list(abundance = function(results) {
    return(results$all$get_attribute("abundance"))
  })
  # Generation
  generation_output <- results_manager$generate()
  expect_equal(
    generation_output[c("summary", "failed_indices", "warning_indices")],
    list(
      summary = "3 of 3 summary metrics/matrices generated from sample results successfully",
      failed_indices = integer(0), warning_indices = NULL
    )
  )
  expect_length(generation_output[["full_log"]], 3)
  expect_equal(results_manager$summary_metric_data, data.frame(index = 1:3)) # no metrics
  expect_equal(
    results_manager$summary_matrix_list,
    list(abundance = matrix(unlist(abundance_all), nrow = 3, ncol = 10, byrow = TRUE))
  )
})
