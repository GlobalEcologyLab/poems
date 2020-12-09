context("Population Transformation (function)")

test_that("setup function", {
  simulator <- SimulatorReference$new()
  # User-defined transformation as a function
  transformation_function <- population_transformation(replicates = 4, time_steps = 10, years_per_step = 1,
                                                       populations = 7, demographic_stochasticity = TRUE,
                                                       density_stages = c(0, 1, 1),
                                                       transformation = function(params) 0.33,
                                                       simulator)
  expect_is(transformation_function, "function")
  expect_named(formals(transformation_function), c("r", "tm", "carrying_capacity", "stage_abundance", "occupied_indices"))
  expect_named(environment(transformation_function)[["params"]],
               c("replicates", "time_steps", "years_per_step", "populations", "stages", "demographic_stochasticity",
                 "density_stages", "simulator"))
  expect_equal(environment(transformation_function)[["params"]][c("replicates", "time_steps", "years_per_step",
                                                                  "populations", "stages", "demographic_stochasticity",
                                                                  "density_stages")],
               list(replicates = 4, time_steps = 10, years_per_step = 1, populations = 7, stages = 3,
                    demographic_stochasticity = TRUE, density_stages = c(0, 1, 1)))
  # User-defined transformation as list with function
  transformation_function <- population_transformation(replicates = 4, time_steps = 10, years_per_step = 1,
                                                       populations = 7, demographic_stochasticity = TRUE,
                                                       density_stages = c(0, 1, 1),
                                                       transformation = list(a = 1, function(params) 0.33, b = 2),
                                                       simulator)
  expect_is(transformation_function, "function")
  expect_named(environment(transformation_function)[["params"]],
               c("replicates", "time_steps", "years_per_step", "populations", "stages", "demographic_stochasticity",
                 "density_stages", "simulator", "a", "b"))
  expect_equal(environment(transformation_function)[["params"]][c("a", "b")], list(a = 1, b = 2))
})

test_that("user-defined transformation", {
  simulator <- SimulatorReference$new()
  test_function_1 <- function(params) { # add one to occupied density stages
    params$simulator$attached$params <- params # attach to reference object
    extra_stage_abundance <- params$stage_abundance
    extra_stage_abundance[which(params$density_stages > 0), params$occupied_indices] <-
      extra_stage_abundance[which(params$density_stages > 0), params$occupied_indices] + 1
    return(extra_stage_abundance)
  }
  transformation_function <- population_transformation(replicates = 4, time_steps = 10, years_per_step = 1,
                                                       populations = 7, demographic_stochasticity = TRUE,
                                                       density_stages = c(0, 1, 1),
                                                       transformation = list(a = 1, test_function_1, b = 2),
                                                       simulator)
  carrying_capacity <- rep(10, 7)
  stage_abundance <- matrix(c(7, 13, 0, 26, 0, 39, 47,
                              2,  0, 6,  8, 0, 12, 13,
                              0,  3, 4,  6, 0,  9, 10), nrow = 3, ncol = 7, byrow = TRUE)
  occupied_indices <- (1:7)[-5]
  expected_stage_abundance <- stage_abundance
  expected_stage_abundance[2:3, (1:7)[-5]] <- expected_stage_abundance[2:3, (1:7)[-5]] + 1
  expect_equal(transformation_function(r = 2, tm = 6, carrying_capacity, stage_abundance, occupied_indices),
               list(stage_abundance = expected_stage_abundance))
  expect_named(simulator$attached$params, c("replicates", "time_steps", "years_per_step", "populations", "stages",
                                            "demographic_stochasticity", "density_stages", "simulator", "a", "b", "r",
                                            "tm", "carrying_capacity", "stage_abundance", "occupied_indices"))
  expect_equal(simulator$attached$params[c("a", "b", "r", "tm", "carrying_capacity", "stage_abundance", "occupied_indices")],
               list(a = 1, b = 2, r = 2, tm = 6, carrying_capacity = carrying_capacity,
                    stage_abundance = stage_abundance, occupied_indices = occupied_indices))
  # Transforming carrying capacity as well
  test_function_2 <- function(params) { # add one to occupied density stages
    extra_stage_abundance <- params$stage_abundance
    extra_stage_abundance[which(params$density_stages > 0), params$occupied_indices] <-
      extra_stage_abundance[which(params$density_stages > 0), params$occupied_indices] + 1
    return(list(stage_abundance = extra_stage_abundance, carrying_capacity = params$carrying_capacity + 10))
  }
  transformation_function <- population_transformation(replicates = 4, time_steps = 10, years_per_step = 1,
                                                       populations = 7, demographic_stochasticity = TRUE,
                                                       density_stages = c(0, 1, 1),
                                                       transformation = list(a = 1, test_function_2, b = 2),
                                                       simulator)
  expect_equal(transformation_function(r = 2, tm = 6, carrying_capacity, stage_abundance, occupied_indices),
               list(stage_abundance = expected_stage_abundance, carrying_capacity = carrying_capacity + 10))

  # Errors and warnings
  test_function = function (params) stop("test error")
  transformation_function <- population_transformation(replicates = 4, time_steps = 10, years_per_step = 1,
                                                       populations = 7, demographic_stochasticity = TRUE,
                                                       density_stages = c(0, 1, 1),
                                                       transformation = list(a = 1, test_function, b = 2),
                                                       simulator)
  expect_error(transformation_function(r = 2, tm = 6, carrying_capacity, stage_abundance, occupied_indices),
               "Error produced within user-defined transformation function")
  test_function <- function(params) params$stage_abundance
  transformation_function <- population_transformation(replicates = 4, time_steps = 10, years_per_step = 1,
                                                       populations = 7, demographic_stochasticity = TRUE,
                                                       density_stages = c(0, 1, 1),
                                                       transformation = list(a = 1, test_function, b = 2),
                                                       simulator, name = "test_transf")
  stage_abundance[1] <- NA
  expect_warning(transformation_function(r = 2, tm = 6, carrying_capacity, stage_abundance, occupied_indices),
                 "Non-finite stage abundances returned by user-defined test_transf function")
  stage_abundance[2] <- -1
  expect_warning(transformation_function(r = 2, tm = 6, carrying_capacity, stage_abundance, occupied_indices),
                 "Negative stage abundances returned by user-defined test_transf function")
  test_function <- function(params) list(stage_abundance = params$stage_abundance, carrying_capacity = params$carrying_capacity)
  transformation_function <- population_transformation(replicates = 4, time_steps = 10, years_per_step = 1,
                                                       populations = 7, demographic_stochasticity = TRUE,
                                                       density_stages = c(0, 1, 1),
                                                       transformation = list(a = 1, test_function, b = 2),
                                                       simulator, name = "test_transf")
  carrying_capacity[1:2] <- c(NA, -10)
  expect_warning(transformation_function(r = 2, tm = 6, carrying_capacity, stage_abundance, occupied_indices),
                 "Non-finite carrying capacities returned by user-defined test_transf function")
  expect_warning(transformation_function(r = 2, tm = 6, carrying_capacity, stage_abundance, occupied_indices),
                 "Negative carrying capacities returned by user-defined test_transf function")
})
