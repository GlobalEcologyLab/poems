context("Population Simulator (function)")

test_that("minimal inputs", {
  expect_error(population_simulator(list()),
               "Minimal inputs required to run simulation should include: time_steps, populations, initial_abundance, stage_matrix, carrying_capacity")
  inputs <- list(time_steps = 10, populations = 1, initial_abundance = 10, stage_matrix = 1.2,
                 carrying_capacity = 40, demographic_stochasticity = FALSE)
  expected_abundance <- array(12, 10)
  for (i in 2:10) expected_abundance[i] <- pmin(round(expected_abundance[i - 1]*1.2), 40)
  expect_equal(population_simulator(inputs), list(all = list(abundance = expected_abundance),
                                                  abundance = array(expected_abundance, c(1, 10))))
})

test_that("results collection", {
  # Single replicate
  inputs <- list(time_steps = 10, populations = 1, initial_abundance = 10, stage_matrix = 1.4,
                 carrying_capacity = 40, demographic_stochasticity = FALSE,
                 harvest = function(params) params$stage_abundance - params$r,
                 results_selection = c("abundance", "harvested"))
  expected_abundance <- array(13:11, c(3, 10))
  expected_harvested <- array(1:3, c(3, 10))
  for (i in 1:3) {
    for (j in 2:10) expected_abundance[i, j] <- pmin(round(expected_abundance[i, j - 1]*1.4), 40) - i
  }
  expect_equal(population_simulator(inputs), list(all = list(abundance = array(expected_abundance[1,]),
                                                             harvested = array(expected_harvested[1,])),
                                                  abundance = array(expected_abundance[1,], c(1, 10)),
                                                  harvested = array(expected_harvested[1,], c(1, 10))))
  # Multiple replicates summarized (default)
  inputs$replicates <- 3
  expect_equal(population_simulator(inputs),
               list(all = list(abundance = list(mean = array(colMeans(expected_abundance)),
                                                sd = array(apply(expected_abundance, 2, stats::sd)),
                                                min = array(expected_abundance[3,]),
                                                max = array(expected_abundance[1,])),
                               harvested = list(mean = array(expected_harvested[2,]),
                                                sd = array(expected_harvested[1,]),
                                                min = array(expected_harvested[1,]),
                                                max = array(expected_harvested[3,]))),
                    abundance = list(mean = array(colMeans(expected_abundance), c(1, 10)),
                                     sd = array(apply(expected_abundance, 2, stats::sd), c(1, 10)),
                                     min = array(expected_abundance[3,], c(1, 10)),
                                     max = array(expected_abundance[1,], c(1, 10))),
                    harvested = list(mean = array(expected_harvested[2,], c(1, 10)),
                                     sd = array(expected_harvested[1,], c(1, 10)),
                                     min = array(expected_harvested[1,], c(1, 10)),
                                     max = array(expected_harvested[3,], c(1, 10)))))
  # Multiple replicates 3D
  inputs$results_selection = c("abundance", "harvested", "replicate")
  expect_equal(population_simulator(inputs), list(all = list(abundance = t(expected_abundance),
                                                             harvested = t(expected_harvested)),
                                                  abundance = array(t(expected_abundance), c(1, 10, 3)),
                                                  harvested = array(t(expected_harvested), c(1, 10, 3))))
})

test_that("module parameter passing and order", {
  inputs <- list(time_steps = 10, populations = 1, initial_abundance = 10, stage_matrix = 1.2,
                 carrying_capacity = 40, demographic_stochasticity = FALSE,
                 density_dependence = function(params) {
                   params$simulator$results$density_params <- names(params)
                   params$simulator$results$order <- unique(c(params$simulator$results$order, "transition"))
                   return(params$transition_array)
                 },
                 translocation = function(params) {
                   params$simulator$results$transl_params <- names(params)
                   params$simulator$results$order <- unique(c(params$simulator$results$order, "translocation"))
                   return(params$stage_abundance)
                 },
                 harvest = function(params) {
                   params$simulator$results$harvest_params <- names(params)
                   params$simulator$results$order <- unique(c(params$simulator$results$order, "harvest"))
                   return(params$stage_abundance)
                 },
                 mortality = function(params) {
                   params$simulator$results$mortality_params <- names(params)
                   params$simulator$results$order <- unique(c(params$simulator$results$order, "mortality"))
                   return(params$stage_abundance)
                 },
                 dispersal = function(params) {
                   params$simulator$results$dispersal_params <- names(params)
                   params$simulator$results$order <- unique(c(params$simulator$results$order, "dispersal"))
                   return(params$stage_abundance)
                 },
                 extra = function(params) {
                   params$simulator$results$extra_params <- names(params)
                   params$simulator$results$order <- unique(c(params$simulator$results$order, "extra"))
                   return(params$stage_abundance)
                 },
                 simulation_order = c("results", "extra", "dispersal", "mortality", "harvest",
                                      "translocation", "transition"))
  expected_abundance <- array(10, 10)
  for (i in 2:10) expected_abundance[i] <- round(expected_abundance[i - 1]*1.2)
  expected_density_params <- c("simulator", "transition_array", "fecundity_mask", "carrying_capacity", "stage_abundance",
                               "population_abundance", "density_abundance", "occupied_indices", "growth_rate_max")
  expected_transform_params <- c("replicates", "time_steps", "years_per_step", "populations", "stages",
                                 "demographic_stochasticity", "density_stages", "simulator", "r", "tm",
                                 "carrying_capacity", "stage_abundance", "occupied_indices")
  expected_dispersal_params <- c("replicates", "time_steps", "years_per_step", "populations", "stages",
                                 "demographic_stochasticity", "density_stages", "dispersal_stages",
                                 "dispersal_source_n_k", "dispersal_target_k", "dispersal_target_n", "simulator",
                                 "r", "tm", "carrying_capacity", "stage_abundance", "occupied_indices")
  expect_equal(population_simulator(inputs),
               list(all = list(abundance = expected_abundance), abundance = array(expected_abundance, c(1, 10)),
                    extra_params = expected_transform_params, order = inputs$simulation_order[-1],
                    dispersal_params = expected_dispersal_params, mortality_params = expected_transform_params,
                    harvest_params = expected_transform_params, transl_params = expected_transform_params,
                    density_params = expected_density_params))
})
