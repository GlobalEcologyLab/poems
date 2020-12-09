context("Population Density Dependence (function)")

test_that("setup function", {
  stage_matrix <- array(c(0, 0.5, 0, 3, 0, 0.7, 4, 0, 0.8), c(3, 3))
  fecundity_mask <- array(c(0, 0, 0, 1, 0, 0, 1, 0, 0), c(3, 3))
  simulator <- SimulatorReference$new()
  # Ceiling density dependence
  density_function <- population_density(populations = 7, stage_matrix = stage_matrix, fecundity_mask = fecundity_mask,
                                         fecundity_max = NULL, density_dependence = "ceiling",
                                         growth_rate_max = NULL, density_affects = NULL, density_stages = NULL,
                                         density_precision = 2, simulator = simulator)
  expect_is(density_function, "function")
  expect_named(formals(density_function), c("carrying_capacity", "stage_abundance"))
  expect_equal(environment(density_function)[["density_stage_indices"]], 1:3)
  expect_equal(environment(density_function)[["density_stages"]], 3)
  expect_equal(environment(density_function)[["density_precision"]], 2)
  # Logistic density dependence (stages > 1)
  density_function <- population_density(populations = 7, stage_matrix = stage_matrix, fecundity_mask = fecundity_mask,
                                         fecundity_max = NULL, density_dependence = "logistic",
                                         growth_rate_max = NULL, density_affects = NULL, density_stages = c(0, 1, 1),
                                         density_precision = NULL, simulator = simulator)
  expect_is(density_function, "function")
  expect_named(formals(density_function), c("transition_array", "carrying_capacity", "stage_abundance", "occupied_indices"))
  expect_equal(environment(density_function)[["density_stage_indices"]], 2:3)
  expect_equal(environment(density_function)[["density_stages"]], 2)
  expect_equal(environment(density_function)[["density_precision"]], 3)
  expect_equal(environment(density_function)[["growth_rate_max"]],
               log(Re((eigen(stage_matrix, only.values = TRUE)$values)[1])))
  expect_equal(environment(density_function)[["density_affects"]], +(stage_matrix > 0))
  expect_equal(environment(density_function)[["maximum_multiplier"]], 1/max(colSums((1 - fecundity_mask)*stage_matrix)))
  multipliers <- (1:(1.25*1000))/1000
  expect_equal(environment(density_function)[["multiplier_lookup"]],
               data.frame(growth_rate = apply(array(multipliers), 1,
                                              function(m) log(Re((eigen(stage_matrix*m, only.values = TRUE)$values)[1]))),
                          multiplier = multipliers))
  expect_is(environment(density_function)[["calculate_multipliers"]], "function")
  expect_named(formals(environment(density_function)[["calculate_multipliers"]]), c("growth_rates"))
  expect_is(environment(density_function)[["apply_multipliers"]], "function")
  expect_named(formals(environment(density_function)[["apply_multipliers"]]), c("transition_array", "multipliers"))
  # Logistic density dependence (stages == 1)
  density_function <- population_density(populations = 7, stage_matrix = matrix(1.2), fecundity_mask = matrix(1),
                                         fecundity_max = NULL, density_dependence = "logistic",
                                         growth_rate_max = NULL, density_affects = NULL, density_stages = NULL,
                                         density_precision = NULL, simulator = simulator)
  expect_is(density_function, "function")
  expect_named(formals(density_function), c("transition_array", "carrying_capacity", "stage_abundance", "occupied_indices"))
  expect_equal(environment(density_function)[["density_stage_indices"]], 1)
  expect_equal(environment(density_function)[["density_stages"]], 1)
  expect_equal(environment(density_function)[["density_precision"]], 3)
  expect_equal(environment(density_function)[["growth_rate_max"]], log(1.2))
  expect_equal(environment(density_function)[["density_affects"]], matrix(1))
  expect_null(environment(density_function)[["maximum_multiplier"]])
  expect_null(environment(density_function)[["multiplier_lookup"]])
  expect_null(environment(density_function)[["calculate_multipliers"]])
  # User-defined density dependence as function
  density_function <- population_density(populations = 7, stage_matrix = stage_matrix, fecundity_mask = fecundity_mask,
                                         fecundity_max = NULL, density_dependence = function(params) 0.33,
                                         growth_rate_max = NULL, density_affects = NULL, density_stages = NULL,
                                         density_precision = NULL, simulator = simulator)
  expect_is(density_function, "function")
  expect_named(formals(density_function), c("transition_array", "carrying_capacity", "stage_abundance", "occupied_indices"))
  expect_equal(environment(density_function)[["additional_attributes"]], list())
  # User-defined density dependence as list with function
  density_function <- population_density(populations = 7, stage_matrix = stage_matrix, fecundity_mask = fecundity_mask,
                                         fecundity_max = NULL, density_dependence = list(a = 1, function(params) 0.33, b = 2),
                                         growth_rate_max = NULL, density_affects = NULL, density_stages = NULL,
                                         density_precision = NULL, simulator = simulator)
  expect_is(density_function, "function")
  expect_equal(environment(density_function)[["additional_attributes"]], list(a = 1, b = 2))
})

test_that("ceiling density dependence", {
  stage_matrix <- array(c(0, 0.5, 0, 3, 0, 0.7, 4, 0, 0.8), c(3, 3))
  fecundity_mask <- array(c(0, 0, 0, 1, 0, 0, 1, 0, 0), c(3, 3))
  simulator <- SimulatorReference$new()
  density_function <- population_density(populations = 7, stage_matrix = stage_matrix, fecundity_mask = fecundity_mask,
                                         fecundity_max = NULL, density_dependence = "ceiling",
                                         growth_rate_max = NULL, density_affects = NULL, density_stages = c(0, 1, 1),
                                         density_precision = NULL, simulator = simulator)
  carrying_capacity <- rep(10, 7)
  stage_abundance <- matrix(c(7, 13, 0, 26, 0, 39, 47,
                              2,  0, 6,  8, 0, 12, 13,
                              0,  3, 4,  6, 0,  9, 10), nrow = 3, ncol = 7, byrow = TRUE)
  expect_equal(density_function(carrying_capacity, stage_abundance)[1, ], stage_abundance[1, ])
  expect_true(all(colSums(density_function(carrying_capacity, stage_abundance)[2:3, ]) <= 10))
})

test_that("calculate and apply multipliers functions", {
  stage_matrix <- array(c(0, 0.5, 0, 3, 0, 0.7, 4, 0, 0.8), c(3, 3))
  fecundity_mask <- array(c(0, 0, 0, 1, 0, 0, 1, 0, 0), c(3, 3))
  variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
  stage_array <- array(stage_matrix, c(3, 3, 7))*variation_array
  stage_array[1, 3, 5:7] <- 4 # maximum fecundity
  simulator <- SimulatorReference$new()
  # Affects fecundities only
  expect_error(density_function <- population_density(populations = 7, stage_matrix = stage_matrix,
                                                      fecundity_mask = fecundity_mask, fecundity_max = NULL,
                                                      density_dependence = "logistic", growth_rate_max = 0.5,
                                                      density_affects = fecundity_mask, density_stages = c(0, 1, 1),
                                                      density_precision = NULL, simulator = simulator),
               "Could not build density lookup table without maximum fecundity")
  density_function <- population_density(populations = 7, stage_matrix = stage_matrix, fecundity_mask = fecundity_mask,
                                         fecundity_max = 4, density_dependence = "logistic",
                                         growth_rate_max = 0.5, density_affects = fecundity_mask,
                                         density_stages = c(0, 1, 1), density_precision = NULL, simulator = simulator)
  multiplier_lookup <- environment(density_function)[["multiplier_lookup"]]
  expect_equal(multiplier_lookup$multiplier[nrow(multiplier_lookup)], round(4/3, 3))
  expect_equal(multiplier_lookup$growth_rate[nrow(multiplier_lookup)],
               log(Re((eigen(array(c(0, 0.5, 0, 1.333*3, 0, 0.7, 4, 0, 0.8), c(3, 3)), only.values = TRUE)$values)[1])))
  expect_equal(environment(density_function)[["calculate_multipliers"]](growth_rates = c(0.4, 0.5, 0.6)),
               array(multiplier_lookup$multiplier[apply(array(c(0.4, 0.5, 0.6)), 1,
                                                        function(r) which.min(abs(multiplier_lookup$growth_rate - r)))]))
  expected_stage_array <- stage_array
  expected_stage_array[1, 2:3, ] <- pmin(rep(seq(0.7, 1.3, 0.1), each = 2)*stage_array[1, 2:3, ], 4)
  expect_equal(environment(density_function)[["apply_multipliers"]](transition_array = stage_array,
                                                                    multipliers = seq(0.7, 1.3, 0.1)),
               expected_stage_array)
  # Affects all
  density_function <- population_density(populations = 7, stage_matrix = stage_matrix, fecundity_mask = fecundity_mask,
                                         fecundity_max = 4, density_dependence = "logistic",
                                         growth_rate_max = 0.5, density_affects = NULL,
                                         density_stages = c(0, 1, 1), density_precision = NULL, simulator = simulator)
  multiplier_lookup <- environment(density_function)[["multiplier_lookup"]]
  expect_equal(multiplier_lookup$multiplier[nrow(multiplier_lookup)], 1/0.8)
  expect_equal(multiplier_lookup$growth_rate[nrow(multiplier_lookup)],
               log(Re((eigen(array(c(stage_matrix[1:6]/0.8, 4, 0, 1), c(3, 3)), only.values = TRUE)$values)[1])))
  expected_stage_array <- stage_array
  expected_stage_array[, ,] <- pmin(rep(seq(0.65, 1.25, 0.1), each = 9)*stage_array, 4)
  expect_equal(environment(density_function)[["apply_multipliers"]](transition_array = stage_array,
                                                                    multipliers = seq(0.65, 1.25, 0.1)),
               expected_stage_array)
})

test_that("logistic density dependence", {
  # Logistic density dependence (stages > 1)
  stage_matrix <- array(c(0, 0.5, 0, 3, 0, 0.7, 4, 0, 0.8), c(3, 3))
  fecundity_mask <- array(c(0, 0, 0, 1, 0, 0, 1, 0, 0), c(3, 3))
  variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
  stage_array <- array(stage_matrix, c(3, 3, 7))*variation_array
  simulator <- SimulatorReference$new()
  density_function <- population_density(populations = 7, stage_matrix = stage_matrix, fecundity_mask = fecundity_mask,
                                         fecundity_max = 4, density_dependence = "logistic",
                                         growth_rate_max = 0.5, density_affects = (1 - fecundity_mask),
                                         density_stages = c(0, 1, 1), density_precision = NULL, simulator = simulator)
  carrying_capacity <- rep(10, 7)
  stage_abundance <- matrix(c(7, 13, 0, 26, 0, 39, 47,
                              2,  0, 6,  8, 0, 12, 13,
                              0,  3, 4,  6, 0,  9, 10), nrow = 3, ncol = 7, byrow = TRUE)
  occupied_indices <- (1:7)[-5]
  multiplier_array <- array(1, c(3, 3, 7))
  multipliers <- environment(density_function)[["calculate_multipliers"]](0.5*(1 - colSums(stage_abundance[2:3, occupied_indices]/10)))
  multiplier_array[2:3, , occupied_indices] <- rep(multipliers, each = 6)
  expected_stage_array <- stage_array*multiplier_array
  expected_stage_array[, , occupied_indices][which(expected_stage_array[, , occupied_indices] > 4)] <- 4
  expect_equal(density_function(transition_array = stage_array, carrying_capacity, stage_abundance, occupied_indices),
               expected_stage_array)
  # Logistic density dependence (stages == 1)
  stage_array <- array(1.2, c(1, 1, 7))*variation_array[1, 1,]
  density_function <- population_density(populations = 7, stage_matrix = matrix(1.2), fecundity_mask = matrix(1),
                                         fecundity_max = NULL, density_dependence = "logistic",
                                         growth_rate_max = NULL, density_affects = NULL, density_stages = NULL,
                                         density_precision = NULL, simulator = simulator)
  stage_abundance <- array(colSums(stage_abundance), c(1, 7))
  expected_stage_array <- stage_array
  expected_stage_array[, , occupied_indices]  <- exp(log(1.2)*(1 - stage_abundance[occupied_indices]/10))
  expect_equal(density_function(transition_array = stage_array, carrying_capacity, stage_abundance, occupied_indices),
               expected_stage_array)
})

test_that("user-defined density dependence", {
  stage_matrix <- array(c(0, 0.5, 0, 3, 0, 0.7, 4, 0, 0.8), c(3, 3))
  fecundity_mask <- array(c(0, 0, 0, 1, 0, 0, 1, 0, 0), c(3, 3))
  variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
  stage_array <- array(stage_matrix, c(3, 3, 7))*variation_array
  simulator <- SimulatorReference$new()
  test_density_function <- function(params) { # implement logistic function
    params$simulator$attached$params <- params # attach to reference object
    growth_rate <- params$growth_rate_max*(1 - params$density_abundance/params$carrying_capacity)
    params$apply_multipliers(params$transition_array, params$calculate_multipliers(growth_rate))
  }
  logistic_function <- population_density(populations = 7, stage_matrix = stage_matrix, fecundity_mask = fecundity_mask,
                                          fecundity_max = 4, density_dependence = "logistic",
                                          growth_rate_max = 0.5, density_affects = (1 - fecundity_mask),
                                          density_stages = c(0, 1, 1), density_precision = NULL, simulator = simulator)
  density_function <- population_density(populations = 7, stage_matrix = stage_matrix, fecundity_mask = fecundity_mask,
                                         fecundity_max = 4, density_dependence = list(a = 1, test_density_function, b = 2),
                                         growth_rate_max = 0.5, density_affects = (1 - fecundity_mask),
                                         density_stages = c(0, 1, 1), density_precision = NULL, simulator = simulator)
  carrying_capacity <- rep(10, 7)
  stage_abundance <- matrix(c(7, 13, 0, 26, 0, 39, 47,
                              2,  0, 6,  8, 0, 12, 13,
                              0,  3, 4,  6, 0,  9, 10), nrow = 3, ncol = 7, byrow = TRUE)
  occupied_indices <- (1:7)[-5]
  expect_equal(logistic_function(transition_array = stage_array, carrying_capacity, stage_abundance, occupied_indices)[,,occupied_indices],
               density_function(transition_array = stage_array, carrying_capacity, stage_abundance, occupied_indices)[,,occupied_indices])
  expect_named(simulator$attached$params, c("simulator", "a", "b", "transition_array", "fecundity_mask", "fecundity_max",
                                            "carrying_capacity", "stage_abundance", "population_abundance", "density_abundance",
                                            "occupied_indices", "growth_rate_max", "calculate_multipliers", "apply_multipliers"))
  expect_equal(simulator$attached$params[c("a", "b", "transition_array", "carrying_capacity", "stage_abundance",
                                           "population_abundance", "density_abundance", "occupied_indices", "growth_rate_max")],
               list(a = 1, b = 2,
                    transition_array = stage_array,
                    carrying_capacity = carrying_capacity,
                    stage_abundance = stage_abundance,
                    population_abundance = colSums(stage_abundance),
                    density_abundance = colSums(stage_abundance[2:3,]),
                    occupied_indices = occupied_indices,
                    growth_rate_max = 0.5))
  # Errors and warnings
  test_density_function = function (params) stop("test error")
  density_function <- population_density(populations = 7, stage_matrix = stage_matrix, fecundity_mask = fecundity_mask,
                                         fecundity_max = 4, density_dependence = list(a = 1, test_density_function, b = 2),
                                         growth_rate_max = 0.5, density_affects = (1 - fecundity_mask),
                                         density_stages = c(0, 1, 1), density_precision = NULL, simulator = simulator)
  expect_error(density_function(transition_array = stage_array, carrying_capacity, stage_abundance, occupied_indices),
               "Error produced within user-defined density dependence function")
  test_density_function = function (params) params$transition_array
  density_function <- population_density(populations = 7, stage_matrix = stage_matrix, fecundity_mask = fecundity_mask,
                                         fecundity_max = 4, density_dependence = list(a = 1, test_density_function, b = 2),
                                         growth_rate_max = 0.5, density_affects = (1 - fecundity_mask),
                                         density_stages = c(0, 1, 1), density_precision = NULL, simulator = simulator)
  stage_array[1] <- NA
  expect_warning(density_function(transition_array = stage_array, carrying_capacity, stage_abundance, occupied_indices),
                 "Non-finite transition rates returned by user-defined density dependence function")
  stage_array[2] <- -1
  expect_warning(density_function(transition_array = stage_array, carrying_capacity, stage_abundance, occupied_indices),
                 "Negative transition rates returned by user-defined density dependence function")
})
