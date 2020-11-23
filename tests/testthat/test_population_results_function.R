context("Population Results (function)")

test_that("setup result functions", {
  coordinates = array(c(1:4, 4:1), c(7, 2))
  initial_abundance <- matrix(c(7, 13, 0, 26, 0, 39, 47,
                                2,  0, 6,  8, 0, 12, 13,
                                0,  3, 4,  6, 0,  9, 10), nrow = 3, ncol = 7, byrow = TRUE)
  results_selection <- c("abundance", "ema", "extirpation", "extinction_location", "harvested", "occupancy")
  # Included stages combined
  result_functions <- population_results(replicates = 1, time_steps = 10, coordinates, initial_abundance,
                                         results_selection = results_selection, result_stages = c(0, 1, 1))
  expect_named(result_functions, c("initialize_attributes", "initialize_replicate", "calculate_at_timestep",
                                   "calculate_at_replicate", "finalize_attributes"))
  expect_is(result_functions$initialize_attributes, "function")
  expect_null(formals(result_functions$initialize_attributes))
  expect_is(result_functions$initialize_replicate, "function")
  expect_named(formals(result_functions$initialize_replicate), c("results"))
  expect_is(result_functions$calculate_at_timestep, "function")
  expect_named(formals(result_functions$calculate_at_timestep), c("r", "tm", "stage_abundance", "harvested", "results"))
  expect_is(result_functions$calculate_at_replicate, "function")
  expect_named(formals(result_functions$calculate_at_replicate), c("r", "stage_abundance", "results"))
  expect_is(result_functions$finalize_attributes, "function")
  expect_named(formals(result_functions$finalize_attributes), c("results"))
  initial_abundance_count <- as.array(colSums(initial_abundance[c(2, 3),]))
  for (i in 1:length(result_functions)) {
    expect_equal(environment(result_functions[[i]])[["result_stage_indices"]], c(2, 3))
    expect_null(environment(result_functions[[i]])[["separated_stage_indices"]])
    expect_equal(environment(result_functions[[i]])[["result_stages"]], 2)
    expect_equal(environment(result_functions[[i]])[["populations"]], 7)
    expect_equal(environment(result_functions[[i]])[["initial_abundance_count"]], initial_abundance_count)
    expect_equal(environment(result_functions[[i]])[["initial_abundance_count_min"]], sum(initial_abundance_count))
  }
  # Separated stage combinations
  result_functions <- population_results(replicates = 1, time_steps = 10, coordinates, initial_abundance,
                                         results_selection = results_selection, result_stages = c(j = 1, a = 2, s = 2))
  initial_abundance_count <- as.array(colSums(initial_abundance))
  expect_equal(environment(result_functions[[1]])[["result_stage_indices"]], c(j = 1, a = 2, s = 3))
  expect_equal(environment(result_functions[[1]])[["separated_stage_indices"]], list(j = c(j = 1), a.s = c(a = 2, s = 3)))
  expect_equal(environment(result_functions[[1]])[["result_stages"]], 3)
  expect_equal(environment(result_functions[[1]])[["populations"]], 7)
  expect_equal(environment(result_functions[[1]])[["initial_abundance_count"]], initial_abundance_count)
  expect_equal(environment(result_functions[[1]])[["initial_abundance_count_min"]], sum(initial_abundance_count))
})

test_that("initialize attributes", {
  coordinates = array(c(1:4, 4:1), c(7, 2))
  initial_abundance <- matrix(c(7, 13, 0, 26, 0, 39, 47,
                                2,  0, 6,  8, 0, 12, 13,
                                0,  3, 4,  6, 0,  9, 10), nrow = 3, ncol = 7, byrow = TRUE)
  results_selection <- c("abundance", "ema", "extirpation", "extinction_location", "harvested", "occupancy")
  # Single replicate and included stages combined
  result_functions <- population_results(replicates = 1, time_steps = 10, coordinates, initial_abundance,
                                         results_selection = results_selection, result_stages = c(0, 1, 1))
  expect_equal(result_functions$initialize_attributes(),
               list(all = list(abundance = array(0, 10), ema = array(0, 10), extirpation = array(as.numeric(NA), 1),
                               extinction_location = array(as.numeric(NA), c(1, 2), dimnames = list(NULL, c("x", "y"))),
                               harvested = array(0, 10), occupancy = array(0, 10)),
                    abundance = array(0, c(7, 10)),
                    extirpation = array(c(NA, NA, NA, NA, 0, NA, NA), 7),
                    last_occupied_abundance_count = as.array(colSums(initial_abundance[c(2, 3),])),
                    harvested = array(0, c(7, 10))))
  # Multiple replicates and separated stage combinations
  results_selection <- c("abundance", "ema", "extirpation", "extinction_location", "harvested", "occupancy", "replicate") # "summarize" (default) or "replicate"
  result_functions <- population_results(replicates = 2, time_steps = 10, coordinates, initial_abundance,
                                         results_selection = results_selection, result_stages = c(j = 1, a = 2, s = 2))
  expect_equal(result_functions$initialize_attributes(),
               list(all = list(abundance = array(0, c(10, 2)),
                               abundance_stages = list(j = array(0, c(10, 2)), a.s = array(0, c(10, 2))),
                               ema = array(0, 10), extirpation = array(as.numeric(NA), 2),
                               extinction_location = array(as.numeric(NA), c(2, 2), dimnames = list(NULL, c("x", "y"))),
                               harvested = array(0, c(10, 2)),
                               harvested_stages = list(j = array(0, c(10, 2)), a.s = array(0, c(10, 2))),
                               occupancy = array(0, c(10, 2))),
                    abundance = array(0, c(7, 10, 2)),
                    abundance_stages = list(j = array(0, c(7, 10, 2)), a.s = array(0, c(7, 10, 2))),
                    extirpation = array(c(NA, NA, NA, NA, 0, NA, NA), c(7, 2)),
                    last_occupied_abundance_count = as.array(colSums(initial_abundance)),
                    harvested = array(0, c(7, 10, 2)),
                    harvested_stages = list(j = array(0, c(7, 10, 2)), a.s = array(0, c(7, 10, 2)))))
  # Summarized replicates and separated stage combinations
  results_selection <- c("abundance", "ema", "extirpation", "extinction_location", "harvested", "occupancy", "summarize")
  result_functions <- population_results(replicates = 3, time_steps = 10, coordinates, initial_abundance,
                                         results_selection = results_selection, result_stages = c(j = 1, a = 2, s = 2))
  expect_equal(result_functions$initialize_attributes(),
               list(all = list(abundance = list(mean = array(0, 10), sd = array(0, 10),
                                                min = array(0, 10), max = array(0, 10)),
                               abundance_stages = list(j = list(mean = array(0, 10), sd = array(0, 10),
                                                                min = array(0, 10), max = array(0, 10)),
                                                       a.s = list(mean = array(0, 10), sd = array(0, 10),
                                                                  min = array(0, 10), max = array(0, 10))),
                               ema = array(0, 10), extirpation = array(as.numeric(NA), 3),
                               extinction_location = array(as.numeric(NA), c(3, 2), dimnames = list(NULL, c("x", "y"))),
                               harvested = list(mean = array(0, 10), sd = array(0, 10),
                                                min = array(0, 10), max = array(0, 10)),
                               harvested_stages = list(j = list(mean = array(0, 10), sd = array(0, 10),
                                                                min = array(0, 10), max = array(0, 10)),
                                                       a.s = list(mean = array(0, 10), sd = array(0, 10),
                                                                  min = array(0, 10), max = array(0, 10))),
                               occupancy = list(mean = array(0, 10), sd = array(0, 10),
                                                min = array(0, 10), max = array(0, 10))),
                    abundance = list(mean = array(0, c(7, 10)), sd = array(0, c(7, 10)),
                                     min = array(0, c(7, 10)), max = array(0, c(7, 10))),
                    abundance_stages = list(j = list(mean = array(0, c(7, 10)), sd = array(0, c(7, 10)),
                                                     min = array(0, c(7, 10)), max = array(0, c(7, 10))),
                                            a.s = list(mean = array(0, c(7, 10)), sd = array(0, c(7, 10)),
                                                       min = array(0, c(7, 10)), max = array(0, c(7, 10)))),
                    extirpation = array(c(NA, NA, NA, NA, 0, NA, NA), c(7, 3)),
                    last_occupied_abundance_count = as.array(colSums(initial_abundance)),
                    harvested = list(mean = array(0, c(7, 10)), sd = array(0, c(7, 10)),
                                     min = array(0, c(7, 10)), max = array(0, c(7, 10))),
                    harvested_stages = list(j = list(mean = array(0, c(7, 10)), sd = array(0, c(7, 10)),
                                                     min = array(0, c(7, 10)), max = array(0, c(7, 10))),
                                            a.s = list(mean = array(0, c(7, 10)), sd = array(0, c(7, 10)),
                                                       min = array(0, c(7, 10)), max = array(0, c(7, 10))))))
})

test_that("initialize replicate", {
  coordinates = array(c(1:4, 4:1), c(7, 2))
  initial_abundance <- matrix(c(7, 13, 0, 26, 0, 39, 47,
                                2,  0, 6,  8, 0, 12, 13,
                                0,  3, 4,  6, 0,  9, 10), nrow = 3, ncol = 7, byrow = TRUE)
  results_selection <- c("abundance", "ema", "extirpation", "extinction_location", "harvested", "occupancy")
  result_functions <- population_results(replicates = 1, time_steps = 10, coordinates, initial_abundance,
                                         results_selection = results_selection, result_stages = c(0, 1, 1))
  expect_equal(result_functions$initialize_replicate(list()),
               list(abundance_count_min = sum(initial_abundance[2:3,]),
                    last_occupied_abundance_count = array(colSums(initial_abundance[2:3,]))))
})

test_that("calculate at timestep (replicate)", {
  coordinates = array(c(1:4, 4:1), c(7, 2))
  initial_abundance <- matrix(c(7, 13, 0, 26, 0, 39, 47,
                                2,  0, 6,  8, 0, 12, 13,
                                0,  3, 4,  6, 0,  9, 10), nrow = 3, ncol = 7, byrow = TRUE)
  harvested <- round(initial_abundance*0.3)
  results_selection <- c("abundance", "ema", "extirpation", "extinction_location", "harvested", "occupancy")
  # Single replicate and included stages combined
  result_functions <- population_results(replicates = 1, time_steps = 10, coordinates, initial_abundance,
                                         results_selection = results_selection, result_stages = c(0, 1, 1))
  results <- result_functions$initialize_replicate(result_functions$initialize_attributes())
  expected_results <- results
  expected_results$all$abundance[2] <- sum(initial_abundance[2:3,])
  expected_results$all$ema[2] <- sum(initial_abundance[2:3,])
  expected_results$all$harvested[2] <- sum(harvested[2:3,])
  expected_results$all$occupancy[2] <- sum(+(colSums(initial_abundance[2:3,]) > 0))
  expected_results$abundance[, 2] <- colSums(initial_abundance[2:3,])
  expected_results$harvested[, 2] <- colSums(harvested[2:3,])
  expect_equal(result_functions$calculate_at_timestep(r = 1, tm = 2, stage_abundance = initial_abundance,
                                                      harvested, results), expected_results)
  # Calculate abundance and harvested separately
  expected_results_abundance <- expected_results
  expected_results_abundance$all$harvested <- results$all$harvested
  expected_results_abundance$harvested <- results$harvested
  expect_equal(result_functions$calculate_at_timestep(r = 1, tm = 2, stage_abundance = initial_abundance,
                                                      harvested = NULL, results), expected_results_abundance)
  expected_results_harvested <- results
  expected_results_harvested$all$harvested <- expected_results$all$harvested
  expected_results_harvested$harvested <- expected_results$harvested
  expect_equal(result_functions$calculate_at_timestep(r = 1, tm = 2, stage_abundance = NULL,
                                                      harvested, results), expected_results_harvested)
  # Multiple replicates and separated stage combinations
  results_selection <- c("abundance", "ema", "extirpation", "extinction_location", "harvested", "occupancy", "replicate") # "summarize" (default) or "replicate"
  result_functions <- population_results(replicates = 2, time_steps = 10, coordinates, initial_abundance,
                                         results_selection = results_selection, result_stages = c(j = 1, a = 2, s = 2))
  results <- result_functions$initialize_replicate(result_functions$initialize_attributes())
  results <- result_functions$calculate_at_timestep(r = 1, tm = 3, stage_abundance = initial_abundance,
                                                    harvested, results)
  results <- result_functions$initialize_replicate(results)
  expected_results <- results
  abundance_r2 <- initial_abundance
  abundance_r2[, 1:6] <- 0
  expected_results$all$abundance[3, 2] <- sum(abundance_r2)
  expected_results$all$abundance_stages$j[3, 2] <- sum(abundance_r2[1,])
  expected_results$all$abundance_stages$a.s[3, 2] <- sum(abundance_r2[2:3,])
  expected_results$all$ema[3] <- mean(c(sum(initial_abundance), sum(abundance_r2)))
  expected_results$all$harvested[3, 2] <- sum(harvested)
  expected_results$all$harvested_stages$j[3, 2] <- sum(harvested[1,])
  expected_results$all$harvested_stages$a.s[3, 2] <- sum(harvested[2:3,])
  expected_results$all$occupancy[3, 2] <- sum(+(colSums(abundance_r2) > 0))
  expected_results$abundance[, 3, 2] <- colSums(abundance_r2)
  expected_results$abundance_stages$j[, 3, 2] <- abundance_r2[1,]
  expected_results$abundance_stages$a.s[, 3, 2] <- colSums(abundance_r2[2:3,])
  expected_results$extirpation[c(1:4, 6), 2] <- 3
  expected_results$last_occupied_abundance_count <- array(colSums(abundance_r2))
  expected_results$abundance_count_min <- sum(abundance_r2)
  expected_results$harvested[, 3, 2] <- colSums(harvested)
  expected_results$harvested_stages$j[, 3, 2] <- harvested[1,]
  expected_results$harvested_stages$a.s[, 3, 2] <- colSums(harvested[2:3,])
  results_2 <- result_functions$calculate_at_timestep(r = 2, tm = 3, stage_abundance = abundance_r2, harvested, results)
  expect_equal(results_2, expected_results)
  expected_results_2 <- expected_results
  abundance_t4 <- abundance_r2*0
  expected_results_2$all$extirpation[2] <- 4
  expected_results_2$extirpation[7, 2] <- 4
  expected_results_2$abundance_count_min <- 0
  expect_equal(result_functions$calculate_at_timestep(r = 2, tm = 4, stage_abundance = abundance_t4,
                                                      harvested*0, results_2), expected_results_2)
  # Calculate abundance and harvested separately
  expected_results_abundance <- expected_results
  expected_results_abundance$all$harvested <- results$all$harvested
  expected_results_abundance$all$harvested_stages <- results$all$harvested_stages
  expected_results_abundance$harvested <- results$harvested
  expected_results_abundance$harvested_stages <- results$harvested_stages
  results_2_a <- result_functions$calculate_at_timestep(r = 2, tm = 3, stage_abundance = abundance_r2, NULL, results)
  expect_equal(results_2_a, expected_results_abundance)
  expected_results_abundance <- expected_results_2
  expected_results_abundance$all$harvested <- expected_results$all$harvested
  expected_results_abundance$all$harvested_stages <- expected_results$all$harvested_stages
  expected_results_abundance$harvested <- expected_results$harvested
  expected_results_abundance$harvested_stages <- expected_results$harvested_stages
  expect_equal(result_functions$calculate_at_timestep(r = 2, tm = 4, stage_abundance = abundance_t4, NULL, results_2),
               expected_results_2)
  expected_results_harvested <- results
  expected_results_harvested$all$harvested <- expected_results$all$harvested
  expected_results_harvested$all$harvested_stages <- expected_results$all$harvested_stages
  expected_results_harvested$harvested <- expected_results$harvested
  expected_results_harvested$harvested_stages <- expected_results$harvested_stages
  expect_equal(result_functions$calculate_at_timestep(r = 2, tm = 3, stage_abundance = NULL, harvested, results),
               expected_results_harvested)
})

test_that("calculate at timestep (summarize)", {
  coordinates = array(c(1:4, 4:1), c(7, 2))
  initial_abundance <- matrix(c(7, 13, 0, 26, 0, 39, 47,
                                2,  0, 6,  8, 0, 12, 13,
                                0,  3, 4,  6, 0,  9, 10), nrow = 3, ncol = 7, byrow = TRUE)
  harvested <- round(initial_abundance*0.3)
  results_selection <- c("abundance", "ema", "extirpation", "extinction_location", "harvested", "occupancy", "summarize")
  # Summarized replicates and separated stage combinations
  result_functions <- population_results(replicates = 3, time_steps = 10, coordinates, initial_abundance,
                                         results_selection = results_selection, result_stages = c(j = 1, a = 2, s = 2))
  results <- result_functions$initialize_replicate(result_functions$initialize_attributes())
  # Calculate expected results
  expected_results <- results
  expected_results$all$abundance$mean[3] <- sum(initial_abundance + 1)
  expected_results$all$abundance$sd[3] <- stats::sd(c(205, 226, 247))^2*(3 - 1)
  expected_results$all$abundance$min[3] <- sum(initial_abundance)
  expected_results$all$abundance$max[3] <- sum(initial_abundance + 2)
  expected_results$all$abundance_stages$j$mean[3] <- sum(initial_abundance[1,] + 1)
  expected_results$all$abundance_stages$j$sd[3] <- stats::sd(c(132, 139, 146))^2*(3 - 1)
  expected_results$all$abundance_stages$j$min[3] <- sum(initial_abundance[1,])
  expected_results$all$abundance_stages$j$max[3] <- sum(initial_abundance[1,] + 2)
  expected_results$all$abundance_stages$a.s$mean[3] <- sum(initial_abundance[2:3,] + 1)
  expected_results$all$abundance_stages$a.s$sd[3] <- stats::sd(c(73, 87, 101))^2*(3 - 1)
  expected_results$all$abundance_stages$a.s$min[3] <- sum(initial_abundance[2:3,])
  expected_results$all$abundance_stages$a.s$max[3] <- sum(initial_abundance[2:3,] + 2)
  expected_results$all$ema[3] <- sum(initial_abundance)
  expected_results$all$harvested$mean[3] <- sum(harvested + 1)
  expected_results$all$harvested$sd[3] <- stats::sd(c(63, 84, 105))^2*(3 - 1)
  expected_results$all$harvested$min[3] <- sum(harvested)
  expected_results$all$harvested$max[3] <- sum(harvested + 2)
  expected_results$all$harvested_stages$j$mean[3] <- sum(harvested[1,] + 1)
  expected_results$all$harvested_stages$j$sd[3] <- stats::sd(c(40, 47, 54))^2*(3 - 1)
  expected_results$all$harvested_stages$j$min[3] <- sum(harvested[1,])
  expected_results$all$harvested_stages$j$max[3] <- sum(harvested[1,] + 2)
  expected_results$all$harvested_stages$a.s$mean[3] <- sum(harvested[2:3,] + 1)
  expected_results$all$harvested_stages$a.s$sd[3] <- stats::sd(c(23, 37, 51))^2*(3 - 1)
  expected_results$all$harvested_stages$a.s$min[3] <- sum(harvested[2:3,])
  expected_results$all$harvested_stages$a.s$max[3] <- sum(harvested[2:3,] + 2)
  expected_results$all$occupancy$mean[3] <- mean(c(6, 7, 7))
  expected_results$all$occupancy$sd[3] <- stats::sd(c(6, 7, 7))^2*(3 - 1)
  expected_results$all$occupancy$min[3] <- sum(+(colSums(initial_abundance) > 0))
  expected_results$all$occupancy$max[3] <- sum(+(colSums(initial_abundance + 2) > 0))
  expected_results$abundance$mean[, 3] <- colSums(initial_abundance + 1)
  expected_results$abundance$sd[, 3] <- apply(apply(matrix(0:2), 1, function (a) colSums(initial_abundance + a)),
                                              1, stats::sd)^2*(3 - 1)
  expected_results$abundance$min[, 3] <- colSums(initial_abundance)
  expected_results$abundance$max[, 3] <- colSums(initial_abundance + 2)
  expected_results$abundance_stages$j$mean[, 3] <- initial_abundance[1,] + 1
  expected_results$abundance_stages$j$sd[, 3] <- apply(apply(matrix(0:2), 1, function (a) initial_abundance[1,] + a),
                                                       1, stats::sd)^2*(3 - 1)
  expected_results$abundance_stages$j$min[, 3] <- initial_abundance[1,]
  expected_results$abundance_stages$j$max[, 3] <- initial_abundance[1,] + 2
  expected_results$abundance_stages$a.s$mean[, 3] <- colSums(initial_abundance[2:3,] + 1)
  expected_results$abundance_stages$a.s$sd[, 3] <- apply(apply(matrix(0:2), 1, function (a) colSums(initial_abundance[2:3,] + a)),
                                                         1, stats::sd)^2*(3 - 1)
  expected_results$abundance_stages$a.s$min[, 3] <- colSums(initial_abundance[2:3,])
  expected_results$abundance_stages$a.s$max[, 3] <- colSums(initial_abundance[2:3,] + 2)
  expected_results$extirpation[5, 2:3] <- NA
  expected_results$harvested$mean[, 3] <- colSums(harvested + 1)
  expected_results$harvested$sd[, 3] <- apply(apply(matrix(0:2), 1, function (a) colSums(harvested + a)),
                                              1, stats::sd)^2*(3 - 1)
  expected_results$harvested$min[, 3] <- colSums(harvested)
  expected_results$harvested$max[, 3] <- colSums(harvested + 2)
  expected_results$harvested_stages$j$mean[, 3] <- harvested[1,] + 1
  expected_results$harvested_stages$j$sd[, 3] <- apply(apply(matrix(0:2), 1, function (a) harvested[1,] + a),
                                                       1, stats::sd)^2*(3 - 1)
  expected_results$harvested_stages$j$min[, 3] <- harvested[1,]
  expected_results$harvested_stages$j$max[, 3] <- harvested[1,] + 2
  expected_results$harvested_stages$a.s$mean[, 3] <- colSums(harvested[2:3,] + 1)
  expected_results$harvested_stages$a.s$sd[, 3] <- apply(apply(matrix(0:2), 1, function (a) colSums(harvested[2:3,] + a)),
                                                         1, stats::sd)^2*(3 - 1)
  expected_results$harvested_stages$a.s$min[, 3] <- colSums(harvested[2:3,])
  expected_results$harvested_stages$a.s$max[, 3] <- colSums(harvested[2:3,] + 2)
  expected_results$last_occupied_abundance_count <- array(colSums(initial_abundance + 2))
  # Run three replicates of a single time step
  results <- result_functions$calculate_at_timestep(r = 1, tm = 3, stage_abundance = initial_abundance,
                                                    harvested, results)
  results <- result_functions$initialize_replicate(results)
  results <- result_functions$calculate_at_timestep(r = 2, tm = 3, stage_abundance = initial_abundance + 1,
                                                    harvested + 1, results)
  results <- result_functions$initialize_replicate(results)
  expect_equal(result_functions$calculate_at_timestep(r = 3, tm = 3, stage_abundance = initial_abundance + 2,
                                                      harvested + 2, results), expected_results)
  # Calculate abundance and harvested separately
  expected_results_abundance <- expected_results
  expected_results_abundance$all$harvested <- results$all$harvested
  expected_results_abundance$all$harvested_stages <- results$all$harvested_stages
  expected_results_abundance$harvested <- results$harvested
  expected_results_abundance$harvested_stages <- results$harvested_stages
  expect_equal(result_functions$calculate_at_timestep(r = 3, tm = 3, stage_abundance = initial_abundance + 2,
                                                      NULL, results), expected_results_abundance)
  expected_results_harvested <- results
  expected_results_harvested$all$harvested <- expected_results$all$harvested
  expected_results_harvested$all$harvested_stages <- expected_results$all$harvested_stages
  expected_results_harvested$harvested <- expected_results$harvested
  expected_results_harvested$harvested_stages <- expected_results$harvested_stages

  expect_equal(result_functions$calculate_at_timestep(r = 3, tm = 3, NULL, harvested + 2, results),
               expected_results_harvested)
})

test_that("calculate at replicate", {
  coordinates = array(c(1:4, 4:1), c(7, 2))
  initial_abundance <- matrix(c(7, 13, 0, 26, 0, 39, 47,
                                0,  0, 6,  8, 0,  0,  0,
                                0,  0, 4,  6, 0,  0,  0), nrow = 3, ncol = 7, byrow = TRUE)
  results_selection <- c("extinction_location")
  # Single replicate and included stages combined
  result_functions <- population_results(replicates = 1, time_steps = 10, coordinates, initial_abundance,
                                         results_selection = results_selection, result_stages = c(0, 1, 1))
  results <- result_functions$initialize_replicate(result_functions$initialize_attributes())
  results <- result_functions$initialize_replicate(results)
  expected_results <- results
  expected_results$all$extinction_location[1,] <- 10/24*coordinates[3,] + 14/24*coordinates[4,]
  stage_abundance <- initial_abundance
  stage_abundance[2:3,] <- 0
  expect_equal(result_functions$calculate_at_replicate(1, stage_abundance, results), expected_results)
  # Multiple replicates
  result_functions <- population_results(replicates = 3, time_steps = 10, coordinates, initial_abundance,
                                         results_selection = results_selection, result_stages = c(0, 1, 1))
  results <- result_functions$initialize_replicate(result_functions$initialize_attributes())
  results <- result_functions$initialize_replicate(results)
  expected_results <- results
  expected_results$all$extinction_location[1,] <- 10/24*coordinates[3,] + 14/24*coordinates[4,]
  expected_results$all$extinction_location[2,] <- coordinates[4,]
  expected_results$last_occupied_abundance_count[3] <- 0
  results <- result_functions$calculate_at_replicate(1, stage_abundance, results)
  results$last_occupied_abundance_count[3] <- 0
  expect_equal(result_functions$calculate_at_replicate(2, stage_abundance, results), expected_results)
})

test_that("finalize_attributes", {
  coordinates = array(c(1:4, 4:1), c(7, 2))
  initial_abundance <- matrix(c(7, 13, 0, 26, 0, 39, 47,
                                2,  0, 6,  8, 0, 12, 13,
                                0,  3, 4,  6, 0,  9, 10), nrow = 3, ncol = 7, byrow = TRUE)
  harvested <- round(initial_abundance*0.3)
  results_selection <- c("abundance", "ema", "extirpation", "extinction_location", "harvested", "occupancy", "summarize")
  # Summarized replicates and separated stage combinations
  result_functions <- population_results(replicates = 5, time_steps = 10, coordinates, initial_abundance,
                                         results_selection = results_selection, result_stages = c(j = 1, a = 2, s = 2))
  results <- result_functions$initialize_replicate(result_functions$initialize_attributes())
  # Run 5 replicates of a single time step
  abundance <- list(initial_abundance, initial_abundance, initial_abundance, initial_abundance, initial_abundance*0)
  abundance[[2]][, 1] <- 0
  abundance[[3]][, 1:2] <- 0
  abundance[[4]][, 1:3] <- 0
  for (i in 1:5) {
    results <- result_functions$initialize_replicate(results)
    results <- result_functions$calculate_at_timestep(r = i, tm = 3, stage_abundance = abundance[[i]],
                                                      harvested + i, results)
  }
  expected_results <- results
  expected_results$all$abundance$sd[3] <- sqrt(expected_results$all$abundance$sd[3]/(5 - 1))
  expected_results$all$abundance_stages$j$sd[3] <- sqrt(expected_results$all$abundance_stages$j$sd[3]/(5 - 1))
  expected_results$all$abundance_stages$a.s$sd[3] <- sqrt(expected_results$all$abundance_stages$a.s$sd[3]/(5 - 1))
  expected_results$all$harvested$sd[3] <- sqrt(expected_results$all$harvested$sd[3]/(5 - 1))
  expected_results$all$harvested_stages$j$sd[3] <- sqrt(expected_results$all$harvested_stages$j$sd[3]/(5 - 1))
  expected_results$all$harvested_stages$a.s$sd[3] <- sqrt(expected_results$all$harvested_stages$a.s$sd[3]/(5 - 1))
  expected_results$all$occupancy$sd[3] <- sqrt(expected_results$all$occupancy$sd[3]/(5 - 1))
  expected_results$abundance$sd[, 3] <- sqrt(expected_results$abundance$sd[, 3]/(5 - 1))
  expected_results$abundance_stages$j$sd[, 3] <- sqrt(expected_results$abundance_stages$j$sd[, 3]/(5 - 1))
  expected_results$abundance_stages$a.s$sd[, 3] <- sqrt(expected_results$abundance_stages$a.s$sd[, 3]/(5 - 1))
  expected_results$harvested$sd[, 3] <- sqrt(expected_results$harvested$sd[, 3]/(5 - 1))
  expected_results$harvested_stages$j$sd[, 3] <- sqrt(expected_results$harvested_stages$j$sd[, 3]/(5 - 1))
  expected_results$harvested_stages$a.s$sd[, 3] <- sqrt(expected_results$harvested_stages$a.s$sd[, 3]/(5 - 1))
  expected_results$occupancy$sd[, 3] <- sqrt(expected_results$occupancy$sd[, 3]/(5 - 1))
  expected_results$extirpation[which(is.na(expected_results$extirpation))] <- Inf
  expected_results$extirpation <- apply(expected_results$extirpation, 1, stats::fivenum)
  expected_results$extirpation[which(is.infinite(expected_results$extirpation))] <- NA
  expected_results$extirpation <- list(min = expected_results$extirpation[1,],
                                       q1 = expected_results$extirpation[2,],
                                       median = expected_results$extirpation[3,],
                                       q3 = expected_results$extirpation[4,],
                                       max = expected_results$extirpation[5,])
  expected_results$abundance_count_min <- NULL
  expected_results$last_occupied_abundance_count <- NULL
  expect_equal(result_functions$finalize_attributes(results), expected_results)
  # Full extirpation
  results <- result_functions$initialize_replicate(result_functions$initialize_attributes())
  for (i in 1:5) { # Run 5 replicates of a single time step
    results <- result_functions$initialize_replicate(results)
    results <- result_functions$calculate_at_timestep(r = i, tm = 3, stage_abundance = abundance[[i]],
                                                      harvested + i, results)
    results <- result_functions$calculate_at_timestep(r = i, tm = 5, stage_abundance = abundance[[i]]*0,
                                                      harvested + i, results)
  }
  expected_extirpation <- results$extirpation
  expected_extirpation <- list(mean = apply(expected_extirpation, 1, mean),
                               sd = apply(expected_extirpation, 1, stats::sd),
                               min = apply(expected_extirpation, 1, min),
                               max = apply(expected_extirpation, 1, max))
  expect_equal(result_functions$finalize_attributes(results)[["extirpation"]], expected_extirpation)
})
