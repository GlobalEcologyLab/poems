context("Population Environmental Stochasticity (function)")

test_that("setup function", {
  fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
  survival_matrix <- array(c(0, 0.5, 0, 0, 0, 0.7, 0, 0, 0.8), c(3, 3))
  standard_deviation <- (fecundity_matrix + survival_matrix) * 0.3
  # No correlation
  env_stoch_function <- population_env_stoch(
    populations = 7, fecundity_matrix, fecundity_max = NULL, survival_matrix,
    standard_deviation, correlation = NULL
  )
  expect_is(env_stoch_function, "function")
  expect_named(formals(env_stoch_function), c("fecundity_array", "survival_array", "occupied_indices"))
  expect_false(environment(env_stoch_function)[["use_env_correlation"]])
  # With correlation
  spatial_correlation <- SpatialCorrelation$new(
    coordinates = array(c(1:4, 4:1), c(7, 2)), correlation_amplitude = 0.6,
    correlation_breadth = 15000, compact_only = FALSE
  )
  spatial_correlation$calculate_correlations(decimals = 6)
  spatial_correlation$calculate_compact_decomposition(threshold = NULL)
  # Environmental correlation matrix
  env_stoch_function <- population_env_stoch(
    populations = 7, fecundity_matrix, fecundity_max = NULL,
    survival_matrix, standard_deviation,
    correlation = list(correlation_matrix = spatial_correlation$correlation_matrix)
  )
  expect_true(environment(env_stoch_function)[["use_env_correlation"]])
  expect_equal(
    environment(env_stoch_function)[["t_decomposition_compact_matrix"]],
    spatial_correlation$t_decomposition_compact_matrix
  )
  expect_equal(
    environment(env_stoch_function)[["t_decomposition_compact_map"]],
    spatial_correlation$t_decomposition_compact_map
  )
  # Pre-calculated transposed (Cholesky) decomposition matrix
  env_stoch_function <- population_env_stoch(
    populations = 7, fecundity_matrix, fecundity_max = NULL,
    survival_matrix, standard_deviation,
    correlation = list(t_decomposition_matrix = spatial_correlation$t_decomposition_matrix)
  )
  expect_true(environment(env_stoch_function)[["use_env_correlation"]])
  expect_equal(
    environment(env_stoch_function)[["t_decomposition_compact_matrix"]],
    spatial_correlation$t_decomposition_compact_matrix
  )
  expect_equal(
    environment(env_stoch_function)[["t_decomposition_compact_map"]],
    spatial_correlation$t_decomposition_compact_map
  )
  # Compact transposed (Cholesky) decomposition matrix and a corresponding map of population indices
  env_stoch_function <- population_env_stoch(
    populations = 7, fecundity_matrix, fecundity_max = NULL,
    survival_matrix, standard_deviation,
    correlation = list(
      t_decomposition_compact_matrix = spatial_correlation$t_decomposition_compact_matrix,
      t_decomposition_compact_map = spatial_correlation$t_decomposition_compact_map
    )
  )
  expect_true(environment(env_stoch_function)[["use_env_correlation"]])
  expect_equal(
    environment(env_stoch_function)[["t_decomposition_compact_matrix"]],
    spatial_correlation$t_decomposition_compact_matrix
  )
  expect_equal(
    environment(env_stoch_function)[["t_decomposition_compact_map"]],
    spatial_correlation$t_decomposition_compact_map
  )
})

test_that("apply environmental stochasticity to transitions", {
  fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
  survival_matrix <- array(c(0, 0.5, 0, 0, 0, 0.7, 0, 0, 0.8), c(3, 3))
  standard_deviation <- (fecundity_matrix + survival_matrix) * 0.3
  variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
  fecundity_array <- array(fecundity_matrix, c(3, 3, 7)) * variation_array
  survival_array <- array(survival_matrix, c(3, 3, 7)) * variation_array
  stage_abundance <- matrix(c(
    7, 13, 0, 26, 0, 39, 47,
    2, 0, 6, 8, 0, 12, 13,
    0, 3, 4, 6, 0, 9, 10
  ), nrow = 3, ncol = 7, byrow = TRUE)
  occupied_indices <- (1:7)[-5]
  # Fecundities sampled from Lognormal distribution
  fecundity_indices <- which(fecundity_array[, , occupied_indices] > 0)
  fecundities <- fecundity_array[, , occupied_indices][fecundity_indices]
  std_devs <- array(standard_deviation, c(3, 3, 7))[, , occupied_indices][fecundity_indices]
  mean_fec_log <- log(fecundities^2 / sqrt(fecundities^2 + std_devs^2))
  sd_fec_log <- sqrt(log(1 + std_devs^2 / fecundities^2))
  # Survivals sampled from Beta distribution
  survival_indices <- which(survival_array[, , occupied_indices] > 0)
  survivals <- survival_array[, , occupied_indices][survival_indices]
  std_devs <- array(standard_deviation, c(3, 3, 7))[, , occupied_indices][survival_indices]
  alpha_surv <- survivals * (survivals * (1 - survivals) / std_devs^2 - 1)
  beta_surv <- (1 - survivals) * (survivals * (1 - survivals) / std_devs^2 - 1)
  # No correlation
  set.seed(123)
  normal_deviates <- stats::rnorm(length(occupied_indices))
  expected_fecundity_array <- fecundity_array
  expected_fecundity_array[, , occupied_indices][fecundity_indices] <-
    stats::qlnorm(stats::pnorm(rep(normal_deviates, each = 2)), meanlog = mean_fec_log, sdlog = sd_fec_log)
  expected_survival_array <- survival_array
  expected_survival_array[, , occupied_indices][survival_indices] <-
    stats::qbeta(stats::pnorm(rep(normal_deviates, each = 3)), shape1 = alpha_surv, shape2 = beta_surv)
  env_stoch_function <- population_env_stoch(
    populations = 7, fecundity_matrix, fecundity_max = NULL, survival_matrix,
    standard_deviation, correlation = NULL
  )
  set.seed(123)
  expect_equal(
    env_stoch_function(fecundity_array, survival_array, occupied_indices),
    list(fecundity_array = expected_fecundity_array, survival_array = expected_survival_array)
  )
  # With correlation
  spatial_correlation <- SpatialCorrelation$new(
    coordinates = array(c(1:4, 4:1), c(7, 2)), correlation_amplitude = 0.6,
    correlation_breadth = 15000, compact_only = FALSE
  )
  spatial_correlation$calculate_correlations(decimals = 6)
  set.seed(123)
  normal_deviates <- (t(chol(spatial_correlation$correlation_matrix)) %*% stats::rnorm(7))[occupied_indices]
  expected_fecundity_array[, , occupied_indices][fecundity_indices] <-
    stats::qlnorm(stats::pnorm(rep(normal_deviates, each = 2)), meanlog = mean_fec_log, sdlog = sd_fec_log)
  expected_survival_array[, , occupied_indices][survival_indices] <-
    stats::qbeta(stats::pnorm(rep(normal_deviates, each = 3)), shape1 = alpha_surv, shape2 = beta_surv)
  env_stoch_function <- population_env_stoch(
    populations = 7, fecundity_matrix, fecundity_max = NULL,
    survival_matrix, standard_deviation,
    correlation = list(correlation_matrix = spatial_correlation$correlation_matrix)
  )
  set.seed(123)
  expect_equal(
    env_stoch_function(fecundity_array, survival_array, occupied_indices),
    list(fecundity_array = expected_fecundity_array, survival_array = expected_survival_array)
  )
  # Handle excessive survivals
  survival_matrix <- array(c(0, 0.5, 0.5, 0, 0.3, 0.7, 0, 0.2, 0.8), c(3, 3))
  standard_deviation <- (fecundity_matrix + survival_matrix) * 0.3
  survival_array <- array(survival_matrix, c(3, 3, 7)) * variation_array
  survival_indices <- which(survival_array[, , occupied_indices] > 0)
  survivals <- survival_array[, , occupied_indices][survival_indices]
  std_devs <- array(standard_deviation, c(3, 3, 7))[, , occupied_indices][survival_indices]
  alpha_surv <- survivals * (survivals * (1 - survivals) / std_devs^2 - 1)
  beta_surv <- (1 - survivals) * (survivals * (1 - survivals) / std_devs^2 - 1)
  expected_survival_array <- survival_array
  expected_survival_array[, , occupied_indices][survival_indices] <-
    stats::qbeta(stats::pnorm(rep(normal_deviates, each = 6)), shape1 = alpha_surv, shape2 = beta_surv)
  total_survivals <- array(.colSums(expected_survival_array[, , occupied_indices], m = 3, n = 3 * 6), c(3, 6))
  excessive_indices <- which(total_survivals > 1, arr.ind = TRUE)
  for (i in 1:nrow(excessive_indices)) {
    expected_survival_array[, , occupied_indices][, excessive_indices[i, 1], excessive_indices[i, 2]] <-
      (expected_survival_array[, , occupied_indices][, excessive_indices[i, 1], excessive_indices[i, 2]] /
        total_survivals[excessive_indices[i, 1], excessive_indices[i, 2]])
  }
  env_stoch_function <- population_env_stoch(
    populations = 7, fecundity_matrix, fecundity_max = NULL,
    survival_matrix, standard_deviation,
    correlation = list(correlation_matrix = spatial_correlation$correlation_matrix)
  )
  set.seed(123)
  expect_equal(
    env_stoch_function(fecundity_array, survival_array, occupied_indices),
    list(fecundity_array = expected_fecundity_array, survival_array = expected_survival_array)
  )
})
