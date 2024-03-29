context("Population Transitions (function)")

test_that("setup function", {
  fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
  survival_matrix <- array(c(0, 0.5, 0, 0, 0, 0.7, 0, 0, 0.8), c(3, 3))
  transition_function <- population_transitions(
    populations = 7, demographic_stochasticity = FALSE,
    fecundity_matrix = fecundity_matrix, fecundity_max = NULL,
    survival_matrix = survival_matrix
  )
  expect_is(transition_function, "function")
  expect_named(formals(transition_function), c("fecundity_array", "survival_array", "stage_abundance", "occupied_indices"))
})

test_that("deterministic transition", {
  fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
  survival_matrix <- array(c(0, 0.5, 0, 0, 0, 0.7, 0, 0, 0.8), c(3, 3))
  variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
  fecundity_array <- array(fecundity_matrix, c(3, 3, 7)) * variation_array
  survival_array <- array(survival_matrix, c(3, 3, 7)) * variation_array
  stage_abundance <- matrix(c(
    7, 13, 0, 26, 0, 39, 47,
    2, 0, 6, 8, 0, 12, 13,
    0, 3, 4, 6, 0, 9, 10
  ), nrow = 3, ncol = 7, byrow = TRUE)
  occupied_indices <- (1:7)[-5]
  expected_stage_abundance <- array(0, c(3, 7))
  for (i in occupied_indices) {
    abundance_matrix <- matrix(stage_abundance[, i], nrow = 3, ncol = 3, byrow = TRUE)
    expected_stage_abundance[, i] <- rowSums(round(fecundity_array[, , i] * abundance_matrix) +
      round(survival_array[, , i] * abundance_matrix))
  }
  transition_function <- population_transitions(
    populations = 7, demographic_stochasticity = FALSE,
    fecundity_matrix = fecundity_matrix, fecundity_max = NULL,
    survival_matrix = survival_matrix
  )
  expect_equal(
    transition_function(fecundity_array, survival_array, stage_abundance, occupied_indices),
    expected_stage_abundance
  )
  # with maximum fecundity
  expected_stage_abundance <- array(0, c(3, 7))
  for (i in occupied_indices) {
    abundance_matrix <- matrix(stage_abundance[, i], nrow = 3, ncol = 3, byrow = TRUE)
    expected_stage_abundance[, i] <- rowSums(pmin(round(fecundity_array[, , i] * abundance_matrix), floor(3.5 * abundance_matrix)) +
      round(survival_array[, , i] * abundance_matrix))
  }
  transition_function <- population_transitions(
    populations = 7, demographic_stochasticity = FALSE,
    fecundity_matrix = fecundity_matrix, fecundity_max = 3.5,
    survival_matrix = survival_matrix
  )
  expect_equal(
    transition_function(fecundity_array, survival_array, stage_abundance, occupied_indices),
    expected_stage_abundance
  )
})

test_that("demographic stochasticity transition", {
  fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
  survival_matrix <- array(c(0, 0.5, 0, 0, 0, 0.7, 0, 0, 0.8), c(3, 3))
  variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
  fecundity_array <- array(fecundity_matrix, c(3, 3, 7)) * variation_array
  survival_array <- array(survival_matrix, c(3, 3, 7)) * variation_array
  stage_abundance <- matrix(c(
    7, 13, 0, 26, 0, 39, 47,
    2, 0, 6, 8, 0, 12, 13,
    0, 3, 4, 6, 0, 9, 10
  ), nrow = 3, ncol = 7, byrow = TRUE)
  occupied_indices <- (1:7)[-5]
  expected_stage_abundance <- array(0, c(3, 7))
  set.seed(123)
  for (i in occupied_indices) {
    abundance_matrix <- matrix(stage_abundance[, i], nrow = 3, ncol = 3, byrow = TRUE)
    expected_stage_abundance[, i] <- .rowSums(stats::rpois(9, fecundity_array[, , i] * abundance_matrix), m = 3, n = 3)
  }
  for (i in occupied_indices) { # separate loop for random order consistent with function
    abundance_matrix <- matrix(stage_abundance[, i], nrow = 3, ncol = 3, byrow = TRUE)
    expected_stage_abundance[, i] <- expected_stage_abundance[, i] +
      .rowSums(stats::rbinom(9, abundance_matrix, survival_array[, , i]), m = 3, n = 3)
  }
  transition_function <- population_transitions(
    populations = 7, demographic_stochasticity = TRUE,
    fecundity_matrix = fecundity_matrix, fecundity_max = NULL,
    survival_matrix = survival_matrix
  )
  set.seed(123)
  expect_equal(
    transition_function(fecundity_array, survival_array, stage_abundance, occupied_indices),
    expected_stage_abundance
  )
  # with maximum fecundity
  set.seed(123)
  transition_function <- population_transitions(
    populations = 7, demographic_stochasticity = TRUE,
    fecundity_matrix = fecundity_matrix, fecundity_max = 4.5,
    survival_matrix = survival_matrix
  )
  fecundity_lookup <- environment(transition_function)[["fecundity_lookup"]]
  fecundity_shifted <- fecundity_array * 0
  fecundity_indices <- which(fecundity_array > 0)
  fecundity_shifted[fecundity_indices] <-
    apply(
      matrix(1:length(fecundity_indices)), 1,
      function(i) {
        fecundity_lookup$shifted[which.min(abs(fecundity_lookup$original -
          fecundity_array[fecundity_indices][i]))]
      }
    )
  expected_stage_abundance <- array(0, c(3, 7))
  set.seed(123)
  for (i in occupied_indices) {
    abundance_matrix <- matrix(stage_abundance[, i], nrow = 3, ncol = 3, byrow = TRUE)
    expected_stage_abundance[, i] <- .rowSums(apply(
      matrix(1:9), 1,
      function(j) {
        sum(pmin(
          stats::rpois(
            abundance_matrix[j],
            fecundity_shifted[, , i][j]
          ),
          4.5
        ))
      }
    ), m = 3, n = 3)
  }
  for (i in occupied_indices) { # separate loop for random order consistent with function
    abundance_matrix <- matrix(stage_abundance[, i], nrow = 3, ncol = 3, byrow = TRUE)
    expected_stage_abundance[, i] <- expected_stage_abundance[, i] +
      .rowSums(stats::rbinom(9, abundance_matrix, survival_array[, , i]), m = 3, n = 3)
  }
  set.seed(123)
  expect_equal(
    transition_function(fecundity_array, survival_array, stage_abundance, occupied_indices),
    expected_stage_abundance
  )
})

test_that("handle excessive survivals", {
  fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
  survival_matrix <- array(c(0, 0.5, 0.5, 0, 0.3, 0.7, 0, 0.2, 0.8), c(3, 3))
  variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
  fecundity_array <- array(fecundity_matrix, c(3, 3, 7)) * variation_array
  survival_array <- array(survival_matrix, c(3, 3, 7)) * variation_array
  stage_abundance <- matrix(c(
    7, 13, 0, 26, 0, 39, 47,
    2, 0, 6, 8, 0, 12, 13,
    0, 3, 4, 6, 0, 9, 10
  ), nrow = 3, ncol = 7, byrow = TRUE)
  occupied_indices <- (1:7)[-5]
  survivals <- array(0, c(3, 3, 7))
  for (i in occupied_indices) {
    survivals[, , i] <- round(survival_array[, , i] * matrix(stage_abundance[, i], nrow = 3, ncol = 3, byrow = TRUE))
  }
  set.seed(123)
  for (s in 1:3) {
    stage_survivals <- colSums(survivals[, s, ])
    for (p in which(stage_survivals > stage_abundance[s, ])) {
      sample_indices <- sample(1:3, size = stage_survivals[p] - stage_abundance[s, p], replace = TRUE, prob = survival_array[, s, p])
      for (sample_index in sample_indices) {
        survivals[sample_index, s, p] <- survivals[sample_index, s, p] - 1
      }
    }
  }
  expected_stage_abundance <- array(0, c(3, 7))
  for (i in occupied_indices) {
    abundance_matrix <- matrix(stage_abundance[, i], nrow = 3, ncol = 3, byrow = TRUE)
    expected_stage_abundance[, i] <- rowSums(round(fecundity_array[, , i] * abundance_matrix) + survivals[, , i])
  }
  transition_function <- population_transitions(
    populations = 7, demographic_stochasticity = FALSE,
    fecundity_matrix = fecundity_matrix, fecundity_max = NULL,
    survival_matrix = survival_matrix
  )
  set.seed(123)
  expect_equal(
    transition_function(fecundity_array, survival_array, stage_abundance, occupied_indices),
    expected_stage_abundance
  )
})
