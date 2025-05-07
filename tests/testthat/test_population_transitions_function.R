context("Population Transitions (function)")

test_that("setup function", {
  fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
  survival_matrix <- array(c(0, 0.5, 0, 0, 0, 0.7, 0, 0, 0.8), c(3, 3))
  transition_function <- population_transitions(
    populations = 7,
    demographic_stochasticity = FALSE,
    fecundity_matrix = fecundity_matrix,
    fecundity_max = NULL,
    survival_matrix = survival_matrix
  )
  expect_is(transition_function, "function")
  expect_named(
    formals(transition_function),
    c(
      "fecundity_array",
      "survival_array",
      "stage_abundance",
      "occupied_indices"
    )
  )
})

test_that("deterministic transition", {
  fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
  survival_matrix <- array(c(0, 0.5, 0, 0, 0, 0.7, 0, 0, 0.8), c(3, 3))
  variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
  fecundity_array <- array(fecundity_matrix, c(3, 3, 7)) * variation_array
  survival_array <- array(survival_matrix, c(3, 3, 7)) * variation_array
  stage_abundance <- matrix(
    c(
      7,
      13,
      0,
      26,
      0,
      39,
      47,
      2,
      0,
      6,
      8,
      0,
      12,
      13,
      0,
      3,
      4,
      6,
      0,
      9,
      10
    ),
    nrow = 3,
    ncol = 7,
    byrow = TRUE
  )
  occupied_indices <- (1:7)[-5]
  expected_stage_abundance <- array(0, c(3, 7))
  for (i in occupied_indices) {
    abundance_matrix <- matrix(
      stage_abundance[, i],
      nrow = 3,
      ncol = 3,
      byrow = TRUE
    )
    expected_stage_abundance[, i] <- rowSums(
      round(fecundity_array[,, i] * abundance_matrix) +
        round(survival_array[,, i] * abundance_matrix)
    )
  }
  transition_function <- population_transitions(
    populations = 7,
    demographic_stochasticity = FALSE,
    fecundity_matrix = fecundity_matrix,
    fecundity_max = NULL,
    survival_matrix = survival_matrix
  )
  expect_equal(
    transition_function(
      fecundity_array,
      survival_array,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance
  )
  # with maximum fecundity
  expected_stage_abundance <- array(0, c(3, 7))
  for (i in occupied_indices) {
    abundance_matrix <- matrix(
      stage_abundance[, i],
      nrow = 3,
      ncol = 3,
      byrow = TRUE
    )
    expected_stage_abundance[, i] <- rowSums(
      pmin(
        round(fecundity_array[,, i] * abundance_matrix),
        floor(3.5 * abundance_matrix)
      ) +
        round(survival_array[,, i] * abundance_matrix)
    )
  }
  transition_function <- population_transitions(
    populations = 7,
    demographic_stochasticity = FALSE,
    fecundity_matrix = fecundity_matrix,
    fecundity_max = 3.5,
    survival_matrix = survival_matrix
  )
  expect_equal(
    transition_function(
      fecundity_array,
      survival_array,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance
  )
})

test_that("demographic stochasticity transition", {
  fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
  survival_matrix <- array(c(0, 0.5, 0, 0, 0, 0.7, 0, 0, 0.8), c(3, 3))
  variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
  fecundity_array <- array(fecundity_matrix, c(3, 3, 7)) * variation_array
  survival_array <- array(survival_matrix, c(3, 3, 7)) * variation_array
  stage_abundance <- matrix(
    c(
      7,
      13,
      0,
      26,
      0,
      39,
      47,
      2,
      0,
      6,
      8,
      0,
      12,
      13,
      0,
      3,
      4,
      6,
      0,
      9,
      10
    ),
    nrow = 3,
    ncol = 7,
    byrow = TRUE
  )
  occupied_indices <- (1:7)[-5]
  expected_stage_abundance <- array(0, c(3, 7))
  set.seed(123)
  for (i in occupied_indices) {
    abundance_matrix <- matrix(
      stage_abundance[, i],
      nrow = 3,
      ncol = 3,
      byrow = TRUE
    )
    expected_stage_abundance[, i] <- .rowSums(
      stats::rpois(9, fecundity_array[,, i] * abundance_matrix),
      m = 3,
      n = 3
    )
  }
  for (i in occupied_indices) {
    # separate loop for random order consistent with function
    abundance_matrix <- matrix(
      stage_abundance[, i],
      nrow = 3,
      ncol = 3,
      byrow = TRUE
    )
    expected_stage_abundance[, i] <- expected_stage_abundance[, i] +
      .rowSums(
        stats::rbinom(9, abundance_matrix, survival_array[,, i]),
        m = 3,
        n = 3
      )
  }
  transition_function <- population_transitions(
    populations = 7,
    demographic_stochasticity = TRUE,
    fecundity_matrix = fecundity_matrix,
    fecundity_max = NULL,
    survival_matrix = survival_matrix
  )
  set.seed(123)
  expect_equal(
    transition_function(
      fecundity_array,
      survival_array,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance
  )
  # with maximum fecundity
  set.seed(123)
  transition_function <- population_transitions(
    populations = 7,
    demographic_stochasticity = TRUE,
    fecundity_matrix = fecundity_matrix,
    fecundity_max = 4.5,
    survival_matrix = survival_matrix
  )
  fecundity_lookup <- environment(transition_function)[["fecundity_lookup"]]
  fecundity_shifted <- fecundity_array * 0
  fecundity_indices <- which(fecundity_array > 0)
  fecundity_shifted[fecundity_indices] <-
    apply(
      matrix(1:length(fecundity_indices)),
      1,
      function(i) {
        fecundity_lookup$shifted[which.min(abs(
          fecundity_lookup$original -
            fecundity_array[fecundity_indices][i]
        ))]
      }
    )
  expected_stage_abundance <- array(0, c(3, 7))
  set.seed(123)
  for (i in occupied_indices) {
    abundance_matrix <- matrix(
      stage_abundance[, i],
      nrow = 3,
      ncol = 3,
      byrow = TRUE
    )
    expected_stage_abundance[, i] <- .rowSums(
      apply(
        matrix(1:9),
        1,
        function(j) {
          sum(pmin(
            stats::rpois(
              abundance_matrix[j],
              fecundity_shifted[,, i][j]
            ),
            4.5
          ))
        }
      ),
      m = 3,
      n = 3
    )
  }
  for (i in occupied_indices) {
    # separate loop for random order consistent with function
    abundance_matrix <- matrix(
      stage_abundance[, i],
      nrow = 3,
      ncol = 3,
      byrow = TRUE
    )
    expected_stage_abundance[, i] <- expected_stage_abundance[, i] +
      .rowSums(
        stats::rbinom(9, abundance_matrix, survival_array[,, i]),
        m = 3,
        n = 3
      )
  }
  set.seed(123)
  expect_equal(
    transition_function(
      fecundity_array,
      survival_array,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance
  )
})

test_that("handle excessive survivals", {
  fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
  survival_matrix <- array(c(0, 0.5, 0.5, 0, 0.3, 0.7, 0, 0.2, 0.8), c(3, 3))
  variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
  fecundity_array <- array(fecundity_matrix, c(3, 3, 7)) * variation_array
  survival_array <- array(survival_matrix, c(3, 3, 7)) * variation_array
  stage_abundance <- matrix(
    c(
      7,
      13,
      0,
      26,
      0,
      39,
      47,
      2,
      0,
      6,
      8,
      0,
      12,
      13,
      0,
      3,
      4,
      6,
      0,
      9,
      10
    ),
    nrow = 3,
    ncol = 7,
    byrow = TRUE
  )
  occupied_indices <- (1:7)[-5]
  survivals <- array(0, c(3, 3, 7))
  for (i in occupied_indices) {
    survivals[,, i] <- round(
      survival_array[,, i] *
        matrix(stage_abundance[, i], nrow = 3, ncol = 3, byrow = TRUE)
    )
  }

  # Identify columns with multiple survival paths
  multiple_survival_columns <- which(colSums(survival_matrix > 0) > 1)

  set.seed(123)
  # Correct excessive survivals
  for (mult_surv_col in multiple_survival_columns) {
    for (i in occupied_indices) {
      # Calculate total survivals for this column/population
      column_survival_total <- sum(survivals[, mult_surv_col, i])
      # Calculate excessive survivals
      excessive_survivals <- column_survival_total -
        stage_abundance[mult_surv_col, i]

      if (excessive_survivals > 0) {
        # Get current survivals for this column/population
        current_surv <- survivals[, mult_surv_col, i]

        # Reduce excess survivals
        excess_remaining <- excessive_survivals
        while (excess_remaining > 0) {
          # Only consider transitions with remaining survivors
          valid_indices <- which(current_surv > 0)
          if (length(valid_indices) == 0) break

          # Sample which transition to reduce based on relative probabilities
          sample_index <- sample(
            valid_indices,
            size = 1,
            prob = survival_array[valid_indices, mult_surv_col, i]
          )

          # Reduce by 1
          survivals[sample_index, mult_surv_col, i] <- survivals[
            sample_index,
            mult_surv_col,
            i
          ] -
            1
          current_surv[sample_index] <- current_surv[sample_index] - 1
          excess_remaining <- excess_remaining - 1
        }
      }
    }
  }

  expected_stage_abundance <- array(0, c(3, 7))
  for (i in occupied_indices) {
    abundance_matrix <- matrix(
      stage_abundance[, i],
      nrow = 3,
      ncol = 3,
      byrow = TRUE
    )
    expected_stage_abundance[, i] <- rowSums(
      round(fecundity_array[,, i] * abundance_matrix) + survivals[,, i]
    )
  }
  transition_function <- population_transitions(
    populations = 7,
    demographic_stochasticity = FALSE,
    fecundity_matrix = fecundity_matrix,
    fecundity_max = NULL,
    survival_matrix = survival_matrix
  )
  set.seed(123)
  expect_equal(
    transition_function(
      fecundity_array,
      survival_array,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance
  )
})
