#' Nested functions for stage-based population transitions.
#'
#' Modular functions for the population simulator for performing staged-based
#' (Leslie/Lefkovitch matrix) transitions via 3D survival and fecundity arrays.
#' 
#' @examples 
#' # Deterministic transition (no stochasticity)
#' fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
#' survival_matrix <- array(c(0, 0.5, 0, 0, 0, 0.7, 0, 0, 0.8), c(3, 3))
#' variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
#' fecundity_array <- array(fecundity_matrix, c(3, 3, 7)) * variation_array
#' survival_array <- array(survival_matrix, c(3, 3, 7)) * variation_array
#' stage_abundance <- matrix(c(
#'   7, 13, 0, 26, 0, 39, 47,
#'   2, 0, 6, 8, 0, 12, 13,
#'  0, 3, 4, 6, 0, 9, 10
#' ), nrow = 3, ncol = 7, byrow = TRUE)
#' occupied_indices <- (1:7)[-5]
#' transition_function <- population_transitions(
#'   populations = 7, demographic_stochasticity = FALSE,
#'   fecundity_matrix = fecundity_matrix, fecundity_max = NULL,
#'   survival_matrix = survival_matrix
#' )
#' transition_function(fecundity_array, survival_array, stage_abundance, 
#'                     occupied_indices)
#'
#' @param populations Number of populations.
#' @param demographic_stochasticity Boolean for choosing demographic stochasticity for transitions.
#' @param fecundity_matrix Matrix of transition fecundity rates (Leslie/Lefkovitch matrix with non-zero fecundities only).
#' @param fecundity_max Maximum transition fecundity rate (in Leslie/Lefkovitch matrix).
#' @param survival_matrix Matrix of transition survival rates (Leslie/Lefkovitch matrix with non-zero survivals only).
#' @return Transition calculation function: \code{function(fecundity_array, survival_array, stage_abundance, occupied_indices)}, where:
#'   \describe{
#'     \item{\code{fecundity_array}}{3D array of fecundity rates (\emph{stages} rows by \emph{stages} columns by \emph{populations} deep).}
#'     \item{\code{survival_array}}{3D array of survival rates (\emph{stages} rows by \emph{stages} columns by \emph{populations} deep).}
#'     \item{\code{stage_abundance}}{Matrix of stage abundances for each population at time step (\emph{stages} rows by \emph{populations} columns).}
#'     \item{\code{occupied_indices}}{Array of indices for those populations occupied.}
#'     \item{\code{returns}}{Transitioned stage abundances.}
#'   }
#' @export population_transitions

population_transitions <- function(populations,
                                   demographic_stochasticity,
                                   fecundity_matrix,
                                   fecundity_max,
                                   survival_matrix) {
  # Extract stages from fecundity matrix dimensions
  stages <- nrow(fecundity_matrix)

  # Transition array and indices for matrix / 3D array operations
  multiple_survival_columns <- which(.colSums(+(survival_matrix > 0), m = stages, n = stages) > 1)
  abundance_array_indices <- array(1:(stages * populations), c(stages, populations))[, rep(1:populations, each = stages)]
  t_array_indices <- aperm(array(1:(stages * stages * populations), c(stages, stages, populations)), c(2, 1, 3))

  # Fecundity indices for selecting fecundities for occupied populations
  t_fecundity_indices <- matrix(which(array(t(fecundity_matrix), c(stages, stages, populations)) > 0), ncol = populations)
  fecundity_t_indices <- matrix(t_array_indices[t_fecundity_indices], ncol = populations)
  abundance_fecundity_indices <- matrix(abundance_array_indices[t_fecundity_indices], ncol = populations)

  # Survival indices for selecting survivals for occupied populations
  t_survival_indices <- matrix(which(array(t(survival_matrix), c(stages, stages, populations)) > 0), ncol = populations)
  survival_t_indices <- matrix(t_array_indices[t_survival_indices], ncol = populations)
  abundance_survival_indices <- matrix(abundance_array_indices[t_survival_indices], ncol = populations)

  # Release unused variables from memory
  abundance_array_indices <- NULL
  t_array_indices <- NULL

  # Create a lookup table for "recovering" mean fecundity when maximum is applied
  if (demographic_stochasticity && is.numeric(fecundity_max)) {
    fecundity_lookup <- data.frame(original = NA, shifted = seq(0.00, fecundity_max, 0.01))
    for (i in 1:nrow(fecundity_lookup)) {
      fecundity_lookup$original[i] <- mean(pmin(stats::rpois(100000, fecundity_lookup$shifted[i]), fecundity_max))
    }
  }

  ## Create a nested function for performing stage-based transformations ##
  calculate <- function(fecundity_array, survival_array, stage_abundance, occupied_indices) {
    # Calculate occupied population number
    occupied_populations <- length(occupied_indices)

    # Generate newborns from transition fecundities
    generated_newborns <- array(0, c(stages, stages, populations))
    occupied_t_fecundity_indices <- as.vector(t_fecundity_indices[, occupied_indices])
    if (demographic_stochasticity) { # use Poisson sampling
      if (is.numeric(fecundity_max)) { # Ensure newborns do not exceed maximum fecundity
        original_fecundity <- fecundity_array[as.vector(fecundity_t_indices[, occupied_indices])]
        shifted_fecundity <- array(0, length(original_fecundity))
        for (i in 1:length(original_fecundity)) {
          if (is.finite(original_fecundity[i])) {
            shifted_fecundity[i] <-
              fecundity_lookup$shifted[which.min(abs(fecundity_lookup$original - original_fecundity[i]))]
          }
        }
        selected_abundance <- stage_abundance[as.vector(abundance_fecundity_indices[, occupied_indices])]
        generated_newborns[occupied_t_fecundity_indices] <-
          apply(
            matrix(1:length(shifted_fecundity)), 1,
            function(i) sum(pmin(stats::rpois(selected_abundance[i], shifted_fecundity[i]), fecundity_max))
          )
      } else {
        generated_newborns[occupied_t_fecundity_indices] <-
          stats::rpois(
            length(occupied_t_fecundity_indices),
            stage_abundance[as.vector(abundance_fecundity_indices[, occupied_indices])] *
              fecundity_array[as.vector(fecundity_t_indices[, occupied_indices])]
          )
      }
    } else { # deterministic
      if (is.numeric(fecundity_max)) { # Ensure newborns do not exceed maximum fecundity
        selected_abundance <- stage_abundance[as.vector(abundance_fecundity_indices[, occupied_indices])]
        generated_newborns[occupied_t_fecundity_indices] <-
          pmin(
            round(selected_abundance * fecundity_array[as.vector(fecundity_t_indices[, occupied_indices])]),
            trunc(selected_abundance * fecundity_max)
          )
      } else {
        generated_newborns[occupied_t_fecundity_indices] <-
          round(stage_abundance[as.vector(abundance_fecundity_indices[, occupied_indices])] *
            fecundity_array[as.vector(fecundity_t_indices[, occupied_indices])])
      }
    }

    # Generate survivals
    generated_survivals <- array(0, c(stages, stages, populations))
    occupied_t_survival_indices <- as.vector(t_survival_indices[, occupied_indices])
    if (demographic_stochasticity) { # use Binomial sampling
      generated_survivals[occupied_t_survival_indices] <-
        stats::rbinom(
          length(occupied_t_survival_indices),
          stage_abundance[as.vector(abundance_survival_indices[, occupied_indices])],
          survival_array[as.vector(survival_t_indices[, occupied_indices])]
        )
    } else { # deterministic
      generated_survivals[occupied_t_survival_indices] <-
        round(stage_abundance[as.vector(abundance_survival_indices[, occupied_indices])] *
          survival_array[as.vector(survival_t_indices[, occupied_indices])])
    }

    # Resolve any excessive survivals (generated more than were existing?)
    for (mult_surv_col in multiple_survival_columns) { # only possible when multiple survival rates in a stage matrix column
      excessive_survivals <- (.colSums(generated_survivals[mult_surv_col, , occupied_indices], m = stages, n = occupied_populations) -
        stage_abundance[mult_surv_col, occupied_indices])
      for (i in which(excessive_survivals > 0)) {
        pop_index <- occupied_indices[i]
        sample_indices <- sample(1:stages, size = excessive_survivals[i], replace = TRUE, prob = survival_array[, mult_surv_col, pop_index])
        for (sample_index in sample_indices) {
          generated_survivals[mult_surv_col, sample_index, pop_index] <- generated_survivals[mult_surv_col, sample_index, pop_index] - 1
        }
      }
    }

    # Update the stage abundance values from the generated newborns and survivals
    stage_abundance[, occupied_indices] <- .colSums(generated_newborns[, , occupied_indices] + generated_survivals[, , occupied_indices],
      m = stages, n = stages * occupied_populations
    )

    return(stage_abundance)
  }

  return(calculate)
}
