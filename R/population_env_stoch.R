#' Nested functions for population environmental stochasticity.
#'
#' Modular functions for the population simulator for performing correlated
#' environmentally stochastic adjustments to transition rates.
#'
#' @examples
#' fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
#' survival_matrix <- array(c(0, 0.5, 0, 0, 0, 0.7, 0, 0, 0.8), c(3, 3))
#' standard_deviation <- (fecundity_matrix + survival_matrix) * 0.3
#' variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
#' fecundity_array <- array(fecundity_matrix, c(3, 3, 7)) * variation_array
#' survival_array <- array(survival_matrix, c(3, 3, 7)) * variation_array
#' stage_abundance <- matrix(c(
#'   7, 13, 0, 26, 0, 39, 47,
#'   2, 0, 6, 8, 0, 12, 13,
#'   0, 3, 4, 6, 0, 9, 10
#' ), nrow = 3, ncol = 7, byrow = TRUE)
#' occupied_indices <- (1:7)[-5]
#' env_stoch_function <- population_env_stoch(
#'   populations = 7, fecundity_matrix, fecundity_max = NULL, survival_matrix,
#'   standard_deviation, correlation = NULL
#' )
#' env_stoch_function(fecundity_array, survival_array, occupied_indices)
#'
#' @param populations Number of populations.
#' @param fecundity_matrix Matrix of transition fecundity rates (Leslie/Lefkovitch matrix with non-zero fecundities only).
#' @param fecundity_max Maximum transition fecundity rate (in Leslie/Lefkovitch matrix).
#' @param survival_matrix Matrix of transition survival rates (Leslie/Lefkovitch matrix with non-zero survivals only).
#' @param standard_deviation Standard deviation matrix for applying environmental stochasticity to transition rates.
#' @param correlation List containing either an environmental correlation matrix (correlation_matrix), a pre-calculated transposed (Cholesky) decomposition matrix (t_decomposition_matrix), or a compact transposed (Cholesky) decomposition matrix (t_decomposition_compact_matrix) and a corresponding map of population indices (t_decomposition_compact_map), as per \emph{SpatialCorrelation} class attributes.
#' @return Environmental stochasticity calculation function: \code{function(fecundity_array, survival_array, occupied_indices)}, where:
#'   \describe{
#'     \item{\code{fecundity_array}}{3D array of fecundity rates (\emph{stages} rows by \emph{stages} columns by \emph{populations} deep).}
#'     \item{\code{survival_array}}{3D array of survival rates (\emph{stages} rows by \emph{stages} columns by \emph{populations} deep).}
#'     \item{\code{occupied_indices}}{Array of indices for those populations occupied.}
#'     \item{\code{returns}}{List containing stochastically varied fecundity and survival arrays.}
#'   }
#' @export population_env_stoch

population_env_stoch <- function(
  populations,
  fecundity_matrix,
  fecundity_max,
  survival_matrix,
  standard_deviation,
  correlation
) {
  # Extract stages from fecundity matrix dimensions
  stages <- nrow(fecundity_matrix)

  # Fecundity and survival indices for selecting variable transition rates for occupied populations
  fecundity_indices <- matrix(
    which(
      array(
        fecundity_matrix * standard_deviation,
        c(stages, stages, populations)
      ) >
        0
    ),
    ncol = populations
  )
  survival_indices <- matrix(
    which(
      array(
        survival_matrix * standard_deviation,
        c(stages, stages, populations)
      ) >
        0
    ),
    ncol = populations
  )
  multiple_survival_columns <- which(
    .colSums(+(survival_matrix > 0), m = stages, n = stages) > 1
  )

  # Flag for environmental correlation
  use_env_correlation <- FALSE

  # Resolve optional correlation parameter(s)
  if (is.list(correlation)) {
    # Calculate the transposed Cholesky decomposition of any correlation matrix in the list
    if ("correlation_matrix" %in% names(correlation)) {
      correlation$correlation_matrix <- chol(correlation$correlation_matrix)
      names(correlation)[which(
        names(correlation) == "correlation_matrix"
      )] <- "t_decomposition_matrix"
    }

    # Compact any transposed Cholesky decomposition into the minimal number of rows, plus mapped to the original matrix
    if ("t_decomposition_matrix" %in% names(correlation)) {
      # Calculate non-zero decomposition data from the decomposition matrix
      t_decomposition_data <- which(
        correlation$t_decomposition_matrix != 0,
        arr.ind = TRUE,
        useNames = TRUE
      )
      t_decomposition_data <- as.data.frame(cbind(
        t_decomposition_data,
        correlation$t_decomposition_matrix[t_decomposition_data]
      ))
      names(t_decomposition_data) <- c("row", "col", "value")
      t_decomposition_data <- t_decomposition_data[
        order(t_decomposition_data$col, t_decomposition_data$row),
      ]
      correlation$t_decomposition_matrix <- NULL # release from memory

      # Create a compact transposed decomposition matrix
      t_decomposition_nonzero_rows <- tabulate(
        t_decomposition_data$col,
        nbins = populations
      )
      t_decomposition_compact_rows <- max(t_decomposition_nonzero_rows)
      correlation$t_decomposition_compact_matrix <- array(
        1:t_decomposition_compact_rows,
        c(t_decomposition_compact_rows, populations)
      )
      correlation$t_decomposition_compact_matrix <- (correlation$t_decomposition_compact_matrix *
        (correlation$t_decomposition_compact_matrix <=
          matrix(
            t_decomposition_nonzero_rows,
            nrow = t_decomposition_compact_rows,
            ncol = populations,
            byrow = TRUE
          )))
      t_decomposition_compact_indices <- which(
        correlation$t_decomposition_compact_matrix != 0
      )
      correlation$t_decomposition_compact_matrix[
        t_decomposition_compact_indices
      ] <- t_decomposition_data$value

      # Create a map to the original region grid rows
      correlation$t_decomposition_compact_map <- array(
        NA,
        c(t_decomposition_compact_rows, populations)
      )
      correlation$t_decomposition_compact_map[
        t_decomposition_compact_indices
      ] <- t_decomposition_data$row
    }
  }

  # Unpack compact decompostion
  if (
    is.list(correlation) &&
      all(
        names(correlation) %in%
          c("t_decomposition_compact_matrix", "t_decomposition_compact_map")
      )
  ) {
    use_env_correlation <- TRUE
    t_decomposition_compact_matrix <- correlation$t_decomposition_compact_matrix
    t_decomposition_compact_map <- correlation$t_decomposition_compact_map
    t_decomposition_compact_rows <- nrow(t_decomposition_compact_matrix)
  }

  # Release unused variable from memory
  correlation <- NULL

  ## Create a nested function for performing correlated environmentally stochastic adjustments to transition rates ##
  calculate <- function(fecundity_array, survival_array, occupied_indices) {
    # Calculate occupied population number
    occupied_populations <- length(occupied_indices)

    # Generate correlated normal deviates for each occupied population (as per Burgman, Ferson & Akcakaya, 1993)
    if (use_env_correlation) {
      occupied_correlated_deviates <- .colSums(
        t_decomposition_compact_matrix[, occupied_indices] *
          stats::rnorm(populations)[t_decomposition_compact_map[,
            occupied_indices
          ]],
        m = t_decomposition_compact_rows,
        n = occupied_populations,
        na.rm = TRUE
      )
    } else {
      occupied_correlated_deviates <- stats::rnorm(occupied_populations)
    }

    # Sample fecundities from the Lognormal distribution (as per Burgman, Ferson & Akcakaya, 1993)
    occupied_fecundity_indices <- as.vector(fecundity_indices[,
      occupied_indices
    ])
    occupied_fecundities <- fecundity_array[occupied_fecundity_indices]
    log_common <- log(
      (standard_deviation[fecundity_indices[, 1]] / occupied_fecundities)^2 + 1
    )
    log_common[which(occupied_fecundities == 0)] <- 0
    deviate_indices <- rep(
      1:occupied_populations,
      each = length(fecundity_indices[, 1])
    )
    fecundity_array[occupied_fecundity_indices] <- occupied_fecundities *
      exp(
        sqrt(log_common) *
          occupied_correlated_deviates[deviate_indices] -
          0.5 * log_common
      )
    if (is.numeric(fecundity_max)) {
      fecundity_array[occupied_fecundity_indices][which(
        fecundity_array[occupied_fecundity_indices] > fecundity_max
      )] <- fecundity_max
    }

    # Sample survivals from the Beta distribution
    occupied_survival_indices <- as.vector(survival_indices[, occupied_indices])
    occupied_survivals <- survival_array[occupied_survival_indices]
    occupied_standard_deviations <- standard_deviation[survival_indices[, 1]] # single population
    excessive_sd_indices <- which(
      occupied_standard_deviations >=
        sqrt(occupied_survivals * (1 - occupied_survivals))
    )
    if (length(excessive_sd_indices)) {
      # resolves limitation of calculating alpha and beta parameters (shouldn't occur often)
      occupied_standard_deviations <- rep(
        occupied_standard_deviations,
        occupied_populations
      )
      occupied_standard_deviations[excessive_sd_indices] <- sqrt(
        occupied_survivals * (1 - occupied_survivals)
      )[excessive_sd_indices] -
        0.001
    }
    occupied_alpha <- occupied_survivals *
      (occupied_survivals *
        (1 - occupied_survivals) /
        occupied_standard_deviations^2 -
        1)
    occupied_beta <- (1 - occupied_survivals) *
      (occupied_survivals *
        (1 - occupied_survivals) /
        occupied_standard_deviations^2 -
        1)
    deviate_indices <- rep(
      1:occupied_populations,
      each = length(survival_indices[, 1])
    )
    survival_array[occupied_survival_indices] <- stats::qbeta(
      stats::pnorm(occupied_correlated_deviates[deviate_indices]),
      occupied_alpha,
      occupied_beta
    )

    # Resolve any excessive survivals (column sums > 1)
    for (mult_surv_col in multiple_survival_columns) {
      # only possible when multiple survival rates in a stage matrix column
      mult_surv_col_sums <- .colSums(
        survival_array[, mult_surv_col, occupied_indices],
        m = stages,
        n = occupied_populations
      )
      excessive_indices <- which(mult_surv_col_sums > 1)
      survival_array[, mult_surv_col, occupied_indices[excessive_indices]] <-
        survival_array[, mult_surv_col, occupied_indices[excessive_indices]] /
        mult_surv_col_sums[rep(excessive_indices, each = stages)]
    }

    return(list(
      fecundity_array = fecundity_array,
      survival_array = survival_array
    ))
  }

  return(calculate)
}
