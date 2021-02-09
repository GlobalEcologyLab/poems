#' Nested functions for population density dependence.
#'
#' Modular functions for the population simulator for performing density dependent
#' adjustments to transition rates.
#'
#' @param populations Number of populations.
#' @param stage_matrix Matrix of transition (fecundity & survival) rates between stages at each time step (Leslie/Lefkovitch matrix).
#' @param fecundity_mask Matrix of 0-1 to indicate which (proportions) of transition rates refer to fecundity.
#' @param fecundity_max Maximum transition fecundity rate (in Leslie/Lefkovitch matrix).
#' @param density_dependence Density dependence can be "ceiling" (default), "logistic" (Ricker), or a user-defined function (optionally nested in a list with additional attributes) for adjusting transition rates: \code{function(params)}, where \emph{params} is a list passed to the function containing:
#'   \describe{
#'     \item{\code{transition_array}}{3D array of transition rates: stages by stages by populations.}
#'     \item{\code{fecundity_mask}}{Matrix of 0-1 to indicate which (proportions) of transition rates refer to fecundity.}
#'     \item{\code{fecundity_max}}{Maximum transition fecundity rate (in Leslie/Lefkovitch matrix).}
#'     \item{\code{carrying_capacity}}{Array of carrying capacity values for each population.}
#'     \item{\code{stage_abundance}}{Matrix of abundance for each stage (rows) and population (columns).}
#'     \item{\code{population_abundance}}{Array of summed population abundances for all stages.}
#'     \item{\code{density_abundance}}{Array of summed population abundances for stages affected by density.}
#'     \item{\code{growth_rate_max}}{Maximum growth rate value or array for populations.}
#'     \item{\code{occupied_indices}}{Array of indices for populations occupied at (current) time step.}
#'     \item{\code{calculate_multipliers}}{Function (\code{function(growth_rates)}) for finding multipliers (when stages > 1) to apply to affected transitions that result in target growth rates (dominant eigenvalues).}
#'     \item{\code{apply_multipliers}}{Function (\code{function(transition_array, multipliers}) for applying (when stages > 1) multipliers to the affected transition rates within a transition array (returns multiplied transition array).}
#'     \item{\code{simulator}}{\code{\link{SimulatorReference}} object with dynamically accessible \emph{attached} and \emph{results} lists.}
#'     \item{\code{additional attributes}}{Additional attributes when density dependence is optionally nested in a list.}
#'   }
#'   returns an adjusted transition array for occupied populations
#' @param growth_rate_max Maximum growth rate (utilized by density dependence processes).
#' @param density_affects Matrix of booleans or numeric (0-1) indicating the transition vital rates affected by density (default is all).
#' @param density_stages Array of booleans or numeric (0,1) for each stage to indicate which stages are affected by density (default is all).
#' @param density_precision Numeric precision of the calculated multipliers (used when stages > 1) applied to affected transition rates (default is 3 decimal places).
#' @param simulator \code{\link{SimulatorReference}} object with dynamically accessible \emph{attached} and \emph{results} lists.
#' @return Density dependent calculation function, either:
#'   \describe{
#'     \item{\code{function(carrying_capacity, stage_abundance)}}{For ceiling density dependence function, OR}
#'     \item{\code{function(transition_array, carrying_capacity, stage_abundance, occupied_indices)}}{For user-defined density dependence function, where:
#'       \describe{
#'         \item{\code{transition_array}}{3D array of transition rates: stages by stages by populations.}
#'         \item{\code{carrying_capacity}}{Array of carrying capacity values for each population.}
#'         \item{\code{stage_abundance}}{Matrix of abundance for each stage (rows) and population (columns).}
#'         \item{\code{occupied_indices}}{Array of indices for populations occupied.}
#'       }
#'     }
#'   }
#' @export population_density

population_density <- function(populations,
                               stage_matrix,
                               fecundity_mask,
                               fecundity_max,
                               density_dependence,
                               growth_rate_max,
                               density_affects,
                               density_stages,
                               density_precision,
                               simulator) {

  # Extract stages from stage matrix dimensions
  stages <- nrow(stage_matrix)

  # Set density stages
  if (!is.null(density_stages)) {
    density_stage_indices <- which(density_stages > 0)
  } else {
    density_stage_indices <- 1:stages
  }
  density_stages <- length(density_stage_indices)

  # Set density precision
  if (is.null(density_precision)) {
    density_precision <- 3
  }

  if (is.character(density_dependence) && density_dependence == "ceiling") {

    ## Create a nested function for applying ceiling density dependence to stage abundances ##
    ceiling_function <- function(carrying_capacity, stage_abundance) {

      # Compare density affected population abundances to capacities
      density_abundance <- .colSums(stage_abundance[density_stage_indices,], m = density_stages, n = populations)
      above_capacity_indices <- which(density_abundance > carrying_capacity)
      if (length(above_capacity_indices)) {

        # Limit stage abundances via affected capacity/abundance ratio
        limited_stage_abundance <- stage_abundance
        limited_stage_abundance[density_stage_indices, above_capacity_indices] <-
          (stage_abundance[density_stage_indices, above_capacity_indices]*
             rep(carrying_capacity[above_capacity_indices]/density_abundance[above_capacity_indices], each = density_stages))
        stage_abundance[density_stage_indices, above_capacity_indices] <- round(limited_stage_abundance[density_stage_indices, above_capacity_indices])

        # Ensure the ceiling values are used (correct differences resulting from rounding)
        ceiling_corrections <- (carrying_capacity[above_capacity_indices] -
                                  .colSums(stage_abundance[density_stage_indices, above_capacity_indices], m = density_stages,
                                           n = length(above_capacity_indices)))
        for (i in which(ceiling_corrections != 0)) {
          sample_indices <- sample(1:density_stages, size = abs(ceiling_corrections[i]), replace = TRUE,
                                   prob = limited_stage_abundance[density_stage_indices, above_capacity_indices[i]])
          for (sample_index in sample_indices) {
            stage_abundance[density_stage_indices[sample_index], above_capacity_indices[i]] <-
              stage_abundance[density_stage_indices[sample_index], above_capacity_indices[i]] + ifelse(ceiling_corrections[i] > 0, 1, -1)
          }
        }
      }

      return(stage_abundance)
    }

    return(ceiling_function)

  } else { # transition altering function

    # Set density parameter defaults when NULL
    if (is.null(growth_rate_max)) {
      growth_rate_max <- log(Re((eigen(stage_matrix, only.values = TRUE)$values)[1]))
    }
    if (is.null(density_affects)) {
      density_affects <- +(stage_matrix > 0)
    }

    if (stages > 1) {

      # Construct a lookup table for transition multipliers and their corresponding growth rates (dominant eigenvalues)
      maximum_multiplier <- 1/max(.colSums((1 - fecundity_mask)*stage_matrix*density_affects, m = stages, n = stages))
      if (!is.finite(maximum_multiplier)) { # limit via fecundities if possible
        if (is.numeric(fecundity_max)) {
          fecundities <- fecundity_mask*stage_matrix*density_affects
          maximum_multiplier <- fecundity_max/min(fecundities[which(fecundities > 0)])
        } else {
          stop("Could not build density lookup table without maximum fecundity", call. = FALSE)
        }
      }
      multiplier_lookup <- data.frame(growth_rate = NA,
                                      multiplier = (1:trunc(maximum_multiplier*10^density_precision))/10^density_precision)
      for (i in 1:length(multiplier_lookup$multiplier)) {
        new_stage_matrix <- multiplier_lookup$multiplier[i]*density_affects*stage_matrix + (1 - density_affects)*stage_matrix
        if (is.numeric(fecundity_max)) { # limit fecundities
          new_stage_matrix[which(fecundity_mask*new_stage_matrix > fecundity_max)] <- fecundity_max
        }
        multiplier_lookup$growth_rate[i] <- log(Re((eigen(new_stage_matrix, only.values = TRUE)$values)[1]))
      }

      # Function for calculating (looking up) transition multipliers given target corresponding growth rates (dominant eigenvalues)
      calculate_multipliers <- function(growth_rates) {
        multipliers <- array(0, length(growth_rates))
        for (i in 1:length(growth_rates)) {
          if (is.finite(growth_rates[i])) {
            multipliers[i] <- multiplier_lookup$multiplier[which.min(abs(multiplier_lookup$growth_rate - growth_rates[i]))]
          }
        }
        return(multipliers)
      }

      # Function for applying transition multipliers to the affected elements of a 3D transition array
      apply_multipliers <- function(transition_array, multipliers) {
        selected_populations <- length(transition_array)/(stages^2)
        transition_array <- array(transition_array, c(stages, stages, selected_populations))
        multiplier_array <- array(rep(multipliers, each = stages*stages), c(stages, stages, selected_populations))
        density_affects_array <- array(density_affects, c(stages, stages, selected_populations))
        transition_array <- multiplier_array*density_affects_array*transition_array + (1 - density_affects_array)*transition_array
        if (is.numeric(fecundity_max)) { # limit fecundities
          fecundity_mask_array <- array(fecundity_mask, c(stages, stages, selected_populations))
          transition_array[which(fecundity_mask_array*transition_array > fecundity_max)] <- fecundity_max
        }
        return(transition_array)
      }
    }

    # Logistic (Ricker)
    if (is.character(density_dependence) && density_dependence == "logistic") {

      ## Create a nested function for applying logistic (Ricker) dependence to stage abundances ##
      logistic_function <- function(transition_array, carrying_capacity, stage_abundance, occupied_indices) {

        # Eliminate zero capacity transitions and update occupied indices
        occupied_zero_indices <- which(carrying_capacity[occupied_indices] <= 0)
        transition_array[, , occupied_indices[occupied_zero_indices]] <- 0
        if (length(occupied_zero_indices)) {
          occupied_indices <- occupied_indices[-occupied_zero_indices]
        }

        # Calculate occupied population number, carrying capacity, density affected population abundances and maximum growth rate
        occupied_populations <- length(occupied_indices)
        carrying_capacity <- carrying_capacity[occupied_indices]
        density_abundance <- .colSums(stage_abundance[density_stage_indices, occupied_indices], m = density_stages, n = occupied_populations)
        if (length(growth_rate_max) == populations) {
          occupied_growth_rate_max <- growth_rate_max[occupied_indices]
        } else { # assume single value
          occupied_growth_rate_max <- growth_rate_max
        }

        # Calculate logistic (Ricker) growth rate
        growth_rate <- occupied_growth_rate_max*(1 - density_abundance/carrying_capacity)

        # Calculate and apply multipliers that result in transition matrix dominant eigenvalues corresponding to growth rate
        if (stages > 1) { # use lookup table
          transition_array[, , occupied_indices] <- apply_multipliers(array(transition_array[, , occupied_indices],
                                                                            c(stages, stages, occupied_populations)),
                                                                      calculate_multipliers(growth_rate))
        } else { # calculate (single transition rate equals dominant eigenvalue)
          transition_array[, , occupied_indices]  <- exp(growth_rate)
        }

        return(transition_array)
      }

      return(logistic_function)
    }

    # Unpack density function and additional attributes from a list
    additional_attributes <- list()
    if (is.list(density_dependence)) {
      function_index <- which(unlist(lapply(density_dependence, is.function)))
      additional_attributes <- density_dependence[-function_index]
      density_dependence <- density_dependence[[function_index]]
    }

    if (is.function(density_dependence)) { # user-defined function

      # List of parameters to pass to the user-defined function
      params <- c(list(simulator = simulator), additional_attributes)

      ## Create a nested function for applying user-defined density dependence to transition rates ##
      user_defined_function <- function(transition_array, carrying_capacity, stage_abundance, occupied_indices) {

        # Add/calculate attributes and functions to be made available to the user-defined function
        params$transition_array <- transition_array
        params$fecundity_mask <- fecundity_mask
        params$fecundity_max <- fecundity_max
        params$carrying_capacity <- carrying_capacity
        params$stage_abundance <- stage_abundance
        params$population_abundance <- .colSums(stage_abundance, m = stages, n = populations)
        params$density_abundance <- .colSums(stage_abundance[density_stage_indices,], m = density_stages, n = populations)
        params$occupied_indices <- occupied_indices
        params$growth_rate_max <- growth_rate_max
        if (stages > 1) {
          params$calculate_multipliers <- calculate_multipliers
          params$apply_multipliers <- apply_multipliers
        }

        # Run user-defined function
        tryCatch({
          transition_array[] <- density_dependence(params)
        },
        error = function(e){
          stop(paste("Error produced within user-defined density dependence function:", as.character(e)), call. = FALSE)
        })

        # Warn if any negative or non-finite
        if (any(!is.finite(transition_array))) {
          warning("Non-finite transition rates returned by user-defined density dependence function", call. = FALSE)
        }
        if (any(transition_array[which(is.finite(transition_array))] < 0)) {
          warning("Negative transition rates returned by user-defined density dependence function", call. = FALSE)
        }

        return(transition_array)
      }

      return(user_defined_function)
    }

  }
}
