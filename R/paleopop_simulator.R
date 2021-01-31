#' Runs a customized population model simulation.
#'
#' Simulates a population model customized for paleontological time-scales, optimized
#' for single-generation transitions and large populations, across multiple generations
#' and returns simulation results. Each generational time-step includes:
#' \enumerate{
#'   \item Density dependence calculations
#'   \item Enviromental stochasticity calculations
#'   \item Generational transition calculations
#'   \item Harvest calculations
#'   \item Dispersal calculations
#'   \item Results collection
#' }
#'
#' @param inputs Nested list/object with named elements:
#'   \describe{
#'     \item{\code{random_seed}}{Number to seed the random number generation for stochasticity.}
#'     \item{\code{time_steps}}{Number of simulation time steps.}
#'     \item{\code{years_per_step}}{Number of years per time step.}
#'     \item{\code{populations}}{Number of populations.}
#'     \item{\code{initial_abundance}}{Array of initial abundances for each population.}
#'     \item{\code{transition_rate}}{Rate of transition (or fecundity) between generations.}
#'     \item{\code{standard_deviation}}{Standard deviation applied to transition rates.}
#'     \item{\code{compact_decomposition}}{List containing a compact transposed (Cholesky) decomposition \emph{matrix} (t_decomposition_compact_matrix) and a corresponding \emph{map} of population indices (t_decomposition_compact_map), as per \code{\link{SpatialCorrelation}} class attributes.}
#'     \item{\code{carrying_capacity}}{Matrix of carrying capacities (\emph{populations} rows by \emph{time_steps} columns).}
#'     \item{\code{density_dependence}}{Density dependence type ("competition", "logistic", or "ceiling").}
#'     \item{\code{growth_rate_max}}{Maximum growth rate (for "competition" or "logistic" density dependence).}
#'     \item{\code{harvest}}{Boolean for utilizing harvesting.}
#'     \item{\code{harvest_max}}{Proportion harvested per year (note: annual time scale - not generational).}
#'     \item{\code{harvest_g}}{The \emph{G} parameter in the harvest function.}
#'     \item{\code{harvest_z}}{The \emph{Z} parameter in the harvest function.}
#'     \item{\code{harvest_max_n}}{Maximum density per grid cell.}
#'     \item{\code{human_density}}{Matrix of human density (fraction) (\emph{populations} rows by \emph{time_steps} columns).}
#'     \item{\code{dispersal_data}}{List of data frames of non-zero dispersal rates and indices for constructing a compact dispersal matrix, and optional changing rates over time, as per class \code{\link{DispersalGenerator}} \emph{dispersal_data} attribute.}
#'     \item{\code{dispersal_target_k}}{Target population carrying capacity threshold for density dependent dispersal.}
#'     \item{\code{abundance_threshold}}{Abundance threshold (that needs to be exceeded) for each population to persist.}
#'     \item{\code{occupancy_threshold}}{Threshold for the number of populations occupied (that needs to be exceeded) for all populations to persist.}
#'     \item{\code{results_selection}}{List of results selection from: "abundance", "ema", "extirpation", "harvested", "occupancy".}
#'   }
#' @return Simulation results as a nested list (as selected):
#'   \describe{
#'     \item{\code{abundance}}{Matrix of simulation abundances (\emph{populations} rows by \emph{time_steps} columns).}
#'     \item{\code{ema}}{Matrix of expected minimum abundances (\emph{populations} rows by \emph{time_steps} columns).}
#'     \item{\code{extirpation}}{Array of extirpation times for each population.}
#'     \item{\code{harvested}}{Matrix of estimated individuals harvested (\emph{populations} rows by \emph{time_steps} columns).}
#'     \item{\code{occupancy}}{Array of number of populations occupied at each time-step.}
#'   }
#' @export paleopop_simulator

paleopop_simulator <- function(inputs) {

  ## Unpack inputs and calculate re-usable variables ##

  # Stop if minimal inputs are not present
  if (is.null(inputs$time_steps) || is.null(inputs$populations) || is.null(inputs$initial_abundance) ||
      is.null(inputs$transition_rate) || is.null(inputs$carrying_capacity)) {
    incomplete_inputs <- if (is.null(inputs$time_steps)) "time_steps"
    incomplete_inputs <- c(incomplete_inputs, if (is.null(inputs$populations)) "populations")
    incomplete_inputs <- c(incomplete_inputs, if (is.null(inputs$initial_abundance)) "initial_abundance")
    incomplete_inputs <- c(incomplete_inputs, if (is.null(inputs$transition_rate)) "transition_rate")
    incomplete_inputs <- c(incomplete_inputs, if (is.null(inputs$carrying_capacity)) "carrying_capacity")
    stop(paste("Minimal inputs required to run simulation should include:",
               paste(incomplete_inputs, collapse = ", ")), call. = FALSE)
  }

  # General model settings
  time_steps <- inputs$time_steps
  years_per_step <- ifelse(is.null(inputs$years_per_step), 1, inputs$years_per_step)
  random_seed <- inputs$random_seed
  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }

  # Transition rate and standard deviation
  transition_rate <- inputs$transition_rate
  standard_deviation <- ifelse(is.null(inputs$standard_deviation), 0, inputs$standard_deviation)
  environmental_stochasticity <- (standard_deviation*transition_rate > 0)

  # Population settings
  populations <- inputs$populations
  population_abundances <- array(inputs$initial_abundance, c(1, populations)) # single column matrix for faster dispersal calculations
  density_dependence <- inputs$density_dependence
  if (is.null(density_dependence)) {
    density_dependence <- "none"
  }
  growth_rate_max <- ifelse(is.null(inputs$growth_rate_max), log(transition_rate), inputs$growth_rate_max)
  abundance_threshold <- inputs$abundance_threshold
  occupancy_threshold <- inputs$occupancy_threshold
  dispersal_target_k <- inputs$dispersal_target_k
  carrying_capacities <- matrix(inputs$carrying_capacity, nrow = populations)
  if (any(is.na(carrying_capacities))) {
    carrying_capacities[which(is.na(carrying_capacities))] <- 0
  }

  # Dispersal and correlation

  # Create re-usable dispersal structures
  dispersal_present <- (!is.null(inputs$dispersal_data) && nrow(inputs$dispersal_data[[1]]))
  if (dispersal_present) {

    # Unpack dispersal data and determine compact matrix dimensions
    dispersal_data <- inputs$dispersal_data[[1]]
    dispersal_compact_rows <- max(dispersal_data[, c("emigrant_row", "immigrant_row")])

    # Create compact array of zeros for quick initialization
    dispersal_zero_array <- array(0, c(dispersal_compact_rows, populations))

    # Create a compact matrix of dispersal rates
    dispersal_compact_matrix <- dispersal_zero_array
    dispersal_compact_matrix[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <- dispersal_data$dispersal_rate

    # Are dispersals changing over time?
    dispersals_change_over_time <- (length(inputs$dispersal_data) > 1)
    if (dispersals_change_over_time) {
      dispersal_data_changes <- inputs$dispersal_data
      dispersal_data_changes[[1]] <- dispersal_data_changes[[1]][NULL,]
    }

    # Does dispersal dependent on target population carrying capacity K?
    dispersal_depends_on_target_pop_k <- (!is.null(dispersal_target_k) && dispersal_target_k > 0)
    if (dispersal_depends_on_target_pop_k) {

      # Create a map of compact array indices for mapping dispersers (emigrants) to target populations
      dispersal_target_pop_map <- dispersal_zero_array
      dispersal_target_pop_map[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <- dispersal_data$target_pop
    }

    # Create a map of compact array indices for mapping dispersers (emigrants) to immigrants
    dispersal_compact_indices <- array(1:(dispersal_compact_rows*populations), c(dispersal_compact_rows, populations))
    dispersal_immigrant_map <- dispersal_zero_array
    dispersal_immigrant_map[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <- dispersal_compact_indices[as.matrix(dispersal_data[, c("immigrant_row", "target_pop")])]

    # Create indices for replicating abundances
    dispersal_abundance_rep_indices <- rep(1, dispersal_compact_rows)

    # Release variables from memory
    dispersal_data <- NULL; dispersal_compact_indices <- NULL

  } # dispersal present?

  # Environmental correlation?
  if (environmental_stochasticity) {

    # Flag for environmental correlation
    use_env_correlation <- FALSE

    # Unpack compact decompostion
    if (!is.null(inputs$compact_decomposition)) {
      use_env_correlation <- TRUE
      t_decomposition_compact_matrix <- inputs$compact_decomposition$matrix
      t_decomposition_compact_map <- inputs$compact_decomposition$map
      t_decomposition_compact_rows <- nrow(t_decomposition_compact_matrix)
    }

  } # environmental stochasticity?

  # Human impact (harvest) settings
  harvest <- ifelse(is.null(inputs$harvest), FALSE, inputs$harvest)
  if (harvest) {
    harvest_max <- inputs$harvest_max
    harvest_g <- inputs$harvest_g
    harvest_z <- inputs$harvest_z
    harvest_max_n <- inputs$harvest_max_n
    human_densities <- inputs$human_density
  }

  # Results required selection
  results_selection <- inputs$results_selection
  if (is.null(results_selection)) results_selection <- "abundance"

  ## Initialization ##

  # Initialize carrying capacity
  carrying_capacity_t_max <- ncol(carrying_capacities)
  if (carrying_capacity_t_max == 1) { # no temporal trend in K
    carrying_capacity <- carrying_capacities[, 1]
  }

  # Intialize results collection list components and other variables
  results <- list()
  if ("abundance" %in% results_selection) {
    results$abundance <- array(0, c(populations, time_steps))
  }
  if ("ema" %in% results_selection) {
    results$ema <- array(0, c(populations, time_steps))
    min_abundances <- population_abundances[1,]
  }
  if ("extirpation" %in% results_selection) {
    results$extirpation <- array(NA, populations)
    results$extirpation[which(population_abundances == 0)] <- 0
  }
  if ("harvested" %in% results_selection) {
    results$harvested <- array(0, c(populations, time_steps))
  }
  if ("occupancy" %in% results_selection) {
    results$occupancy <- array(0, time_steps)
  }

  # Dispersal tracking (for testing/debug purposes)
  if ("dispersal_tracking" %in% names(inputs) ||
      ("attached" %in% names(inputs) && "dispersal_tracking" %in% names(inputs$attached))) {
    results$emigrants <- array(0, c(populations, time_steps))
    results$immigrants <- array(0, c(populations, time_steps))
    dispersal_tracking <- TRUE
  } else {
    dispersal_tracking <- FALSE
  }

  ## Simulation time steps ##
  for (tm in 1:time_steps) {

    # Set transitions for each population
    transitions <- array(transition_rate, populations)

    # Apply occupancy threshold when present
    if (!is.null(occupancy_threshold)) {
      if (length(which(as.logical(population_abundances))) <= occupancy_threshold) {
        population_abundances[] <- 0
      }
    }

    # Load carrying capacity for each population for time if temporal trend in K
    if (carrying_capacity_t_max > 1) {
      carrying_capacity <- carrying_capacities[, min(tm, carrying_capacity_t_max)]
    }

    ## Density dependence calculations ##

    # Selective calculations based on occupied populations and carrying capacity
    occupied_indices <- which(as.logical(carrying_capacity*population_abundances[1,])) # > 0
    occupied_populations <- length(occupied_indices)
    zero_indices <- which(carrying_capacity <= 0 & as.logical(population_abundances))

    if (occupied_populations && !is.null(density_dependence) && density_dependence %in% c("competition", "logistic")) {

      # Focus on occupied populations
      selected_carrying_capacity <- carrying_capacity[occupied_indices]
      selected_population_abundances <- population_abundances[occupied_indices]

      # Calculate growth rate
      if (density_dependence == "competition") { # Beverton-Holt
        growth_rate <- growth_rate_max - log(exp(growth_rate_max)*selected_population_abundances/selected_carrying_capacity -
                                               selected_population_abundances/selected_carrying_capacity + 1)
      } else if (density_dependence == "logistic") { # Ricker
        growth_rate <- growth_rate_max*(1 - selected_population_abundances/selected_carrying_capacity)
      }

      # Calculate density dependent multipliers and apply to transitions
      density_dependence_multipliers <- exp(growth_rate)/transition_rate
      transitions[occupied_indices] <- transitions[occupied_indices]*density_dependence_multipliers

      # Set any negative values to zero
      negative_indices <- occupied_indices[which(transitions[occupied_indices] < 0)]
      if (length(negative_indices)) {
        transitions[negative_indices] <- 0
      }

    } # occupied populations?

    ## Environmental stochasticity calculations ##
    if (occupied_populations && environmental_stochasticity) {

      # Generate correlated normal deviates for each occupied population (as per Burgman, Ferson & Akcakaya, 1993)
      if (use_env_correlation) {
        occupied_correlated_deviates <- .colSums(t_decomposition_compact_matrix[, occupied_indices]*stats::rnorm(populations)[t_decomposition_compact_map[, occupied_indices]],
                                                 m = t_decomposition_compact_rows, n = occupied_populations, na.rm = TRUE)
      } else {
        occupied_correlated_deviates <- stats::rnorm(occupied_populations)
      }

      # Sample from lognormal distribution (as per Burgman, Ferson & Akcakaya, 1993)
      if (length(occupied_indices)) {
        log_common <- log((standard_deviation/transitions[occupied_indices])^2 + 1)
        log_common[which(transitions[occupied_indices] == 0)] <- 0
        transitions[occupied_indices] <- transitions[occupied_indices]*exp(sqrt(log_common)*occupied_correlated_deviates - 0.5*log_common)
      }

      # Set any negative values to zero
      negative_indices <- occupied_indices[which(transitions[occupied_indices] < 0)]
      if (length(negative_indices)) {
        transitions[negative_indices] <- 0
      }

    } # occupied populations & environmental stochasticity?

    ## Generational transition calculations ##

    # Remove populations no longer having carrying capacity (determined above)
    population_abundances[zero_indices] <- 0

    # Sample the next generation's population abundances from a Poisson distribution
    if (occupied_populations) {
      population_abundances[occupied_indices] <- stats::rpois(occupied_populations, transitions[occupied_indices]*population_abundances[occupied_indices])
    }

    # Limit abundances to carrying capacities when "ceiling" density dependence
    if (occupied_populations && density_dependence == "ceiling") {
      above_capacity_indices <- occupied_indices[which(population_abundances[occupied_indices] > carrying_capacity[occupied_indices])]
      if (length(above_capacity_indices)) {
        population_abundances[above_capacity_indices] <- carrying_capacity[above_capacity_indices]
      }
    }

    ## Harvest calculations ##
    if (harvest || "harvested" %in% results_selection) {
      occupied_indices <- occupied_indices[which(as.logical(population_abundances[occupied_indices]))] # > 0
      occupied_populations <- length(occupied_indices)
      harvested <- array(0, populations)
    }
    if (occupied_populations && harvest) {

      # Focus on human presence in occupied cells
      harvest_rate <- array(0, occupied_populations)
      human_presence_indices <- which(as.logical(human_densities[occupied_indices, tm]))
      human_presence_occupied_indices <- occupied_indices[human_presence_indices]

      # Calculate annual harvest rate
      if (length(human_presence_indices)) {
        prey_density <- population_abundances[human_presence_occupied_indices]/harvest_max_n
        prey_z <- prey_density^harvest_z
        max_functional_response <- (harvest_max*prey_z)/(harvest_g + prey_z) # at max human density
        functional_response <- max_functional_response*human_densities[human_presence_occupied_indices, tm] # at current human density
        harvest_rate[human_presence_indices] <- functional_response/prey_density
      }

      # Convert to generational time scale
      harvest_rate <- 1 - (1 - harvest_rate)^years_per_step

      # Calculate numbers harvested
      harvested[occupied_indices] <- stats::rpois(occupied_populations, population_abundances[occupied_indices]*harvest_rate)

      # Remove harvested from population & ensure no negative values
      population_abundances[occupied_indices] <- population_abundances[occupied_indices] - harvested[occupied_indices]
      population_abundances[occupied_indices][which(population_abundances[occupied_indices] < 0)] <- 0

    } # occupied populations & harvest?

    ## Dispersal calculations ##
    if (occupied_populations && dispersal_present) {

      # Apply any spatio-temporal dispersal changes
      if (tm == 1) {
        dispersal_compact_matrix_tm <- dispersal_compact_matrix
      } else if (dispersals_change_over_time && nrow(dispersal_data_changes[[tm]])) { # and tm > 1
        dispersal_compact_matrix_tm[as.matrix(dispersal_data_changes[[tm]][,c("emigrant_row","source_pop")])] <- dispersal_data_changes[[tm]]$dispersal_rate
      }

      # Select dispersals for occupied populations and their non-zero indices
      occupied_dispersals <- dispersal_compact_matrix_tm[, occupied_indices]
      occupied_dispersal_indices <- which(as.logical(occupied_dispersals)) # > 0

      # Modify dispersal rates when dispersal depends on target population carrying capacity K
      if (dispersal_depends_on_target_pop_k) {

        # Calculate the (below-threshold) multipliers
        dd_multipliers <- array(1, populations)
        modify_pop_indices <- which(carrying_capacity < dispersal_target_k)
        dd_multipliers[modify_pop_indices] <- carrying_capacity[modify_pop_indices]/dispersal_target_k

        # Select multipliers via target populations for non-zero occupied dispersals
        selected_dd_multipliers <- dd_multipliers[dispersal_target_pop_map[, occupied_indices][occupied_dispersal_indices]]

        # Apply modifying multipliers to dispersals
        modify_indices <- which(selected_dd_multipliers < 1)
        if (length(modify_indices)) {
          modify_dipersal_indices <- occupied_dispersal_indices[modify_indices]
          occupied_dispersals[modify_dipersal_indices] <- occupied_dispersals[modify_dipersal_indices]*selected_dd_multipliers[modify_indices]
          occupied_dispersal_indices <- which(as.logical(occupied_dispersals)) # > 0
        }

        # Release variables from memory
        modify_pop_indices <- NULL; dd_multipliers <- NULL; selected_dd_multipliers <- NULL; modify_indices <- NULL; modify_dipersal_indices <- NULL

      } # dispersal depends on target pop k?

      # Disperser generation via abundance and corresponding dispersal rates
      occupied_abundances <- population_abundances[occupied_indices]
      occupied_abundances_rep <- population_abundances[dispersal_abundance_rep_indices, occupied_indices]
      dispersers <- array(0, c(dispersal_compact_rows, occupied_populations))

      # Generate dispersers using a binomial distribution
      dispersers[occupied_dispersal_indices] <- stats::rbinom(length(occupied_dispersal_indices), occupied_abundances_rep[occupied_dispersal_indices], occupied_dispersals[occupied_dispersal_indices])

      # Release variables from memory
      occupied_dispersals <- NULL; occupied_abundances_rep <- NULL; occupied_dispersal_indices <- NULL

      # Calculate emigrants
      emigrants <- array(0, occupied_populations)
      emigrants[] <- .colSums(dispersers, m = dispersal_compact_rows, n = occupied_populations)

      # Check consistency of emigrants (not to exceed abundances)
      excessive_indices <- which(emigrants > occupied_abundances)
      if (length(excessive_indices) > 0) { # reduce emigrants to equal abundance via random sampling
        for (excessive_index in excessive_indices) {
          excessive_rows <- which(as.logical(dispersers[, excessive_index])) # > 0
          excessive_dispersers <- dispersers[excessive_rows, excessive_index]
          disperser_reduction <- emigrants[excessive_index] - occupied_abundances[excessive_index]
          # for (remove_row_index in sample(rep(excessive_rows, times = excessive_dispersers), disperser_reduction)) {
          #   dispersers[remove_row_index, excessive_index] <- dispersers[remove_row_index, excessive_index] - 1
          # }
          for (remove_row_index in rep(excessive_rows,
                                       times = excessive_dispersers)[sample(sum(excessive_dispersers),
                                                                            size = disperser_reduction)]) {
            dispersers[remove_row_index, excessive_index] <- dispersers[remove_row_index, excessive_index] - 1
          }
        }
        emigrants[excessive_indices] <- occupied_abundances[excessive_indices]
      }

      # Update population abundances
      population_abundances[occupied_indices] <- population_abundances[occupied_indices] - emigrants

      # Dispersal tracking
      if (dispersal_tracking) {
        results$emigrants[occupied_indices, tm] <- emigrants
      }

      # Release variables from memory
      occupied_abundances <- NULL; emigrants <- NULL; excessive_indices <- NULL

      # Calculate immigrants via dispersal immigrant map
      disperser_indices <- which(as.logical(dispersers)) # >0
      immigrant_array <- dispersal_zero_array
      immigrant_array[dispersal_immigrant_map[, occupied_indices][disperser_indices]] <- dispersers[disperser_indices]
      immigrants <- .colSums(immigrant_array, m = dispersal_compact_rows, n = populations)

      # Update population abundances
      population_abundances[1,] <- population_abundances[1,] + immigrants

      # Dispersal tracking
      if (dispersal_tracking) {
        results$immigrants[, tm] <- immigrants
      }

      # Release variables from memory
      dispersers <- NULL; immigrant_array <- NULL; immigrants <- NULL

    } # occupied populations & dispersal present?

    ## Apply threshold to population abundances ##
    if (!is.null(abundance_threshold)) {
      below_threshold_indices <- which(as.logical(population_abundances) & population_abundances <= abundance_threshold)
      if (length(below_threshold_indices)) {
        population_abundances[below_threshold_indices] <- 0
      }
    }

    ## Results collection ##
    if ("abundance" %in% results_selection) {
      results$abundance[, tm] <- population_abundances[1,]
    }
    if ("ema" %in% results_selection) {
      min_abundances <- pmin(min_abundances, population_abundances[1,])
      results$ema[,tm] <- min_abundances
    }
    if ("extirpation" %in% results_selection) {
      results$extirpation <- pmin(results$extirpation, rep(tm, populations), na.rm = TRUE)
      results$extirpation[which(as.logical(population_abundances))] <- NA
    }
    if ("harvested" %in% results_selection) {
      results$harvested[, tm] <- harvested
    }
    if ("occupancy" %in% results_selection) {
      results$occupancy[tm] <- sum(as.logical(population_abundances))
    }

  } ### End time steps ###

  return(results)

} ### End paleopop ###
