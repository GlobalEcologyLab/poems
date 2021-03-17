#' Runs a stage-based demographic population model simulation.
#'
#' Simulates a stage-based demographic population model and returns simulation results
#' across multiple replicate runs. Processes ran at each simulation time-step include:
#' \enumerate{
#'   \item Density dependence calculations (ceiling, logistic, or user-defined)
#'   \item Environmental stochasticity calculations
#'   \item Stage transition (stochastic) calculations
#'   \item Translocation calculations (user-defined)
#'   \item Harvest calculations (user-defined)
#'   \item Mortality calculations (user-defined)
#'   \item Dispersal calculations (default or user-defined)
#'   \item Results collection
#' }
#'
#' @param inputs Nested list/object with named elements:
#'   \describe{
#'     \item{\code{random_seed}}{Number to seed the random number generation for stochasticity.}
#'     \item{\code{replicates}}{Number of replicate simulation runs (default is 1).}
#'     \item{\code{time_steps}}{Number of simulation time steps.}
#'     \item{\code{years_per_step}}{Number of years per time step (default is 1).}
#'     \item{\code{populations}}{Number of populations.}
#'     \item{\code{coordinates}}{Data frame (or matrix) of X-Y population coordinates.}
#'     \item{\code{stages}}{Number of lifecycle stages.}
#'     \item{\code{initial_abundance}}{Array (or matrix) of initial abundances (at each stage in rows) for each population (in columns).}
#'     \item{\code{stage_matrix}}{Matrix of transition (fecundity & survival) rates between stages at each time step (Leslie/Lefkovitch matrix).}
#'     \item{\code{fecundity_mask}}{Matrix of 0-1 to indicate which (proportions) of transition rates refer to fecundity.}
#'     \item{\code{fecundity_max}}{Maximum transition fecundity rate (in Leslie/Lefkovitch matrix).}
#'     \item{\code{demographic_stochasticity}}{Boolean for choosing demographic stochasticity for transition, dispersal, harvest and/or other processes (default is TRUE).}
#'     \item{\code{standard_deviation}}{Standard deviation matrix (or single value) for applying environmental stochasticity to transition rates.}
#'     \item{\code{correlation}}{List containing either an environmental correlation matrix (correlation_matrix), a pre-calculated transposed (Cholesky) decomposition matrix (t_decomposition_matrix), or a compact transposed (Cholesky) decomposition matrix (t_decomposition_compact_matrix) and a corresponding map of population indices (t_decomposition_compact_map), as per \code{\link{SpatialCorrelation}} class attributes.}
#'     \item{\code{carrying_capacity}}{Array (matrix) of carrying capacity values at each population cell (\emph{populations} rows by \emph{time_steps} columns when across time).}
#'     \item{\code{density_dependence}}{Density dependence can be "ceiling" (default), "logistic" (Ricker), or a user-defined function (optionally nested in a list with additional attributes) for adjusting transition rates: \code{function(params)}, where \emph{params} is a list passed to the function containing:
#'       \describe{
#'         \item{\code{transition_array}}{3D array of transition rates: stages by stages by populations.}
#'         \item{\code{fecundity_mask}}{Matrix of 0-1 to indicate which (proportions) of transition rates refer to fecundity.}
#'         \item{\code{fecundity_max}}{Maximum transition fecundity rate (in Leslie/Lefkovitch matrix).}
#'         \item{\code{carrying_capacity}}{Array of carrying capacity values for each population.}
#'         \item{\code{stage_abundance}}{Matrix of abundances for each stage (rows) and population (columns).}
#'         \item{\code{population_abundance}}{Array of summed population abundances for all stages.}
#'         \item{\code{density_abundance}}{Array of summed population abundances for stages affected by density.}
#'         \item{\code{growth_rate_max}}{Maximum growth rate value or array for populations.}
#'         \item{\code{occupied_indices}}{Array of indices for populations occupied at (current) time step.}
#'         \item{\code{calculate_multipliers}}{Function (\code{function(growth_rates)}) for finding multipliers (when stages > 1) to apply to affected transitions that result in target growth rates (dominant eigenvalues).}
#'         \item{\code{apply_multipliers}}{Function (\code{function(transition_array, multipliers}) for applying multipliers (when stages > 1) to the affected transition rates within a transition array (returns multiplied array).}
#'         \item{\code{simulator}}{\code{\link{SimulatorReference}} object with dynamically accessible \emph{attached} and \emph{results} lists.}
#'         \item{\code{optional attributes}}{Additional numeric attributes when density dependence is optionally nested in a list.}
#'       }
#'       returns a transformed transition 3D array
#'     }
#'     \item{\code{growth_rate_max}}{Maximum growth rate (utilized by density dependence processes).}
#'     \item{\code{density_affects}}{Matrix of booleans or numeric (0-1) indicating the transition vital rates affected by density (default is all).}
#'     \item{\code{density_stages}}{Array of booleans or numeric (0,1) for each stage to indicate which stages are affected by density (default is all).}
#'     \item{\code{density_precision}}{Numeric precision of the calculated multipliers (used when stages > 1) applied to affected transition rates (default is 3 decimal places).}
#'     \item{\code{translocation}}{An optional user-defined function (optionally nested in a list with additional attributes) for applying translocation or spatio-temporal management (to abundances): \code{function(params)}, where \emph{params} is a list passed to the function containing:
#'       \describe{
#'         \item{\code{replicates}}{Number of replicate simulation runs.}
#'         \item{\code{time_steps}}{Number of simulation time steps.}
#'         \item{\code{years_per_step}}{Number of years per time step.}
#'         \item{\code{populations}}{Number of populations.}
#'         \item{\code{stages}}{Number of lifecycle stages.}
#'         \item{\code{demographic_stochasticity}}{Boolean for optionally choosing demographic stochasticity for the transformation.}
#'         \item{\code{density_stages}}{Array of booleans or numeric (0,1) for each stage to indicate which stages are affected by density.}
#'         \item{\code{r}}{Simulation replicate.}
#'         \item{\code{tm}}{Simulation time step.}
#'         \item{\code{carrying_capacity}}{Array of carrying capacity values for each population at time step.}
#'         \item{\code{stage_abundance}}{Matrix of (current) abundance for each stage (rows) and population (columns) at time step.}
#'         \item{\code{occupied_indices}}{Array of indices for populations occupied at (current) time step.}
#'         \item{\code{simulator}}{\code{\link{SimulatorReference}} object with dynamically accessible \emph{attached} and \emph{results} lists.}
#'         \item{\code{additional attributes}}{Additional attributes when the transformation is optionally nested in a list.}
#'       }
#'       returns a transformed stage abundance matrix (or a list with stage abundance and carrying capacity)
#'     }
#'     \item{\code{harvest}}{An optional user-defined function (optionally nested in a list with additional attributes) for applying harvesting (to abundances): \code{function(params)} as per translocation.}
#'     \item{\code{mortality}}{An optional user-defined function (optionally nested in a list with additional attributes) for applying mortality (to abundances): \code{function(params)} as per translocation.}
#'     \item{\code{dispersal}}{Either a matrix of dispersal rates between populations (source columns to target rows) or a list of data frames of non-zero dispersal rates and indices for constructing a compact dispersal matrix, and optional changing rates over time (as per class \code{\link{DispersalGenerator}} \emph{dispersal_data} attribute). Alternatively a user-defined function (optionally nested in a list with additional attributes) may be used: \code{function(params)}, where \emph{params} is a list passed to the function containing:
#'       \describe{
#'         \item{\code{replicates}}{Number of replicate simulation runs.}
#'         \item{\code{time_steps}}{Number of simulation time steps.}
#'         \item{\code{years_per_step}}{Number of years per time step.}
#'         \item{\code{populations}}{Number of populations.}
#'         \item{\code{demographic_stochasticity}}{Boolean for optionally choosing demographic stochasticity for the transformation.}
#'         \item{\code{density_stages}}{Array of booleans or numeric (0,1) for each stage to indicate which stages are affected by density.}
#'         \item{\code{dispersal_stages}}{Array of relative dispersal (0-1) for each stage to indicate the degree to which each stage participates in dispersal.}
#'         \item{\code{r}}{Simulation replicate.}
#'         \item{\code{tm}}{Simulation time step.}
#'         \item{\code{carrying_capacity}}{Array of carrying capacity values for each population at time step.}
#'         \item{\code{stage_abundance}}{Matrix of abundance for each stage (rows) and population (columns) at time step.}
#'         \item{\code{occupied_indices}}{Array of indices for populations occupied at time step.}
#'         \item{\code{simulator}}{\code{\link{SimulatorReference}} object with dynamically accessible \emph{attached} and \emph{results} lists.}
#'         \item{\code{additional attributes}}{Additional attributes when the transformation is optionally nested in a list.}
#'       }
#'       returns the post-dispersal abundance matrix
#'     }
#'     \item{\code{dispersal_stages}}{Array of relative dispersal (0-1) for each stage to indicate the degree to which each stage participates in dispersal (default is 1 for all stages).}
#'     \item{\code{dispersal_source_n_k}}{Dispersal proportion (p) density dependence via source population abundance divided by carrying capacity (n/k), where p is reduced via a linear slope (defined by two list items) from n/k <= \emph{cutoff} (p = 0) to n/k >= \emph{threshold} (aliases: \emph{dispersal_n_k_cutoff} & \emph{dispersal_n_k_threshold}).}
#'     \item{\code{dispersal_target_k}}{Dispersal rate (r) density dependence via target population carrying capacity (k), where r is reduced via a linear slope (through the origin) when k <= \emph{threshold} (alias: \emph{dispersal_k_threshold}).}
#'     \item{\code{dispersal_target_n}}{Dispersal rate (r) density dependence via target population abundance (n), where r is reduced via a linear slope (defined by two list items) from n >= \emph{threshold} to n <= \emph{cutoff} (r = 0) or visa-versa (aliases: \emph{dispersal_n_threshold} & \emph{dispersal_n_cutoff}).}
#'     \item{\code{abundance_threshold}}{Abundance threshold (that needs to be exceeded) for each population to persist.}
#'     \item{\code{simulation_order}}{A vector of simulation process names in configured order of execution (default is "transition", "translocation", "harvest" (plus harvested results), "mortality", "dispersal", "results" (except harvested).}
#'     \item{\code{additional transformation functions}}{Additional user-defined abundance transformation functions (optionally nested in lists with additional attributes) are utilised when listed in \emph{simulation_order} (function as per translocation).}
#'     \item{\code{results_selection}}{List of results selection from: "abundance" (default), "ema", "extirpation", "extinction_location", "harvested", "occupancy"; "summarize" (default) or "replicate".}
#'     \item{\code{result_stages}}{Array of booleans or numeric (0, 1, 2, ...) for each stage to indicate which stages are included/combined (each unique digit > 0; optionally named) in the results (default is 1 for all stages).}
#'   }
#' @return Selected simulation results as a nested list summarized (mean, sd, min, max) across multiple replicates (default), or 2-3D arrays including results for each replicate:
#'   \describe{
#'     \item{\code{abundance}}{Matrix or 3D array of simulation abundance: \emph{populations} rows by \emph{time_steps} columns (by \emph{replicates} deep).}
#'     \item{\code{abundance_stages}}{List of matrices or 3D arrays of simulation abundance for unique stage combinations when present: each \emph{populations} rows by \emph{time_steps} columns (by \emph{replicates} deep).}
#'     \item{\code{all$abundance}}{Array or matrix of total abundance across populations: \emph{time_steps} (rows by \emph{replicates} columns).}
#'     \item{\code{all$abundance_stages}}{List of arrays or matrices of total abundance across populations for unique stage combinations when present: each \emph{time_steps} (rows by \emph{replicates} columns).}
#'     \item{\code{all$ema}}{Array of expected minimum abundance at each time step (averaged across replicates).}
#'     \item{\code{extirpation}}{Array or matrix of extirpation times: \emph{populations} (rows by \emph{replicates} columns).}
#'     \item{\code{all$extirpation}}{Array of extirpation time across populations for each replicate.}
#'     \item{\code{all$extinction_location}}{The weighted centroid of cells occupied in the time-step prior to the extirpation of all populations (if it occurred) for each replicate.}
#'     \item{\code{harvested}}{Matrix or 3D array of individuals harvested: \emph{populations} rows by \emph{time_steps} columns (by \emph{replicates} deep).}
#'     \item{\code{harvested_stages}}{List of matrices or 3D arrays of individuals harvested for unique stage combinations when present: each \emph{populations} rows by \emph{time_steps} columns (by \emph{replicates} deep).}
#'     \item{\code{all$harvested}}{Array or matrix of individuals harvested across populations: \emph{time_steps} (rows by \emph{replicates} columns).}
#'     \item{\code{all$harvested_stages}}{List of arrays or matrices of individuals harvested across populations for unique stage combinations when present: each \emph{time_steps} (rows by \emph{replicates} columns).}
#'     \item{\code{all$occupancy}}{Array or matrix of the number of populations occupied at each time-step: \emph{time_steps} (rows by \emph{replicates} columns).}
#'     \item{\code{additional results}}{Additional results may be attached via user-defined functions (using \code{params$simulator$results}).}
#'   }
#'
#' @examples
#' # U Island example region
#' coordinates <- data.frame(x = rep(seq(177.01, 177.05, 0.01), 5),
#'                           y = rep(seq(-18.01, -18.05, -0.01), each = 5))
#' template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
#' template_raster[][-c(7, 9, 12, 14, 17:19)] <- NA # make U Island
#' region <- Region$new(template_raster = template_raster)
#' # Harvest function
#' harvest <- list(rate = 0.3,
#'                 function(params) round(params$stage_abundance*(1 - params$rate)))
#' # Population model
#' stage_matrix <- matrix(c(0,   2.5, # Leslie/Lefkovitch matrix
#'                          0.8, 0.5), nrow = 2, ncol = 2, byrow = TRUE)
#' pop_model <- PopulationModel$new(region = region,
#'                                  time_steps = 10, # years
#'                                  populations = region$region_cells, # 7
#'                                  stage_matrix = stage_matrix,
#'                                  initial_abundance = rep(10, 7),
#'                                  carrying_capacity = array(70:1, c(7, 10)),
#'                                  harvest = harvest,
#'                                  results_selection = c("abundance", "harvested"))
#' # Simulations
#' population_simulator(pop_model) # model
#' inputs <- pop_model$get_attributes()
#' population_simulator(inputs) # list
#'
#' @include SimulatorReference.R
#' @include population_transitions.R
#' @include population_env_stoch.R
#' @include population_density.R
#' @include population_transformation.R
#' @include population_dispersal.R
#' @include population_results.R
#' @export population_simulator

population_simulator <- function(inputs) {

  # Stop if minimal inputs are not present
  if (is.null(inputs$time_steps) || is.null(inputs$populations) || is.null(inputs$initial_abundance) ||
      is.null(inputs$stage_matrix) || is.null(inputs$carrying_capacity)) {
    incomplete_inputs <- if (is.null(inputs$time_steps)) "time_steps"
    incomplete_inputs <- c(incomplete_inputs, if (is.null(inputs$populations)) "populations")
    incomplete_inputs <- c(incomplete_inputs, if (is.null(inputs$initial_abundance)) "initial_abundance")
    incomplete_inputs <- c(incomplete_inputs, if (is.null(inputs$stage_matrix)) "stage_matrix")
    incomplete_inputs <- c(incomplete_inputs, if (is.null(inputs$carrying_capacity)) "carrying_capacity")
    stop(paste("Minimal inputs required to run simulation should include:",
               paste(incomplete_inputs, collapse = ", ")), call. = FALSE)
  }

  ## Unpack inputs and calculate/initialize re-usable variables ##

  # General simulation settings
  if (!is.null(inputs$random_seed)) {
    set.seed(inputs$random_seed)
  }
  replicates <- ifelse(is.null(inputs$replicates), 1, inputs$replicates)
  time_steps <- inputs$time_steps
  years_per_step <- ifelse(is.null(inputs$years_per_step), 1, inputs$years_per_step)
  populations <- inputs$populations

  # Stage-based transition matrix
  stage_matrix <- as.matrix(inputs$stage_matrix)
  stages <- ifelse(is.null(inputs$stages), nrow(stage_matrix), inputs$stages)

  # Initial abundance for each stage and population
  if ("PopulationModel" %in% class(inputs) && !is.null(inputs$region) && inputs$region$use_raster &&
      any(class(inputs$initial_abundance) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
    initial_abundance <- as.matrix(inputs$initial_abundance[inputs$region$region_indices])
  } else {
    initial_abundance <- as.matrix(inputs$initial_abundance)
  }
  if (nrow(initial_abundance) == populations) {
    initial_abundance <- t(initial_abundance) # population columns
  }
  if (stages > 1 && nrow(initial_abundance) == 1) { # distribute rows via stable stages
    stable_stage_dist <- ((stage_matrix %*% (Re((eigen(stage_matrix)$vectors)[,1])))/(sum((stage_matrix %*% (Re((eigen(stage_matrix)$vectors)[,1]))))))[,1]
    stage_abundance <- as.matrix(round(matrix(stable_stage_dist)[, rep(1, populations)]*initial_abundance[rep(1, stages),]))
    abundance_corrections <- initial_abundance - .colSums(stage_abundance, m = stages, n = populations)
    for (i in which(abundance_corrections != 0)) { # ensure the original initial abundance values are retained
      sample_indices <- sample(1:stages, size = abs(abundance_corrections[i]), replace = TRUE, prob = stable_stage_dist)
      for (sample_index in sample_indices) {
        stage_abundance[sample_index, i] <- stage_abundance[sample_index, i] + ifelse(abundance_corrections[i] > 0, 1, -1)
      }
    }
    initial_abundance <- stage_abundance
  }
  population_abundance <- .colSums(initial_abundance, m = stages, n = populations)

  # Fecundity and survival matrices
  fecundity_mask <- inputs$fecundity_mask
  if (is.null(fecundity_mask)) {
    if (nrow(stage_matrix) > 1) {
      fecundity_mask <- +upper.tri(stage_matrix)
    } else {
      fecundity_mask <- +(stage_matrix > 0)
    }
  }
  fecundity_matrix <- fecundity_mask*stage_matrix
  fecundity_max <- inputs$fecundity_max
  survival_matrix <- (1 - fecundity_mask)*stage_matrix

  # Transition setup (generates function)
  demographic_stochasticity <- ifelse(is.null(inputs$demographic_stochasticity), TRUE, inputs$demographic_stochasticity)
  transition_function <- population_transitions(populations, demographic_stochasticity, fecundity_matrix,
                                                fecundity_max, survival_matrix)

  # Environmental stochasticity and correlation setup (generates function)
  standard_deviation <- ifelse(is.null(inputs$standard_deviation), 0, inputs$standard_deviation)
  if (stages > 1 && length(standard_deviation) == 1) { # calculate via dominant eigen value
    standard_deviation <- stage_matrix*standard_deviation/Re((eigen(stage_matrix, only.values = TRUE)$values)[1])
  }
  environmental_stochasticity <- any(standard_deviation*stage_matrix > 0)
  if (environmental_stochasticity) {
    env_stoch_function <- population_env_stoch(populations, fecundity_matrix, fecundity_max, survival_matrix,
                                               standard_deviation, inputs$correlation)
  }

  # Simulator reference object for dynamic attachments and results (accessed via user-defined functions)
  simulator <- SimulatorReference$new()

  # Capacity and density dependence setup (generates functions)
  if ("PopulationModel" %in% class(inputs) && !is.null(inputs$region) && inputs$region$use_raster &&
      any(class(inputs$carrying_capacity) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
    carrying_capacity_matrix <- matrix(inputs$carrying_capacity[inputs$region$region_indices], nrow = populations)
  } else {
    carrying_capacity_matrix <- matrix(inputs$carrying_capacity, nrow = populations)
  }
  if (any(is.na(carrying_capacity_matrix))) {
    carrying_capacity_matrix[which(is.na(carrying_capacity_matrix))] <- 0
  }
  carrying_capacity_t_max <- ncol(carrying_capacity_matrix)
  if (carrying_capacity_t_max == 1) { # no temporal trend in K
    carrying_capacity <- carrying_capacity_matrix[, 1]
  }
  density_dependence <- inputs$density_dependence
  if (is.null(density_dependence)) {
    density_dependence <- "ceiling"
  }
  density_stages <- inputs$density_stages
  density_function <- population_density(populations, stage_matrix, fecundity_mask, fecundity_max, density_dependence,
                                         inputs$growth_rate_max, inputs$density_affects, density_stages,
                                         density_precision = inputs$density_precision, simulator)
  if ((is.function(density_dependence) || is.list(density_dependence))) {
    density_dependence <- "adjusts_transitions"
  }

  # Translocation, harvest, mortality, and translocation (generates nested functions for transforming abundances)
  if (is.null(density_stages)) { # default is all
    density_stages <- rep(1, stages)
  }
  translocation_function <- population_transformation(replicates, time_steps, years_per_step, populations, demographic_stochasticity,
                                                      density_stages, inputs$translocation, simulator, name = "translocation")
  harvest_function <- population_transformation(replicates, time_steps, years_per_step, populations, demographic_stochasticity,
                                                density_stages, inputs$harvest, simulator, name = "harvest")
  mortality_function <- population_transformation(replicates, time_steps, years_per_step, populations, demographic_stochasticity,
                                                  density_stages, inputs$mortality, simulator, name = "mortality")

  # Dispersal setup (generates function)
  dispersal_stages <- inputs$dispersal_stages
  if (is.null(dispersal_stages)) {
    dispersal_stages <- array(1, stages)
  }
  dispersal_source_n_k <- inputs$dispersal_source_n_k
  if (is.null(dispersal_source_n_k)) { # try aliases
    dispersal_source_n_k <- list(cutoff = inputs$dispersal_n_k_cutoff, threshold = inputs$dispersal_n_k_threshold)
  }
  dispersal_target_k <- inputs$dispersal_target_k
  if (is.null(dispersal_target_k)) { # try alias
    dispersal_target_k <- inputs$dispersal_k_threshold
  }
  dispersal_target_n <- inputs$dispersal_target_n
  if (is.null(dispersal_target_n)) { # try aliases
    dispersal_target_n <- list(threshold = inputs$dispersal_n_threshold, cutoff = inputs$dispersal_n_cutoff)
  }
  dispersal_function <- population_dispersal(replicates, time_steps, years_per_step, populations, demographic_stochasticity,
                                             density_stages, inputs$dispersal, dispersal_stages, dispersal_source_n_k,
                                             dispersal_target_k, dispersal_target_n, simulator)

  # Abundance threshold
  abundance_threshold <- inputs$abundance_threshold

  # Simulation order
  simulation_order <- inputs$simulation_order
  if (!is.character(simulation_order)) {
    simulation_order <- c("transition", "translocation", "harvest", "mortality", "dispersal", "results")
  }
  if (!"results" %in% simulation_order) {
    simulation_order <- c(simulation_order, "results")
  }

  # Other user-defined abundance transformation functions
  other_user_defined <- simulation_order[which(!(simulation_order %in% c("transition", "translocation", "harvest", "mortality", "dispersal", "results")))]
  if (length(other_user_defined)) {
    other_functions = list()
    for (other in other_user_defined) {
      if (other %in% names(inputs)) {
        other_functions[[other]] <- population_transformation(replicates, time_steps, years_per_step, populations,
                                                              demographic_stochasticity, density_stages,
                                                              inputs[[other]], simulator, name = other)
      } else if ("PopulationModel" %in% class(inputs)) {
        other_functions[[other]] <- population_transformation(replicates, time_steps, years_per_step, populations,
                                                              demographic_stochasticity, density_stages,
                                                              inputs$get_attribute(other), simulator, name = other)
      }
    }
  }

  # Results setup (generates nested functions) - results placed in simulator reference object for user-defined function accessiblity
  results_selection <- inputs$results_selection
  result_functions <- population_results(replicates, time_steps, inputs$coordinates, initial_abundance,
                                         results_selection = results_selection, result_stages = inputs$result_stages)
  results_list <- result_functions$initialize_attributes()

  ### Replicates ###
  for (r in 1:replicates) {

    # Initialize populations
    stage_abundance <- initial_abundance
    population_abundance <- .colSums(initial_abundance, m = stages, n = populations)
    occupied_indices <- which(as.logical(population_abundance)) # > 0
    occupied_populations <- length(occupied_indices)

    # Initialize harvested
    if ("harvested" %in% results_selection) {
      harvested <- array(0 , c(stages, populations))
    } else {
      harvested <- NULL
    }

    # (Re-)Initialize result collection variables
    results_list <- result_functions$initialize_replicate(results_list)

    ### Simulation time steps ###
    for (tm in 1:time_steps) {

      # Set transition 3D array (a Leslie/Lefkovitch matrix for each population)
      transition_array <- array(stage_matrix, c(stages, stages, populations))

      # Load carrying capacity for each population for time if temporal trend in K
      if (carrying_capacity_t_max > 1) {
        carrying_capacity <- carrying_capacity_matrix[, min(tm, carrying_capacity_t_max)]
      }

      ## Run simulation processes in configured order ##
      for (process in simulation_order) {

        ## Transition, density dependence and environmental stochasticity calculations ##
        if (process == "transition") {

          if (occupied_populations) {

            # Apply density dependence function to transition array and set fecundity and survival arrays
            if (density_dependence %in% c("logistic", "adjusts_transitions")) {
              transition_array <- density_function(transition_array, carrying_capacity, stage_abundance, occupied_indices)
              fecundity_array <- transition_array*array(fecundity_mask, c(stages, stages, populations))
              survival_array <- transition_array*(1 - array(fecundity_mask, c(stages, stages, populations)))
              # Remove populations without capacity
              zero_indices <- which(.colSums(transition_array[, ,occupied_indices], m = stages*stages, n = occupied_populations) <= 0)
              if (length(zero_indices)) {
                stage_abundance[, occupied_indices[zero_indices]] <- 0
                population_abundance[occupied_indices[zero_indices]] <- 0
                occupied_indices <- which(as.logical(population_abundance)) # > 0
                occupied_populations <- length(occupied_indices)
              }
            } else {
              fecundity_array <- array(fecundity_matrix, c(stages, stages, populations))
              survival_array <- array(survival_matrix, c(stages, stages, populations))
            }
          }

          if (occupied_populations) {

            # Apply environmental stochasticity to fecundity and survival arrays
            if (environmental_stochasticity) {
              generated_arrays <- env_stoch_function(fecundity_array, survival_array, occupied_indices)
              fecundity_array <- generated_arrays$fecundity_array
              survival_array <- generated_arrays$survival_array
            }

            # Perform stage-based transitions
            stage_abundance <- transition_function(fecundity_array, survival_array, stage_abundance, occupied_indices)

            # Limit abundances to carrying capacity when "ceiling" density dependence
            if (density_dependence == "ceiling") {
              stage_abundance <- density_function(carrying_capacity, stage_abundance)
            }
          }
        }

        ## Translocation calculations ##
        if (process == "translocation" && is.function(translocation_function)) {
          transformed <- translocation_function(r, tm, carrying_capacity, stage_abundance, occupied_indices)
          stage_abundance <- transformed$stage_abundance
          if ("carrying_capacity" %in% names(transformed)) carrying_capacity <- transformed$carrying_capacity
        }

        ## Harvest calculations ##
        if (process == "harvest") {
          if (occupied_populations && is.function(harvest_function)) {
            preharvest_abundance <- stage_abundance
            transformed <- harvest_function(r, tm, carrying_capacity, stage_abundance, occupied_indices)
            stage_abundance <- transformed$stage_abundance
            if ("carrying_capacity" %in% names(transformed)) carrying_capacity <- transformed$carrying_capacity
            harvested <- preharvest_abundance - stage_abundance
          } else {
            harvested <- 0*stage_abundance
          }
          if ("harvested" %in% results_selection) {
            results_list <- result_functions$calculate_at_timestep(r, tm, NULL, harvested, results_list)
          }
        }

        ## Mortality calculations ##
        if (occupied_populations && process == "mortality" && is.function(mortality_function)) {
          transformed <- mortality_function(r, tm, carrying_capacity, stage_abundance, occupied_indices)
          stage_abundance <- transformed$stage_abundance
          if ("carrying_capacity" %in% names(transformed)) carrying_capacity <- transformed$carrying_capacity
        }

        ## Dispersal calculations ##
        if (occupied_populations && process == "dispersal" && is.function(dispersal_function)) {
          stage_abundance <- dispersal_function(r, tm, carrying_capacity, stage_abundance, occupied_indices)
        }

        ## Other user-defined abundance transformation functions ##
        if (process %in% other_user_defined && is.function(other_functions[[process]])) {
          transformed <- other_functions[[process]](r, tm, carrying_capacity, stage_abundance, occupied_indices)
          stage_abundance <- transformed$stage_abundance
          if ("carrying_capacity" %in% names(transformed)) carrying_capacity <- transformed$carrying_capacity
        }

        ## Apply threshold to population abundances ##
        if (process == "results") {
          if (occupied_populations && !is.null(abundance_threshold)) {
            below_threshold_indices <- which(as.logical(population_abundance) & population_abundance <= abundance_threshold)
            if (length(below_threshold_indices)) {
              stage_abundance[, below_threshold_indices] <- 0
              population_abundance[below_threshold_indices] <- 0
            }
          }
        }

        # Update population abundance and occupied indices
        population_abundance <- .colSums(stage_abundance, m = stages, n = populations)
        occupied_indices <- which(as.logical(population_abundance)) # > 0
        occupied_populations <- length(occupied_indices)

        ## Results calculation/collection ##
        if (process == "results") {
          results_list <- result_functions$calculate_at_timestep(r, tm, stage_abundance, NULL, results_list)
        }

      } ## end simulation order loop ##

    } ### End time steps ###

    ## Results calculation/collection ##
    results_list <- result_functions$calculate_at_replicate(r, stage_abundance, results_list)

  } ### End replicates ###

  ## Finalize results calculation and collation ##
  results_list <- result_functions$finalize_attributes(results_list)

  return(c(results_list, simulator$results))

} ### End population simulator ###
