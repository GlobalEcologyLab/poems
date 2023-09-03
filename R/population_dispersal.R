#' Nested functions for population dispersal.
#'
#' Modular functions for the population simulator for performing dispersal of stage
#' abundance at a specified time step via dispersal rates provided.
#'
#' @param replicates Number of replicate simulation runs.
#' @param time_steps Number of simulation time steps.
#' @param years_per_step Number of years per time step.
#' @param populations Number of populations.
#' @param demographic_stochasticity Boolean for optionally choosing demographic stochasticity for the transformation.
#' @param density_stages Array of booleans or numeric (0,1) for each stage to indicate which stages are affected by density.
#' @param dispersal Either a matrix of dispersal rates between populations (source columns to target rows) or a list of data frames of non-zero dispersal rates and indices for constructing a compact dispersal matrix, and optional changing rates over time (as per class \code{\link{DispersalGenerator}} \emph{dispersal_data} attribute). Alternatively a user-defined function (optionally nested in a list with additional attributes) may be used: \code{function(params)}, where \emph{params} is a list passed to the function containing:
#'   \describe{
#'     \item{\code{replicates}}{Number of replicate simulation runs.}
#'     \item{\code{time_steps}}{Number of simulation time steps.}
#'     \item{\code{years_per_step}}{Number of years per time step.}
#'     \item{\code{populations}}{Number of populations.}
#'     \item{\code{stages}}{Number of life cycle stages.}
#'     \item{\code{demographic_stochasticity}}{Boolean for optionally choosing demographic stochasticity for the transformation.}
#'     \item{\code{density_stages}}{Array of booleans or numeric (0,1) for each stage to indicate which stages are affected by density.}
#'     \item{\code{dispersal_stages}}{Array of relative dispersal (0-1) for each stage to indicate the degree to which each stage participates in dispersal.}
#'     \item{\code{dispersal_source_n_k}}{Dispersal proportion (p) density dependence via source population abundance divided by carrying capacity (n/k), where p is reduced via a linear slope (defined by two list items) from n/k <= \emph{cutoff} (p = 0) to n/k >= \emph{threshold}.}
#'     \item{\code{dispersal_target_k}}{Dispersal rate (r) density dependence via target population carrying capacity (k), where r is reduced via a linear slope (through the origin) when k <= \emph{threshold}.}
#'     \item{\code{dispersal_target_n}}{Dispersal rate (r) density dependence via target population abundance (n), where r is reduced via a linear slope (defined by two list items) from n >= \emph{threshold} to n <= \emph{cutoff} (r = 0) or visa-versa.}
#'     \item{\code{dispersal_target_n_k}}{Dispersal rate (r) density dependence via target population abundance divided by carrying capacity (n/k), where r is reduced via a linear slope (defined by two list items) from n/k >= \emph{threshold} to n/k <= \emph{cutoff} (r = 0) or visa-versa.}
#'     \item{\code{r}}{Simulation replicate.}
#'     \item{\code{tm}}{Simulation time step.}
#'     \item{\code{carrying_capacity}}{Array of carrying capacity values for each population at time step.}
#'     \item{\code{stage_abundance}}{Matrix of abundance for each stage (rows) and population (columns) at time step.}
#'     \item{\code{occupied_indices}}{Array of indices for populations occupied at time step.}
#'     \item{\code{simulator}}{\code{\link{SimulatorReference}} object with dynamically accessible \emph{attached} and \emph{results} lists.}
#'     \item{\code{additional attributes}}{Additional attributes when the transformation is optionally nested in a list.}
#'   }
#'   returns the post-dispersal abundance matrix
#' @param dispersal_stages Array of relative dispersal (0-1) for each stage to indicate the degree to which each stage participates in dispersal (default is 1 for all stages).
#' @param dispersal_source_n_k Dispersal proportion (p) density dependence via source population abundance divided by carrying capacity (n/k), where p is reduced via a linear slope (defined by two list items) from n/k <= \emph{cutoff} (p = 0) to n/k >= \emph{threshold} or visa-versa.
#' @param dispersal_target_k Dispersal rate (r) density dependence via target population carrying capacity (k), where r is reduced via a linear slope (through the origin) when k <= \emph{threshold}.
#' @param dispersal_target_n Dispersal rate (r) density dependence via target population abundance (n), where r is reduced via a linear slope (defined by two list items) from n >= \emph{threshold} to n <= \emph{cutoff} (r = 0) or visa-versa.
#' @param dispersal_target_n_k Dispersal rate (r) density dependence via target population abundance divided by carrying capacity (n/k), where r is reduced via a linear slope (defined by two list items) from n/k >= \emph{threshold} to n/k <= \emph{cutoff} (r = 0) or visa-versa.
#' @param simulator \code{\link{SimulatorReference}} object with dynamically accessible \emph{attached} and \emph{results} lists.
#' @return Dispersal function: \code{function(r, tm, carrying_capacity, stage_abundance, occupied_indices)}, where:
#'   \describe{
#'     \item{\code{r}}{Simulation replicate.}
#'     \item{\code{tm}}{Simulation time step.}
#'     \item{\code{carrying_capacity}}{Array of carrying capacity values for each population at time step.}
#'     \item{\code{stage_abundance}}{Matrix of abundance for each stage (rows) and population (columns) at time step.}
#'     \item{\code{occupied_indices}}{Array of indices for populations occupied at time step.}
#'     \item{\code{returns}}{New stage abundance matrix with dispersal applied.}
#'   }
#' @export population_dispersal

population_dispersal <- function(replicates,
                                 time_steps,
                                 years_per_step,
                                 populations,
                                 demographic_stochasticity,
                                 density_stages,
                                 dispersal,
                                 dispersal_stages,
                                 dispersal_source_n_k,
                                 dispersal_target_k,
                                 dispersal_target_n,
                                 dispersal_target_n_k = NULL,
                                 simulator) {

  if (is.null(dispersal)) { # no dispersal
    return(NULL)
  }

  # User-defined function?
  if (is.function(dispersal) || (is.list(dispersal) && length(which(unlist(lapply(dispersal, is.function)))))) {

    # Derive stages
    stages <- length(dispersal_stages)

    # Unpack transformation function and additional attributes from a list
    additional_attributes <- list()
    if (is.list(dispersal)) {
      function_index <- which(unlist(lapply(dispersal, is.function)))
      additional_attributes <- dispersal[-function_index]
      dispersal <- dispersal[[function_index]]
    }

    if (is.function(dispersal)) { # user-defined function

      # List of parameters to pass to the user-defined function
      params <- c(list(replicates = replicates, time_steps = time_steps, years_per_step = years_per_step,
                       populations = populations, stages = stages, demographic_stochasticity = demographic_stochasticity,
                       density_stages = density_stages, dispersal_stages = dispersal_stages,
                       dispersal_source_n_k = dispersal_source_n_k, dispersal_target_k = dispersal_target_k,
                       dispersal_target_n = dispersal_target_n, dispersal_target_n_k = dispersal_target_n_k,
                       simulator = simulator),
                  additional_attributes)

      ## Create a nested function for applying user-defined dispersal of stage abundance ##
      user_defined_function <- function(r, tm, carrying_capacity, stage_abundance, occupied_indices) {

        # Add attributes to be made available to the user-defined function
        params$r <- r
        params$tm <- tm
        params$carrying_capacity <- carrying_capacity
        params$stage_abundance <- stage_abundance
        params$occupied_indices <- occupied_indices

        # Run user-defined dispersal function
        tryCatch({
          stage_abundance <- dispersal(params)
        },
        error = function(e){
          stop(paste("Error produced within user-defined dispersal function:", as.character(e)), call. = FALSE)
        })

        # Warn if any negative or non-finite
        if (any(!is.finite(stage_abundance))) {
          warning("Non-finite abundances returned by user-defined dispersal function", call. = FALSE)
        }
        if (any(stage_abundance[which(is.finite(stage_abundance))] < 0)) {
          warning("Negative abundances returned by user-defined dispersal function", call. = FALSE)
        }

        return(stage_abundance)
      }

      return(user_defined_function)
    }
  }

  # Assumed to be matrix or generated dispersal data

  # Initialize reusable dispersal attributes
  if (is.matrix(dispersal)) { # create compact matrix data

    # Calculate the indices of non-zero dispersals
    dispersal_data <- data.frame(which(dispersal > 0, arr.ind = TRUE))
    dispersal_data <- dispersal_data[order(dispersal_data[, 2], dispersal_data[, 1]),]
    names(dispersal_data) <- c("target_pop", "source_pop")

    # Calculate indices for constructing compacted dispersal matrices for emigrants and immigrants
    dispersal_rows <- tabulate(dispersal_data$source_pop, nbins = populations)
    dispersal_cols <- tabulate(dispersal_data$target_pop, nbins = populations)
    dispersal_compact_rows <- max(dispersal_rows, dispersal_cols)
    compact_emigrant_matrix <- array(1:dispersal_compact_rows, c(dispersal_compact_rows, populations))
    compact_immigrant_matrix <- compact_emigrant_matrix*(compact_emigrant_matrix <= matrix(dispersal_cols, nrow = dispersal_compact_rows, ncol = populations, byrow = TRUE))
    compact_emigrant_matrix <- compact_emigrant_matrix*(compact_emigrant_matrix <= matrix(dispersal_rows, nrow = dispersal_compact_rows, ncol = populations, byrow = TRUE))

    # Map the row of each compact matrix to the original target (for emigrants) or source (for immigrants) populations
    dispersal_data$emigrant_row <- which(compact_emigrant_matrix > 0, arr.ind = TRUE, useNames = FALSE)[,1]
    dispersal_data$immigrant_row <- which(compact_immigrant_matrix > 0, arr.ind = TRUE, useNames = FALSE)[,1]
    target_sorted_indices <- dispersal_data[order(dispersal_data$target_pop, dispersal_data$source_pop), c("target_pop", "source_pop")]
    dispersal_data$immigrant_row <- dispersal_data$immigrant_row[order(target_sorted_indices$source_pop, target_sorted_indices$target_pop)]

    # Add dispersal rates
    dispersal_data$dispersal_rate <- dispersal[as.matrix(dispersal_data[c("target_pop", "source_pop")])]
    dispersals_change_over_time <- FALSE

    # Release variables from memory
    dispersal_rows <- NULL; dispersal_cols <- NULL; compact_emigrant_matrix = NULL; compact_immigrant_matrix = NULL; target_sorted_indices = NULL

  } else if (is.list(dispersal) && is.data.frame(dispersal[[1]]) && nrow(dispersal[[1]])) { # compact matrix data in list (as per DispersalModel class)

    # Unpack dispersal data and determine compact matrix dimensions
    dispersal_data <- dispersal[[1]]
    dispersal_compact_rows <- max(dispersal_data[, c("emigrant_row", "immigrant_row")])

    # Are dispersals changing over time?
    dispersals_change_over_time <- (length(dispersal) > 1)
    if (dispersals_change_over_time) {
      dispersal_data_changes <- dispersal
      dispersal_data_changes[[1]] <- dispersal_data_changes[[1]][NULL,]
    }

  } else {

    return(NULL)
  }

  # Release dispersal from memory
  dispersal <- NULL

  # Create a compact matrix of dispersal rates
  dispersal_compact_matrix <- array(0, c(dispersal_compact_rows, populations))
  dispersal_compact_matrix[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <- dispersal_data$dispersal_rate

  # Does dispersal depend on source population abundance N divided by carrying capacity K?
  dispersal_depends_on_source_pop_n_k <- (is.list(dispersal_source_n_k) && (is.numeric(dispersal_source_n_k$cutoff) ||
                                                                              is.numeric(dispersal_source_n_k$threshold)))

  # Does dispersal depend on target population carrying capacity K, abundance N, or N/K?
  dispersal_depends_on_target_pop_k <- is.numeric(dispersal_target_k)
  dispersal_depends_on_target_pop_n <- (is.list(dispersal_target_n) && (is.numeric(dispersal_target_n$threshold) ||
                                                                          is.numeric(dispersal_target_n$cutoff)))
  dispersal_depends_on_target_pop_n_k <- (is.list(dispersal_target_n_k) && (is.numeric(dispersal_target_n_k$threshold) ||
                                                                              is.numeric(dispersal_target_n_k$cutoff)))

  # Setup density dependence dispersal parameters
  if (dispersal_depends_on_source_pop_n_k) {

    # Convert NULL to zero in source N/K cutoff or one in threshold
    if (dispersal_depends_on_source_pop_n_k) {
      if (is.null(dispersal_source_n_k$cutoff)) dispersal_source_n_k$cutoff <- 0
      if (is.null(dispersal_source_n_k$threshold)) dispersal_source_n_k$threshold <- 1
    }

    # Check threshold > cutoff
    if (dispersal_source_n_k$threshold <= dispersal_source_n_k$cutoff) {
      dispersal_depends_on_source_pop_n_k <- FALSE
      warning("Dispersal density dependence for source N/K threshold must be greater than cutoff => not used", call. = FALSE)
    }
  }
  if (dispersal_depends_on_target_pop_k || dispersal_depends_on_target_pop_n || dispersal_depends_on_target_pop_n_k) {

    if (dispersal_depends_on_target_pop_n) {

      # Convert NULL to zero in target N threshold or cutoff
      if (is.null(dispersal_target_n$threshold)) dispersal_target_n$threshold <- 0
      if (is.null(dispersal_target_n$cutoff)) dispersal_target_n$cutoff <- 0
    }
    if (dispersal_depends_on_target_pop_n_k) {

      # Convert NULL to zero in target N/K threshold or cutoff
      if (is.null(dispersal_target_n_k$threshold)) dispersal_target_n_k$threshold <- 0
      if (is.null(dispersal_target_n_k$cutoff)) dispersal_target_n_k$cutoff <- 0
    }

    # Create a map of compact array indices for mapping dispersers (emigrants) to target populations
    dispersal_target_pop_map <- array(0, c(dispersal_compact_rows, populations))
    dispersal_target_pop_map[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <- dispersal_data$target_pop
  }

  # Create a map of compact array indices for mapping dispersers (emigrants) to immigrants
  dispersal_compact_indices <- array(1:(dispersal_compact_rows*populations), c(dispersal_compact_rows, populations))
  dispersal_immigrant_map <- array(0, c(dispersal_compact_rows, populations))
  dispersal_immigrant_map[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <- dispersal_compact_indices[as.matrix(dispersal_data[, c("immigrant_row", "target_pop")])]

  # Release variables from memory
  dispersal_data <- NULL; dispersal_compact_indices <- NULL

  ## Create a nested function for performing dispersal ##
  dispersal_function = function(r, tm, carrying_capacity, stage_abundance, occupied_indices) {

    # Calculate occupied population number
    occupied_populations <- length(occupied_indices)

    # Apply any spatio-temporal dispersal changes
    dispersal_compact_matrix_tm <- simulator$attached$dispersal_compact_matrix_tm
    if (tm == 1 || !dispersals_change_over_time) {
      dispersal_compact_matrix_tm <- dispersal_compact_matrix
    } else if (dispersals_change_over_time && nrow(dispersal_data_changes[[tm]])) { # and tm > 1
      dispersal_compact_matrix_tm[as.matrix(dispersal_data_changes[[tm]][, c("emigrant_row","source_pop")])] <- dispersal_data_changes[[tm]]$dispersal_rate
    }
    simulator$attached$dispersal_compact_matrix_tm <- dispersal_compact_matrix_tm

    # Select dispersals for occupied populations
    occupied_dispersals <- dispersal_compact_matrix_tm[, occupied_indices]

    # Calculate density abundance
    if (dispersal_depends_on_source_pop_n_k || dispersal_depends_on_target_pop_n || dispersal_depends_on_target_pop_n_k) {
      density_abundance <- .colSums(stage_abundance*as.numeric(density_stages), m = length(density_stages), n = populations)
    }

    # Modify dispersal rates when dispersal depends on source population N/K
    if (dispersal_depends_on_source_pop_n_k) {

      # Density dependent multipliers
      dd_multipliers <- array(1, populations)

      # Calculate the source N/K multipliers
      abundance_on_capacity <- density_abundance/carrying_capacity
      dd_multipliers[which(abundance_on_capacity <= dispersal_source_n_k$cutoff)] <- 0
      modify_pop_indices <- which(carrying_capacity > 0 & dd_multipliers > 0 &
                                    abundance_on_capacity < dispersal_source_n_k$threshold)
      dd_multipliers[modify_pop_indices] <- ((abundance_on_capacity[modify_pop_indices] -
                                                array(dispersal_source_n_k$cutoff, populations)[modify_pop_indices])/
                                               array(dispersal_source_n_k$threshold - dispersal_source_n_k$cutoff,
                                                     populations)[modify_pop_indices]*
                                               dd_multipliers[modify_pop_indices])

      # Apply modifying multipliers to dispersals
      occupied_dispersals <- (occupied_dispersals*matrix(dd_multipliers[occupied_indices],
                                                         nrow = dispersal_compact_rows,
                                                         ncol = occupied_populations, byrow = TRUE))

    } # dispersal depends on source pop N/K?

    # Select occupied dispersal non-zero indices
    occupied_dispersal_indices <- which(as.logical(occupied_dispersals)) # > 0

    # Modify dispersal rates when dispersal depends on target population K, N, or N/K
    if (dispersal_depends_on_target_pop_k || dispersal_depends_on_target_pop_n || dispersal_depends_on_target_pop_n_k) {

      # Density dependent multipliers
      dd_multipliers <- array(1, populations)

      # Calculate the (below-threshold) target K multipliers
      if (dispersal_depends_on_target_pop_k) {
        modify_pop_indices <- which(carrying_capacity < dispersal_target_k)
        dd_multipliers[modify_pop_indices] <- (carrying_capacity[modify_pop_indices]/
                                                 array(dispersal_target_k, populations)[modify_pop_indices])
      }

      # Calculate the target N multipliers
      if (dispersal_depends_on_target_pop_n) {
        if (all(dispersal_target_n$threshold < dispersal_target_n$cutoff)) { # overcrowded cell avoidance \
          dd_multipliers[which(density_abundance >= dispersal_target_n$cutoff)] <- 0
          modify_pop_indices <- which(density_abundance > dispersal_target_n$threshold & dd_multipliers > 0)
          dd_multipliers[modify_pop_indices] <- ((array(dispersal_target_n$cutoff, populations)[modify_pop_indices] -
                                                    density_abundance[modify_pop_indices])/
                                                   array(dispersal_target_n$cutoff - dispersal_target_n$threshold,
                                                         populations)[modify_pop_indices]*
                                                   dd_multipliers[modify_pop_indices])
        } else if (all(dispersal_target_n$threshold > dispersal_target_n$cutoff)) { # seek company /
          dd_multipliers[which(density_abundance <= dispersal_target_n$cutoff)] <- 0
          modify_pop_indices <- which(density_abundance < dispersal_target_n$threshold & dd_multipliers > 0)
          dd_multipliers[modify_pop_indices] <- ((density_abundance[modify_pop_indices] -
                                                    array(dispersal_target_n$cutoff, populations)[modify_pop_indices])/
                                                   array(dispersal_target_n$threshold - dispersal_target_n$cutoff,
                                                         populations)[modify_pop_indices]*
                                                   dd_multipliers[modify_pop_indices])
        }
      }

      # Calculate the target N/K multipliers
      if (dispersal_depends_on_target_pop_n_k) {
        dd_multipliers[which(carrying_capacity <= 0)] <- 0
        abundance_on_capacity <- density_abundance/carrying_capacity
        if (all(dispersal_target_n_k$threshold < dispersal_target_n_k$cutoff)) { # overcrowded cell avoidance \
          dd_multipliers[which(abundance_on_capacity >= dispersal_target_n_k$cutoff)] <- 0
          modify_pop_indices <- which(abundance_on_capacity > dispersal_target_n_k$threshold & dd_multipliers > 0)
          dd_multipliers[modify_pop_indices] <- ((array(dispersal_target_n_k$cutoff, populations)[modify_pop_indices] -
                                                    abundance_on_capacity[modify_pop_indices])/
                                                   array(dispersal_target_n_k$cutoff - dispersal_target_n_k$threshold,
                                                         populations)[modify_pop_indices]*
                                                   dd_multipliers[modify_pop_indices])
        } else if (all(dispersal_target_n_k$threshold > dispersal_target_n_k$cutoff)) { # seek company /
          dd_multipliers[which(abundance_on_capacity <= dispersal_target_n_k$cutoff)] <- 0
          modify_pop_indices <- which(abundance_on_capacity < dispersal_target_n_k$threshold & dd_multipliers > 0)
          dd_multipliers[modify_pop_indices] <- ((abundance_on_capacity[modify_pop_indices] -
                                                    array(dispersal_target_n_k$cutoff, populations)[modify_pop_indices])/
                                                   array(dispersal_target_n_k$threshold - dispersal_target_n_k$cutoff,
                                                         populations)[modify_pop_indices]*
                                                   dd_multipliers[modify_pop_indices])
        }
      }

      # Select multipliers via target populations for non-zero occupied dispersals
      selected_dd_multipliers <- dd_multipliers[dispersal_target_pop_map[, occupied_indices][occupied_dispersal_indices]]

      # Apply modifying multipliers to dispersals
      modify_indices <- which(selected_dd_multipliers < 1)
      if (length(modify_indices)) {
        modify_dipersal_indices <- occupied_dispersal_indices[modify_indices]
        occupied_dispersals[modify_dipersal_indices] <- occupied_dispersals[modify_dipersal_indices]*selected_dd_multipliers[modify_indices]
        occupied_dispersal_indices <- which(as.logical(occupied_dispersals)) # > 0
      }

    } # dispersal depends on target pop N, K or N/K?

    # Perform dispersal for each participating stage
    for (stage in which(dispersal_stages > 0)) {

      # Disperser generation via abundance and corresponding dispersal rates
      occupied_abundance <- stage_abundance[stage, occupied_indices]
      occupied_abundance_rep <- stage_abundance[rep(stage, dispersal_compact_rows), occupied_indices]
      dispersers <- array(0, c(dispersal_compact_rows, occupied_populations))

      # Generate dispersers
      if (demographic_stochasticity) { # via binomial distribution
        dispersers[occupied_dispersal_indices] <- stats::rbinom(length(occupied_dispersal_indices), occupied_abundance_rep[occupied_dispersal_indices],
                                                                occupied_dispersals[occupied_dispersal_indices]*dispersal_stages[stage])
      } else { # deterministic
        dispersers[occupied_dispersal_indices] <- round(occupied_abundance_rep[occupied_dispersal_indices]*
                                                          occupied_dispersals[occupied_dispersal_indices]*dispersal_stages[stage])
      }

      # Calculate emigrants
      emigrants <- array(.colSums(dispersers, m = dispersal_compact_rows, n = occupied_populations))

      # Check consistency of emigrants (not to exceed abundances)
      excessive_indices <- which(emigrants > occupied_abundance)
      if (length(excessive_indices) > 0) { # reduce emigrants to equal abundance via random sampling
        for (excessive_index in excessive_indices) {
          excessive_rows <- which(as.logical(dispersers[, excessive_index])) # > 0
          excessive_dispersers <- dispersers[excessive_rows, excessive_index]
          disperser_reduction <- emigrants[excessive_index] - occupied_abundance[excessive_index]
          for (remove_row_index in rep(excessive_rows,
                                       times = excessive_dispersers)[sample(sum(excessive_dispersers),
                                                                            size = disperser_reduction)]) {
            dispersers[remove_row_index, excessive_index] <- dispersers[remove_row_index, excessive_index] - 1
          }
        }
        emigrants[excessive_indices] <- occupied_abundance[excessive_indices]
      }

      # Update occupied stage abundance
      stage_abundance[stage, occupied_indices] <- stage_abundance[stage, occupied_indices] - emigrants

      # Calculate immigrants via dispersal immigrant map
      disperser_indices <- which(as.logical(dispersers)) # > 0
      immigrant_array <- array(0, c(dispersal_compact_rows, populations))
      immigrant_array[dispersal_immigrant_map[, occupied_indices][disperser_indices]] <- dispersers[disperser_indices]
      immigrants <- .colSums(immigrant_array, m = dispersal_compact_rows, n = populations)

      # Update population abundances
      stage_abundance[stage,] <- stage_abundance[stage,] + immigrants

    }

    # Perform additional dispersal for overcrowded cells (only to cells with room)
    if ((dispersal_depends_on_target_pop_n && all(dispersal_target_n$threshold < dispersal_target_n$cutoff)) ||
        (dispersal_depends_on_target_pop_n_k && all(dispersal_target_n_k$threshold < dispersal_target_n_k$cutoff))) {

      # Flags for dependencies
      depends_on_target_pop_n <- (dispersal_depends_on_target_pop_n && all(dispersal_target_n$threshold < dispersal_target_n$cutoff))
      depends_on_target_pop_n_k <- (dispersal_depends_on_target_pop_n_k && all(dispersal_target_n_k$threshold < dispersal_target_n_k$cutoff))

      # Get all updated dispersal rates
      dispersals <- dispersal_compact_matrix_tm
      dispersals[, occupied_indices] <- occupied_dispersals

      # Identify overcrowded cells based on stages affected by density
      density_abundance <- .colSums(stage_abundance*as.numeric(density_stages), m = length(density_stages), n = populations)
      stage_indices <- which(density_stages > 0 & dispersal_stages > 0)
      excessive_indices <- c()
      if (depends_on_target_pop_n) {
        excessive_indices <- which(density_abundance > dispersal_target_n$cutoff)
      }
      if (depends_on_target_pop_n_k) {
        excessive_indices <- unique(c(excessive_indices,
                                      which(density_abundance/carrying_capacity > dispersal_target_n_k$cutoff)))
      }

      # Disperse excess from each overcrowded cell (in random order)
      for (excessive_index in excessive_indices[sample(length(excessive_indices))]) {

        # Determine dispersal targets and rates with enough room for excess from overcrowded cell
        dispersal_indices <- which(dispersals[, excessive_index] > 0)
        target_indices <- dispersal_target_pop_map[, excessive_index][dispersal_indices]
        if (depends_on_target_pop_n && depends_on_target_pop_n_k) {
          indices_with_room <- which((density_abundance < dispersal_target_n$cutoff &
                                        (density_abundance + 1)/carrying_capacity <= dispersal_target_n_k$cutoff)[target_indices])
        } else if (depends_on_target_pop_n) {
          indices_with_room <- which((density_abundance < dispersal_target_n$cutoff)[target_indices])
        } else if (depends_on_target_pop_n_k) {
          indices_with_room <- which(((density_abundance + 1)/carrying_capacity <= dispersal_target_n_k$cutoff)[target_indices])
        }
        dispersal_indices <- dispersal_indices[indices_with_room]
        target_indices <- target_indices[indices_with_room]

        # Disperse excess one at a time sampled via the cell stage abundance distribution
        rep_stage_indices <- rep(stage_indices, times = stage_abundance[stage_indices, excessive_index])
        abundance_excess <- 0
        if (depends_on_target_pop_n) {
          abundance_excess <- density_abundance[excessive_index] - dispersal_target_n$cutoff
        }
        if (depends_on_target_pop_n_k) {
          abundance_excess <- max(abundance_excess, density_abundance[excessive_index] - floor(dispersal_target_n_k$cutoff*carrying_capacity[excessive_index]))
        }
        for (stage_i in rep_stage_indices[sample(1:length(rep_stage_indices), size = abundance_excess)]) {
          if (length(target_indices)) {

            # Sample target cell
            target_i <- target_indices[sample(length(target_indices), size = 1,
                                              prob = dispersals[dispersal_indices, excessive_index])]

            # Perform dispersal
            stage_abundance[stage_i, excessive_index] <- stage_abundance[stage_i, excessive_index] - 1 # emigrant
            stage_abundance[stage_i, target_i] <- stage_abundance[stage_i, target_i] + 1 # immigrant

            # Update target density abundance and potential targets if it becomes full
            density_abundance[target_i] <- density_abundance[target_i] + 1
            if ((depends_on_target_pop_n && density_abundance[target_i] >= dispersal_target_n$cutoff) ||
                (depends_on_target_pop_n_k && density_abundance[target_i]/carrying_capacity[target_i] >= dispersal_target_n_k$cutoff)) { # remove from potential targets
              full_index <- which(target_indices == target_i)
              target_indices <- target_indices[-full_index]
              dispersal_indices <- dispersal_indices[-full_index]
            }
          }
        }
      }
    }

    return(stage_abundance)
  }

  return(dispersal_function)
}
