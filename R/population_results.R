#' Nested functions for initializing, calculating and collecting population simulator results.
#'
#' Modular functions for the population simulator for initializing, calculating and
#' collecting simulator results.
#'
#' @examples
#' coordinates <- array(c(1:4, 4:1), c(7, 2))
#' initial_abundance <- matrix(c(
#'   7, 13, 0, 26, 0, 39, 47,
#'   2, 0, 6, 8, 0, 12, 13,
#'   0, 3, 4, 6, 0, 9, 10
#' ), nrow = 3, ncol = 7, byrow = TRUE)
#' results_selection <- c(
#'   "abundance", "ema", "extirpation",
#'   "extinction_location", "harvested", "occupancy"
#' )
#' result_functions <- population_results(
#'   replicates = 1, time_steps = 10, coordinates, initial_abundance,
#'   results_selection = results_selection, result_stages = c(0, 1, 1)
#' )
#' result_functions$initialize_attributes()
#'
#' @param replicates Number of replicate simulation runs.
#' @param time_steps Number of simulation time steps.
#' @param coordinates Data frame (or matrix) of X-Y population coordinates.
#' @param initial_abundance Matrix of initial abundances at each stage (in rows) for each population (in columns).
#' @param results_selection List of results selection from: "abundance" (default), "ema", "extirpation", "extinction_location", "harvested", "occupancy"; "summarize" (default) or "replicate".
#' @param result_stages Array of booleans or numeric (0, 1, 2, ...) for each stage to indicate which stages are included/combined (each unique digit > 0; optionally named) in the results (default is 1 for all stages).
#' @return List of result functions:
#'   \describe{
#'     \item{\code{initialize_attributes = function())}}{Constructs and returns an initialized nested list for the selected result attributes.}
#'     \item{\code{initialize_replicate = function(results)}}{Initializes and returns nested result attributes at the start of each replicate.}
#'     \item{\code{calculate_at_timestep = function(r, tm, stage_abundance, harvested, results)}}{Appends and calculates (non-NULL) results and returns nested result attributes at the end of each time step (tm) within replicate (r).}
#'     \item{\code{finalize_attributes = function(results)}}{Finalizes result calculations at the end of the simulation.}
#'   }
#' @export population_results

population_results <- function(
  replicates,
  time_steps,
  coordinates,
  initial_abundance,
  results_selection = NULL,
  result_stages = NULL
) {
  # Set defaults when NULL
  if (is.null(results_selection)) {
    results_selection <- c("abundance", "summarize")
  }
  separated_stage_indices <- NULL
  if (!is.null(result_stages)) {
    result_stage_indices <- which(result_stages > 0)
    if (length(unique(result_stages[result_stage_indices])) > 1) {
      # separated stage combinations for abundance and harvest results
      separated_stage_indices <- apply(
        as.array(unique(result_stages[result_stage_indices])),
        1,
        function(s) which(result_stages == s)
      )
      names(separated_stage_indices) <- lapply(
        separated_stage_indices,
        function(s) paste(unique(names(s)), collapse = ".")
      )
    }
  } else {
    result_stage_indices <- 1:nrow(initial_abundance)
  }
  result_stages <- length(result_stage_indices)

  # Initialize abundance count
  populations <- ncol(initial_abundance)
  initial_abundance_count <- as.array(.colSums(
    initial_abundance[result_stage_indices, ],
    m = result_stages,
    n = populations
  ))

  # Maintain initial abundance count for EMA calculations
  if ("ema" %in% results_selection) {
    initial_abundance_count_min <- sum(initial_abundance_count)
  }

  ## Create a nested function for initializing the results attributes ##
  initialize_attributes <- function() {
    # Initialize lists for results for each population and for all populations
    results <- list()
    results$all <- list()

    # Abundance results
    if ("abundance" %in% results_selection) {
      results$abundance <- array(0, c(populations, time_steps))
      results$all$abundance <- array(0, time_steps)
      if (replicates > 1) {
        if ("replicate" %in% results_selection) {
          results$abundance <- array(0, c(populations, time_steps, replicates))
          results$all$abundance <- array(0, c(time_steps, replicates))
        } else {
          # summarize (default)
          results$abundance <- list(
            mean = results$abundance,
            sd = results$abundance,
            min = results$abundance,
            max = results$abundance
          )
          results$all$abundance <- list(
            mean = results$all$abundance,
            sd = results$all$abundance,
            min = results$all$abundance,
            max = results$all$abundance
          )
        }
      }
      if (length(separated_stage_indices)) {
        # include separated stage combinations
        results$abundance_stages <- lapply(
          separated_stage_indices,
          function(s) results$abundance
        )
        results$all$abundance_stages <- lapply(
          separated_stage_indices,
          function(s) results$all$abundance
        )
      }
    }

    # Expected minimum abundance (EMA) results
    if ("ema" %in% results_selection) {
      results$all$ema <- array(0, time_steps)
    }

    # Extirpation results
    if ("extirpation" %in% results_selection) {
      results$extirpation <- array(as.numeric(NA), populations)
      results$extirpation[which(initial_abundance_count == 0)] <- 0
      results$all$extirpation <- array(as.numeric(NA), replicates)
      results$all$extirpation[all(initial_abundance_count == 0)] <- 0
      if (replicates > 1) {
        results$extirpation <- array(
          results$extirpation,
          c(populations, replicates)
        )
      }
    }

    # Extinction location results
    if ("extinction_location" %in% results_selection) {
      results$all$extinction_location <- array(as.numeric(NA), c(replicates, 2))
      colnames(results$all$extinction_location) <- c("x", "y")
      results$last_occupied_abundance_count <- initial_abundance_count
    }

    # Harvested results
    if ("harvested" %in% results_selection) {
      results$harvested <- array(0, c(populations, time_steps))
      results$all$harvested <- array(0, time_steps)
      if (replicates > 1) {
        if ("replicate" %in% results_selection) {
          results$harvested <- array(0, c(populations, time_steps, replicates))
          results$all$harvested <- array(0, c(time_steps, replicates))
        } else {
          # summarize (default)
          results$harvested <- list(
            mean = results$harvested,
            sd = results$harvested,
            min = results$harvested,
            max = results$harvested
          )
          results$all$harvested <- list(
            mean = results$all$harvested,
            sd = results$all$harvested,
            min = results$all$harvested,
            max = results$all$harvested
          )
        }
      }
      if (length(separated_stage_indices)) {
        # include separated stage combinations
        results$harvested_stages <- lapply(
          separated_stage_indices,
          function(s) results$harvested
        )
        results$all$harvested_stages <- lapply(
          separated_stage_indices,
          function(s) results$all$harvested
        )
      }
    }

    # Occupancy results
    if ("occupancy" %in% results_selection) {
      results$all$occupancy <- array(0, time_steps)
      if (replicates > 1) {
        if ("replicate" %in% results_selection) {
          results$all$occupancy <- array(0, c(time_steps, replicates))
        } else {
          # summarize (default)
          results$all$occupancy <- list(
            mean = results$all$occupancy,
            sd = results$all$occupancy,
            min = results$all$occupancy,
            max = results$all$occupancy
          )
        }
      }
    }

    return(results)
  }

  ## Create a nested function for (re-)initializing the results attributes at the beginning of each replicate ##
  initialize_replicate <- function(results) {
    if ("ema" %in% results_selection) {
      results$abundance_count_min <- initial_abundance_count_min
    }
    if ("extinction_location" %in% results_selection) {
      results$last_occupied_abundance_count <- initial_abundance_count
    }
    return(results)
  }

  ## Create a nested function for calculating/collecting (non-NULL) results at time step ##
  calculate_at_timestep <- function(
    r,
    tm,
    stage_abundance,
    harvested,
    results
  ) {
    if (!is.null(stage_abundance)) {
      # results calculated from abundance

      # Select abundance counts for included stages
      abundance_count <- as.array(.colSums(
        stage_abundance[result_stage_indices, ],
        m = result_stages,
        n = populations
      ))
      all_abundance_count <- sum(abundance_count)
      if (length(separated_stage_indices)) {
        # include separated stage combinations
        abundance_stages_count <- lapply(
          separated_stage_indices,
          function(s)
            as.array(.colSums(
              stage_abundance[s, ],
              m = length(s),
              n = populations
            ))
        )
        all_abundance_stages_count <- lapply(abundance_stages_count, sum)
      }

      # Abundance results
      if ("abundance" %in% results_selection) {
        if (replicates > 1) {
          if ("replicate" %in% results_selection) {
            results$abundance[, tm, r] <- abundance_count
            results$all$abundance[tm, r] <- all_abundance_count
          } else {
            # summarize (default)
            previous_abundance_mean <- results$abundance$mean[, tm]
            results$abundance$mean[, tm] <- previous_abundance_mean +
              (abundance_count - previous_abundance_mean) / r
            previous_abundance_var_by_r <- results$abundance$sd[, tm] # store variance by r and transform later
            results$abundance$sd[, tm] <- previous_abundance_var_by_r +
              ((abundance_count - previous_abundance_mean) *
                (abundance_count - results$abundance$mean[, tm]))
            if (r == 1) {
              results$abundance$min[, tm] <- abundance_count
              results$abundance$max[, tm] <- abundance_count
            } else {
              results$abundance$min[, tm] <- pmin(
                results$abundance$min[, tm],
                abundance_count
              )
              results$abundance$max[, tm] <- pmax(
                results$abundance$max[, tm],
                abundance_count
              )
            }
            previous_all_abundance_mean <- results$all$abundance$mean[tm]
            results$all$abundance$mean[tm] <- previous_all_abundance_mean +
              (all_abundance_count - previous_all_abundance_mean) / r
            previous_all_abundance_var_by_r <- results$all$abundance$sd[tm] # store variance by r and transform later
            results$all$abundance$sd[tm] <- previous_all_abundance_var_by_r +
              ((all_abundance_count - previous_all_abundance_mean) *
                (all_abundance_count - results$all$abundance$mean[tm]))
            results$all$abundance$min[tm] <- ifelse(
              r == 1,
              all_abundance_count,
              min(results$all$abundance$min[tm], all_abundance_count)
            )
            results$all$abundance$max[tm] <- ifelse(
              r == 1,
              all_abundance_count,
              max(results$all$abundance$max[tm], all_abundance_count)
            )
          }
        } else {
          # single replicate
          results$abundance[, tm] <- abundance_count
          results$all$abundance[tm] <- all_abundance_count
        }
        if (length(separated_stage_indices)) {
          # include separated stage combinations
          for (i in 1:length(separated_stage_indices)) {
            if (replicates > 1) {
              if ("replicate" %in% results_selection) {
                results$abundance_stages[[i]][,
                  tm,
                  r
                ] <- abundance_stages_count[[i]]
                results$all$abundance_stages[[i]][
                  tm,
                  r
                ] <- all_abundance_stages_count[[i]]
              } else {
                # summarize (default)
                previous_abundance_stages_mean <- results$abundance_stages[[
                  i
                ]]$mean[, tm]
                results$abundance_stages[[i]]$mean[,
                  tm
                ] <- previous_abundance_stages_mean +
                  (abundance_stages_count[[i]] -
                    previous_abundance_stages_mean) /
                    r
                previous_abundance_stages_var_by_r <- results$abundance_stages[[
                  i
                ]]$sd[, tm] # store variance by r and transform later
                results$abundance_stages[[i]]$sd[,
                  tm
                ] <- previous_abundance_stages_var_by_r +
                  ((abundance_stages_count[[i]] -
                    previous_abundance_stages_mean) *
                    (abundance_stages_count[[i]] -
                      results$abundance_stages[[i]]$mean[, tm]))
                if (r == 1) {
                  results$abundance_stages[[i]]$min[,
                    tm
                  ] <- abundance_stages_count[[i]]
                  results$abundance_stages[[i]]$max[,
                    tm
                  ] <- abundance_stages_count[[i]]
                } else {
                  results$abundance_stages[[i]]$min[, tm] <- pmin(
                    results$abundance_stages[[i]]$min[, tm],
                    abundance_stages_count[[i]]
                  )
                  results$abundance_stages[[i]]$max[, tm] <- pmax(
                    results$abundance_stages[[i]]$max[, tm],
                    abundance_stages_count[[i]]
                  )
                }
                previous_all_abundance_stages_mean <- results$all$abundance_stages[[
                  i
                ]]$mean[tm]
                results$all$abundance_stages[[i]]$mean[
                  tm
                ] <- previous_all_abundance_stages_mean +
                  (all_abundance_stages_count[[i]] -
                    previous_all_abundance_stages_mean) /
                    r
                previous_all_abundance_stages_var_by_r <- results$all$abundance_stages[[
                  i
                ]]$sd[tm] # store variance by r and transform later
                results$all$abundance_stages[[i]]$sd[
                  tm
                ] <- previous_all_abundance_stages_var_by_r +
                  ((all_abundance_stages_count[[i]] -
                    previous_all_abundance_stages_mean) *
                    (all_abundance_stages_count[[i]] -
                      results$all$abundance_stages[[i]]$mean[tm]))
                results$all$abundance_stages[[i]]$min[tm] <- ifelse(
                  r == 1,
                  all_abundance_stages_count[[i]],
                  min(
                    results$all$abundance_stages[[i]]$min[tm],
                    all_abundance_stages_count[[i]]
                  )
                )
                results$all$abundance_stages[[i]]$max[tm] <- ifelse(
                  r == 1,
                  all_abundance_stages_count[[i]],
                  max(
                    results$all$abundance_stages[[i]]$max[tm],
                    all_abundance_stages_count[[i]]
                  )
                )
              }
            } else {
              # single replicate
              results$abundance_stages[[i]][, tm] <- abundance_stages_count[[i]]
              results$all$abundance_stages[[i]][
                tm
              ] <- all_abundance_stages_count[[i]]
            }
          }
        }
      }

      # Expected minimum abundance (EMA) results
      if ("ema" %in% results_selection) {
        abundance_count_min <- pmin(
          all_abundance_count,
          results$abundance_count_min
        )
        if (replicates > 1) {
          results$all$ema[tm] <- results$all$ema[tm] +
            (abundance_count_min - results$all$ema[tm]) / r
        } else {
          # single replicate
          results$all$ema[tm] <- abundance_count_min
        }
        results$abundance_count_min <- abundance_count_min
      }

      # Extirpation results
      if ("extirpation" %in% results_selection) {
        if (replicates > 1) {
          results$extirpation[, r] <- pmin(
            results$extirpation[, r],
            rep(tm, populations),
            na.rm = TRUE
          )
          results$extirpation[which(as.logical(abundance_count)), r] <- NA
        } else {
          # single replicate
          results$extirpation <- pmin(
            results$extirpation,
            rep(tm, populations),
            na.rm = TRUE
          )
          results$extirpation[which(as.logical(abundance_count))] <- NA
        }
        results$all$extirpation[r] <- min(
          results$all$extirpation[r],
          tm,
          na.rm = TRUE
        )
        results$all$extirpation[r][as.logical(all_abundance_count)] <- NA
      }

      # Extinction location results
      if ("extinction_location" %in% results_selection) {
        if (all_abundance_count > 0) {
          results$last_occupied_abundance_count <- abundance_count
        }
      }

      # Occupancy results
      if ("occupancy" %in% results_selection) {
        if (replicates > 1) {
          if ("replicate" %in% results_selection) {
            results$all$occupancy[tm, r] <- sum(as.logical(abundance_count))
          } else {
            # summarize (default)
            occupancy_count <- sum(as.logical(abundance_count))
            previous_all_occupancy_mean <- results$all$occupancy$mean[tm]
            results$all$occupancy$mean[tm] <- previous_all_occupancy_mean +
              (occupancy_count - previous_all_occupancy_mean) / r
            previous_all_occupancy_var_by_r <- results$all$occupancy$sd[tm] # store variance by r and transform later
            results$all$occupancy$sd[tm] <- previous_all_occupancy_var_by_r +
              ((occupancy_count - previous_all_occupancy_mean) *
                (occupancy_count - results$all$occupancy$mean[tm]))
            results$all$occupancy$min[tm] <- ifelse(
              r == 1,
              occupancy_count,
              min(results$all$occupancy$min[tm], occupancy_count)
            )
            results$all$occupancy$max[tm] <- ifelse(
              r == 1,
              occupancy_count,
              max(results$all$occupancy$max[tm], occupancy_count)
            )
          }
        } else {
          # single replicate
          results$all$occupancy[tm] <- sum(as.logical(abundance_count))
        }
      }
    } # results calculated from abundance

    if (!is.null(harvested)) {
      # results calculated from harvested

      # Select harvested counts for included stages
      harvested_count <- as.array(.colSums(
        harvested[result_stage_indices, ],
        m = result_stages,
        n = populations
      ))
      all_harvested_count <- sum(harvested_count)
      if (length(separated_stage_indices)) {
        # include separated stage combinations
        harvested_stages_count <- lapply(
          separated_stage_indices,
          function(s)
            as.array(.colSums(harvested[s, ], m = length(s), n = populations))
        )
        all_harvested_stages_count <- lapply(harvested_stages_count, sum)
      }

      # Harvest results
      if ("harvested" %in% results_selection) {
        if (replicates > 1) {
          if ("replicate" %in% results_selection) {
            results$harvested[, tm, r] <- harvested_count
            results$all$harvested[tm, r] <- all_harvested_count
          } else {
            # summarize (default)
            previous_harvested_mean <- results$harvested$mean[, tm]
            results$harvested$mean[, tm] <- previous_harvested_mean +
              (harvested_count - previous_harvested_mean) / r
            previous_harvested_var_by_r <- results$harvested$sd[, tm] # store variance by r and transform later
            results$harvested$sd[, tm] <- previous_harvested_var_by_r +
              ((harvested_count - previous_harvested_mean) *
                (harvested_count - results$harvested$mean[, tm]))
            if (r == 1) {
              results$harvested$min[, tm] <- harvested_count
              results$harvested$max[, tm] <- harvested_count
            } else {
              results$harvested$min[, tm] <- pmin(
                results$harvested$min[, tm],
                harvested_count
              )
              results$harvested$max[, tm] <- pmax(
                results$harvested$max[, tm],
                harvested_count
              )
            }
            previous_all_harvested_mean <- results$all$harvested$mean[tm]
            results$all$harvested$mean[tm] <- previous_all_harvested_mean +
              (all_harvested_count - previous_all_harvested_mean) / r
            previous_all_harvested_var_by_r <- results$all$harvested$sd[tm] # store variance by r and transform later
            results$all$harvested$sd[tm] <- previous_all_harvested_var_by_r +
              ((all_harvested_count - previous_all_harvested_mean) *
                (all_harvested_count - results$all$harvested$mean[tm]))
            results$all$harvested$min[tm] <- ifelse(
              r == 1,
              all_harvested_count,
              min(results$all$harvested$min[tm], all_harvested_count)
            )
            results$all$harvested$max[tm] <- ifelse(
              r == 1,
              all_harvested_count,
              max(results$all$harvested$max[tm], all_harvested_count)
            )
          }
        } else {
          # single replicate
          results$harvested[, tm] <- harvested_count
          results$all$harvested[tm] <- all_harvested_count
        }
        if (length(separated_stage_indices)) {
          # include separated stage combinations
          for (i in 1:length(separated_stage_indices)) {
            if (replicates > 1) {
              if ("replicate" %in% results_selection) {
                results$harvested_stages[[i]][,
                  tm,
                  r
                ] <- harvested_stages_count[[i]]
                results$all$harvested_stages[[i]][
                  tm,
                  r
                ] <- all_harvested_stages_count[[i]]
              } else {
                # summarize (default)
                previous_harvested_stages_mean <- results$harvested_stages[[
                  i
                ]]$mean[, tm]
                results$harvested_stages[[i]]$mean[,
                  tm
                ] <- previous_harvested_stages_mean +
                  (harvested_stages_count[[i]] -
                    previous_harvested_stages_mean) /
                    r
                previous_harvested_stages_var_by_r <- results$harvested_stages[[
                  i
                ]]$sd[, tm] # store variance by r and transform later
                results$harvested_stages[[i]]$sd[,
                  tm
                ] <- previous_harvested_stages_var_by_r +
                  ((harvested_stages_count[[i]] -
                    previous_harvested_stages_mean) *
                    (harvested_stages_count[[i]] -
                      results$harvested_stages[[i]]$mean[, tm]))
                if (r == 1) {
                  results$harvested_stages[[i]]$min[,
                    tm
                  ] <- harvested_stages_count[[i]]
                  results$harvested_stages[[i]]$max[,
                    tm
                  ] <- harvested_stages_count[[i]]
                } else {
                  results$harvested_stages[[i]]$min[, tm] <- pmin(
                    results$harvested_stages[[i]]$min[, tm],
                    harvested_stages_count[[i]]
                  )
                  results$harvested_stages[[i]]$max[, tm] <- pmax(
                    results$harvested_stages[[i]]$max[, tm],
                    harvested_stages_count[[i]]
                  )
                }
                previous_all_harvested_stages_mean <- results$all$harvested_stages[[
                  i
                ]]$mean[tm]
                results$all$harvested_stages[[i]]$mean[
                  tm
                ] <- previous_all_harvested_stages_mean +
                  (all_harvested_stages_count[[i]] -
                    previous_all_harvested_stages_mean) /
                    r
                previous_all_harvested_stages_var_by_r <- results$all$harvested_stages[[
                  i
                ]]$sd[tm] # store variance by r and transform later
                results$all$harvested_stages[[i]]$sd[
                  tm
                ] <- previous_all_harvested_stages_var_by_r +
                  ((all_harvested_stages_count[[i]] -
                    previous_all_harvested_stages_mean) *
                    (all_harvested_stages_count[[i]] -
                      results$all$harvested_stages[[i]]$mean[tm]))
                results$all$harvested_stages[[i]]$min[tm] <- ifelse(
                  r == 1,
                  all_harvested_stages_count[[i]],
                  min(
                    results$all$harvested_stages[[i]]$min[tm],
                    all_harvested_stages_count[[i]]
                  )
                )
                results$all$harvested_stages[[i]]$max[tm] <- ifelse(
                  r == 1,
                  all_harvested_stages_count[[i]],
                  max(
                    results$all$harvested_stages[[i]]$max[tm],
                    all_harvested_stages_count[[i]]
                  )
                )
              }
            } else {
              # single replicate
              results$harvested_stages[[i]][, tm] <- harvested_stages_count[[i]]
              results$all$harvested_stages[[i]][
                tm
              ] <- all_harvested_stages_count[[i]]
            }
          }
        }
      }
    } # results calculated from harvested

    return(results)
  }

  ## Create a nested function for calculating/collecting results at replicate ##
  calculate_at_replicate <- function(r, stage_abundance, results) {
    # Sum abundance counts for included stages
    all_abundance_count <- sum(.colSums(
      stage_abundance[result_stage_indices, ],
      m = result_stages,
      n = populations
    ))

    # Extinction location results
    if ("extinction_location" %in% results_selection && !is.null(coordinates)) {
      if (all_abundance_count == 0) {
        # => extinct
        last_pop_indices <- which(as.logical(
          results$last_occupied_abundance_count
        ))
        if (length(last_pop_indices) > 1) {
          # abundance-weighted average of last occupied coordinates
          abundance_weights <- matrix(
            rep(results$last_occupied_abundance_count[last_pop_indices], 2),
            ncol = 2
          )
          results$all$extinction_location[r, ] <- (.colSums(
            coordinates[last_pop_indices, ] * abundance_weights,
            m = length(last_pop_indices),
            n = 2
          ) /
            .colSums(abundance_weights, m = length(last_pop_indices), n = 2))
        } else {
          # last occupied coordinates
          results$all$extinction_location[r, ] <- as.numeric(coordinates[
            last_pop_indices,
          ])
        }
      }
    }

    return(results)
  }

  ## Create a nested function for finalizing result calculations at the end of all replicates ##
  finalize_attributes <- function(results) {
    # Finalize/summarize results
    if (replicates > 1 && !("replicate" %in% results_selection)) {
      # Transform stored variance values by r (using n - 1, as per stats::sd)
      if ("abundance" %in% results_selection) {
        results$abundance$sd <- sqrt(results$abundance$sd / (replicates - 1))
        results$all$abundance$sd <- sqrt(
          results$all$abundance$sd / (replicates - 1)
        )
        if (length(separated_stage_indices)) {
          # include separated stage combinations
          for (i in 1:length(separated_stage_indices)) {
            results$abundance_stages[[i]]$sd <- sqrt(
              results$abundance_stages[[i]]$sd / (replicates - 1)
            )
            results$all$abundance_stages[[i]]$sd <- sqrt(
              results$all$abundance_stages[[i]]$sd / (replicates - 1)
            )
          }
        }
      }
      if ("harvested" %in% results_selection) {
        results$harvested$sd <- sqrt(results$harvested$sd / (replicates - 1))
        results$all$harvested$sd <- sqrt(
          results$all$harvested$sd / (replicates - 1)
        )
        if (length(separated_stage_indices)) {
          # include separated stage combinations
          for (i in 1:length(separated_stage_indices)) {
            results$harvested_stages[[i]]$sd <- sqrt(
              results$harvested_stages[[i]]$sd / (replicates - 1)
            )
            results$all$harvested_stages[[i]]$sd <- sqrt(
              results$all$harvested_stages[[i]]$sd / (replicates - 1)
            )
          }
        }
      }
      if ("occupancy" %in% results_selection) {
        results$all$occupancy$sd <- sqrt(
          results$all$occupancy$sd / (replicates - 1)
        )
      }

      # Summarize extirpation dependent on the presence of NAs
      #   (since mean and sd cannot be computed with NAs => no extirpation)
      if ("extirpation" %in% results_selection) {
        if (any(is.na(results$extirpation))) {
          # 5 number summary when NAs present
          results$extirpation[which(is.na(results$extirpation))] <- time_steps +
            1
          results$extirpation <- apply(results$extirpation, 1, stats::fivenum)
          results$extirpation[which(results$extirpation > time_steps)] <- NA
          results$extirpation <- list(
            min = results$extirpation[1, ],
            q1 = results$extirpation[2, ],
            median = results$extirpation[3, ],
            q3 = results$extirpation[4, ],
            max = results$extirpation[5, ]
          )
        } else {
          # mean, sd, min, max
          results$extirpation <- list(
            mean = apply(results$extirpation, 1, mean),
            sd = apply(results$extirpation, 1, stats::sd),
            min = apply(results$extirpation, 1, min),
            max = apply(results$extirpation, 1, max)
          )
        }
      }
    }

    # Remove working abundance counts
    results$abundance_count_min <- NULL
    results$last_occupied_abundance_count <- NULL

    return(results)
  }

  return(list(
    initialize_attributes = initialize_attributes,
    initialize_replicate = initialize_replicate,
    calculate_at_timestep = calculate_at_timestep,
    calculate_at_replicate = calculate_at_replicate,
    finalize_attributes = finalize_attributes
  ))
}
