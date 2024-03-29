#' R6 class representing population simulator results.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class encapsulating and dynamically generating
#' spatially-explicit \code{\link{population_simulator}} results, as well as optional
#' re-generated \code{\link{Generator}} outputs.
#'
#' @examples
#' # U Island example region
#' coordinates <- data.frame(
#'   x = rep(seq(177.01, 177.05, 0.01), 5),
#'   y = rep(seq(-18.01, -18.05, -0.01), each = 5)
#' )
#' template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
#' template_raster[][-c(7, 9, 12, 14, 17:19)] <- NA # make U Island
#' region <- Region$new(template_raster = template_raster)
#' raster::plot(region$region_raster,
#'   main = "Example region (indices)",
#'   xlab = "Longitude (degrees)", ylab = "Latitude (degrees)",
#'   colNA = "blue"
#' )
#' # Sample results occupancy (ignore cell 2 in last 5 time steps)
#' occupancy_raster <- region$raster_from_values(array(1, c(7, 13)))
#' occupancy_raster[region$region_indices][2, 9:13] <- 0
#' occupancy_raster[region$region_indices]
#' # Population simulation example results
#' example_results <- list(abundance = t(apply(matrix(11:17), 1, function(n) {
#'   c(rep(n, 3), round(n * exp(-(0:9) / 2)))
#' })))
#' example_results$harvested <- round(example_results$abundance * 0.3)
#' example_results
#' # Population results object
#' pop_results <- PopulationResults$new(
#'   region = region,
#'   time_steps = 13,
#'   burn_in_steps = 3,
#'   occupancy_mask = occupancy_raster,
#'   trend_interval = 1:5
#' )
#' pop_results$get_attribute_names(all = TRUE)
#' # Clone (for each population simulation results)
#' results_clone <- pop_results$new_clone(results = example_results)
#' results_clone$all$get_attribute("abundance")
#' results_clone$get_attributes(c(
#'   "abundance", "all$abundance",
#'   "abundance_trend", "all$abundance_trend",
#'   "all$ema", # only defined for all
#'   "extirpation", "all$extirpation",
#'   "all$extinction_location", # only defined for all
#'   "harvested", "all$harvested",
#'   "occupancy", "all$occupancy"
#' ))
#'
#' @importFrom R6 R6Class
#' @importFrom trend sens.slope
#' @include SimulationResults.R
#' @export PopulationResults

PopulationResults <- R6Class("PopulationResults",
  inherit = SimulationResults,
  public = list(

    ## Attributes ##

    # object_generator     [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list()

    ## Methods ##

    # Inherited methods (from GenericClass, GenericModel, SpatialModel & SimulationResults) #
    #   initialize(results = NULL, parent = NULL, ...)
    #   new_clone(...)
    #   get_attribute_names(all = FALSE)
    #   get_attributes(params = NULL, remove_burn_in = TRUE)
    #   get_attribute(param)
    #   get_attribute_aliases(params = NULL)
    #   set_attributes(params = list(), ...)

    # Overwritten/overridden methods #

    # New methods (see active attributes) #
  ), # end public

  private = list(

    ## Attributes ##

    # Model attributes #
    .model_attributes = c(
      "region", "coordinates", "time_steps", "burn_in_steps", "occupancy_mask", "trend_interval",
      "abundance", "abundance_stages", "abundance_trend", "ema", "extirpation",
      "extinction_location", "harvested", "harvested_stages", "occupancy"
    ),
    # .region              [inherited]
    # .time_steps          [inherited]
    # .burn_in_steps       [inherited]
    # .occupancy_mask      [inherited]
    .trend_interval = NULL,
    .abundance = NULL,
    .abundance_stages = NULL,
    .abundance_trend = NULL,
    .ema = NULL,
    .extirpation = NULL,
    .extinction_location = NULL,
    .harvested = NULL,
    .harvested_stages = NULL,
    .occupancy = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c(
      "region", "coordinates", "time_steps", "burn_in_steps", "occupancy_mask", "trend_interval",
      "abundance", "abundance_stages", "abundance_trend", "ema", "extirpation",
      "extinction_location", "harvested", "harvested_stages", "occupancy"
    )

    # Model reference attributes #
    # .all                 [inherited]
    # .parent              [inherited]
    # .default             [inherited]

    # Dynamic attributes #
    # .attribute_aliases   [inherited]

    # Errors and warnings #
    # .error_messages      [inherited]
    # .warning_messages    [inherited]
  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    # Model attributes accessors #
    #   Inherited class will dynamically generate results via other attributes wherever possible

    #' @field model_attributes A vector of model attribute names.
    model_attributes = function(value) { # inherited
      if (missing(value)) {
        super$model_attributes
      } else {
        super$model_attributes <- value
      }
    },

    #' @field region A \code{\link{Region}} (or inherited class) object specifying the study region.
    region = function(value) { # inherited
      if (missing(value)) {
        super$region
      } else {
        super$region <- value
      }
    },

    #' @field coordinates Data frame (or matrix) of X-Y population (WGS84) coordinates in longitude (degrees West) and latitude (degrees North) (get and set), or distance-based coordinates dynamically returned by region raster (get only).
    coordinates = function(value) { # inherited
      if (missing(value)) {
        super$coordinates
      } else {
        super$coordinates <- value
      }
    },

    #' @field time_steps Number of simulation time steps.
    time_steps = function(value) { # inherited
      if (missing(value)) {
        super$time_steps
      } else {
        super$time_steps <- value
      }
    },

    #' @field burn_in_steps Optional number of initial 'burn-in' time steps to be ignored.
    burn_in_steps = function(value) { # inherited
      if (missing(value)) {
        super$burn_in_steps
      } else {
        super$burn_in_steps <- value
      }
    },

    #' @field occupancy_mask Optional binary mask array (matrix), data frame, or raster (stack) for each cell at each time-step of the simulation including burn-in.
    occupancy_mask = function(value) {
      if (missing(value)) {
        if (!is.null(super$occupancy_mask) && !is.null(self$region) && self$region$use_raster) {
          super$occupancy_mask[region$region_indices]
        } else {
          super$occupancy_mask
        }
      } else {
        if (!is.null(value) && !is.null(self$region) && self$region$use_raster &&
          !any(class(value) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
          value <- region$raster_from_values(value)
        }
        super$occupancy_mask <- value
      }
    },

    #' @field trend_interval Optional time-step range (indices) for trend calculations (assumes indices begin after the burn-in when utilized).
    trend_interval = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent)) {
          self$parent$trend_interval
        } else {
          private$.trend_interval
        }
      } else {
        private$.trend_interval <- value
      }
    },

    #' @field abundance Population abundance across simulation time-steps (summary list or replicate array).
    abundance = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent)) { # all populations
          if (is.null(private$.abundance) || !is.null(self$parent$occupancy_mask)) { # calculate from individual populations
            if (is.array(self$parent$abundance)) { # replicate array
              if (length(dim(self$parent$abundance)) == 2) {
                abundance_array <- array(self$parent$abundance, c(dim(self$parent$abundance), 1))
                array_dim <- ncol(self$parent$abundance)
              } else {
                abundance_array <- self$parent$abundance
                array_dim <- dim(abundance_array)[c(2, 3)]
              }
              array(.colSums(abundance_array,
                m = dim(abundance_array)[1], n = prod(dim(abundance_array)[2:3]),
                na.rm = TRUE
              ), array_dim) # returned
            } else if (is.list(self$parent$abundance) && "mean" %in% names(self$parent$abundance) && is.array(self$parent$abundance$mean)) { # summary list
              # note min, max, sd not resolvable
              list(mean = as.array(.colSums(self$parent$abundance[["mean"]],
                m = dim(self$parent$abundance[["mean"]])[1],
                n = dim(self$parent$abundance[["mean"]])[2], na.rm = TRUE
              ))) # returned
            }
          } else if (!is.null(private$.abundance) && is.numeric(self$burn_in_steps)) { # apply burn-in
            if (is.list(private$.abundance) && "mean" %in% names(private$.abundance)) { # summary list
              duration_indices <- (self$burn_in_steps + 1):length(private$.abundance$mean)
              abundance_list <- list()
              for (param in names(private$.abundance)) {
                abundance_list[[param]] <- private$.abundance[[param]][duration_indices]
              }
              abundance_list # returned
            } else { # replicate array
              duration_indices <- (self$burn_in_steps + 1):nrow(as.matrix(private$.abundance))
              if (length(dim(private$.abundance)) == 2) {
                private$.abundance[duration_indices, ] # returned
              } else {
                private$.abundance[duration_indices] # returned
              }
            }
          } else {
            private$.abundance
          }
        } else { # individual populations
          if (is.array(private$.abundance)) { # replicate array
            array_dim <- dim(private$.abundance)
            if (is.numeric(self$burn_in_steps)) { # calculate time step indices
              duration_indices <- (self$burn_in_steps + 1):dim(private$.abundance)[2]
              array_dim[2] <- array_dim[2] - self$burn_in_steps
            } else {
              duration_indices <- 1:dim(private$.abundance)[2]
            }
            if (!is.null(self$occupancy_mask)) { # apply occupancy mask to stored abundance
              private$.abundance <- private$.abundance * array(self$occupancy_mask, dim(private$.abundance))
            }
            if (length(dim(private$.abundance)) == 3) {
              array(private$.abundance[, duration_indices, ], array_dim) # returned
            } else {
              array(private$.abundance[, duration_indices], array_dim) # returned
            }
          } else if (is.list(private$.abundance) && "mean" %in% names(private$.abundance) && is.array(private$.abundance$mean)) { # summary list
            array_dim <- dim(private$.abundance$mean)
            if (is.numeric(self$burn_in_steps)) { # calculate time step indices
              duration_indices <- (self$burn_in_steps + 1):dim(private$.abundance$mean)[2]
              array_dim[2] <- array_dim[2] - self$burn_in_steps
            } else {
              duration_indices <- 1:dim(private$.abundance$mean)[2]
            }
            abundance_list <- list()
            for (param in names(private$.abundance)) {
              if (!is.null(self$occupancy_mask)) { # apply occupancy mask to stored abundance summary
                private$.abundance[[param]] <- private$.abundance[[param]] * array(self$occupancy_mask, dim(private$.abundance$mean))
              }
              abundance_list[[param]] <- array(private$.abundance[[param]][, duration_indices], array_dim)
            }
            abundance_list # returned
          } else {
            private$.abundance # returned
          }
        }
      } else {
        private$.abundance <- value
      }
    },

    #' @field abundance_stages Population abundance for combined stages across simulation time-steps (list of summary lists or replicate arrays for each combined stage).
    abundance_stages = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent)) { # all populations
          if (is.null(private$.abundance_stages) || !is.null(self$parent$occupancy_mask)) { # calculate from individual populations
            if (is.list(self$parent$abundance_stages)) {
              abundance_stages_list <- lapply(self$parent$abundance_stages, function(s) NULL)
              for (i in 1:length(self$parent$abundance_stages)) {
                if (is.array(self$parent$abundance_stages[[i]])) { # replicate array
                  if (length(dim(self$parent$abundance_stages[[i]])) == 2) {
                    abundance_array <- array(self$parent$abundance_stages[[i]], c(dim(self$parent$abundance_stages[[i]]), 1))
                    array_dim <- ncol(self$parent$abundance_stages[[i]])
                  } else {
                    abundance_array <- self$parent$abundance_stages[[i]]
                    array_dim <- dim(abundance_array)[c(2, 3)]
                  }
                  abundance_stages_list[[i]] <- array(.colSums(abundance_array,
                    m = dim(abundance_array)[1],
                    n = prod(dim(abundance_array)[2:3]), na.rm = TRUE
                  ), array_dim)
                } else if (is.list(self$parent$abundance_stages[[i]]) && "mean" %in% names(self$parent$abundance_stages[[i]]) &&
                  is.array(self$parent$abundance_stages[[i]]$mean)) { # summary list
                  # note min, max, sd not resolvable
                  abundance_stages_list[[i]] <- list(mean = as.array(.colSums(self$parent$abundance_stages[[i]][["mean"]],
                    m = dim(self$parent$abundance_stages[[i]][["mean"]])[1],
                    n = dim(self$parent$abundance_stages[[i]][["mean"]])[2],
                    na.rm = TRUE
                  )))
                }
              }
              abundance_stages_list # returned
            }
          } else if (!is.null(private$.abundance_stages) && is.numeric(self$burn_in_steps)) { # apply burn-in
            if (is.list(private$.abundance_stages)) {
              abundance_stages_list <- lapply(private$.abundance_stages, function(s) NULL)
              for (i in 1:length(private$.abundance_stages)) {
                if (is.list(private$.abundance_stages[[i]]) && "mean" %in% names(private$.abundance_stages[[i]])) { # summary list
                  duration_indices <- (self$burn_in_steps + 1):length(private$.abundance_stages[[i]]$mean)
                  abundance_stages_list[[i]] <- list()
                  for (param in names(private$.abundance_stages[[i]])) {
                    abundance_stages_list[[i]][[param]] <- private$.abundance_stages[[i]][[param]][duration_indices]
                  }
                } else { # replicate array
                  duration_indices <- (self$burn_in_steps + 1):nrow(as.matrix(private$.abundance_stages[[i]]))
                  if (length(dim(private$.abundance_stages[[i]])) == 2) {
                    abundance_stages_list[[i]] <- private$.abundance_stages[[i]][duration_indices, ]
                  } else {
                    abundance_stages_list[[i]] <- private$.abundance_stages[[i]][duration_indices]
                  }
                }
              }
              abundance_stages_list # returned
            }
          } else {
            private$.abundance_stages
          }
        } else { # individual populations
          if (is.list(private$.abundance_stages)) {
            abundance_stages_list <- lapply(private$.abundance_stages, function(s) NULL)
            for (i in 1:length(private$.abundance_stages)) {
              if (is.array(private$.abundance_stages[[i]])) { # replicate array
                array_dim <- dim(private$.abundance_stages[[i]])
                if (is.numeric(self$burn_in_steps)) { # calculate time step indices
                  duration_indices <- (self$burn_in_steps + 1):dim(private$.abundance_stages[[i]])[2]
                  array_dim[2] <- array_dim[2] - self$burn_in_steps
                } else {
                  duration_indices <- 1:dim(private$.abundance_stages[[i]])[2]
                }
                if (!is.null(self$occupancy_mask)) { # apply occupancy mask to stored abundance
                  private$.abundance_stages[[i]] <- private$.abundance_stages[[i]] * array(self$occupancy_mask, dim(private$.abundance))
                }
                if (length(dim(private$.abundance_stages[[i]])) == 3) {
                  abundance_stages_list[[i]] <- array(private$.abundance_stages[[i]][, duration_indices, ], array_dim)
                } else {
                  abundance_stages_list[[i]] <- array(private$.abundance_stages[[i]][, duration_indices], array_dim)
                }
              } else if (is.list(private$.abundance_stages[[i]]) && "mean" %in% names(private$.abundance_stages[[i]]) && is.array(private$.abundance_stages[[i]]$mean)) { # summary list
                array_dim <- dim(private$.abundance_stages[[i]]$mean)
                if (is.numeric(self$burn_in_steps)) { # calculate time step indices
                  duration_indices <- (self$burn_in_steps + 1):dim(private$.abundance_stages[[i]]$mean)[2]
                  array_dim[2] <- array_dim[2] - self$burn_in_steps
                } else {
                  duration_indices <- 1:dim(private$.abundance_stages[[i]]$mean)[2]
                }
                abundance_stages_list[[i]] <- list()
                for (param in names(private$.abundance_stages[[i]])) {
                  if (!is.null(self$occupancy_mask)) { # apply occupancy mask to stored abundance summary
                    private$.abundance_stages[[i]][[param]] <- private$.abundance_stages[[i]][[param]] * array(self$occupancy_mask, dim(private$.abundance_stages[[i]]$mean))
                  }
                  abundance_stages_list[[i]][[param]] <- array(private$.abundance_stages[[i]][[param]][, duration_indices], array_dim)
                }
              }
            }
            abundance_stages_list # returned
          } else {
            private$.abundance_stages # returned
          }
        }
      } else {
        private$.abundance_stages <- value
      }
    },

    #' @field abundance_trend Trend or average Sen's \code{\link[trend:sens.slope]{slope}} of abundance (optionally across a time-step interval).
    abundance_trend = function(value) {
      if (missing(value)) {
        if (is.null(private$.abundance_trend)) {
          abundance <- self$abundance
          if (is.list(abundance) && "mean" %in% names(abundance)) {
            abundance <- abundance$mean
          }
          if (is.array(abundance)) {
            if (!is.null(self$parent)) { # all populations
              private$.abundance_trend <- array(NA, ncol(as.matrix(abundance))) # single or replicates
              for (i in 1:ncol(as.matrix(abundance))) {
                if (is.numeric(self$trend_interval) && min(self$trend_interval) >= 1 && max(self$trend_interval) <= nrow(as.matrix(abundance))) {
                  private$.abundance_trend[i] <- as.numeric(sens.slope(as.matrix(abundance)[self$trend_interval, i])$estimates)
                } else {
                  private$.abundance_trend[i] <- as.numeric(sens.slope(as.matrix(abundance)[, i])$estimates)
                }
              }
            } else { # individual populations
              private$.abundance_trend <- array(NA, nrow(as.matrix(abundance))) # populations
              for (i in 1:nrow(as.matrix(abundance))) {
                if (is.numeric(self$trend_interval) && min(self$trend_interval) >= 1 && max(self$trend_interval) <= ncol(as.matrix(abundance))) {
                  private$.abundance_trend[i] <- as.numeric(sens.slope(as.matrix(abundance)[i, self$trend_interval])$estimates)
                } else {
                  private$.abundance_trend[i] <- as.numeric(sens.slope(as.matrix(abundance)[i, ])$estimates)
                }
              }
            }
          }
        }
        private$.abundance_trend
      } else {
        private$.abundance_trend <- value
      }
    },

    #' @field ema Array of population expected minimum abundance (EMA) across simulation time-steps.
    ema = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent) && (is.null(private$.ema) || !is.null(self$parent$occupancy_mask))) { # Calculate via abundance
          burn_in_steps <- self$burn_in_steps
          self$burn_in_steps <- NULL # allow access to abundance including burn-in
          abundance <- self$abundance
          if (is.list(abundance) && "mean" %in% names(abundance)) {
            abundance <- abundance$mean
          }
          if (is.array(abundance)) {
            abundance <- as.matrix(abundance)
            ema_matrix <- array(rep(abundance[1, ], each = nrow(abundance)), dim(abundance))
            for (i in 2:nrow(abundance)) {
              ema_matrix[i, ] <- pmin(abundance[i, ], ema_matrix[i - 1, ])
            }
            if (is.array(private$.ema)) {
              private$.ema <- as.array(pmin(private$.ema, .rowMeans(ema_matrix, m = nrow(ema_matrix), n = ncol(ema_matrix))))
            } else {
              private$.ema <- as.array(.rowMeans(ema_matrix, m = nrow(ema_matrix), n = ncol(ema_matrix)))
            }
          }
          self$burn_in_steps <- burn_in_steps # restore
        }
        if (is.array(private$.ema) && is.numeric(self$burn_in_steps)) { # remove burn-in
          private$.ema[(self$burn_in_steps + 1):length(private$.ema)] # returned
        } else {
          private$.ema # returned
        }
      } else {
        private$.ema <- value
      }
    },

    #' @field extirpation Array of population extirpation times.
    extirpation = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent)) { # all populations
          if (is.null(private$.extirpation) || !is.null(self$parent$occupancy_mask)) { # calculate via individual populations
            apply(as.matrix(self$parent$extirpation), 2, max) # returned
          } else {
            if (!is.null(private$.extirpation) && is.numeric(self$burn_in_steps)) { # subtract burn-in from extirpation times
              extirpation <- private$.extirpation - self$burn_in_steps
              extirpation[which(extirpation < 0)] <- 0
              extirpation # returned
            } else {
              private$.extirpation # returned
            }
          }
        } else { # individual populations
          if (is.null(private$.extirpation) || !is.null(self$occupancy_mask)) { # calculate from individual populations
            if (is.array(self$abundance)) { # replicate array (call applies mask to stored abundance)
              if (length(dim(private$.abundance)) == 2) { # make 3D array
                abundance_array <- array(private$.abundance, c(dim(private$.abundance), 1))
              } else {
                abundance_array <- private$.abundance
              }
              private$.extirpation <- array(NA, dim(abundance_array)[c(1, 3)])
              for (i in 1:dim(abundance_array)[3]) { # replicates
                private$.extirpation[which(abundance_array[, 1, i] == 0), i] <- 1
                for (j in 2:dim(abundance_array)[2]) { # minimum to-date across time
                  private$.extirpation[, i] <- pmin(private$.extirpation[, i], rep(j, dim(abundance_array)[1]), na.rm = TRUE)
                  private$.extirpation[which(as.logical(abundance_array[, j, i])), i] <- NA
                }
              }
              if (length(dim(private$.abundance)) == 2) { # drop 3rd dimension
                private$.extirpation <- private$.extirpation[, 1]
              }
            } else if (is.list(private$.abundance) && "mean" %in% names(private$.abundance) && is.array(private$.abundance$mean)) { # summary list
              private$.extirpation <- array(NA, nrow(private$.abundance$mean))
              private$.extirpation[which(private$.abundance$mean[, 1] == 0)] <- 1
              for (i in 2:dim(private$.abundance$mean)[2]) { # minimum to-date across time
                private$.extirpation <- pmin(private$.extirpation, rep(i, length(private$.extirpation)), na.rm = TRUE)
                private$.extirpation[which(as.logical(private$.abundance$mean[, i]))] <- NA
              }
            }
          }
          if (!is.null(private$.extirpation) && is.numeric(self$burn_in_steps)) { # subtract burn-in from extirpation times
            if (is.list(private$.extirpation)) { # summary list
              extirpation_list <- list()
              for (param in names(private$.extirpation)) {
                extirpation_list[[param]] <- private$.extirpation[[param]] - self$burn_in_steps
                extirpation_list[[param]][which(extirpation_list[[param]] < 0)] <- 0
              }
              extirpation_list # returned
            } else { # replicate array
              extirpation <- private$.extirpation - self$burn_in_steps
              extirpation[which(extirpation < 0)] <- 0
              extirpation # returned
            }
          } else {
            private$.extirpation # returned
          }
        }
      } else {
        private$.extirpation <- value
      }
    },

    #' @field extinction_location The weighted centroid of cells occupied in the time-step prior to the extirpation of all populations (if it occurred).
    extinction_location = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent) && (is.null(private$.extinction_location) || !is.null(self$parent$occupancy_mask)) &&
          !is.null(self$parent$coordinates) && is.numeric(self$extirpation) && !is.null(self$parent$abundance)) {
          abundance <- self$parent$abundance
          if (is.list(abundance) && "mean" %in% names(abundance)) { # use summary mean
            abundance <- abundance$mean
          }
          if (length(dim(abundance)) == 2) { # make 3D array
            array_dim <- ncol(abundance)
            abundance <- array(abundance, c(dim(abundance), 1))
          } else {
            array_dim <- dim(abundance)[c(2, 3)]
          }
          extinction_location <- array(NA, c(dim(abundance)[3], 2))
          colnames(extinction_location) <- c("x", "y")
          for (i in 1:dim(abundance)[3]) {
            if (!is.na(self$extirpation[i]) && self$extirpation[i] > 1) {
              last_pop_indices <- which(as.logical(abundance[, self$extirpation[i] - 1, i]))
              if (length(last_pop_indices) > 1) {
                abundance_weights <- matrix(rep(abundance[last_pop_indices, self$extirpation[i] - 1, i], 2), ncol = 2)
                extinction_location[i, ] <- (.colSums(self$parent$coordinates[last_pop_indices, ] * abundance_weights, m = length(last_pop_indices), n = 2) /
                  .colSums(abundance_weights, m = length(last_pop_indices), n = 2))
              } else {
                extinction_location[i, ] <- as.numeric(self$parent$coordinates[last_pop_indices, ])
              }
            }
          }
          private$.extinction_location <- extinction_location[1:dim(abundance)[3], ]
        }
        private$.extinction_location # returned
      } else {
        private$.extinction_location <- value
      }
    },

    #' @field harvested Number of animals harvested from each population across simulation time-steps (summary list or replicate array).
    harvested = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent)) { # all populations
          if (is.null(private$.harvested) || !is.null(self$parent$occupancy_mask)) { # calculate from individual populations
            if (is.array(self$parent$harvested)) { # replicate array
              if (length(dim(self$parent$harvested)) == 2) {
                harvested_array <- array(self$parent$harvested, c(dim(self$parent$harvested), 1))
                array_dim <- ncol(self$parent$harvested)
              } else {
                harvested_array <- self$parent$harvested
                array_dim <- dim(harvested_array)[c(2, 3)]
              }
              array(.colSums(harvested_array,
                m = dim(harvested_array)[1], n = prod(dim(harvested_array)[2:3]),
                na.rm = TRUE
              ), array_dim) # returned
            } else if (is.list(self$parent$harvested) && "mean" %in% names(self$parent$harvested) && is.array(self$parent$harvested$mean)) { # summary list
              # note min, max, sd not resolvable
              list(mean = as.array(.colSums(self$parent$harvested[["mean"]],
                m = dim(self$parent$harvested[["mean"]])[1],
                n = dim(self$parent$harvested[["mean"]])[2], na.rm = TRUE
              ))) # returned
            }
          } else if (!is.null(private$.harvested) && is.numeric(self$burn_in_steps)) { # apply burn-in
            if (is.list(private$.harvested) && "mean" %in% names(private$.harvested)) { # summary list
              duration_indices <- (self$burn_in_steps + 1):length(private$.harvested$mean)
              harvested_list <- list()
              for (param in names(private$.harvested)) {
                harvested_list[[param]] <- private$.harvested[[param]][duration_indices]
              }
              harvested_list # returned
            } else { # replicate array
              duration_indices <- (self$burn_in_steps + 1):nrow(as.matrix(private$.harvested))
              if (length(dim(private$.harvested)) == 2) {
                private$.harvested[duration_indices, ] # returned
              } else {
                private$.harvested[duration_indices] # returned
              }
            }
          } else {
            private$.harvested
          }
        } else { # individual populations
          if (is.array(private$.harvested)) { # replicate array
            array_dim <- dim(private$.harvested)
            if (is.numeric(self$burn_in_steps)) { # calculate time step indices
              duration_indices <- (self$burn_in_steps + 1):dim(private$.harvested)[2]
              array_dim[2] <- array_dim[2] - self$burn_in_steps
            } else {
              duration_indices <- 1:dim(private$.harvested)[2]
            }
            if (!is.null(self$occupancy_mask)) { # apply occupancy mask to stored harvested
              private$.harvested <- private$.harvested * array(self$occupancy_mask, dim(private$.harvested))
            }
            if (length(dim(private$.harvested)) == 3) {
              array(private$.harvested[, duration_indices, ], array_dim) # returned
            } else {
              array(private$.harvested[, duration_indices], array_dim) # returned
            }
          } else if (is.list(private$.harvested) && "mean" %in% names(private$.harvested) && is.array(private$.harvested$mean)) { # summary list
            array_dim <- dim(private$.harvested$mean)
            if (is.numeric(self$burn_in_steps)) { # calculate time step indices
              duration_indices <- (self$burn_in_steps + 1):dim(private$.harvested$mean)[2]
              array_dim[2] <- array_dim[2] - self$burn_in_steps
            } else {
              duration_indices <- 1:dim(private$.harvested$mean)[2]
            }
            harvested_list <- list()
            for (param in names(private$.harvested)) {
              if (!is.null(self$occupancy_mask)) { # apply occupancy mask to stored harvested summary
                private$.harvested[[param]] <- private$.harvested[[param]] * array(self$occupancy_mask, dim(private$.harvested$mean))
              }
              harvested_list[[param]] <- array(private$.harvested[[param]][, duration_indices], array_dim)
            }
            harvested_list # returned
          } else {
            private$.harvested # returned
          }
        }
      } else {
        private$.harvested <- value
      }
    },

    #' @field harvested_stages Number of animals harvested from each population for combined stages across simulation time-steps (list of summary lists or replicate arrays for each combined stage).
    harvested_stages = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent)) { # all populations
          if (is.null(private$.harvested_stages) || !is.null(self$parent$occupancy_mask)) { # calculate from individual populations
            if (is.list(self$parent$harvested_stages)) {
              harvested_stages_list <- lapply(self$parent$harvested_stages, function(s) NULL)
              for (i in 1:length(self$parent$harvested_stages)) {
                if (is.array(self$parent$harvested_stages[[i]])) { # replicate array
                  if (length(dim(self$parent$harvested_stages[[i]])) == 2) {
                    harvested_array <- array(self$parent$harvested_stages[[i]], c(dim(self$parent$harvested_stages[[i]]), 1))
                    array_dim <- ncol(self$parent$harvested_stages[[i]])
                  } else {
                    harvested_array <- self$parent$harvested_stages[[i]]
                    array_dim <- dim(harvested_array)[c(2, 3)]
                  }
                  harvested_stages_list[[i]] <- array(.colSums(harvested_array,
                    m = dim(harvested_array)[1],
                    n = prod(dim(harvested_array)[2:3]), na.rm = TRUE
                  ), array_dim)
                } else if (is.list(self$parent$harvested_stages[[i]]) && "mean" %in% names(self$parent$harvested_stages[[i]]) &&
                  is.array(self$parent$harvested_stages[[i]]$mean)) { # summary list
                  # note min, max, sd not resolvable
                  harvested_stages_list[[i]] <- list(mean = as.array(.colSums(self$parent$harvested_stages[[i]][["mean"]],
                    m = dim(self$parent$harvested_stages[[i]][["mean"]])[1],
                    n = dim(self$parent$harvested_stages[[i]][["mean"]])[2],
                    na.rm = TRUE
                  )))
                }
              }
              harvested_stages_list # returned
            }
          } else if (!is.null(private$.harvested_stages) && is.numeric(self$burn_in_steps)) { # apply burn-in
            if (is.list(private$.harvested_stages)) {
              harvested_stages_list <- lapply(private$.harvested_stages, function(s) NULL)
              for (i in 1:length(private$.harvested_stages)) {
                if (is.list(private$.harvested_stages[[i]]) && "mean" %in% names(private$.harvested_stages[[i]])) { # summary list
                  duration_indices <- (self$burn_in_steps + 1):length(private$.harvested_stages[[i]]$mean)
                  harvested_stages_list[[i]] <- list()
                  for (param in names(private$.harvested_stages[[i]])) {
                    harvested_stages_list[[i]][[param]] <- private$.harvested_stages[[i]][[param]][duration_indices]
                  }
                } else { # replicate array
                  duration_indices <- (self$burn_in_steps + 1):nrow(as.matrix(private$.harvested_stages[[i]]))
                  if (length(dim(private$.harvested_stages[[i]])) == 2) {
                    harvested_stages_list[[i]] <- private$.harvested_stages[[i]][duration_indices, ]
                  } else {
                    harvested_stages_list[[i]] <- private$.harvested_stages[[i]][duration_indices]
                  }
                }
              }
              harvested_stages_list # returned
            }
          } else {
            private$.harvested_stages
          }
        } else { # individual populations
          if (is.list(private$.harvested_stages)) {
            harvested_stages_list <- lapply(private$.harvested_stages, function(s) NULL)
            for (i in 1:length(private$.harvested_stages)) {
              if (is.array(private$.harvested_stages[[i]])) { # replicate array
                array_dim <- dim(private$.harvested_stages[[i]])
                if (is.numeric(self$burn_in_steps)) { # calculate time step indices
                  duration_indices <- (self$burn_in_steps + 1):dim(private$.harvested_stages[[i]])[2]
                  array_dim[2] <- array_dim[2] - self$burn_in_steps
                } else {
                  duration_indices <- 1:dim(private$.harvested_stages[[i]])[2]
                }
                if (!is.null(self$occupancy_mask)) { # apply occupancy mask to stored harvested
                  private$.harvested_stages[[i]] <- private$.harvested_stages[[i]] * array(self$occupancy_mask, dim(private$.harvested))
                }
                if (length(dim(private$.harvested_stages[[i]])) == 3) {
                  harvested_stages_list[[i]] <- array(private$.harvested_stages[[i]][, duration_indices, ], array_dim)
                } else {
                  harvested_stages_list[[i]] <- array(private$.harvested_stages[[i]][, duration_indices], array_dim)
                }
              } else if (is.list(private$.harvested_stages[[i]]) && "mean" %in% names(private$.harvested_stages[[i]]) && is.array(private$.harvested_stages[[i]]$mean)) { # summary list
                array_dim <- dim(private$.harvested_stages[[i]]$mean)
                if (is.numeric(self$burn_in_steps)) { # calculate time step indices
                  duration_indices <- (self$burn_in_steps + 1):dim(private$.harvested_stages[[i]]$mean)[2]
                  array_dim[2] <- array_dim[2] - self$burn_in_steps
                } else {
                  duration_indices <- 1:dim(private$.harvested_stages[[i]]$mean)[2]
                }
                harvested_stages_list[[i]] <- list()
                for (param in names(private$.harvested_stages[[i]])) {
                  if (!is.null(self$occupancy_mask)) { # apply occupancy mask to stored harvested summary
                    private$.harvested_stages[[i]][[param]] <- private$.harvested_stages[[i]][[param]] * array(self$occupancy_mask, dim(private$.harvested_stages[[i]]$mean))
                  }
                  harvested_stages_list[[i]][[param]] <- array(private$.harvested_stages[[i]][[param]][, duration_indices], array_dim)
                }
              }
            }
            harvested_stages_list # returned
          } else {
            private$.harvested_stages # returned
          }
        }
      } else {
        private$.harvested_stages <- value
      }
    },

    #' @field occupancy Array of the number of populations occupied at each time-step.
    occupancy = function(value) {
      if (missing(value)) {
        if ((is.null(private$.occupancy) || !is.null(self$occupancy_mask) || (!is.null(self$parent) && !is.null(self$parent$occupancy_mask))) && !is.null(self$abundance)) { # calculate via abundance
          if (!is.null(self$parent) && !is.null(self$parent$abundance)) { # all populations
            abundance <- self$parent$abundance
            if (is.list(abundance) && "mean" %in% names(abundance)) { # use summary mean
              abundance <- abundance$mean
            }
            if (length(dim(abundance)) == 2) { # make 3D array
              array_dim <- ncol(abundance)
              abundance <- array(abundance, c(dim(abundance), 1))
            } else {
              array_dim <- dim(abundance)[c(2, 3)]
            }
            array(.colSums(as.logical(abundance),
              m = dim(abundance)[1], n = prod(dim(abundance)[2:3]),
              na.rm = TRUE
            ), array_dim) # returned
          } else if (!is.null(self$abundance)) { # individual populations
            abundance <- self$abundance
            if (is.list(abundance) && "mean" %in% names(abundance)) { # use summary mean
              abundance <- abundance$mean
            }
            +(abundance > 0) # returned
          }
        } else {
          if (!is.null(private$.occupancy) && !is.null(self$parent) && is.numeric(self$burn_in_steps)) { # subtract burn-in
            if (is.list(private$.occupancy)) { # summary list
              occupancy_list <- list()
              for (param in names(private$.occupancy)) {
                occupancy_list[[param]] <- private$.occupancy[[param]][(self$burn_in_steps + 1):length(private$.occupancy[[param]])]
              }
              occupancy_list # returned
            } else { # replicate array
              occupancy_matrix <- as.matrix(private$.occupancy)
              occupancy_matrix[(self$burn_in_steps + 1):nrow(occupancy_matrix), ] # returned
            }
          } else {
            private$.occupancy # returned
          }
        }
      } else {
        private$.occupancy <- value
      }
    },

    # Model reference attribute accessors #

    #' @field all Nested simulation results for all cells.
    all = function(value) { # inherited
      if (missing(value)) {
        super$all
      } else {
        super$all <- value
      }
    },

    #' @field parent Parent simulation results for individual cells.
    parent = function(value) { # inherited
      if (missing(value)) {
        super$parent
      } else {
        super$parent <- value
      }
    },

    #' @field default Default value/attribute utilized when applying primitive metric functions (e.g. max) to the results.
    default = function(value) { # inherited
      if (missing(value)) {
        super$default
      } else {
        super$default <- value
      }
    },

    # Dynamic attribute accessors #

    #' @field attribute_aliases A list of alternative alias names for model attributes (form: \code{alias = "attribute"}) to be used with the set and get attributes methods.
    attribute_aliases = function(value) { # inherited
      if (missing(value)) {
        super$attribute_aliases
      } else {
        super$attribute_aliases <- value
      }
    },

    # Errors and warnings accessors #

    #' @field error_messages A vector of error messages encountered when setting model attributes.
    error_messages = function(value) { # inherited
      if (missing(value)) {
        super$error_messages
      } else {
        super$error_messages <- value
      }
    },

    #' @field warning_messages A vector of warning messages encountered when setting model attributes.
    warning_messages = function(value) { # inherited
      if (missing(value)) {
        super$warning_messages
      } else {
        super$warning_messages <- value
      }
    }
  ) # end active
)
