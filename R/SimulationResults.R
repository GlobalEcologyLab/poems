#' R6 class representing simulation results.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class for encapsulating and dynamically generating
#' spatially-explicit simulation results, as well as optional re-generated
#' \code{\link{Generator}} outputs.
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
#' # Sample results occupancy (ignore cell 2 in last 3 time steps)
#' occupancy_raster <- region$raster_from_values(array(1, c(7, 13)))
#' occupancy_raster[region$region_indices][2, 11:13] <- 0
#' occupancy_raster[region$region_indices]
#' # Simulation example results
#' example_results <- list(abundance = region$raster_from_values(
#'   t(apply(
#'     matrix(11:17), 1,
#'     function(n) c(rep(n, 3), round(n * exp(-(0:9) / log(n))))
#'   ))
#' ))
#' example_results$abundance[region$region_indices]
#' # Simulation results object
#' sim_results <- SimulationResults$new(
#'   region = region,
#'   time_steps = 13,
#'   burn_in_steps = 3,
#'   occupancy_mask = occupancy_raster
#' )
#' # Clone (for each simulation results)
#' results_clone <- sim_results$new_clone(results = example_results)
#' results_clone$get_attribute("abundance")
#' results_clone$get_attribute("abundance")[region$region_indices]
#' results_clone$all$get_attribute("abundance")
#' results_clone$get_attribute("all$abundance")
#'
#' @importFrom R6 R6Class
#' @include SpatialModel.R
#' @export SimulationResults

SimulationResults <- R6Class("SimulationResults",
  inherit = SpatialModel,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericModel & SpatialModel) #
    #   get_attribute(param)
    #   get_attribute_aliases(params = NULL)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets attributes from a results list or file, and sets object attributes individually and/or from a list.
    #' @param results A list containing results or a file path to simulation results.
    #' @param parent Parent simulation results for individual cells (used when nesting a simulation results clone for all cells).
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(results = NULL, parent = NULL, ...) {
      if (!is.null(results)) {
        if (is.character(results) && file.exists(results)) {
          results <- readRDS(results)
        } else if (is.character(results)) {
          stop(paste("Could not read results from", results), call. = FALSE)
        }
        if (!is.list(results)) {
          stop(paste("Could not read results from type/class", class(results)[1]), call. = FALSE)
        }
      }
      super$initialize(...)
      self$attribute_aliases <- c(self$attribute_aliases, list(burn_in_duration = "burn_in_steps"))
      if (!is.null(parent)) {
        self$parent <- parent
      } else {
        self$all <- self$new_clone(parent = self)
        if (is.list(results)) {
          self$all$set_attributes(params = results$all)
          results$all <- NULL
          self$set_attributes(params = results)
        }
      }
    },

    #' @description
    #' Creates a new (re-initialized) object of the current (inherited) object class with optionally passed parameters.
    #' @param ... Parameters passed via the inherited class constructor (defined in initialize and run via new).
    #' @return New object of the current (inherited) class.
    new_clone = function(...) {
      if ("parent" %in% names(list(...))) {
        super$new_clone(...)
      } else {
        super$new_clone(
          time_steps = self$time_steps, burn_in_steps = self$burn_in_steps,
          occupancy_mask = self$occupancy_mask, default = self$default, ...
        )
      }
    },

    #' @description
    #' Returns an array of all attribute names including public and private model attributes, as well as attached attributes, error and warning messages.
    #' @param all Boolean to indicate if a nested list for all cells (when present) should be also listed (default is FALSE).
    #' @return Array of all attribute names with optional inclusion of attribute names of nested results for all cells.
    get_attribute_names = function(all = FALSE) {
      if (all && !is.null(self$all) && length(self$all$get_attribute_names())) {
        return(c(super$get_attribute_names(), paste0("all$", self$all$get_attribute_names())))
      } else {
        return(super$get_attribute_names())
      }
    },

    #' @description
    #' Returns a list of values for selected attributes or attribute aliases (when array of parameter names provided) or all attributes (when no params).
    #' @param params Array of attribute names to return (all when NULL).
    #' @param remove_burn_in Boolean to indicate whether or not to remove burn-in steps from the attribute values (default = TRUE; mostly for internal use).
    #' @return List of selected or all attributes values.
    get_attributes = function(params = NULL, remove_burn_in = TRUE) {
      attribute_list <- list()
      if (is.null(params)) {
        params <- self$get_attribute_names()
        if (!is.null(self$parent)) { # include parent names
          params <- unique(c(params, self$parent$get_attribute_names()))
        }
      }
      for (param in params) {
        param_root <- unlist(strsplit(param, "$", fixed = TRUE))[1]
        if (param_root == "all") {
          param_name <- unlist(strsplit(param, "$", fixed = TRUE))[2]
          attribute_list[[param]] <- self$all$get_attributes(param_name)[[param_name]]
        } else {
          attribute_list[[param]] <- super$get_attributes(param)[[param]]
          if (!param %in% private$.active_attributes) { # not handled in an accessor
            if (is.null(attribute_list[[param]]) && !is.null(self$parent)) { # calculate via parent
              parent_value <- self$parent$get_attributes(param, remove_burn_in = FALSE)[[param]]
              if (any(class(parent_value) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
                parent_value <- parent_value[]
              }
              if (is.matrix(parent_value)) {
                attribute_list[[param]] <- .colSums(parent_value, m = nrow(parent_value), n = ncol(parent_value), na.rm = TRUE)
                self$set_attributes(attribute_list[param])
              }
            }
            # Apply occupancy mask and/or burn-in
            if (is.numeric(attribute_list[[param]][]) && length(attribute_list[[param]][]) > 1) {
              if (is.null(self$parent) && !is.null(self$occupancy_mask)) {
                if (nrow(as.matrix(attribute_list[[param]][])) == nrow(as.matrix(self$occupancy_mask[]))) {
                  if (ncol(as.matrix(self$occupancy_mask[])) %in% c(1, ncol(as.matrix(attribute_list[[param]][])))) {
                    attribute_list[[param]][] <- as.matrix(attribute_list[[param]][] * self$occupancy_mask[])
                  } else if (ncol(as.matrix(attribute_list[[param]][])) > 1) {
                    self$error_messages <- sprintf("The column/layer dimension of the occupancy mask and the %s result are inconsistent", param)
                  }
                } else if (is.matrix(attribute_list[[param]][])) {
                  self$error_messages <- sprintf("The row/cell dimension of the occupancy mask and the %s result are inconsistent", param)
                }
              }
            }
            if (remove_burn_in && !is.null(self$burn_in_steps)) {
              if (is.matrix(attribute_list[[param]][])) {
                if (ncol(attribute_list[[param]][]) > self$burn_in_steps) {
                  duration_indices <- (self$burn_in_steps + 1):ncol(as.matrix(attribute_list[[param]][]))
                  if (any(class(attribute_list[[param]]) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
                    attribute_list[[param]] <- attribute_list[[param]][[duration_indices]]
                  } else {
                    attribute_list[[param]] <- attribute_list[[param]][, duration_indices]
                  }
                } else {
                  self$error_messages <- sprintf("The burn-in steps exceed the number of columns/layers of the %s result", param)
                }
              } else if (is.vector(attribute_list[[param]])) {
                if (!is.null(self$parent)) {
                  occupancy_mask <- self$parent$occupancy_mask
                } else {
                  occupancy_mask <- self$occupancy_mask
                }
                if ((is.null(self$time_steps) && (is.null(occupancy_mask) || ncol(as.matrix(occupancy_mask[])) == 1)) ||
                  (is.numeric(self$time_steps) && length(attribute_list[[param]]) == self$time_steps) ||
                  (is.numeric(occupancy_mask[]) && length(attribute_list[[param]]) == ncol(as.matrix(occupancy_mask[])))) {
                  if (length(attribute_list[[param]]) > self$burn_in_steps) {
                    duration_indices <- (self$burn_in_steps + 1):length(attribute_list[[param]])
                    attribute_list[[param]] <- attribute_list[[param]][duration_indices]
                  } else {
                    self$error_messages <- sprintf("The burn-in steps exceed the length of the %s result", param)
                  }
                }
              }
            }
          }
        }
      }
      return(attribute_list)
    },

    #' @description
    #' Sets given attributes (optionally via alias names) individually and/or from a list.
    #' @param params List of parameters/attributes.
    #' @param ... Parameters/attributes passed individually.
    set_attributes = function(params = list(), ...) {
      params <- c(list(...), params) # prioritise individual parameters
      for (param in names(params)) { # check region raster consistency
        if (!param %in% private$.active_attributes) { # not handled in an accessor
          if (any(class(params[[param]]) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
            if (!is.null(self$region) && !self$region$raster_is_consistent(params[[param]])) {
              self$error_messages <- sprintf("The %s result is not consistent with the defined region raster", param)
              params[[param]] <- NULL
            } else if (is.numeric(self$time_steps) && !raster::nlayers(params[[param]]) %in% c(1, self$time_steps)) {
              self$error_messages <- sprintf("The number of raster layers in the %s result must be one or match the number of time steps", param)
              params[[param]] <- NULL
            }
          } else if (is.numeric(params[[param]]) && !is.null(self$region) && self$region$use_raster &&
            nrow(as.matrix(params[[param]])) == self$region$region_cells) {
            self$error_messages <- sprintf("The %s result must be a raster layer, stack or brick (consistent with the defined region)", param)
            params[[param]] <- NULL
          }
        }
      }
      super$set_attributes(params = params)
    }

    # New methods (see active attributes) #
  ), # end public

  private = list(

    ## Attributes ##

    # Model attributes #
    .model_attributes = c("region", "time_steps", "burn_in_steps", "occupancy_mask"),
    # .region            [inherited]
    .time_steps = NULL,
    .burn_in_steps = NULL,
    .occupancy_mask = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("region", "coordinates", "time_steps", "burn_in_steps", "occupancy_mask"),

    # Model reference attributes #
    .all = NULL,
    .parent = NULL,
    .default = NULL

    # Dynamic attributes #
    # .attribute_aliases [inherited]

    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]
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
    time_steps = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent)) {
          self$parent$time_steps
        } else {
          private$.time_steps
        }
      } else {
        if (!is.null(self$parent)) {
          self$parent$time_steps <- value
        } else {
          private$.time_steps <- value
        }
      }
    },

    #' @field burn_in_steps Optional number of initial 'burn-in' time steps to be ignored.
    burn_in_steps = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent)) {
          self$parent$burn_in_steps
        } else {
          private$.burn_in_steps
        }
      } else {
        if (is.numeric(value) && is.numeric(self$time_steps) && value >= self$time_steps) {
          stop("Burn-in must be less than the number of simulation time steps", call. = FALSE)
        }
        if (!is.null(self$parent)) {
          self$parent$burn_in_steps <- value
        } else {
          private$.burn_in_steps <- value
        }
      }
    },

    #' @field occupancy_mask Optional binary mask array (matrix), data frame, or raster (stack) for each cell at each time-step of the simulation including burn-in.
    occupancy_mask = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent)) {
          NULL
        } else {
          private$.occupancy_mask
        }
      } else {
        if (any(class(value) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
          if (!is.null(self$region) && !self$region$raster_is_consistent(value)) {
            stop("Occupancy mask raster must be consistent with the defined region raster", call. = FALSE)
          }
          cells <- length(which(!is.na(value[[1]][])))
          width <- raster::nlayers(value)
        } else if (!is.null(value) && !is.null(self$region) && self$region$use_raster) {
          stop("Occupancy mask must be a raster layer, stack or brick consistent with the defined region", call. = FALSE)
        } else if (is.matrix(value)) {
          cells <- nrow(value)
          width <- ncol(value)
        } else {
          cells <- length(value)
          width <- 1
        }
        if (!is.null(value) && is.numeric(self$time_steps) && !width %in% c(1, self$time_steps)) {
          stop("The number of occupancy mask layers/columns must be one or match the number of time steps", call. = FALSE)
        }
        if (!is.null(value) && !is.null(self$region) && cells != self$region$region_cells) {
          stop("The number of occupancy mask rows must be consistent with the region (finite) cells", call. = FALSE)
        }
        private$.occupancy_mask <- value
      }
    },

    # Model reference attribute accessors #

    #' @field all Nested simulation results for all cells.
    all = function(value) {
      if (missing(value)) {
        private$.all
      } else {
        private$.all <- value
      }
    },

    #' @field parent Parent simulation results for individual cells.
    parent = function(value) {
      if (missing(value)) {
        private$.parent
      } else {
        private$.parent <- value
      }
    },

    #' @field default Default value/attribute utilized when applying primitive metric functions (e.g. max) to the results.
    default = function(value) {
      if (missing(value)) {
        if (is.character(private$.default) && private$.default %in% self$get_attribute_names(all = TRUE)) {
          self$get_attribute(private$.default)
        } else {
          private$.default
        }
      } else {
        private$.default <- value
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
