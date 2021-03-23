#' R6 class representing a dispersal friction.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class functionality for modeling sea, ice and other
#' frictional barriers to dispersal within a spatially-explicit population model. The
#' dispersal friction model utilizes the
#' \code{\link[gdistance:gdistance]{gdistance}} package functionality to
#' calculate distance multipliers to modify distance-based dispersal rates for
#' simulated migrations in a spatio-temporal frictional landscape. The frictional
#' landscape is defined via conductance/permeability values, the inverse of friction,
#' which ranges from zero (barrier) to one (no friction) with values in-between
#' representing some friction. For example, a conductance value of 1/5 = 0.2 represents
#' a landscape in which simulated animals move 5 times slower than a non-friction
#' landscape. In this example the resultant distance multiplier would be 5, thus
#' reducing the effective dispersal range.
#'
#' @examples
#' #' U Island example region
#' coordinates <- data.frame(x = rep(seq(177.01, 177.05, 0.01), 5),
#'                           y = rep(seq(-18.01, -18.05, -0.01), each = 5))
#' template_raster <- Region$new(coordinates = coordinates)$region_raster #' full extent
#' template_raster[][-c(7, 9, 12, 14, 17:19)] <- NA #' make U Island
#' region <- Region$new(template_raster = template_raster)
#' raster::plot(region$region_raster, main = "Example region (indices)",
#'              xlab = "Longitude (degrees)", ylab = "Latitude (degrees)",
#'              colNA = "blue")
#'
#' #' Dispersal distances
#' dispersal_gen <- DispersalGenerator$new(region = region)
#' dispersal_gen$set_attributes(params = list(p = 0.5, b = 700, r = 3000))
#' distances <- round(dispersal_gen$calculate_distance_matrix()) #' in m
#' dispersal_gen$calculate_distance_data()
#' dispersal_indices <- as.matrix(dispersal_gen$distance_data$base[,1:2])
#'
#' #' Distance multipliers with friction in cell 4
#' dispersal_friction <- DispersalFriction$new(region = region,
#'                                             conductance = c(1, 1, 1, 0.5, 1, 1, 1))
#' multipliers <- dispersal_friction$calculate_distance_multipliers(dispersal_indices)
#' cbind(dispersal_indices, distance = distances[dispersal_indices],
#'       multiplier = multipliers[[1]])
#'
#' #' Note that crossing the water is avoided.
#'
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom R6 R6Class
#' @include Region.R
#' @include SpatialModel.R
#' @export DispersalFriction

DispersalFriction <- R6Class("DispersalFriction",
  inherit = SpatialModel,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericClass, GenericModel & SpatialModel) #
    #   initialize(region = NULL, ...)
    #   new_clone(...)
    #   get_attribute_names()
    #   get_attributes(params = NULL)
    #   get_attribute(param)
    #   get_attribute_aliases(params = NULL)
    #   set_attributes(params = list(), ...)

    # New methods #

    #' @description
    #' Calculates and returns spatio-temporal dispersal distance multipliers for each in-range migration.
    #' @param dispersal_indices Two-column matrix representing the target and source coordinate index for each in-range migration.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    #' @return Temporal list of dispersal distance multiplier arrays with values for each in-range migration.
    calculate_distance_multipliers = function(dispersal_indices, ...) {

      # Set attributes
      if (length(list(...))) {
        self$set_attributes(...)
      }

      # Ensure region/coordinates are set
      if (is.null(self$coordinates) || self$region$region_cells == 0) {
        stop("Distance multipliers calculation requires region/coordinates to be set first", call. = FALSE)
      }

      # Ensure dispersal indices are correctly set and are consistent with coordinates
      if (is.null(dispersal_indices) || !all(is.integer(dispersal_indices)) || !all(dispersal_indices >= 1) ||
          !is.matrix(dispersal_indices) || ncol(dispersal_indices) != 2 ||
          nrow(dispersal_indices) > self$region$region_cells^2 ||
          max(dispersal_indices) > self$region$region_cells) {
        stop("Dispersal indices must be a two-column matrix representing the target and source coordinate index for each in-range migration", call. = FALSE)
      }

      tryCatch({

        # Obtain a region that uses a raster
        if (self$region$use_raster) {
          raster_region <- self$region
        } else {
          raster_region <- Region$new(coordinates = self$region$coordinates, use_raster = TRUE)
        }

        if (is.null(self$conductance)) { # set as 1 for all non-NA cells
          self$conductance <- raster_region$region_raster*0 + 1
        }

        suppressWarnings({
          # Calculate raster, transition matrix, then least cost distances for no friction
          no_friction_rast <- raster_region$region_raster
          no_friction_rast[] <- 1 # include NAs # [raster_region$region_indices] <- 1
          no_friction_transitions <- gdistance::transition(no_friction_rast, transitionFunction = mean, directions = self$transition_directions)
          no_friction_transitions <- gdistance::geoCorrection(no_friction_transitions, type = "c", scl = TRUE, multpl = FALSE)
          no_friction_costs <- as.matrix(gdistance::costDistance(no_friction_transitions, as.matrix(self$coordinates)))
          no_friction_costs <- no_friction_costs[dispersal_indices]
          no_friction_transitions <- NULL # release from memory
        })

        # Calculate the (within range) distance multipliers for each time step in parallel
        doParallel::registerDoParallel(cores = self$parallel_cores)
        self <- self # Ensure that this object consistently becomes available within each parallel thread
        distance_multipliers <- foreach(i = 1:ncol(as.matrix(self$conductance[])),
                                        .packages = c("raster"),
                                        .errorhandling = c("stop")) %dopar% {
          suppressWarnings({
            # Calculate raster, transition matrix, then least cost distances for friction for time step
            if (any(class(self$conductance) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
              conductance_rast <- raster::subset(self$conductance, i)
            } else { # assume friction matrix
              conductance_rast <- raster_region$region_raster
              conductance_rast[raster_region$region_indices] <- self$conductance[, i]
            }
            conductance_transitions <- gdistance::transition(conductance_rast, transitionFunction = mean, directions = self$transition_directions)
            conductance_transitions <- gdistance::geoCorrection(conductance_transitions, type = "c", scl = TRUE, multpl = FALSE)
            conductance_costs <- as.matrix(gdistance::costDistance(conductance_transitions, as.matrix(self$coordinates)))
            conductance_costs <- conductance_costs[dispersal_indices]
            conductance_transitions <- NULL # release from memory
          })
          if (!is.null(self$write_to_dir)) {
            file_path <- file.path(self$write_to_dir, sprintf("multipliers_%s.RData", i))
            saveRDS(conductance_costs/no_friction_costs, file = file_path)
            file_path # return to parallel rbind
          } else {
            conductance_costs/no_friction_costs # return to parallel rbind
          }
        }

      },
      error = function(e){
        self$error_messages <- gsub("\n", "", as.character(e), fixed = TRUE)
      })
      doParallel::stopImplicitCluster()

      if (!is.null(self$error_messages)) {
        error_messages <- self$error_messages; self$error_messages <- NULL
        stop(paste(c("Encountered in calculating distance multipliers:", error_messages), collapse = "\n"), call. = FALSE)
      } else {
        return(distance_multipliers)
      }
    }

  ), # end public

  private = list(

    ## Attributes ##

    # Model attributes #
    .model_attributes = c("region", "coordinates", "parallel_cores", "write_to_dir", "transition_directions", "conductance"),
    .region = NULL,
    .parallel_cores = 1,
    .write_to_dir = NULL,
    .transition_directions = 8,
    .conductance = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("region", "coordinates", "parallel_cores", "write_to_dir", "transition_directions", "conductance")

    # Dynamic attributes #
    # .attribute_aliases [inherited]

    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]

  ), # end private

  # Active binding accessors for private attributes (above)
  active = list(

    # Model attribute accessors #

    #' @field model_attributes A vector of model attribute names.
    model_attributes = function(value) { # inherited
      if (missing(value)) {
        super$model_attributes
      } else {
        super$model_attributes <- value
      }
    },

    #' @field region A \code{\link{Region}} (or inherited class) object specifying the study region.
    region = function(value) {
      if (missing(value)) {
        private$.region
      } else {
        if ("Region" %in% class(value)) {
          if ((value$use_raster && is.null(value$region_raster)) || (!value$use_raster && is.null(value$coordinates))) {
            warning("Spatial region has not been defined within the region object", call. = FALSE)
          }
          if (!is.null(self$conductance)) {
            if (any(class(self$conductance) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
              value$use_raster = TRUE
              if (!value$raster_is_consistent(self$conductance)) {
                stop("Region must be consistent with the conductance raster", call. = FALSE)
              }
            } else { # assume conductance matrix
              if (value$region_cells > 0 && value$region_cells != nrow(self$conductance)) {
                stop("Region must be consistent with conductance matrix dimensions", call. = FALSE)
              }
            }
          }
        } else if (!is.null(value)) {
          stop("Region should be a Region (or inherited class) object", call. = FALSE)
        }
        private$.region <- value
      }
    },

    #' @field coordinates Data frame (or matrix) of X-Y population (WGS84) coordinates in longitude (degrees West) and latitude (degrees North) (get and set), or distance-based coordinates dynamically returned by region raster (get only).
    coordinates = function(value) { # use non-raster region
      if (missing(value)) {
        self$region$coordinates
      } else {
        region <- Region$new(coordinates = value, use_raster = FALSE)
        if (!is.null(value) && !is.null(self$conductance)) {
          if (any(class(self$conductance) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
            region$use_raster <- TRUE
            if (!region$raster_is_consistent(self$conductance)) {
              stop("Region coordinates must be consistent with the conductance raster", call. = FALSE)
            }
          } else { # assume friction matrix
            if (nrow(value) != nrow(self$conductance)) {
              stop("Region coordinates must be consistent with conductance matrix dimensions", call. = FALSE)
            }
          }
        }
        self$region <- region
      }
    },

    #' @field parallel_cores Number of cores for running the simulations in parallel.
    parallel_cores = function(value) {
      if (missing(value)) {
        private$.parallel_cores
      } else {
        private$.parallel_cores <- value
      }
    },

    #' @field write_to_dir Directory path for storing distance multipliers when memory performance is an issue.
    write_to_dir = function(value) {
      if (missing(value)) {
        private$.write_to_dir
      } else {
        if (!is.character(value) || !dir.exists(value)) {
          stop("Dispersal friction: write_to_dir must be a existing directory path (string)", call. = FALSE)
        }
        private$.write_to_dir <- value
      }
    },

    #' @field transition_directions Number of transition directions or neighbors in which cells are connected: usually 4, 8 (default), or 16 (see \code{\link[gdistance:transition]{gdistance::transition}}).
    transition_directions = function(value) {
      if (missing(value)) {
        private$.transition_directions
      } else {
        private$.transition_directions <- value
      }
    },

    #' @field conductance Matrix/raster of conductance (inverse friction) values (range: 0 = barrier; 0 < some friction < 1; 1 = no friction) for each grid cell (rows/cells) at each simulation time step (columns/layers).
    conductance = function(value) {
      if (missing(value)) {
        private$.conductance
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            value <- utils::read.csv(file = value)
          } else if (length(grep(".RDATA", toupper(value), fixed = TRUE)) || length(grep(".RDS", toupper(value), fixed = TRUE))) {
            value <- readRDS(file = value)
          } else if (length(grep(".GRD", toupper(value), fixed = TRUE))) {
            value <- raster::brick(value)
          } else {
            value <- utils::read.table(file = value)
          }
        }
        if (!is.null(value)) {
          if (any(class(value) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
            if (!is.null(self$region) && !self$region$raster_is_consistent(value)) {
              stop("Conductance raster must be consistent with the defined region raster", call. = FALSE)
            }
          } else { # assume matrix-like object
            value <- as.matrix(value)
            if (!is.null(self$region$coordinates) && nrow(value) != nrow(self$region$coordinates)) {
              stop("Conductance matrix dimensions must be consistent with region/coordinates", call. = FALSE)
            }
          }
        }
        private$.conductance <- value
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
