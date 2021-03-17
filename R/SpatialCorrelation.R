#' R6 class representing a spatial correlation.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class functionality for modeling spatial correlations
#' within a spatially-explicit model. It provides functionality for calculating
#' correlations between region cells using a distance-based function:
#' \emph{a*exp(-distance/b)}, where \emph{a} (amplitude) and \emph{b} (breadth) are
#' configurable model attributes. It then calculates the Cholesky decomposition of
#' the correlation matrix (via \code{\link[base:chol]{chol}}), which is utilized to
#' generate (optionally temporal) correlated normal deviates. A compacted version of
#' the decomposed matrix can also generated for computational efficiency.
#'
#' @examples
#' # U Island example region
#' coordinates <- data.frame(x = rep(seq(177.01, 177.05, 0.01), 5),
#'                           y = rep(seq(-18.01, -18.05, -0.01), each = 5))
#' template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
#' template_raster[][-c(7, 9, 12, 14, 17:19)] <- NA # make U Island
#' region <- Region$new(template_raster = template_raster)
#' # Spatial correlation
#' env_corr <- SpatialCorrelation$new(region = region, amplitude = 0.4, breadth = 500)
#' env_corr$calculate_distance_matrix() # m
#' env_corr$calculate_correlations(decimals = 5)
#' env_corr$correlation_matrix
#' env_corr$calculate_cholesky_decomposition(decimals = 2)
#' env_corr$t_decomposition_matrix
#' env_corr$get_compact_decomposition()
#' # Scale to km
#' env_corr$distance_scale <- 1000
#' env_corr$calculate_distance_matrix() # km
#'
#' @importFrom R6 R6Class
#' @include SpatialModel.R
#' @export SpatialCorrelation

SpatialCorrelation <- R6Class("SpatialCorrelation",
  inherit = SpatialModel,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericClass, GenericModel & SpatialModel) #
    #   new_clone(...)
    #   get_attribute_names()
    #   get_attributes(params = NULL)
    #   get_attribute(param)
    #   get_attribute_aliases(params = NULL)
    #   set_attributes(params = list(), ...)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets given attributes individually and/or from a list.
    #' @param compact_only Boolean to indicate that only the compact versions of matrices will be maintained once calculated.
    #' @param attribute_aliases Optional list of extra alias names for model attributes (form: \code{alias = "attribute"}) to be used with the set and get attributes methods.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(compact_only = TRUE, attribute_aliases = NULL, ...) {
      self$compact_only <- compact_only
      attribute_aliases <- c(attribute_aliases, # Append default aliases
                             list(amplitude = "correlation_amplitude", correlation_a = "correlation_amplitude", a = "correlation_amplitude",
                                  breadth = "correlation_breadth", correlation_b = "correlation_breadth", b = "correlation_breadth"))
      super$initialize(attribute_aliases = attribute_aliases, ...)
    },

    # New methods #

    #' @description
    #' Returns a matrix with the calculated distance (in meters by default) between each pair of region cells.
    #' @param use_longlat Optional boolean indicating use of (WGS84) coordinates in longitude (degrees West) and latitude (degrees North).
    #' @return Matrix with distances between region cells.
    calculate_distance_matrix = function(use_longlat = NULL) {
      if (!is.null(self$region)) {
        coordinates <- self$region$coordinates
        if (is.null(coordinates)) {
          stop("Distance matrix calculation requires the region to be defined with coordinates or a raster first", call. = FALSE)
        }
        if (!self$region$use_raster || (is.logical(use_longlat) && use_longlat) ||
            length(grep("longlat", as.character(raster::crs(self$region$region_raster)), fixed = TRUE)) > 0) {
          return(geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)/self$distance_scale)
        } else { # assume coordinates in meters
          if (is.na(raster::crs(self$region$region_raster))) {
            warning("No coordinate reference system (CRS) specified: assuming coordinates are in meters", call. = FALSE)
          }
          return(as.matrix(stats::dist(coordinates))/self$distance_scale)
        }
      } else {
        stop("Distance matrix calculation requires region to be set first", call. = FALSE)
      }
    },

    #' @description
    #' Calculates the correlation matrix by applying the distance-based correlation function.
    #' @param distance_matrix Optional pre-calculated matrix with distances between region cells.
    #' @param decimals Optional number of decimal places for correlation values.
    #' @param threshold Optional threshold (minimum value) for correlation values (default 0.0000001).
    #' @param ... Parameters passed via a \emph{params} list or individually.
    calculate_correlations = function(distance_matrix = NULL, decimals = NULL, threshold = 0.0000001, ...) {

      # Set attributes
      self$correlation_matrix <- NULL
      if (length(list(...))) {
        self$set_attributes(...)
      }

      # Ensure region and distance classes are set
      if (is.null(self$region) || is.null(self$correlation_amplitude) || is.null(self$correlation_breadth)) {
        stop("Correlation calculations require region and function parameter settings amplitude and breadth", call. = FALSE)
      }

      # Ensure pre-calculated distance matrix is consistent with region
      if (!is.null(distance_matrix) && !is.null(self$region)) {
        if (!is.matrix(distance_matrix) || nrow(distance_matrix) != self$region$region_cells || ncol(distance_matrix) != self$region$region_cells) {
          stop("Distance matrix dimensions must be consistent with the number of region cells", call. = FALSE)
        }
      }

      # Matrix of pairwise distances between region grid centroids (km)
      if (is.null(distance_matrix)) {
        distance_matrix <- self$calculate_distance_matrix()
      }

      # Calculate correlation values between each region cell based on a*exp(-distance/b)
      self$correlation_matrix <- self$correlation_amplitude*exp(-1*distance_matrix/self$correlation_breadth)
      diag(self$correlation_matrix) <- 1

      # Rounding?
      if (!is.null(decimals)) {
        self$correlation_matrix <- round(self$correlation_matrix, as.numeric(decimals))
      } else if (is.numeric(threshold)) {
        self$correlation_matrix[which(self$correlation_matrix < threshold)] <- 0
      }

    },

    #' @description
    #' Calculates the transposed Cholesky decomposition of the correlation matrix (via \code{\link[base:chol]{chol}}).
    #' @param distance_matrix Optional pre-calculated matrix with distances between region cells.
    #' @param decimals Optional number of decimal places for correlation values.
    #' @param threshold Optional threshold (minimum value) for correlation values (default 0.0000001).
    #' @param ... Parameters passed via a \emph{params} list or individually.
    calculate_cholesky_decomposition = function(distance_matrix = NULL, decimals = NULL, threshold = 0.0000001, ...) {

      # Set attributes
      self$t_decomposition_matrix <- NULL
      if (length(list(...)) && !is.null(self$correlation_matrix)) {
        self$set_attributes(...)
      }

      # Calculate correlations when required
      if (is.null(self$correlation_matrix)) {
        self$calculate_correlations(distance_matrix = distance_matrix, decimals = decimals, threshold = threshold, ...)
      }

      # Attempt to calculate the transposed Cholesky decomposition matrix
      self$t_decomposition_matrix <- tryCatch(chol(self$correlation_matrix), error = function(e) e)
      if (is.matrix(self$t_decomposition_matrix)) {

        if (self$compact_only) { # clear the correlation matrix
          self$correlation_matrix <- NULL
        }

        # Rounding?
        if (!is.null(decimals)) {
          self$t_decomposition_matrix <- round(self$t_decomposition_matrix, as.numeric(decimals))
        } else if (is.numeric(threshold)) {
          self$t_decomposition_matrix[which(self$t_decomposition_matrix < threshold)] <- 0
        }

      } else { # assume error caught

        error_message <- "for unknown reasons"
        if ("error" %in% class(self$t_decomposition_matrix)) {
          error_message <- paste("because", self$t_decomposition_matrix$message)
        }
        self$t_decomposition_matrix <- NULL

        stop(paste("Cholesky decomposition could not be calculated", error_message), call. = FALSE)
      }

    },

    #' @description
    #' Compacts the transposed Cholesky decomposition of the correlation matrix into the minimal number of rows, which are mapped to the original matrix.
    #' @param distance_matrix Optional pre-calculated matrix with distances between region cells.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    calculate_compact_decomposition = function(distance_matrix = NULL, ...) {

      # Set attributes
      self$t_decomposition_compact_matrix <- NULL
      self$t_decomposition_compact_map <- NULL
      if (length(list(...)) && !is.null(self$t_decomposition_matrix)) {
        self$set_attributes(...)
      }

      # Calculate Cholesky decomposition when required
      if (is.null(self$t_decomposition_matrix)) {
        self$calculate_cholesky_decomposition(distance_matrix = distance_matrix, ...)
      }

      # Calculate non-zero decomposition data from the decomposition matrix
      t_decomposition_data <- which(self$t_decomposition_matrix != 0, arr.ind = TRUE, useNames = TRUE)
      t_decomposition_data <- as.data.frame(cbind(t_decomposition_data, self$t_decomposition_matrix[t_decomposition_data]))
      region_cells <- nrow(self$t_decomposition_matrix)
      if (self$compact_only) { # clear the full decomposition matrix
        self$t_decomposition_matrix <- NULL
      }
      names(t_decomposition_data) <- c("row", "col", "value")
      t_decomposition_data <- t_decomposition_data[order(t_decomposition_data$col, t_decomposition_data$row),]

      # Create a compact transposed decomposition matrix
      if (!is.null(self$region)) {
        region_cells <- self$region$region_cells # should be same as rows/columns in full matrix
      }
      t_decomposition_nonzero_rows <- tabulate(t_decomposition_data$col, nbins = region_cells)
      t_decomposition_compact_rows <- max(t_decomposition_nonzero_rows)
      self$t_decomposition_compact_matrix <- array(1:t_decomposition_compact_rows, c(t_decomposition_compact_rows, region_cells))
      self$t_decomposition_compact_matrix <- self$t_decomposition_compact_matrix*(self$t_decomposition_compact_matrix <= matrix(t_decomposition_nonzero_rows, nrow = t_decomposition_compact_rows, ncol = region_cells, byrow = TRUE))
      t_decomposition_compact_indices <- which(self$t_decomposition_compact_matrix != 0)
      self$t_decomposition_compact_matrix[t_decomposition_compact_indices] <- t_decomposition_data$value

      # Create a map to the original region grid rows
      self$t_decomposition_compact_map <- array(NA, c(t_decomposition_compact_rows, region_cells))
      self$t_decomposition_compact_map[t_decomposition_compact_indices] <- t_decomposition_data$row

    },

    #' @description
    #' Returns a compact transposed Cholesky decomposition of the correlation matrix and a corresponding map of region cell indices in a list with names: matrix, map.
    #' @param distance_matrix Optional pre-calculated matrix with distances between region cells.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    #' @return List containing a compact Cholesky decomposition matrix and a corresponding map of region cell indices (for the compacted rows).
    get_compact_decomposition = function(distance_matrix = NULL, ...) {

      # Calculate compact decomposition when required
      if (!is.null(distance_matrix) || is.null(self$t_decomposition_compact_matrix) || is.null(self$t_decomposition_compact_map)) {
        self$calculate_compact_decomposition(distance_matrix = distance_matrix, ...)
      } else if (length(list(...))) { # set attributes
        self$set_attributes(...)
      }

      # Pack the decomposition compact matrix and map into a list
      return(list(matrix = self$t_decomposition_compact_matrix, map = self$t_decomposition_compact_map))

    },

    #' @description
    #' Generates correlated normal deviates using the spatial correlation, utilizing the optional random seed and optional temporal correlation across time steps.
    #' @param random_seed Optional seed for the random generation of correlated deviates.
    #' @param temporal_correlation Optional temporal correlation coefficient (0-1; default = 1).
    #' @param time_steps Optional number of time steps for temporal correlation (default = 1 or none).
    #' @return Array (non-temporal) or matrix (temporal) of correlated normal deviates.
    generate_correlated_normal_deviates = function(random_seed = NULL, temporal_correlation = 1, time_steps = 1) {

      # Ensure compact correlation decomposition is calculated
      if (is.null(self$t_decomposition_compact_matrix) || is.null(self$t_decomposition_compact_map)) {
        return("The compact correlation decomposition needs to be calculated before correlated normal deviates can be generated")
      }

      # Resolve dimensions
      region_cells <- self$region$region_cells
      compact_rows <- nrow(self$t_decomposition_compact_matrix)

      # Set random seed when present
      if (!is.null(random_seed)) {
        set.seed(random_seed)
      }

      # Generate temporal deviates
      if (time_steps > 1) {
        if (temporal_correlation < 1) {
          deviates <- array(stats::rnorm(region_cells*time_steps), c(region_cells, time_steps))
          # Calculate a Cholesky decomposition for temporal correlation between sequential time-steps
          if (temporal_correlation > 0) {
            time_step_correlation <- array(temporal_correlation, c(2, 2))
            diag(time_step_correlation) <- 1
            time_step_decomposition <- chol(time_step_correlation)[,2]
            for (i in 2:time_steps) {
              deviates[,i] <- time_step_decomposition[1]*deviates[,i-1] + time_step_decomposition[2]*deviates[,i]
            }
          }
        } else { # temporal correlation = 1 : duplicate deviates across time
          deviates <- array(stats::rnorm(region_cells), c(region_cells, time_steps))
        }

        # Apply spatial correlation to the temporal deviates
        for (i in 1:time_steps) {
          deviates[,i] <- .colSums(self$t_decomposition_compact_matrix*deviates[self$t_decomposition_compact_map, i],
                                   m = compact_rows, n = region_cells, na.rm=TRUE)
        }

        return(deviates)

      } else { # Generate spatially correlated only

        return(.colSums(self$t_decomposition_compact_matrix*stats::rnorm(region_cells)[self$t_decomposition_compact_map],
                        m = compact_rows, n = region_cells, na.rm=TRUE))
      }
    }

  ), # end public

  private = list(

    ## Attributes ##

    # Model attributes #
    .model_attributes = c("region", "distance_scale", "correlation_amplitude", "correlation_breadth", "correlation_matrix",
                          "t_decomposition_matrix", "compact_only", "t_decomposition_compact_matrix",
                          "t_decomposition_compact_map"),
    # .region            [inherited]
    .distance_scale = 1,
    .correlation_amplitude = NULL,
    .correlation_breadth = NULL,
    .correlation_matrix = NULL,
    .t_decomposition_matrix = NULL,
    .compact_only = NULL,
    .t_decomposition_compact_matrix = NULL,
    .t_decomposition_compact_map = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("region", "distance_scale", "coordinates", "correlation_amplitude", "correlation_breadth",
                           "correlation_matrix", "t_decomposition_matrix", "compact_only", "t_decomposition_compact_matrix",
                           "t_decomposition_compact_map")

    # Dynamic attributes #
    # .attribute_aliases [inherited]

    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]

  ), # end private

  # Active binding accessors for private attributes (above) #
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
        if (!is.null(self$correlation_matrix) || !is.null(self$t_decomposition_matrix) ||
            !is.null(self$t_decomposition_compact_matrix) || !is.null(self$t_decomposition_compact_map)) {
          stop("Calculated correlations/decompositions are already associated with the existing region", call. = FALSE)
        } else {
          if ("Region" %in% class(value)) {
            if ((value$use_raster && is.null(value$region_raster)) || (!value$use_raster && is.null(value$coordinates))) {
              warning("Spatial region has not been defined within the region object", call. = FALSE)
            }
          } else if (!is.null(value)) {
            stop("Region should be a Region (or inherited class) object", call. = FALSE)
          }
          private$.region <- value
        }
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

    #' @field distance_scale Scale of distance values in meters (default = 1). Usage: set to 1 for values in meters, or to 1000 for values in kilometers.
    distance_scale = function(value) {
      if (missing(value)) {
        private$.distance_scale
      } else {
        private$.distance_scale <- value
      }
    },

    #' @field correlation_amplitude Correlation function: \emph{a*exp(-distance/b)} \emph{a} parameter. Represents the amplitude or maximum magnitude of correlation values between model cells.
    correlation_amplitude = function(value) {
      if (missing(value)) {
        private$.correlation_amplitude
      } else {
        if (!is.null(self$correlation_matrix) || !is.null(self$t_decomposition_matrix) ||
            !is.null(self$t_decomposition_compact_matrix) || !is.null(self$t_decomposition_compact_map)) {
          stop("Calculated correlations/decompositions are already associated with the existing correlation parameters", call. = FALSE)
        } else if (!is.null(value) && is.numeric(value) && (value > 1 || value < 0)) {
          stop("Correlation function parameter amplitude must be between 0 and 1 inclusively", call. = FALSE)
        } else if (!is.null(value) && !is.numeric(value)) {
          stop("Correlation function parameter amplitude must be numeric", call. = FALSE)
        } else {
          private$.correlation_amplitude <- value
        }
      }
    },

    #' @field correlation_breadth Correlation function: \emph{a*exp(-distance/b)} \emph{b} parameter. Represents the breadth of the correlation between region cells. Typically estimated via average distance between correlated region cells.
    correlation_breadth = function(value) {
      if (missing(value)) {
        private$.correlation_breadth
      } else {
        if (!is.null(self$correlation_matrix) || !is.null(self$t_decomposition_matrix) ||
            !is.null(self$t_decomposition_compact_matrix) || !is.null(self$t_decomposition_compact_map)) {
          stop("Calculated correlations/decompositions are already associated with the existing correlation parameters", call. = FALSE)
        } else if (!is.null(value) && is.numeric(value) && value <= 0) {
          stop("Correlation function parameter breadth must be positive/non-zero", call. = FALSE)
        } else if (!is.null(value) && !is.numeric(value)) {
          stop("Correlation function parameter breadth must be numeric", call. = FALSE)
        } else {
          private$.correlation_breadth <- value
        }
      }
    },

    #' @field correlation_matrix Correlation matrix calculated via correlation function: \emph{a*exp(-distance/b)}.
    correlation_matrix = function(value) {
      if (missing(value)) {
        private$.correlation_matrix
      } else {
        private$.correlation_matrix <- value
      }
    },

    #' @field t_decomposition_matrix The transposed Cholesky decomposition of the correlation matrix (see \code{\link[base:chol]{chol}}).
    t_decomposition_matrix = function(value) {
      if (missing(value)) {
        private$.t_decomposition_matrix
      } else {
        private$.t_decomposition_matrix <- value
      }
    },

    #' @field compact_only Boolean to indicate that only the compact versions of matrices will be maintained once calculated.
    compact_only = function(value) {
      if (missing(value)) {
        private$.compact_only
      } else {
        private$.compact_only <- value
      }
    },

    #' @field t_decomposition_compact_matrix A compact (rows) version of the transposed Cholesky decomposition of the correlation matrix.
    t_decomposition_compact_matrix = function(value) {
      if (missing(value)) {
        private$.t_decomposition_compact_matrix
      } else {
        private$.t_decomposition_compact_matrix <- value
      }
    },

    #' @field t_decomposition_compact_map A map of the original region cell rows for the compact transposed decomposition matrix.
    t_decomposition_compact_map = function(value) {
      if (missing(value)) {
        private$.t_decomposition_compact_map
      } else {
        private$.t_decomposition_compact_map <- value
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
