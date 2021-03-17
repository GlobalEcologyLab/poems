#' R6 class representing a nested container for dispersal generator attributes
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class representing a nested container for
#' \code{\link{DispersalGenerator}} attributes that are maintained when new model clones
#' are created. The container maintains \emph{input} and \emph{output} attribute names,
#' file, function and distribution templates, correlation parameters (for distribution
#' generation), rounding decimals, occupancy mask, and other
#' \code{\link{DispersalGenerator}} attributes that need to be maintained when cloning.
#'
#' @importFrom R6 R6Class
#' @include GenerativeTemplate.R
#' @export DispersalTemplate

DispersalTemplate <- R6Class("DispersalTemplate",
  inherit = GenerativeTemplate,
  public = list(

    ## Attributes ##

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list()

    ## Methods ##

    # Inherited methods (from GenerativeTemplate) #
    # initialize()

  ), # end public

  private = list(

    ## Attributes ##
    # .description            [inherited]
    # .inputs                 [inherited]
    # .outputs                [inherited]
    # .file_templates         [inherited]
    # .function_templates     [inherited]
    # .distribution_templates [inherited]
    # .uses_correlations      [inherited]
    # .spatial_correlation    [inherited]
    # .temporal_correlation   [inherited]
    # .time_steps             [inherited]
    # .decimals               [inherited]
    # .occupancy_mask         [inherited]
    .dispersal_friction = NULL,
    .distance_classes = NULL,
    .max_distance_classes = 1000,
    .distance_scale = 1,
    .distance_data = NULL,
    .dispersal_function_data = NULL,
    .dispersal_proportion = NULL,
    .dispersal_breadth = NULL,
    .dispersal_max_distance = NULL

  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    #' @field description A brief description of what the generator generates.
    description = function(value) { # inherited
      if (missing(value)) {
        super$description
      } else {
        super$description <- value
      }
    },

    #' @field inputs An array of input attribute names for the generator.
    inputs = function(value) { # inherited
      if (missing(value)) {
        super$inputs
      } else {
        super$inputs <- value
      }
    },

    #' @field outputs An array of output attribute names for the generator.
    outputs = function(value) { # inherited
      if (missing(value)) {
        super$outputs
      } else {
        super$outputs <- value
      }
    },

    #' @field file_templates A nested list of file template attributes.
    file_templates = function(value) { # inherited
      if (missing(value)) {
        super$file_templates
      } else {
        super$file_templates <- value
      }
    },

    #' @field function_templates A nested list of function template attributes.
    function_templates = function(value) { # inherited
      if (missing(value)) {
        super$function_templates
      } else {
        super$function_templates <- value
      }
    },

    #' @field distribution_templates A list of distribution template attributes.
    distribution_templates = function(value) { # inherited
      if (missing(value)) {
        super$distribution_templates
      } else {
        super$distribution_templates <- value
      }
    },

    #' @field uses_correlations A boolean to indicate that a \code{\link{SpatialCorrelation}} (or inherited class) object is used for generating correlated random deviates.
    uses_correlations = function(value) { # inherited
      if (missing(value)) {
        super$uses_correlations
      } else {
        super$uses_correlations <- value
      }
    },

    #' @field spatial_correlation A \code{\link{SpatialCorrelation}} (or inherited class) object for generating correlated random deviates.
    spatial_correlation = function(value) { # inherited
      if (missing(value)) {
        super$spatial_correlation
      } else {
        super$spatial_correlation <- value
      }
    },

    #' @field temporal_correlation Absolute correlation coefficient between simulation time steps for all grid cells (0-1; default = 1).
    temporal_correlation = function(value) { # inherited
      if (missing(value)) {
        super$temporal_correlation
      } else {
        super$temporal_correlation <- value
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

    #' @field decimals Number of decimal places applied to generated data outputs (default: NULL = no rounding).
    decimals = function(value) { # inherited
      if (missing(value)) {
        super$decimals
      } else {
        super$decimals <- value
      }
    },

    #' @field occupancy_mask Optional binary mask array (matrix), data frame, or raster (stack) for generated (time-series) data outputs.
    occupancy_mask = function(value) { # inherited
      if (missing(value)) {
        super$occupancy_mask
      } else {
        super$occupancy_mask <- value
      }
    },

    #' @field dispersal_friction A \code{\link{DispersalFriction}} (or inherited class) object for dispersal distance multiplier data.
    dispersal_friction = function(value) {
      if (missing(value)) {
        private$.dispersal_friction
      } else {
        private$.dispersal_friction <- value
      }
    },

    #' @field distance_classes Vector of distance interval boundaries (in km) for calculating discrete dispersal rates.
    distance_classes = function(value) {
      if (missing(value)) {
        private$.distance_classes
      } else {
        private$.distance_classes <- value
      }
    },

    #' @field max_distance_classes The maximum number of distance classes when they are calculated automatically via the maximum distance (default: 1000).
    max_distance_classes = function(value) {
      if (missing(value)) {
        private$.max_distance_classes
      } else {
        private$.max_distance_classes <- value
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

    #' @field distance_data Data frame of distance classes including indices for the construction of compact matrices (columns: target_pop, source_pop, compact_row, distance_class).
    distance_data = function(value) {
      if (missing(value)) {
        private$.distance_data
      } else {
        private$.distance_data <- value
      }
    },

    #' @field dispersal_function_data Data frame of discrete dispersal function values. Optional first column may provide distance intervals (non-inclusive lower bounds).
    dispersal_function_data = function(value) {
      if (missing(value)) {
        private$.dispersal_function_data
      } else {
        private$.dispersal_function_data <- value
      }
    },

    #' @field dispersal_proportion Dispersal function: \emph{p*exp(-distance/b)} \emph{p} parameter. Represents the proportion and limit of dispersers between model cells.
    dispersal_proportion = function(value) {
      if (missing(value)) {
        private$.dispersal_proportion
      } else {
        private$.dispersal_proportion <- value
      }
    },

    #' @field dispersal_breadth Dispersal function: \emph{p*exp(-distance/b)} \emph{b} parameter. Represents the breadth of the dispersal between model cells. Typically estimated via average migration distance.
    dispersal_breadth = function(value) {
      if (missing(value)) {
        private$.dispersal_breadth
      } else {
        private$.dispersal_breadth <- value
      }
    },

    #' @field dispersal_max_distance Dispersal maximum distance or range (\emph{r}) parameter limits the use of the dispersal function: \emph{p*exp(-distance/b)}. The function is utilized when \emph{distance <= r} otherwise the dispersal rate is set to zero.
    dispersal_max_distance = function(value) {
      if (missing(value)) {
        private$.dispersal_max_distance
      } else {
        private$.dispersal_max_distance <- value
      }
    }

  ) # end active
)
