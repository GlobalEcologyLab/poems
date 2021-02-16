#' R6 class representing a nested container for generator attributes
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class representing a nested container for
#' \code{\link{Generator}} attributes that are maintained when new model clones are
#' created. The container maintains \emph{input} and \emph{output} attribute names,
#' file, function and distribution templates, correlation parameters (for distribution
#' generation), rounding decimals, occupancy mask, and any inherited class model
#' attributes that need to be maintained when cloning.
#'
#' @importFrom R6 R6Class
#' @export GenerativeTemplate

GenerativeTemplate <- R6Class("GenerativeTemplate",
  public = list(

    ## Attributes ##

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    #' @description
    #' Initialization method initializes the generator templates.
    initialize = function() {
      self$file_templates <- list()
      self$function_templates <- list()
      self$distribution_templates <- list()
    }

  ), # end public

  private = list(

    ## Attributes ##
    .description = NULL,
    .inputs = NULL,
    .outputs = NULL,
    .file_templates = NULL,
    .function_templates = NULL,
    .distribution_templates = NULL,
    .uses_correlations = FALSE,
    .spatial_correlation = NULL,
    .temporal_correlation = 1,
    .time_steps = 1,
    .generate_rasters = NULL,
    .decimals = NULL,
    .occupancy_mask = NULL

  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    #' @field description A brief description of what the generator generates.
    description = function(value) {
      if (missing(value)) {
        private$.description
      } else {
        private$.description <- value
      }
    },

    #' @field inputs An array of input attribute names for the generator.
    inputs = function(value) {
      if (missing(value)) {
        private$.inputs
      } else {
        private$.inputs <- value
      }
    },

    #' @field outputs An array of output attribute names for the generator.
    outputs = function(value) {
      if (missing(value)) {
        private$.outputs
      } else {
        private$.outputs <- value
      }
    },

    #' @field file_templates A list of file template attributes.
    file_templates = function(value) {
      if (missing(value)) {
        private$.file_templates
      } else {
        private$.file_templates <- value
      }
    },

    #' @field function_templates A list of function template attributes.
    function_templates = function(value) {
      if (missing(value)) {
        private$.function_templates
      } else {
        private$.function_templates <- value
      }
    },

    #' @field distribution_templates A list of distribution template attributes.
    distribution_templates = function(value) {
      if (missing(value)) {
        private$.distribution_templates
      } else {
        private$.distribution_templates <- value
      }
    },

    #' @field uses_correlations A boolean to indicate that a \code{\link{SpatialCorrelation}} (or inherited class) object is used for generating correlated random deviates.
    uses_correlations = function(value) {
      if (missing(value)) {
        private$.uses_correlations
      } else {
        private$.uses_correlations <- value
      }
    },

    #' @field spatial_correlation A \code{\link{SpatialCorrelation}} (or inherited class) object for generating correlated random deviates.
    spatial_correlation = function(value) {
      if (missing(value)) {
        private$.spatial_correlation
      } else {
        private$.spatial_correlation <- value
      }
    },

    #' @field temporal_correlation Absolute correlation coefficient between simulation time steps for all grid cells (0-1; default = 1).
    temporal_correlation = function(value) {
      if (missing(value)) {
        private$.temporal_correlation
      } else {
        private$.temporal_correlation <- value
      }
    },

    #' @field time_steps Number of simulation time steps (default = 1).
    time_steps = function(value) {
      if (missing(value)) {
        private$.time_steps
      } else {
        private$.time_steps <- value
      }
    },

    #' @field generate_rasters Boolean to indicate if rasters should be generated (default: NULL).
    generate_rasters = function(value) {
      if (missing(value)) {
        private$.generate_rasters
      } else {
        private$.generate_rasters <- value
      }
    },

    #' @field decimals Number of decimal places applied to the generated values (default: NULL = no rounding).
    decimals = function(value) {
      if (missing(value)) {
        private$.decimals
      } else {
        private$.decimals <- value
      }
    },

    #' @field occupancy_mask Optional binary mask array (matrix), data frame, or raster (stack) for generated (time-series) data.
    occupancy_mask = function(value) {
      if (missing(value)) {
        private$.occupancy_mask
      } else {
        private$.occupancy_mask <- value
      }
    }

  ) # end active
)
