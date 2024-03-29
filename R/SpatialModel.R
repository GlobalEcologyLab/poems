#' R6 class representing a spatial model
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class representing a generic (abstract)
#' spatially-explicit model. It extends \code{\link{GenericModel}} with the addition of
#' a study region specification.
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
#' # Example spatial model
#' model1 <- SpatialModel$new(region = region, a_layers = 3)
#' model1$coordinates
#' model1$set_attributes(a_values = array(8:28, c(7, 3)))
#' model1$region$raster_from_values(model1$get_attribute("a_values"))
#'
#' @importFrom R6 R6Class
#' @include GenericModel.R
#' @include Region.R
#' @export SpatialModel

SpatialModel <- R6Class("SpatialModel",
  inherit = GenericModel,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericModel) #
    #   get_attribute_names()
    #   get_attributes(params = NULL)
    #   get_attribute(param)
    #   get_attribute_aliases(params = NULL)
    #   set_attributes(params = list(), ...)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets given attributes individually and/or from a list.
    #' @param region A \code{\link{Region}} (or inherited class) object specifying the study region.
    #' @param ... Parameters passed individually.
    initialize = function(region = NULL, ...) {
      if (!is.null(region)) {
        self$region <- region
      }
      super$initialize(...)
    },

    #' @description
    #' Creates a new (re-initialized) object of the current (inherited) object class with optionally passed parameters.
    #' @param ... Parameters passed via the inherited class constructor (defined in initialize and run via new).
    #' @return New object of the current (inherited) class.
    new_clone = function(...) {
      return(super$new_clone(region = self$region, ...))
    }
  ), # end public

  private = list(

    ## Attributes ##

    # Model attributes #
    .model_attributes = c("region", "coordinates"),
    .region = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("region", "coordinates")

    # Dynamic attributes #
    # .attribute_aliases [inherited]

    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]
  ), # end private

  # Active binding accessors for private model attributes (above) #
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
        } else if (!is.null(value)) {
          stop("Region should be a Region (or inherited class) object", call. = FALSE)
        }
        private$.region <- value
      }
    },

    #' @field coordinates Data frame (or matrix) of X-Y population (WGS84) coordinates in longitude (degrees West) and latitude (degrees North) (get and set), or distance-based coordinates dynamically returned by region raster (get only).
    coordinates = function(value) {
      if (missing(value)) {
        self$region$coordinates
      } else {
        # Create a region with coordinates
        self$region <- Region$new(coordinates = value, use_raster = FALSE)
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
