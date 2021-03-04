#' R6 class representing a paleontological region.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class representing a study region of temporally changing
#' spatial grid cells, defined via a \emph{RasterLayer} object
#' (see \code{\link[raster:raster-package]{raster}}) and a temporal mask indicating
#' which cells are included at each time step.
#'
#' @importFrom R6 R6Class
#' @include Region.R
#' @export PaleoRegion

PaleoRegion <- R6Class("PaleoRegion",
  inherit = Region,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericClass & Region) #
    #   new_clone(...)
    #   raster_is_consistent(check_raster)
    #   raster_from_values(values)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets temporally changing raster layers for paleontological region.
    #' @param template_raster A \emph{RasterLayer}, \emph{RasterBrick}, or \emph{RasterStack} object (see \code{\link[raster:raster-package]{raster}}) defining the paleontological region with example finite values (NAs elsewhere)
    #' @param remove_zeros Boolean to indicate that cells that are zero across all layers (times) are to be removed, i.e. set to NA (default is FALSE).
    #' @param ... Additional parameters passed individually.
    initialize = function(template_raster = NULL, remove_zeros = FALSE, ...) {
      if (!is.null(template_raster)) {
        if (!(any(class(template_raster) %in% c("RasterLayer", "RasterBrick", "RasterStack")))) {
          stop("Template raster should be a raster::RasterLayer, RasterBrick, RasterStack (or inherited class) object", call. = FALSE)
        }
        if (remove_zeros) {
          template_raster[which(apply(as.matrix(template_raster[]), 1, function(x) all(x == 0 | is.na(x))))] <- NA
        }
        finite_indices <- which(apply(as.matrix(template_raster[]), 1, function(x) any(is.finite(x))))
        self$temporal_mask <- is.finite(template_raster[finite_indices])
        self$region_raster <- template_raster[[1]]
        self$region_raster[finite_indices] <- 1:length(finite_indices)
      }
      super$initialize(...)
    },

    #' @description
    #' Converts an array (or matrix) of values into a raster (or stack) consistent with the region raster (matching extent, resolution, and finite/NA cells), and with the temporal mask (if any) applied.
    #' @param values An array (or matrix) of values to be placed in the raster (or stack) having dimensions consistent with the region cell number.
    #' @return  A \emph{RasterLayer} (or \emph{RasterStack/Brick}) object consistent with the region raster with temporal mask (if any) applied.
    raster_from_values = function(values) {
      value_raster <- super$raster_from_values(values)
      if (!is.null(self$temporal_mask) && ncol(as.matrix(values)) == ncol(self$temporal_mask)) {
        value_raster[self$region_indices] <- value_raster[self$region_indices]*(self$temporal_mask | NA)
      }
      return(value_raster)
    },

    #' @description
    #' Returns the temporal mask as a raster stack/brick object consistent with the region raster.
    #' @return  A \emph{RasterStack/Brick}) object with temporal mask values of 1 (true) and NA elsewhere.
    temporal_mask_raster = function() {
      if (!is.null(self$temporal_mask)) {
        return(self$raster_from_values(self$temporal_mask))
      }
    }

  ), # end public

  private = list(

    ## Attributes ##

    # .coordinates        [inherited]
    # .region_raster      [inherited]
    # .use_raster         [inherited]
    # .strict_consistency [inherited]
    .temporal_mask = NULL

  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    #' @field coordinates Data frame (or matrix) of X-Y population (WGS84) coordinates in longitude (degrees West) and latitude (degrees North) (get and set), or distance-based coordinates dynamically returned by region raster (get only).
    coordinates = function(value) { # inherited
      if (missing(value)) {
        super$coordinates
      } else {
        super$coordinates <- value
      }
    },

    #' @field region_raster A \emph{RasterLayer} object (see \code{\link[raster:raster-package]{raster}}) defining the region with finite values (NAs elsewhere).
    region_raster = function(value) { # inherited
      if (missing(value)) {
        super$region_raster
      } else {
        super$region_raster <- value
      }
    },

    #' @field use_raster Boolean to indicate that a raster is to be used to define the region (default TRUE).
    use_raster = function(value) { # inherited
      if (missing(value)) {
        super$use_raster
      } else {
        super$use_raster <- value
      }
    },

    #' @field strict_consistency Boolean to indicate that, as well as resolution, extent and CRS, consistency checks also ensure that a raster's finite/occupiable cells are the same or a subset of those defined by the region (default TRUE).
    strict_consistency = function(value) { # inherited
      if (missing(value)) {
        super$strict_consistency
      } else {
        super$strict_consistency <- value
      }
    },

    #' @field temporal_mask Matrix of booleans indicating which region cells are included at each time step.
    temporal_mask = function(value) {
      if (missing(value)) {
        private$.temporal_mask
      } else {
        private$.temporal_mask <- value
      }
    },

    #' @field region_cells Dynamically calculated number of region coordinates or raster cells with finite/non-NA values.
    region_cells = function(value) { # inherited
      if (missing(value)) {
        super$region_cells
      } else {
        super$region_cells <- value
      }
    },

    #' @field region_indices Dynamically calculated region indices for raster cells with finite/non-NA values (all if not a raster).
    region_indices = function(value) { # inherited
      if (missing(value)) {
        super$region_indices
      } else {
        super$region_indices <- value
      }
    }

  ) # end active
)
