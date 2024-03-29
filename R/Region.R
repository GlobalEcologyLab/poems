#' R6 class representing a study region.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class representing a study region of spatial grid cells
#' defined via a list of longitude/latitude cell coordinates (WGS84), or a
#' \emph{RasterLayer} object (see \code{\link[raster:raster-package]{raster}}).
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
#'   main = "Example region (cell indices)",
#'   xlab = "Longitude (degrees)", ylab = "Latitude (degrees)",
#'   colNA = "blue"
#' )
#' region$region_cells
#' region$coordinates
#' # Generate value layers
#' value_brick <- region$raster_from_values(array(8:28, c(7, 3)))
#' raster::plot(value_brick,
#'   main = "Example value layers",
#'   xlab = "Longitude (degrees)", ylab = "Latitude (degrees)",
#'   colNA = "blue"
#' )
#' value_brick[region$region_indices]
#'
#' @importFrom R6 R6Class
#' @include GenericClass.R
#' @export Region

Region <- R6Class("Region",
  inherit = GenericClass,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericClass) #
    #   new_clone(...)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets coordinates or raster for region.
    #' @param coordinates Data frame (or matrix) of X-Y coordinates (WGS84) in longitude (degrees West) and latitude (degrees North).
    #' @param template_raster A \emph{RasterLayer} object (see \code{\link[raster:raster-package]{raster}}) defining the region with example finite values (NAs elsewhere)
    #' @param region_raster A \emph{RasterLayer} object (see \code{\link[raster:raster-package]{raster}}) defining the region with finite cell indices (NAs elsewhere).
    #' @param use_raster Boolean to indicate that a raster is to be used to define the region (default TRUE).
    #' @param ... Additional parameters passed individually.
    initialize = function(coordinates = NULL, template_raster = NULL, region_raster = NULL, use_raster = TRUE, ...) {
      super$initialize(...)
      if (!is.null(coordinates) && (!is.null(template_raster) || !is.null(region_raster))) {
        stop("Region should be specified with a set of coordinates or a raster, not both", call. = FALSE)
      }
      if (!is.null(use_raster)) {
        self$use_raster <- use_raster
      }
      if (!is.null(coordinates)) {
        self$coordinates <- coordinates
      }
      if (!is.null(template_raster)) {
        if (!("RasterLayer" %in% class(template_raster))) {
          stop("Template raster should be a raster::RasterLayer (or inherited class) object", call. = FALSE)
        }
        template_raster[which(!is.na(template_raster[]))] <- 1:length(which(!is.na(template_raster[])))
        self$region_raster <- template_raster
      }
      if (!is.null(region_raster)) {
        self$region_raster <- region_raster
      }
    },

    # New methods #

    #' @description
    #' Returns a boolean to indicate if a raster is consistent with the region raster (matching extent, resolution, and finite/NA cells).
    #' @param check_raster A \emph{RasterLayer}, \emph{RasterStack} or \emph{RasterBrick} object (see \code{\link[raster:raster-package]{raster}}) to check for consistency with the region raster.
    #' @return Boolean to indicate if the raster is consistent with the region raster.
    raster_is_consistent = function(check_raster) {
      region_raster <- self$region_raster
      if (!is.null(region_raster)) {
        if (any(class(check_raster) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
          region_non_finites <- which(!is.finite(region_raster[]))
          return(raster::compareRaster(check_raster, region_raster, stopiffalse = FALSE) &&
            (all(!is.finite(check_raster[region_non_finites])) || !self$strict_consistency))
        } else {
          return(FALSE)
        }
      } else {
        return(FALSE)
      }
    },

    #' @description
    #' Converts an array (or matrix) of values into a raster (or stack) consistent with the region raster (matching extent, resolution, and finite/NA cells).
    #' @param values An array (or matrix) of values to be placed in the raster (or stack) having dimensions consistent with the region cell number.
    #' @return  A \emph{RasterLayer} (or \emph{RasterStack/Brick}) object consistent with the region raster.
    raster_from_values = function(values) {
      value_matrix <- as.matrix(values)
      if (!self$use_raster) {
        stop("Raster (or stack) can only be generated when the use_raster parameter is TRUE", call. = FALSE)
      }
      if (nrow(value_matrix) != self$region_cells) {
        stop("Values must have a length or dimensions consistent with the number of region (non-NA) cells", call. = FALSE)
      }
      if (ncol(value_matrix) > 1) { # stack/brick
        value_raster <- raster::stack(replicate(ncol(value_matrix), self$region_raster))
      } else {
        value_raster <- self$region_raster
      }
      value_raster[self$region_indices] <- value_matrix
      names(value_raster) <- colnames(value_matrix)
      return(value_raster)
    }
  ), # end public

  private = list(

    ## Attributes ##
    .coordinates = NULL,
    .region_raster = NULL,
    .use_raster = NULL,
    .strict_consistency = TRUE
  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    #' @field coordinates Data frame (or matrix) of X-Y population (WGS84) coordinates in longitude (degrees West) and latitude (degrees North) (get and set), or distance-based coordinates dynamically returned by region raster (get only).
    coordinates = function(value) {
      if (missing(value)) {
        if (self$use_raster && (!is.null(private$.coordinates) || !is.null(private$.region_raster))) {
          as.data.frame(raster::coordinates(self$region_raster)[self$region_indices, ])
        } else {
          private$.coordinates
        }
      } else {
        if (!is.null(value) && !is.null(self$region_raster)) {
          stop("Region is already associated with a raster (and its coordinates)", call. = FALSE)
        }
        if (is.character(value)) {
          if (file.exists(value)) {
            if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
              value <- utils::read.csv(file = value)[, 1:2]
            } else if (length(grep(".RDATA", toupper(value), fixed = TRUE)) || length(grep(".RDS", toupper(value), fixed = TRUE))) {
              value <- readRDS(file = value)
            } else {
              value <- read.table(file = value)[, 1:2]
            }
          } else {
            stop(paste("Could not read coordinates from", value), call. = FALSE)
          }
        }
        if (!is.null(value)) {
          if (length(as.matrix(value)) == 2) { # single cell
            value <- as.list(value)
            self$use_raster <- FALSE
          }
          value <- data.frame(value)
          names(value) <- c("x", "y")
        }
        private$.coordinates <- value
      }
    },

    #' @field region_raster A \emph{RasterLayer} object (see \code{\link[raster:raster-package]{raster}}) defining the region with finite values (NAs elsewhere).
    region_raster = function(value) {
      if (missing(value)) {
        if (is.null(private$.region_raster) && !is.null(private$.coordinates) && self$use_raster) {
          # suppressWarnings(raster::rasterFromXYZ(cbind(private$.coordinates, cell = 1:nrow(private$.coordinates)),
          #                                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) # warnings?
          raster::rasterFromXYZ(cbind(private$.coordinates, cell = 1:nrow(private$.coordinates)),
            crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
          )
        } else {
          private$.region_raster
        }
      } else {
        if (!is.null(value) && !("RasterLayer" %in% class(value))) {
          stop("Region raster should be a raster::RasterLayer (or inherited class) object", call. = FALSE)
        }
        if (!is.null(value) && !is.null(private$.coordinates)) {
          stop("Region is already associated with a set of coordinates", call. = FALSE)
        }
        if (raster::fromDisk(value)) { # move to memory
          raster::values(value) <- raster::values(value)
        }
        private$.region_raster <- value
      }
    },

    #' @field use_raster Boolean to indicate that a raster is to be used to define the region (default TRUE).
    use_raster = function(value) {
      if (missing(value)) {
        private$.use_raster
      } else {
        private$.use_raster <- value
      }
    },

    #' @field strict_consistency Boolean to indicate that, as well as resolution, extent and CRS, consistency checks also ensure that a raster's finite/occupiable cells are the same or a subset of those defined by the region (default TRUE).
    strict_consistency = function(value) {
      if (missing(value)) {
        private$.strict_consistency
      } else {
        private$.strict_consistency <- value
      }
    },

    #' @field region_cells Dynamically calculated number of region coordinates or raster cells with finite/non-NA values.
    region_cells = function(value) {
      if (missing(value)) {
        if (self$use_raster) {
          region_raster <- self$region_raster
          if (!is.null(region_raster)) {
            length(which(!is.na(region_raster[])))
          } else {
            0
          }
        } else { # use coordinates
          if (!is.null(self$coordinates)) {
            nrow(self$coordinates)
          } else {
            0
          }
        }
      } else {
        stop("Cannot set dynamically calculated number of region cells", call. = FALSE)
      }
    },

    #' @field region_indices Dynamically calculated region indices for raster cells with finite/non-NA values (all if not a raster).
    region_indices = function(value) {
      if (missing(value)) {
        if (self$use_raster) {
          region_raster <- self$region_raster
          if (!is.null(region_raster)) {
            region_indices <- which(!is.na(region_raster[]))
            region_indices[order(region_raster[region_indices])]
          } else {
            NA
          }
        } else { # use coordinates
          if (!is.null(self$coordinates)) {
            1:nrow(self$coordinates)
          } else {
            NA
          }
        }
      } else {
        stop("Cannot set dynamically calculated region cell indices", call. = FALSE)
      }
    }
  ) # end active
)
