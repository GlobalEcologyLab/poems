#' Thylacine vignette Tasmania raster
#'
#' A \emph{raster} dataset defining the grid cells of the Tasmanian study region
#' for the Thylacine example vignette.
#'
#' @examples
#' data(tasmania_raster)
#' tasmania_region <- Region$new(
#'   template_raster = tasmania_raster
#' )
#' raster::plot(tasmania_region$region_raster)
#'
#' @format A \emph{raster::RasterLayer} object:
#' \describe{
#'   \item{dimensions}{32 rows by 40 columns grid}
#'   \item{resolution}{0.1 by 0.1 degree grid cells}
#'   \item{extent}{longitude 144.5 to 148.5 degrees; latitude -43.8025 to
#'                 -40.6025 degrees}
#'   \item{CRS}{WGS84 longitude-latitude}
#'   \item{values}{region defined by 795 cells with value of 1 (surrounded by
#'                 non-region \code{NA} values)}
#' }
#' @source https://doi.org/10.1111/2041-210X.13720
#' @name tasmania_raster
NULL

#' Tasmania land-use modifier raster
#'
#' A \emph{raster} dataset (11 timesteps) defining the intensity land-use cover
#' for each grid-cell in the Tasmania study region. NB. This dataset is
#' projected and will not natively overlay the other \emph{raster} datasets
#' contained in \emph{poems}.
#'
#' @examples
#' data(tasmania_raster)
#' data(tasmania_modifier)
#' tasmania_region <- Region$new(
#'   template_raster = tasmania_modifier[[1]]
#' )
#' tasmania_region$raster_is_consistent(tasmania_raster)
#' raster::plot(tasmania_modifier)
#'
#' @format A \emph{raster::RasterBrick} object:
#' \describe{
#'   \item{dimensions}{36 rows, 34 columns, 11 layers}
#'   \item{resolution}{10km by 10km grid cells}
#'   \item{extent}{-211571.8, 128428.2, -182583.2, 177416.8 (xmin, xmax, ymin,
#'                 ymax)}
#'   \item{CRS}{+proj=laea +lat_0=-42.2 +lon_0=147 +x_0=0 +y_0=0 +datum=WGS84
#'              +units=m +no_defs}
#'   \item{values}{region defined by 1224 cells with values between 0-1. Values
#'                 of 1 indicate extensive land use modification)}
#' }
#' @source https://doi.org/10.1111/2041-210X.13720
#' @name tasmania_modifier
NULL
