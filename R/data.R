#' Thylacine vignette Tasmania raster
#'
#' A \emph{raster} dataset defining the grid cells of the Tasmanian study region for the
#' Thylacine example vignette.
#'
#' @format A \emph{raster::RasterLayer} object:
#' \describe{
#'   \item{dimensions}{32 rows by 40 columns grid}
#'   \item{resolution}{0.1 by 0.1 degree grid cells}
#'   \item{extent}{longitude 144.5 to 148.5 degrees; latitude -43.8025 to -40.6025 degrees}
#'   \item{CRS}{WGS84 longitude-latitude}
#'   \item{values}{region defined by 795 cells with value of 1 (surrounded by non-region \code{NA} values)}
#' }
#' @source TBA
"tasmania_raster"

#' Thylacine vignette Tasmania IBRA data
#'
#' A dataset describing the nine Interim Bioregionalisation of Australia (IBRA)
#' bioregions for the Tasmanian study region of the Thylacine example vignette.
#'
#' @format A data frame with 9 rows and 4 variables:
#' \describe{
#'   \item{index}{Cross-reference index for each bioregion}
#'   \item{key}{Additional alphabetical cross-reference for each bioregion}
#'   \item{abbr}{Abbreviated name for each bioregion}
#'   \item{name}{Full name for each bioregion}
#' }
#' @source TBA
"tasmania_ibra_data"

#' Thylacine vignette Tasmania IBRA raster
#'
#' A \emph{raster} dataset defining the grid cells of the nine Interim
#' Bioregionalisation of Australia (IBRA) bioregions for the Tasmanian study region of
#' the Thylacine example vignette.
#'
#' @format A \emph{raster::RasterLayer} object:
#' \describe{
#'   \item{dimensions}{32 rows by 40 columns grid}
#'   \item{resolution}{0.1 by 0.1 degree grid cells}
#'   \item{extent}{longitude 144.5 to 148.5 degrees; latitude -43.8025 to -40.6025 degrees}
#'   \item{CRS}{WGS84 longitude-latitude}
#'   \item{values}{IBRA bioregions defined by cells with values 1 to 9 (as per \code{index} in \code{\link{tasmania_ibra_data}})}
#' }
#' @source TBA
"tasmania_ibra_raster"

#' Thylacine vignette habitat suitability raster
#'
#' A \emph{raster} dataset defining estimated habitat suitability values for each grid
#' cells of the Tasmanian study region of the Thylacine example vignette.
#'
#' @format A \emph{raster::RasterLayer} object:
#' \describe{
#'   \item{dimensions}{32 rows by 40 columns grid}
#'   \item{resolution}{0.1 by 0.1 degree grid cells}
#'   \item{extent}{longitude 144.5 to 148.5 degrees; latitude -43.8025 to -40.6025 degrees}
#'   \item{CRS}{WGS84 longitude-latitude}
#'   \item{values}{Estimated habitat suitability values of 0 to 1}
#' }
#' @source TBA
"thylacine_hs_raster"

#' Thylacine vignette bounty record
#'
#' A dataset containing the historical record of the Thylacine bounty numbers submitted
#' across the Tasmanian study region, and for each of the nine Interim Bioregionalisation
#' of Australia (IBRA) bioregions for Thylacine example vignette.
#'
#' @format A data frame with 22 rows and 11 variables:
#' \describe{
#'   \item{Year}{Year during bounty period from 1888 to 1909}
#'   \item{Total}{Total Tasmania-wide bounty submitted}
#'   \item{FUR}{Bounty submitted in IBRA bioregion: Furneaux}
#'   \item{BEN}{Bounty submitted in IBRA bioregion: Ben Lomond}
#'   \item{TNM}{Bounty submitted in IBRA bioregion: Tasmanian Northern Midlands}
#'   \item{TSE}{Bounty submitted in IBRA bioregion: Tasmanian South East}
#'   \item{TW}{Bounty submitted in IBRA bioregion: Tasmanian West}
#'   \item{TNS}{Bounty submitted in IBRA bioregion: Tasmanian Northern Slopes}
#'   \item{TSR}{Bounty submitted in IBRA bioregion: Tasmanian Southern Ranges}
#'   \item{TCH}{Bounty submitted in IBRA bioregion: Tasmanian Central Highlands}
#'   \item{KIN}{Bounty submitted in IBRA bioregion: King}
#' }
#' @source TBA
"thylacine_bounty_record"

#' Thylacine vignette demonstration example metrics
#'
#' A dataset containing precalculated summary metrics for use when running the Thylacine
#' example vignette in demonstration mode. The values were obtained by running the
#' vignette code for 20,000 model simulations with \code{DEMONSTRATION = FALSE}.
#'
#' @format A data frame with 20,000 rows and 4 variables:
#' \describe{
#'   \item{index}{Example simulation number from 1 to 20,000}
#'   \item{bounty_slope_error}{Root mean squared error (RMSE) from estimated total bounty submitted across three intervals (see vignette)}
#'   \item{ibra_extirpation_error}{RMSE from estimated extirpation date for each IBRA bioregion (see vignette)}
#'   \item{total_extinction}{Total extinction date for each example simulation (\code{NA} when persistent beyond 1967)}
#' }
#' @source Precalculated demonstration via example simulation runs.
"thylacine_example_metrics"

#' Thylacine vignette demonstration example matrices
#'
#' A dataset containing precalculated summary matrices for use when running the
#' Thylacine example vignette in demonstration mode. The values were obtained by running
#' the vignette code for 20,000 model simulations with \code{DEMONSTRATION = FALSE}.
#' Note that some matrices were only stored for the selected 'best' 200 models.
#'
#' @format A list containing the following matrices:
#' \describe{
#'   \item{extirpation}{200 row by 795 column matrix of cell extirpation dates for the 'best' 200 models}
#'   \item{total_bounty}{200 row by 80 column matrix of bounty submitted each year for the 'best' 200 models}
#'   \item{ibra_bounty}{200 row by 9 column matrix of total bounty submitted each IBRA bioregion for the 'best' 200 models}
#'   \item{bounty_slope}{20,000 row by 3 column matrix of calculated slope of total bounty submitted across 3 intervals for each sample simulation}
#'   \item{ibra_extirpation}{20,000 row by 9 column matrix of extirpation dates for each IBRA bioregion for each sample simulation}
#' }
#' @source Precalculated demonstration via example simulation runs.
"thylacine_example_matrices"

#' Thylacine vignette demonstration example (re-run) metrics
#'
#' A dataset containing precalculated (re-run) summary metrics for use when running the
#' Thylacine example vignette in demonstration mode. The values were obtained by running
#' the vignette code for 10 replicate re-runs of the selected 'best' 200 model
#' simulations with \code{DEMONSTRATION = FALSE}.
#'
#' @format A data frame with 2,000 rows and 4 variables:
#' \describe{
#'   \item{index}{Example simulation number from 1 to 2,000}
#'   \item{bounty_slope_error}{Root mean squared error (RMSE) from estimated total bounty submitted across three intervals (see vignette)}
#'   \item{ibra_extirpation_error}{RMSE from estimated extirpation date for each IBRA bioregion (see vignette)}
#'   \item{total_extinction}{Total extinction date for each example simulation (\code{NA} when persistent beyond 1967)}
#' }
#' @source Precalculated demonstration via example simulation re-runs.
"thylacine_example_metrics_rerun"

#' Thylacine vignette demonstration example (re-run) matrices
#'
#' A dataset containing precalculated (re-run) summary matrices for use when running the
#' Thylacine example vignette in demonstration mode. The values were obtained by running
#' the vignette code for 10 replicate re-runs of the selected 'best' 200 model
#' simulations with \code{DEMONSTRATION = FALSE}.
#'
#' @format A list containing the following matrices:
#' \describe{
#'   \item{bounty_slope}{2,000 row by 3 column matrix of calculated slope of total bounty submitted across 3 intervals for each sample simulation}
#'   \item{ibra_extirpation}{2,000 row by 9 column matrix of extirpation dates for each IBRA bioregion for each sample simulation}
#' }
#' @source Precalculated demonstration via example simulation re-runs.
"thylacine_example_matrices_rerun"
