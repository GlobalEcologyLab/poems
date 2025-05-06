#' R6 class representing a dispersal generator.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class functionality for modeling dispersals within a
#' spatially-explicit population model. The model calculates dispersal rates between
#' population model cells using a distance-based function: \emph{p*exp(-distance/b)}
#' for \emph{distance <= r} (otherwise zero), where \emph{p} (proportion), \emph{b}
#' (breadth or average distance) and \emph{r} (range or maximum distance) are
#' configurable model attributes. The dispersal rates are adjusted to limit
#' emigration from each cell to \emph{p}. The model also generates data for
#' constructing compacted dispersal matrices. It dynamically generates attributes
#' defined as \emph{outputs} (default: \emph{dispersal_data}) given sampled
#' \emph{inputs} (default: \emph{dispersal_proportion} and
#' \emph{dispersal_max_distance}). An optional \code{\link{DispersalFriction}} object
#' may be utilized to modify (equivalent) distances given a (spatio-temporal) frictional
#' landscape. When this landscape includes temporal changes, the generated
#' \emph{dispersal_data} will be a temporal list of changing dispersal rates.
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
#'
#' # Distance-based dispersal generator
#' dispersal_gen <- DispersalGenerator$new(
#'   region = region,
#'   dispersal_max_distance = 3000, # in m
#'   inputs = c("dispersal_p", "dispersal_b"),
#'   decimals = 5
#' )
#' dispersal_gen$calculate_distance_data() # pre-calculate
#' dispersal_gen$generate(input_values = list(
#'   dispersal_p = 0.5,
#'   dispersal_b = 700
#' ))
#'
#' @importFrom R6 R6Class
#' @importFrom fossil earth.dist
#' @import raster
#' @include Generator.R
#' @include DispersalTemplate.R
#' @export DispersalGenerator

DispersalGenerator <- R6Class(
  "DispersalGenerator",
  inherit = Generator,
  public = list(
    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericClass, GenericModel, SpatialModel & Generator) #
    #   new_clone(...)
    #   get_attribute_names()
    #   get_attributes(params = NULL)
    #   get_attribute(param)
    #   get_attribute_aliases(params = NULL)
    #   set_attributes(params = list(), ...)
    #   generate(input_values = list())
    #   add_file_template(param, path_template, path_params = c(), file_type = "RDS")
    #   add_function_template(param, func_source, call_template, call_params = c())
    #   add_distribution_template(param, distr_type = c("uniform", "normal", "lognormal", "beta", "triangular"),
    #                             distr_params = list(), sample = NULL, random_seed = NULL)
    #   read_file(param)
    #   run_function(param)
    #   sample_distribution(param)
    #   add_generative_requirements(params = list(), ...)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets the generative template and requirements, optionally the dispersal friction object, as well as any attributes passed via a \emph{params} list or individually.
    #' @param generative_template Optional nested object for generative attributes that need to be maintained when a new clone object is generated for a sample simulation (usually a ).
    #' @param generative_requirements Optional list of attribute names and the template setting (\emph{"file"} or \emph{"function"}) that is required to generate their values (otherwise default functionality is used).
    #' @param dispersal_friction Optional \code{\link{DispersalFriction}} (or inherited class) object for dispersal distance multiplier data.
    #' @param attribute_aliases Optional list of extra alias names for model attributes (form: \code{alias = "attribute"}) to be used with the set and get attributes methods.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(
      generative_template = NULL,
      generative_requirements = NULL,
      dispersal_friction = NULL,
      attribute_aliases = NULL,
      ...
    ) {
      if (is.null(generative_template)) {
        # when new object
        self$generative_template <- DispersalTemplate$new()
        attribute_aliases <- c(
          attribute_aliases, # Append default aliases
          list(
            proportion = "dispersal_proportion",
            dispersal_p = "dispersal_proportion",
            p = "dispersal_proportion",
            breadth = "dispersal_breadth",
            dispersal_b = "dispersal_breadth",
            b = "dispersal_breadth",
            max_distance = "dispersal_max_distance",
            dispersal_r = "dispersal_max_distance",
            r = "dispersal_max_distance"
          )
        )
        param_names <- names(c(list(...), list(...)$params))
        if (!("description" %in% param_names)) {
          self$description <- "dispersal"
        }
        if (!("inputs" %in% param_names)) {
          self$inputs <- c(
            "dispersal_proportion",
            "dispersal_breadth",
            "dispersal_max_distance"
          )
        } else {
          param_list <- list(...)
          if ("inputs" %in% param_list$params) {
            self$inputs <- param_list$params$inputs
          } else {
            self$inputs <- param_list$inputs
          }
        }
        if (!("outputs" %in% param_names)) {
          self$outputs <- c("dispersal_data")
        }
        generative_template <- self$generative_template
      }
      if (is.null(generative_requirements)) {
        generative_requirements <- list(dispersal_data = "default")
      }
      super$initialize(
        generative_template = generative_template,
        generative_requirements = generative_requirements,
        attribute_aliases = attribute_aliases,
        ...
      )
      if (!is.null(dispersal_friction)) {
        self$dispersal_friction <- dispersal_friction
      }
    },

    #' @description
    #' Returns a boolean to indicate that all the default, file and/or function template settings that are required for attribute generation are present.
    #' @return Boolean to indicate that the required settings for attribute generation are present.
    generative_requirements_satisfied = function() {
      satisfied <- super$generative_requirements_satisfied()
      if (is.list(satisfied)) {
        function_params <- c(
          "dispersal_proportion",
          "dispersal_breadth",
          "dispersal_max_distance"
        )
        function_data_required <- (length(self$get_attributes(
          function_params
        )) +
          length(which(
            self$get_attribute_aliases(function_params) %in% self$inputs
          )) <
          3)
        dispersal_matrix_default <- ("dispersal_matrix" %in%
          names(satisfied) &&
          self$generative_requirements$dispersal_matrix == "default")
        if (dispersal_matrix_default) {
          satisfied$dispersal_matrix <- (!is.null(self$coordinates) &&
            !is.null(self$distance_classes) &&
            (!is.null(self$dispersal_function_data) ||
              !function_data_required) &&
            !is.null(self$distance_data))
        }
        dispersal_data_default <- ("dispersal_data" %in%
          names(satisfied) &&
          self$generative_requirements$dispersal_data == "default")
        if (dispersal_data_default) {
          satisfied$dispersal_data <- (!is.null(self$coordinates) &&
            !is.null(self$distance_classes) &&
            (!is.null(self$dispersal_function_data) || !function_data_required))
          satisfied$dispersal_data <- (satisfied$dispersal_data &&
            !is.null(self$distance_data))
        }
        # Add any attributes that are missing (for error message)
        if (dispersal_matrix_default || dispersal_data_default) {
          if (is.null(self$coordinates)) {
            satisfied$coordinates <- FALSE
          }
          if (is.null(self$distance_classes)) {
            satisfied$distance_classes <- FALSE
          }
          if (function_data_required && is.null(self$dispersal_function_data)) {
            satisfied$dispersal_function_data <- FALSE
          }
        }
        if (dispersal_matrix_default || dispersal_data_default) {
          if (is.null(self$distance_data)) {
            satisfied$distance_data <- FALSE
          }
        }
      }
      return(satisfied)
    },

    # New methods #

    #' @description
    #' Sets the distance classes to a sequence of values from minimum to maximum in steps of interval size.
    #' @param minimum Minimum or first distance class sequence value (default = 1).
    #' @param maximum Maximum or last distance class value (default = 10).
    #' @param interval Interval or distance class sequence step size  (default = 1).
    set_distance_classes = function(minimum = 1, maximum = 10, interval = 1) {
      self$distance_classes <- seq(minimum, maximum, interval)
    },

    #' @description
    #' Returns a matrix with the calculated distance (in meters by default) between each pair of region cells.
    #' @param use_longlat Optional boolean indicating use of (WGS84) coordinates in longitude (degrees West) and latitude (degrees North).
    #' @return Matrix with distances between region cells.
    calculate_distance_matrix = function(use_longlat = NULL) {
      if (!is.null(self$region)) {
        coordinates <- self$region$coordinates
        if (is.null(coordinates)) {
          stop(
            "Distance matrix calculation requires the region to be defined with coordinates or a raster first",
            call. = FALSE
          )
        }
        if (
          !self$region$use_raster ||
            (is.logical(use_longlat) && use_longlat) ||
            length(grep(
              "longlat",
              as.character(raster::crs(self$region$region_raster)),
              fixed = TRUE
            )) >
              0
        ) {
          return(
            earth.dist(coordinates, dist = FALSE) * 1000 / self$distance_scale
          )
        } else {
          # assume coordinates in meters
          if (is.na(raster::crs(self$region$region_raster))) {
            warning(
              "No coordinate reference system (CRS) specified: assuming coordinates are in meters",
              call. = FALSE
            )
          }
          return(as.matrix(stats::dist(coordinates)) / self$distance_scale)
        }
      } else {
        stop(
          "Distance matrix calculation requires region/coordinates to be set first",
          call. = FALSE
        )
      }
    },

    #' @description
    #' Calculates the distance class for within-range populations using the set/provided distance classes. Also calculates indices for constructing compact matrices.
    #' @param distance_matrix Optional pre-calculated matrix with distances between population cells (population rows by population columns).
    #' @param ... Parameters passed via a \emph{params} list or individually.
    calculate_distance_data = function(distance_matrix = NULL, ...) {
      # Set attributes
      self$distance_data <- NULL
      if (length(list(...))) {
        self$set_attributes(...)
      }

      # Ensure coordinates and distance classes are set
      if (
        is.null(self$coordinates) ||
          is.null(self$region) ||
          self$region$region_cells == 0 ||
          is.null(self$distance_classes)
      ) {
        stop(
          "Distance data calculation requires region/coordinates and distance classes to be set first",
          call. = FALSE
        )
      }

      # Ensure pre-calculated distance matrix is consistent with coordinates
      if (!is.null(distance_matrix) && self$region$region_cells > 0) {
        if (
          !is.matrix(distance_matrix) ||
            nrow(distance_matrix) != self$region$region_cells ||
            ncol(distance_matrix) != self$region$region_cells
        ) {
          stop(
            "Distance matrix dimensions must be consistent with region/coordinates",
            call. = FALSE
          )
        }
      }

      # Calculate distance matrix: populations by populations matrix of pairwise distances between grid centroids (km)
      if (is.null(distance_matrix)) {
        distance_matrix <- self$calculate_distance_matrix()
      }

      # Calculate the indices of distances within the maximum dispersal range
      distance_data <- which(
        distance_matrix > 0 & distance_matrix <= max(self$distance_classes),
        arr.ind = TRUE
      )
      distance_data <- distance_data[
        order(distance_data[, 2], distance_data[, 1]),
      ]
      colnames(distance_data) <- c("target_pop", "source_pop")
      distances_within_range <- distance_matrix[distance_data]
      distance_matrix <- NULL # release from memory

      # Ensure distance data is present
      if (length(distance_data) == 0) {
        stop(
          "No distance data was generated with the current distance classes",
          call. = FALSE
        )
      }

      # Calculate dispersal friction object distance multipliers for the within range indices
      if (!is.null(self$dispersal_friction)) {
        distance_multipliers <- self$dispersal_friction$calculate_distance_multipliers(
          distance_data
        )
        if (length(distance_multipliers) == 1) {
          # apply multipliers and update distances
          if (!is.null(self$dispersal_friction$write_to_dir)) {
            distances_within_range <- distances_within_range *
              readRDS(distance_multipliers[[1]])
          } else {
            distances_within_range <- distances_within_range *
              distance_multipliers[[1]]
          }
          out_of_range <- which(
            distances_within_range > max(self$distance_classes)
          )
          distances_within_range <- distances_within_range[-out_of_range]
          distance_data <- distance_data[-out_of_range, ]
        }
      }

      # Calculate and append indices for constructing compact distance matrices
      distance_data <- data.frame(distance_data)
      populations <- self$region$region_cells
      dispersal_rows <- tabulate(distance_data$source_pop, nbins = populations)
      compact_rows <- max(dispersal_rows)
      compact_matrix <- array(1:compact_rows, c(compact_rows, populations))
      compact_matrix <- compact_matrix *
        (compact_matrix <=
          matrix(
            dispersal_rows,
            nrow = compact_rows,
            ncol = populations,
            byrow = TRUE
          ))
      # Map the row of each compact matrix to the original target population
      distance_data$compact_row <- which(
        compact_matrix > 0,
        arr.ind = TRUE,
        useNames = FALSE
      )[, 1]

      # Calculate base (no friction) distance classes for distances within the maximum dispersal range
      base_distance_classes <- as.numeric(cut(
        distances_within_range,
        breaks = c(0, self$distance_classes)
      ))
      self$distance_data <- list(
        base = data.frame(distance_data, distance_class = base_distance_classes)
      )

      # Calculate the sequential changes in distance class using dispersal friction object distance multipliers
      if (
        !is.null(self$dispersal_friction) && length(distance_multipliers) > 1
      ) {
        current_distance_classes <- base_distance_classes
        sequential_distance_data <- list()
        for (i in 1:length(distance_multipliers)) {
          previous_distance_classes <- current_distance_classes
          if (!is.null(self$dispersal_friction$write_to_dir)) {
            current_distance_classes <- as.numeric(cut(
              distances_within_range * readRDS(distance_multipliers[[i]]),
              breaks = c(0, self$distance_classes, Inf)
            ))
          } else {
            current_distance_classes <- as.numeric(cut(
              distances_within_range * distance_multipliers[[i]],
              breaks = c(0, self$distance_classes, Inf)
            ))
          }
          changed_indices <- which(
            current_distance_classes != previous_distance_classes
          )
          sequential_distance_data[[i]] <- data.frame(
            distance_data[changed_indices, ],
            distance_class = current_distance_classes[changed_indices]
          )
        }
        distance_multipliers <- NULL # release from memory
        self$distance_data$changes <- sequential_distance_data
      }
    },

    #' @description
    #' Calculates, using the conditional dispersal limiting function for a simulation sample, a dispersal matrix, or a list of data frames of non-zero dispersal rates and indices for constructing a compact dispersal matrix (default), and optional changing rates over time (via \code{\link{DispersalFriction}} object).
    #' @param type Optional type selector (\emph{"data"} or \emph{"matrix"}) to determine whether to calculate a dispersal matrix or data frame (default).
    #' @return Returns character string message when calculation prerequisites are not met (for simulation logging).
    calculate_dispersals = function(type = "data") {
      # Ensure distance data are calculated
      if (is.null(self$distance_data)) {
        return(
          "Dispersal distance data needs to be calculated before dispersals can be generated"
        )
      }

      # Calculate dispersals using distance data and sampled dispersal function parameters
      if (
        !is.null(self$dispersal_proportion) &&
          !is.null(self$dispersal_breadth) &&
          !is.null(self$dispersal_max_distance)
      ) {
        # Calculate dispersal rates for each distance class (discrete values)
        dispersal_rate_classes <- c(
          ifelse(
            self$distance_classes <= self$dispersal_max_distance,
            self$dispersal_proportion *
              exp(-1 * self$distance_classes / self$dispersal_breadth),
            0
          ),
          0
        )

        # Select base (non-friction) data for non-zero dispersal classes
        nonzero_base_data <- self$distance_data$base[
          which(
            self$distance_data$base$distance_class <=
              length(which(dispersal_rate_classes > 0))
          ),
        ]

        # Calculate a compact matrix of dispersals for the base (non-friction) data (original compact indices)
        base_compact_rows <- max(self$distance_data$base$compact_row)
        populations <- self$region$region_cells
        compact_matrix <- array(0, c(base_compact_rows, populations))
        compact_dispersal_indices <- as.matrix(nonzero_base_data[, c(
          "compact_row",
          "source_pop"
        )])
        compact_matrix[compact_dispersal_indices] <- dispersal_rate_classes[
          nonzero_base_data$distance_class
        ]

        # Calculate multipliers to set the total proportion migrating from each cell (without friction) to <= p
        multipliers <- self$dispersal_proportion /
          .colSums(compact_matrix, m = base_compact_rows, n = populations)
        multipliers[which(!is.finite(multipliers) | multipliers > 1)] <- 1

        # Apply multipliers to the base compact dispersal matrix
        compact_matrix <- compact_matrix *
          matrix(
            multipliers,
            nrow = base_compact_rows,
            ncol = populations,
            byrow = TRUE
          )

        # Extract dispersal rates and round when required (then update non-zero base/non-friction dispersal data)
        nonzero_base_data$dispersal_rate <- compact_matrix[
          compact_dispersal_indices
        ]
        if (!is.null(self$decimals)) {
          nonzero_base_data$dispersal_rate <- round(
            nonzero_base_data$dispersal_rate,
            self$decimals
          )
          nonzero_base_data <- nonzero_base_data[
            which(nonzero_base_data$dispersal_rate > 0),
          ]
          compact_dispersal_indices <- as.matrix(nonzero_base_data[, c(
            "compact_row",
            "source_pop"
          )])
        }

        # Calculate indices for constructing further compacted dispersal matrices for emigrants and immigrants
        dispersal_rows <- tabulate(
          nonzero_base_data$source_pop,
          nbins = populations
        )
        dispersal_cols <- tabulate(
          nonzero_base_data$target_pop,
          nbins = populations
        )
        nonzero_compact_rows <- max(dispersal_rows, dispersal_cols)
        if (nonzero_compact_rows) {
          compact_emigrant_matrix <- array(
            1:nonzero_compact_rows,
            c(nonzero_compact_rows, populations)
          )
          compact_immigrant_matrix <- compact_emigrant_matrix *
            (compact_emigrant_matrix <=
              matrix(
                dispersal_cols,
                nrow = nonzero_compact_rows,
                ncol = populations,
                byrow = TRUE
              ))
          compact_emigrant_matrix <- compact_emigrant_matrix *
            (compact_emigrant_matrix <=
              matrix(
                dispersal_rows,
                nrow = nonzero_compact_rows,
                ncol = populations,
                byrow = TRUE
              ))
          # Map the row of each compact matrix to the original target (for emigrants) or source (for immigrants) populations
          nonzero_base_data$emigrant_row <- which(
            compact_emigrant_matrix > 0,
            arr.ind = TRUE,
            useNames = FALSE
          )[, 1]
          nonzero_base_data$immigrant_row <- which(
            compact_immigrant_matrix > 0,
            arr.ind = TRUE,
            useNames = FALSE
          )[, 1]
          target_sorted_indices <- nonzero_base_data[
            order(nonzero_base_data$target_pop, nonzero_base_data$source_pop),
            c("target_pop", "source_pop")
          ]
          nonzero_base_data$immigrant_row <- nonzero_base_data$immigrant_row[order(
            target_sorted_indices$source_pop,
            target_sorted_indices$target_pop
          )]
        } else {
          nonzero_base_data$emigrant_row <- numeric(0)
          nonzero_base_data$immigrant_row <- numeric(0)
        }

        # Calculate the sequential changes in dispersals when dispersal friction object is present
        if (
          !is.null(self$dispersal_friction) &&
            !is.null(self$distance_data$changes) &&
            nonzero_compact_rows
        ) {
          # Calculate the initial dispersal data by applying the first (friction) distance changes to the (compact) base data
          compact_matrix[as.matrix(self$distance_data$changes[[1]][, c(
            "compact_row",
            "source_pop"
          )])] <-
            dispersal_rate_classes[
              self$distance_data$changes[[1]]$distance_class
            ] *
            multipliers[self$distance_data$changes[[1]]$source_pop]

          # Construct the dispersal data from the base (non-friction) data for the non-zero base indices (ensures all indices present for changes)
          self$dispersal_data <- list(data.frame(
            nonzero_base_data[, c(
              "target_pop",
              "source_pop",
              "emigrant_row",
              "immigrant_row"
            )],
            dispersal_rate = compact_matrix[compact_dispersal_indices]
          ))

          # Round when required
          if (!is.null(self$decimals)) {
            self$dispersal_data[[1]]$dispersal_rate <- round(
              self$dispersal_data[[1]]$dispersal_rate,
              self$decimals
            )
            compact_matrix <- compact_matrix * 0 > 0 # all FALSE
            compact_matrix[compact_dispersal_indices] <- TRUE # used to resolve decimal rounding in changes
          } else {
            compact_matrix <- NULL # release from memory
          }

          if (type == "data") {
            # Map the original distance classes and the new emigrant and immigrant row indices via compact matrices
            original_distance_class_map <- emigrant_row_map <- immigrant_row_map <- array(
              NA,
              c(base_compact_rows, populations)
            )
            original_distance_class_map[as.matrix(self$distance_data$base[, c(
              "compact_row",
              "source_pop"
            )])] <- self$distance_data$base$distance_class
            emigrant_row_map[as.matrix(nonzero_base_data[, c(
              "compact_row",
              "source_pop"
            )])] <- nonzero_base_data$emigrant_row
            immigrant_row_map[as.matrix(nonzero_base_data[, c(
              "compact_row",
              "source_pop"
            )])] <- nonzero_base_data$immigrant_row

            # Calculate subsequent changes in dispersals
            for (i in 2:length(self$distance_data$changes)) {
              # Select data for non-zero dispersal classes
              original_distance_classes <- original_distance_class_map[as.matrix(self$distance_data$changes[[
                i
              ]][, c("compact_row", "source_pop")])]
              nonzero_dispersal_indices <- which(
                original_distance_classes <=
                  length(which(dispersal_rate_classes > 0))
              )
              nonzero_change_data <- self$distance_data$changes[[i]][
                nonzero_dispersal_indices,
              ]

              # Select rows that don't round to zero in the base data
              if (!is.null(self$decimals)) {
                nonzero_change_data <- nonzero_change_data[
                  which(compact_matrix[as.matrix(nonzero_change_data[, c(
                    "compact_row",
                    "source_pop"
                  )])]),
                ]
              }

              # Calculate/construct dispersal rates using class and multiplier (based on non-friction data)
              self$dispersal_data[[i]] <- data.frame(
                nonzero_change_data[, c("target_pop", "source_pop")],
                emigrant_row = emigrant_row_map[as.matrix(nonzero_change_data[, c(
                  "compact_row",
                  "source_pop"
                )])],
                immigrant_row = immigrant_row_map[as.matrix(nonzero_change_data[, c(
                  "compact_row",
                  "source_pop"
                )])],
                dispersal_rate = dispersal_rate_classes[
                  nonzero_change_data$distance_class
                ] *
                  multipliers[nonzero_change_data$source_pop]
              )
              # Round when required
              if (
                !is.null(self$decimals) && length(self$dispersal_data[[i]]) > 0
              ) {
                self$dispersal_data[[i]]$dispersal_rate <- round(
                  self$dispersal_data[[i]]$dispersal_rate,
                  self$decimals
                )
              }
            }
          } else {
            # matrix
            self$dispersal_matrix <- array(0, c(populations, populations))
            self$dispersal_matrix[as.matrix(self$dispersal_data[[1]][, c(
              "target_pop",
              "source_pop"
            )])] <- self$dispersal_data[[1]]$dispersal_rate
          }
        } else {
          # no dispersal friction object

          # Set dispersals from non-zero dispersal data
          if (type == "matrix") {
            self$dispersal_matrix <- array(0, c(populations, populations))
            self$dispersal_matrix[as.matrix(nonzero_base_data[, c(
              "target_pop",
              "source_pop"
            )])] <- nonzero_base_data$dispersal_rate
          } else {
            self$dispersal_data <- list(nonzero_base_data[, c(
              "target_pop",
              "source_pop",
              "emigrant_row",
              "immigrant_row",
              "dispersal_rate"
            )])
          }
        }
      } else {
        return(
          "Dispersal calculation requires sample parameter settings for proportion, breadth & maximum distance (look-up data may be missing)"
        )
      }
    }
  ), # end public

  private = list(
    ## Attributes ##

    # Model attributes #
    .model_attributes = c(
      "region",
      "coordinates",
      "dispersal_proportion",
      "dispersal_breadth",
      "dispersal_max_distance",
      "dispersal_index",
      "dispersal_matrix",
      "dispersal_data"
    ),
    .region = NULL,
    .dispersal_proportion = NULL,
    .dispersal_breadth = NULL,
    .dispersal_max_distance = NULL,
    .dispersal_index = NULL,
    .dispersal_matrix = NULL,
    .dispersal_data = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c(
      "region",
      "coordinates",
      "description",
      "inputs",
      "outputs",
      "file_templates",
      "function_templates",
      "distribution_templates",
      "uses_correlations",
      "spatial_correlation",
      "temporal_correlation",
      "time_steps",
      "decimals",
      "occupancy_mask",
      "template_attached",
      "dispersal_friction",
      "distance_classes",
      "distance_scale",
      "distance_data",
      "dispersal_function_data",
      "dispersal_proportion",
      "dispersal_breadth",
      "dispersal_max_distance",
      "dispersal_index",
      "dispersal_matrix",
      "dispersal_data"
    )

    # Dynamic attributes #
    # .attribute_aliases       [inherited]

    # Generative attributes #
    # .generative_template     [inherited]
    # .generative_requirements [inherited]

    # Errors and warnings #
    # .error_messages          [inherited]
    # .warning_messages        [inherited]
  ), # end private

  # Active binding accessors for private attributes (above and template nested) #
  active = list(
    # Model attributes accessors #

    #' @field model_attributes A vector of model attribute names.
    model_attributes = function(value) {
      # inherited
      if (missing(value)) {
        super$model_attributes
      } else {
        super$model_attributes <- value
      }
    },

    #' @field region A \code{\link{Region}} (or inherited class) object specifying the study region.
    region = function(value) {
      # inherited
      if (missing(value)) {
        super$region
      } else {
        super$region <- value
      }
    },

    #' @field coordinates Data frame (or matrix) of X-Y population (WGS84) coordinates in longitude (degrees West) and latitude (degrees North) (get and set), or distance-based coordinates dynamically returned by region raster (get only).
    coordinates = function(value) {
      # inherited
      if (missing(value)) {
        super$coordinates
      } else {
        super$coordinates <- value
      }
    },

    # Template-nested model attribute accessors #

    #' @field description A brief description of what the generator generates.
    description = function(value) {
      # inherited
      if (missing(value)) {
        super$description
      } else {
        super$description <- value
      }
    },

    #' @field inputs An array of input attribute names for the generator.
    inputs = function(value) {
      # inherited
      if (missing(value)) {
        super$inputs
      } else {
        super$inputs <- value
      }
    },

    #' @field outputs An array of output attribute names for the generator.
    outputs = function(value) {
      # inherited
      if (missing(value)) {
        super$outputs
      } else {
        super$outputs <- value
      }
    },

    #' @field file_templates A nested list of file template attributes.
    file_templates = function(value) {
      # inherited
      if (missing(value)) {
        super$file_templates
      } else {
        super$file_templates <- value
      }
    },

    #' @field function_templates A nested list of function template attributes.
    function_templates = function(value) {
      # inherited
      if (missing(value)) {
        super$function_templates
      } else {
        super$function_templates <- value
      }
    },

    #' @field distribution_templates A list of distribution template attributes.
    distribution_templates = function(value) {
      # inherited
      if (missing(value)) {
        super$distribution_templates
      } else {
        super$distribution_templates <- value
      }
    },

    #' @field uses_correlations A boolean to indicate that a \code{\link{SpatialCorrelation}} (or inherited class) object is used for generating correlated random deviates.
    uses_correlations = function(value) {
      # inherited
      if (missing(value)) {
        super$uses_correlations
      } else {
        super$uses_correlations <- value
      }
    },

    #' @field spatial_correlation A \code{\link{SpatialCorrelation}} (or inherited class) object for generating correlated random deviates.
    spatial_correlation = function(value) {
      # inherited
      if (missing(value)) {
        super$spatial_correlation
      } else {
        super$spatial_correlation <- value
      }
    },

    #' @field temporal_correlation Absolute correlation coefficient between simulation time steps for all grid cells (0-1; default = 1).
    temporal_correlation = function(value) {
      # inherited
      if (missing(value)) {
        super$temporal_correlation
      } else {
        super$temporal_correlation <- value
      }
    },

    #' @field time_steps Number of simulation time steps.
    time_steps = function(value) {
      # inherited
      if (missing(value)) {
        super$time_steps
      } else {
        super$time_steps <- value
      }
    },

    #' @field decimals Number of decimal places applied to generated data outputs (default: NULL = no rounding).
    decimals = function(value) {
      # inherited
      if (missing(value)) {
        super$decimals
      } else {
        super$decimals <- value
      }
    },

    #' @field occupancy_mask Optional binary mask array (matrix), data frame, or raster (stack) for generated (time-series) data outputs.
    occupancy_mask = function(value) {
      # inherited
      if (missing(value)) {
        super$occupancy_mask
      } else {
        super$occupancy_mask <- value
      }
    },

    #' @field template_attached A list of template-nested dynamically attached model attributes that are maintained via shallow or \emph{new} cloning.
    template_attached = function(value) {
      # inherited
      if (missing(value)) {
        super$template_attached
      } else {
        super$template_attached <- value
      }
    },

    #' @field dispersal_friction A \code{\link{DispersalFriction}} (or inherited class) object for dispersal distance multiplier data.
    dispersal_friction = function(value) {
      if (missing(value)) {
        self$generative_template$dispersal_friction
      } else {
        if (!is.null(value) && !("DispersalFriction" %in% class(value))) {
          stop(
            "Dispersal friction must be a DispersalFriction or inherited class object",
            call. = FALSE
          )
        } else if (!is.null(value)) {
          # Protect consistency of existing distance data associated with an existing dispersal friction object
          if (
            !is.null(self$distance_data) && !is.null(self$dispersal_friction)
          ) {
            stop(
              "Dispersal generator distance data is already associated with the existing dispersal friction object",
              call. = FALSE
            )
            # Check region/coordinates consistency
          } else if (
            !is.null(value$region) &&
              !is.null(self$region) &&
              (value$region$region_cells != self$region$region_cells ||
                !all(value$coordinates == self$coordinates) ||
                (self$region$use_raster &&
                  !self$region$raster_is_consistent(
                    value$region$region_raster
                  )))
          ) {
            stop(
              "Dispersal friction object is inconsistent with the dispersal generator region/coordinates",
              call. = FALSE
            )
          } else if (
            is.null(value$region) &&
              !is.null(self$region) &&
              !is.null(value$conductance) &&
              any(
                class(value$conductance) %in%
                  c("RasterLayer", "RasterStack", "RasterBrick")
              ) &&
              (self$region$use_raster &&
                !self$region$raster_is_consistent(value$conductance) ||
                !self$region$use_raster)
          ) {
            stop(
              "Conductance raster is inconsistent with the dispersal generator region",
              call. = FALSE
            )
          } else if (
            is.null(value$region) &&
              !is.null(self$region) &&
              !is.null(value$conductance) &&
              is.matrix(value$conductance) &&
              nrow(value$conductance) != self$region$region_cells
          ) {
            stop(
              "Conductance matrix dimensions are inconsistent with the dispersal generator region/coordinates ",
              call. = FALSE
            )
          } else if (!is.null(self$distance_data)) {
            # Existing distance data will lack temporal changes
            warning(
              "Dispersal generator distance data will need to be re-calculated with the dispersal friction object",
              call. = FALSE
            )
            self$distance_data <- NULL
          }
          # Copy region/coordinates appropriately
          if (is.null(value$region) && !is.null(self$region)) {
            value$region <- self$region
          } else if (!is.null(value$region) && is.null(self$region)) {
            self$region <- value$region
          }
          self$generative_template$dispersal_friction <- value
        } else {
          # set model (to NULL)
          self$generative_template$dispersal_friction <- value
          if (
            !is.null(self$distance_data) &&
              "changes" %in% names(self$distance_data)
          ) {
            self$distance_data$changes <- NULL
          }
        }
      }
    },

    #' @field distance_classes Vector of distance interval boundaries for calculating discrete dispersal rates.
    distance_classes = function(value) {
      if (missing(value)) {
        if (
          is.null(self$generative_template$distance_classes) &&
            is.numeric(self$dispersal_max_distance)
        ) {
          # Use max distance to generate classes (up to 1000|max_distance_classes)
          seq_step <- max(
            trunc(self$dispersal_max_distance / self$max_distance_classes),
            1
          )
          self$generative_template$distance_classes <- seq(
            seq_step,
            self$dispersal_max_distance,
            seq_step
          )
        }
        self$generative_template$distance_classes
      } else {
        if (!is.null(self$distance_data)) {
          stop(
            "Dispersal generator distance data is already associated with the existing distance classes",
            call. = FALSE
          )
        } else {
          self$generative_template$distance_classes <- value
        }
      }
    },

    #' @field max_distance_classes The maximum number of distance classes when they are calculated automatically via the maximum distance (default: 1000).
    max_distance_classes = function(value) {
      if (missing(value)) {
        self$generative_template$max_distance_classes
      } else {
        self$generative_template$max_distance_classes <- value
      }
    },

    #' @field distance_scale Scale of distance values in meters (default = 1). Usage: set to 1 for values in meters, or to 1000 for values in kilometers.
    distance_scale = function(value) {
      if (missing(value)) {
        self$generative_template$distance_scale
      } else {
        self$generative_template$distance_scale <- value
      }
    },

    #' @field distance_data Data frame of distance classes including indices for the construction of compact matrices (columns: target_pop, source_pop, compact_row, distance_class).
    distance_data = function(value) {
      if (missing(value)) {
        self$generative_template$distance_data
      } else {
        self$generative_template$distance_data <- value
      }
    },

    #' @field dispersal_function_data Data frame of discrete dispersal function
    #' values. Optional first column may provide distance intervals (non-inclusive lower bounds).
    dispersal_function_data = function(value) {
      if (missing(value)) {
        self$generative_template$dispersal_function_data
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            self$generative_template$dispersal_function_data <- utils::read.csv(
              file = value
            )
          } else if (
            length(grep(".RDATA", toupper(value), fixed = TRUE)) ||
              length(grep(".RDS", toupper(value), fixed = TRUE))
          ) {
            self$generative_template$dispersal_function_data <- readRDS(
              file = value
            )
          } else {
            self$generative_template$dispersal_function_data <- utils::read.table(
              file = value
            )
          }
        } else {
          if (!is.null(value)) {
            self$generative_template$dispersal_function_data <- as.data.frame(
              value
            )
          } else {
            self$generative_template$dispersal_function_data <- value
          }
        }
      }
    },

    # Local and nested model attribute accessors #

    #' @field dispersal_proportion Dispersal function:
    #' \emph{p*exp(-distance/b)} \emph{p} parameter. Represents the proportion
    #' and limit of dispersers between model cells. This represents a maximum
    #' potential proportion of dispersers; other factors such as population
    #' density and carrying capacity may limit the actual proportion of
    #' dispersers.
    dispersal_proportion = function(value) {
      if (missing(value)) {
        if (
          any(
            self$get_attribute_aliases("dispersal_proportion") %in% self$inputs
          )
        ) {
          private$.dispersal_proportion
        } else {
          self$generative_template$dispersal_proportion
        }
      } else {
        if (
          any(
            self$get_attribute_aliases("dispersal_proportion") %in% self$inputs
          )
        ) {
          private$.dispersal_proportion <- value
        } else {
          self$generative_template$dispersal_proportion <- value
        }
      }
    },

    #' @field dispersal_breadth Dispersal function: \emph{p*exp(-distance/b)} \emph{b} parameter. Represents the breadth of the dispersal between model cells. Typically estimated via average migration distance.
    dispersal_breadth = function(value) {
      if (missing(value)) {
        if (
          !is.null(self$dispersal_function_data) &&
            !is.null(self$dispersal_index) &&
            any(
              self$get_attribute_aliases("dispersal_breadth") %in%
                names(self$dispersal_function_data)
            )
        ) {
          # Use function look-up data
          function_data_column <- which(
            names(self$dispersal_function_data) %in%
              self$get_attribute_aliases("dispersal_breadth")
          )
          self$dispersal_function_data[[function_data_column]][
            self$dispersal_index
          ]
        } else {
          if (
            any(
              self$get_attribute_aliases("dispersal_breadth") %in% self$inputs
            )
          ) {
            private$.dispersal_breadth
          } else {
            self$generative_template$dispersal_breadth
          }
        }
      } else {
        if (
          any(self$get_attribute_aliases("dispersal_breadth") %in% self$inputs)
        ) {
          private$.dispersal_breadth <- value
        } else {
          self$generative_template$dispersal_breadth <- value
        }
      }
    },

    #' @field dispersal_max_distance Dispersal maximum distance or range (\emph{r}) parameter limits the use of the dispersal function: \emph{p*exp(-distance/b)}. The function is utilized when \emph{distance <= r} otherwise the dispersal rate is set to zero.
    dispersal_max_distance = function(value) {
      if (missing(value)) {
        if (
          !is.null(self$dispersal_function_data) &&
            !is.null(self$dispersal_index) &&
            any(
              names(self$dispersal_function_data) %in%
                self$get_attribute_aliases("dispersal_max_distance")
            )
        ) {
          # Use function look-up data
          function_data_column <- which(
            names(self$dispersal_function_data) %in%
              self$get_attribute_aliases("dispersal_max_distance")
          )
          self$dispersal_function_data[[function_data_column]][
            self$dispersal_index
          ]
        } else if (
          is.null(private$.dispersal_max_distance) &&
            is.null(self$generative_template$dispersal_max_distance) &&
            is.numeric(self$distance_classes)
        ) {
          max(self$distance_classes)
        } else {
          if (
            any(
              self$get_attribute_aliases("dispersal_max_distance") %in%
                self$inputs
            )
          ) {
            private$.dispersal_max_distance
          } else {
            self$generative_template$dispersal_max_distance
          }
        }
      } else {
        if (
          any(
            self$get_attribute_aliases("dispersal_max_distance") %in%
              self$inputs
          )
        ) {
          private$.dispersal_max_distance <- value
        } else {
          self$generative_template$dispersal_max_distance <- value
        }
        if (
          !is.null(self$dispersal_function_data) &&
            is.null(self$dispersal_index) &&
            is.numeric(value)
        ) {
          # Use maximum distance to derive function data index
          if (
            !(names(self$dispersal_function_data)[1] %in%
              self$get_attribute_aliases(c(
                "dispersal_proportion",
                "dispersal_breadth",
                "dispersal_max_distance"
              )))
          ) {
            # Assume first column provides distance intervals (non-inclusive lower bounds)
            dispersal_index <- as.numeric(cut(
              value,
              breaks = c(self$dispersal_function_data[, 1], Inf)
            ))
            if (is.finite(dispersal_index))
              self$dispersal_index <- dispersal_index
          } else if (
            any(
              names(self$dispersal_function_data) %in%
                self$get_attribute_aliases("dispersal_max_distance")
            )
          ) {
            # Use max_distance column as distance intervals (non-inclusive lower bounds)
            function_data_column <- which(
              names(self$dispersal_function_data) %in%
                self$get_attribute_aliases("dispersal_max_distance")
            )
            dispersal_index <- as.numeric(cut(
              value,
              breaks = c(
                self$dispersal_function_data[[function_data_column]],
                Inf
              )
            ))
            if (is.finite(dispersal_index))
              self$dispersal_index <- dispersal_index
          }
        }
      }
    },

    # Local (non-nested) model attribute accessors #

    #' @field dispersal_index Sampled index for the dispersal function data frame (to look-up dispersal function parameters).
    dispersal_index = function(value) {
      if (missing(value)) {
        private$.dispersal_index
      } else {
        private$.dispersal_index <- value
        if (is.numeric(value) && !is.null(self$dispersal_function_data)) {
          parameter_values <- as.list(self$dispersal_function_data[value, ])
          self$set_attributes(parameter_values)
        }
      }
    },

    #' @field dispersal_matrix Dispersal matrix calculated via dispersal function.
    dispersal_matrix = function(value) {
      if (missing(value)) {
        if (
          is.null(private$.dispersal_matrix) &&
            "dispersal_matrix" %in% names(self$generative_requirements)
        ) {
          template_type <- self$generative_requirements[["dispersal_matrix"]]
          if (template_type == "file") {
            private$.dispersal_matrix <- self$read_file("dispersal_matrix")
          } else if (template_type == "function") {
            private$.dispersal_matrix <- self$run_function("dispersal_matrix")
          } else if (template_type == "default") {
            # use internal function
            message <- self$calculate_dispersals(type = "matrix")
            if (is.character(message)) {
              self$error_messages <- message
            }
          }
        }
        private$.dispersal_matrix
      } else {
        private$.dispersal_matrix <- value
      }
    },

    #' @field dispersal_data Data frame of non-zero dispersal rates including indices for the construction of compact matrices (columns: target_pop, source_pop, emigrant_row, immigrant_row, dispersal_rate).
    dispersal_data = function(value) {
      if (missing(value)) {
        if (
          is.null(private$.dispersal_data) &&
            "dispersal_data" %in% names(self$generative_requirements)
        ) {
          template_type <- self$generative_requirements[["dispersal_data"]]
          if (template_type == "file") {
            private$.dispersal_data <- self$read_file("dispersal_data")
          } else if (template_type == "function") {
            private$.dispersal_data <- self$run_function("dispersal_data")
          } else if (template_type == "default") {
            # use internal function
            message <- self$calculate_dispersals(type = "data")
            if (is.character(message)) {
              self$error_messages <- message
            }
          }
        }
        private$.dispersal_data
      } else {
        private$.dispersal_data <- value
      }
    },

    # Dynamic attribute accessors #

    #' @field attribute_aliases A list of alternative alias names for model attributes (form: \code{alias = "attribute"}) to be used with the set and get attributes methods.
    attribute_aliases = function(value) {
      # inherited
      if (missing(value)) {
        super$attribute_aliases
      } else {
        super$attribute_aliases <- value
      }
    },

    # Generative attribute accessors #

    #' @field generative_template A nested \code{\link{DispersalTemplate}} (or inherited class) object for model attributes that are maintained via shallow or \emph{new} cloning.
    generative_template = function(value) {
      # inherited
      if (missing(value)) {
        super$generative_template
      } else {
        super$generative_template <- value
      }
    },

    #' @field generative_requirements A list of attribute names and the template setting (\emph{"file"}, \emph{"function"}, or \emph{"default"}) that is required to generate their values.
    generative_requirements = function(value) {
      # inherited
      if (missing(value)) {
        super$generative_requirements
      } else {
        super$generative_requirements <- value
      }
    },

    # Errors and warnings accessors #

    #' @field error_messages A vector of error messages encountered when setting model attributes.
    error_messages = function(value) {
      # inherited
      if (missing(value)) {
        super$error_messages
      } else {
        super$error_messages <- value
      }
    },

    #' @field warning_messages A vector of warning messages encountered when setting model attributes.
    warning_messages = function(value) {
      # inherited
      if (missing(value)) {
        super$warning_messages
      } else {
        super$warning_messages <- value
      }
    }
  ) # end active
)
