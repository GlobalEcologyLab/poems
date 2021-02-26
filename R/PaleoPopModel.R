#' R6 class representing a population model for the paleopop simulator
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class representing a spatially-explicit
#' demographic-based population model. It extends the \code{\link{SimulationModel}}
#' class with parameters for the \code{\link{paleopop_simulator}}. It inherits
#' functionality for creating a nested model, whereby a nested template model with
#' fixed parameters is maintained when a model is cloned for various sampled parameters.
#' Also provided are extensions to the methods for checking the consistency and
#' completeness of model parameters.
#'
#' @importFrom R6 R6Class
#' @include SimulationModel.R
#' @include Region.R
#' @export PaleoPopModel

PaleoPopModel <- R6Class("PaleoPopModel",
  inherit = SimulationModel,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericClass, GenericModel, SpatialModel, SimulationModel & PopulationModel) #
    #   initialize(template = NULL, required_attributes = NULL, ...)
    #   new_clone(...)
    #   get_attribute_names()
    #   get_attributes(params = list(), ...)
    #   get_attribute(param)
    #   get_attribute_aliases(params = NULL)
    #   set_attributes(params = list(), ...)
    #   set_sample_attributes(params = list(), ...)
    #   is_consistent(params = NULL)
    #   inconsistent_attributes(include_nas = FALSE)
    #   is_complete()
    #   incomplete_attributes(include_nas = FALSE)

    # Overwritten/overridden methods #

    #' @description
    #' Returns a boolean to indicate if (optionally selected or all) model attributes (such as dimensions) are consistent.
    #' @param params Optional array of parameter/attribute names.
    #' @return List of booleans (or NAs) to indicate consistency of selected/all attributes.
    list_consistency = function(params = NULL) {
      if (is.null(params)) { # all model attributes
        super$list_consistency()
      } else { # listed attributes
        params <- c(params)
        local_params <- params[which(params %in% c("coordinates", "random_seed", "populations", "initial_abundance",
                                                   "transition_rate", "compact_decomposition", "density_dependence",
                                                   "growth_rate_max", "dispersal_data", "dispersal_target_k", "harvest",
                                                   "harvest_max", "harvest_g", "harvest_z", "harvest_max_n", "human_density",
                                                   "abundance_threshold", "occupancy_threshold", "results_selection"))]
        consistent_list <- super$list_consistency(params[which(!params %in% local_params)])
        for (param in local_params) {
          param_value <- self$get_attributes(param)[[param]]
          if (is.null(param_value)) { # ignore incomplete attributes
            consistent_list[[param]] <- NA
          } else {
            consistent_list[[param]] <-
              switch(param,
                     coordinates =
                       if (is.numeric(self$populations)) {
                         ((is.data.frame(param_value) || is.matrix(param_value)) &&
                            nrow(param_value) == self$populations && ncol(param_value) == 2)
                       } else {
                         NA
                       },
                     random_seed = (is.numeric(param_value) && param_value > 0),
                     populations = (is.numeric(param_value) && param_value > 0) &&
                       if (!is.null(self$region) && is.numeric(self$region$region_cells)) {
                         (param_value == self$region$region_cells)
                       } else TRUE,
                     initial_abundance =
                       if (is.numeric(self$populations)) {
                         (is.numeric(param_value) && length(param_value) == self$populations)
                       } else {
                         NA
                       },
                     transition_rate =
                       if (is.numeric(param_value) && is.numeric(self$stages)) {
                         all(dim(as.matrix(param_value)) == self$stages)
                       } else {
                         NA
                       },
                     compact_decomposition =
                       if (is.numeric(self$populations)) {
                         (is.list(param_value) && "matrix" %in% names(param_value) && "map" %in% names(param_value) &&
                            is.matrix(param_value$matrix) && ncol(param_value$matrix) == self$populations &&
                            is.matrix(param_value$map) && ncol(param_value$map) == self$populations &&
                            nrow(param_value$matrix) == nrow(param_value$map) &&
                            min(param_value$map, na.rm = TRUE) >= 1 &&
                            max(param_value$map, na.rm = TRUE) <= self$populations)
                       } else {
                         NA
                       },
                     density_dependence = (is.character(param_value) && param_value %in%  c("competition", "logistic", "ceiling")),
                     growth_rate_max = , # or
                     dispersal_target_k =
                       if (length(param_value) == 1) {
                         TRUE
                       } else {
                         if (is.numeric(self$populations)) {
                           (is.numeric(param_value) && length(param_value) == self$populations)
                         } else {
                           NA
                         }
                       },
                     dispersal_data =
                       if (is.numeric(self$populations) && (length(param_value) == 1 || is.numeric(self$time_steps))) {
                         (is.list(param_value) && (length(param_value) == 1 || length(param_value) == self$time_steps) &&
                            is.data.frame(param_value[[1]]) && ncol(param_value[[1]]) == 5 &&
                            min(param_value[[1]][,1]) >= 1 && max(param_value[[1]][,1]) <= self$populations &&
                            min(param_value[[1]][,2]) >= 1 && max(param_value[[1]][,2]) <= self$populations)
                       } else {
                         NA
                       },
                     harvest = (is.logical(param_value)),
                     harvest_max = , # or
                     harvest_g = ,   # or
                     harvest_z = ,   # or
                     harvest_max_n = (is.numeric(param_value) && param_value >= 0),
                     human_density =
                       if (is.numeric(self$populations) && is.numeric(self$time_steps)) {
                         (is.numeric(param_value) && ((self$populations == 1 && length(param_value) == self$time_steps) ||
                                                        (is.matrix(param_value) && nrow(param_value) == self$populations &&
                                                           ncol(param_value) == self$time_steps)))
                       } else {
                         NA
                       },
                     abundance_threshold = , # or
                     occupancy_threshold = (is.numeric(param_value) && param_value >= 0),
                     results_selection = (is.character(param_value) &&
                                            all(param_value %in% c("abundance", "ema", "extirpation", "harvested", "occupancy")) &&
                                            any(c("abundance", "ema", "extirpation", "harvested", "occupancy") %in% param_value))
              )
          }
        }
        return(consistent_list)
      }
    },

    #' @description
    #' Returns a list of booleans (or NAs) for each parameter to indicate attributes that are necessary to simulate the model have been set and are consistent/valid.
    #' @return List of booleans (or NAs) for each parameter to indicate to indicate completeness (and consistency).
    list_completeness = function() {
      complete_list <- list()
      for (param in self$model_attributes) {
        if (param %in% self$required_attributes) {
          if (param == "harvest") {
            harvest_params <- c("harvest_max", "harvest_g", "harvest_z", "harvest_max_n", "human_density")
            complete_list[[param]] <- (length(self$get_attributes(param)) > 0 && self$is_consistent(param) &&
                                         (!self$harvest || length(which(unlist(self$list_consistency(harvest_params)))) == 5))
          } else {
            complete_list[[param]] <- length(self$get_attributes(param)) > 0 && self$is_consistent(param)
          }
        } else { # still must be consistent and interdependencies satisfied
          if (length(self$get_attributes(param)) > 0) {
            if (param == "density_dependence") {
              param_value <- self$get_attributes(param)[[param]]
              complete_list[[param]] <- (self$is_consistent(param) &&
                                           (param_value == "ceiling" ||
                                              (param_value %in% c("competition", "logistic") &&
                                                 length(which(unlist(self$list_consistency("growth_rate_max")))))))
            } else {
              complete_list[[param]] <- self$is_consistent(param)
            }
          } else {
            complete_list[[param]] <- NA
          }
        }
      }
      return(complete_list)
    }

  ), # end public

  private = list(

    ## Attributes ##

    # Associated (default) simulation function #
    .simulation_function = "paleopop_simulator",

    # Model attributes #
    .model_attributes = c("coordinates", "random_seed", "time_steps", "years_per_step", "populations",
                          "initial_abundance", "transition_rate", "standard_deviation", "compact_decomposition",
                          "carrying_capacity", "density_dependence", "growth_rate_max", "dispersal_data",
                          "dispersal_target_k", "harvest", "harvest_max", "harvest_g", "harvest_z",
                          "harvest_max_n", "human_density", "abundance_threshold", "occupancy_threshold",
                          "results_selection"),
    # .region             [inherited]
    # .random_seed        [inherited]
    # .replicates         [inherited]
    # .time_steps         [inherited]
    # .years_per_step     [inherited]
    .populations = NULL,
    .initial_abundance = NULL,
    .transition_rate = NULL,
    .standard_deviation = NULL,
    .compact_decomposition = NULL,
    .carrying_capacity = NULL,
    .density_dependence = NULL,
    .growth_rate_max = NULL,
    .dispersal_data = NULL,
    .dispersal_target_k = NULL,
    .harvest = NULL,
    .harvest_max = NULL,
    .harvest_g = NULL,
    .harvest_z = NULL,
    .harvest_max_n = NULL,
    .human_density = NULL,
    .abundance_threshold = NULL,
    .occupancy_threshold = NULL,
    .results_selection = c("abundance"),

    # Attributes accessible via model get/set methods #
    .active_attributes = c("coordinates", "random_seed", "time_steps", "years_per_step", "populations",
                           "initial_abundance", "transition_rate", "standard_deviation", "compact_decomposition",
                           "carrying_capacity", "density_dependence", "growth_rate_max", "dispersal_data",
                           "dispersal_target_k", "harvest", "harvest_max", "harvest_g", "harvest_z",
                           "harvest_max_n", "human_density", "abundance_threshold", "occupancy_threshold",
                           "results_selection"),

    # Dynamic attributes #
    .attribute_aliases = list(density = "harvest_max_n"),
    # .template_model    [inherited]
    # .sample_attributes [inherited]

    # Vector of required attributes (names)
    .required_attributes = c("time_steps", "years_per_step", "populations", "initial_abundance", "transition_rate",
                             "standard_deviation", "carrying_capacity", "harvest", "results_selection")

    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]

  ), # end private

  # Active binding accessors for private model attributes (above) #
  active = list(

    # Associated (default) simulation function #

    #' @field simulation_function Name (character string) or source path of the default simulation function, which takes a model as an input and returns the simulation results.
    simulation_function = function(value) { # inherited
      if (missing(value)) {
        super$simulation_function
      } else {
        super$simulation_function <- value
      }
    },

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
    region = function(value) { # inherited
      if (missing(value)) {
        super$region
      } else {
        super$region <- value
      }
    },

    #' @field coordinates Data frame (or matrix) of X-Y population coordinates (WGS84) in longitude (degrees West) and latitude (degrees North).
    coordinates = function(value) { # use non-raster region
      if (missing(value)) {
        self$region$coordinates
      } else {
        if (!is.null(self$region)) {
          self$region$coordinates <- value
        } else { # create a region with coordinates
          self$region <- Region$new(coordinates = value, use_raster = FALSE)
        }
      }
    },

    #' @field random_seed Number to seed the random number generation for stochasticity.
    random_seed = function(value) { # inherited
      if (missing(value)) {
        super$random_seed
      } else {
        super$random_seed <- value
      }
    },

    # replicates                  [inherited but not used]

    #' @field time_steps Number of simulation time steps.
    time_steps = function(value) { # inherited
      if (missing(value)) {
        super$time_steps
      } else {
        super$time_steps <- value
      }
    },

    #' @field years_per_step Number of years per time step.
    years_per_step = function(value) { # inherited
      if (missing(value)) {
        super$years_per_step
      } else {
        super$years_per_step <- value
      }
    },

    #' @field populations Number of population cells.
    populations = function(value) {
      if (missing(value)) {
        if (!is.null(self$region) && is.finite(self$region$region_cells)) {
          value <- self$region$region_cells
        } else {
          if (is.null(self$template_model) || "populations" %in% self$sample_attributes) {
            value <- private$.populations
          } else {
            value <- self$template_model$populations
          }
        }
        if (is.null(value) && is.numeric(self$initial_abundance)) {
          value <- nrow(as.matrix(self$initial_abundance))
        }
        value
      } else {
        if (is.null(self$template_model) || "populations" %in% self$sample_attributes) {
          private$.populations <- value
        } else {
          self$template_model$populations <- value
        }
      }
    },

    #' @field initial_abundance Array (matrix) of initial abundance values at each population cell.
    initial_abundance = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "initial_abundance" %in% self$sample_attributes) {
          private$.initial_abundance
        } else {
          self$template_model$initial_abundance
        }
      } else {
        if (is.null(self$template_model) || "initial_abundance" %in% self$sample_attributes) {
          private$.initial_abundance <- value
        } else {
          self$template_model$initial_abundance <- value
        }
      }
    },

    #' @field transition_rate Rate (numeric) of transition between generations at each time-step.
    transition_rate = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "transition_rate" %in% self$sample_attributes) {
          private$.transition_rate
        } else {
          self$template_model$transition_rate
        }
      } else {
        if (is.null(self$template_model) || "transition_rate" %in% self$sample_attributes) {
          private$.transition_rate <- value
        } else {
          self$template_model$transition_rate <- value
        }
      }
    },

    #' @field standard_deviation Standard deviation (numeric) for applying environmental stochasicity to transition rates.
    standard_deviation = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "standard_deviation" %in% self$sample_attributes) {
          private$.standard_deviation
        } else {
          self$template_model$standard_deviation
        }
      } else {
        if (is.null(self$template_model) || "standard_deviation" %in% self$sample_attributes) {
          private$.standard_deviation <- value
        } else {
          self$template_model$standard_deviation <- value
        }
      }
    },

    #' @field compact_decomposition List containing a compact transposed (Cholesky) decomposition \emph{matrix} (t_decomposition_compact_matrix) and a corresponding \emph{map} of population indices (t_decomposition_compact_map), as per \code{\link{SpatialCorrelation}} class attributes.
    compact_decomposition = function(value) {
      if (is.null(self$template_model) || "compact_decomposition" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.compact_decomposition
        } else {
          private$.compact_decomposition <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$compact_decomposition
        } else {
          self$template_model$compact_decomposition <- value
        }
      }
    },

    #' @field carrying_capacity Array (or matrix) of carrying capacity values at each population cell (across time).
    carrying_capacity = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "carrying_capacity" %in% self$sample_attributes) {
          private$.carrying_capacity
        } else {
          self$template_model$carrying_capacity
        }
      } else {
        if (is.null(self$template_model) || "carrying_capacity" %in% self$sample_attributes) {
          private$.carrying_capacity <- value
        } else {
          self$template_model$carrying_capacity <- value
        }
      }
    },

    #' @field density_dependence Density dependence type ("competition", "logistic", or "ceiling").
    density_dependence = function(value) {
      if (!(missing(value) || is.null(value) || (is.character(value) && (value %in% c("competition", "logistic", "ceiling"))))) {
        stop("Density dependence must be competition, logistic, or ceiling", call. = FALSE)
      }
      if (is.null(self$template_model) || "density_dependence" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.density_dependence
        } else {
          private$.density_dependence <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$density_dependence
        } else {
          self$template_model$density_dependence <- value
        }
      }
    },

    #' @field growth_rate_max Maximum growth rate (utilized by density dependence processes).
    growth_rate_max = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "growth_rate_max" %in% self$sample_attributes) {
          private$.growth_rate_max
        } else {
          self$template_model$growth_rate_max
        }
      } else {
        if (is.null(self$template_model) || "growth_rate_max" %in% self$sample_attributes) {
          private$.growth_rate_max <- value
        } else {
          self$template_model$growth_rate_max <- value
        }
      }
    },

    #' @field dispersal_data List of data frames of non-zero dispersal rates and indices for constructing a compact dispersal matrix, and optional changing rates over time, as per class \code{\link{DispersalGenerator}} \emph{dispersal_data} attribute.
    dispersal_data = function(value) {
      if (is.null(self$template_model) || "dispersal_data" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.dispersal_data
        } else {
          private$.dispersal_data <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$dispersal_data
        } else {
          self$template_model$dispersal_data <- value
        }
      }
    },

    #' @field dispersal_target_k Target population carrying capacity threshold for density dependent dispersal.
    dispersal_target_k = function(value) {
      if (is.null(self$template_model) || "dispersal_target_k" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.dispersal_target_k
        } else {
          private$.dispersal_target_k <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$dispersal_target_k
        } else {
          self$template_model$dispersal_target_k <- value
        }
      }
    },

    #' @field harvest Boolean for utilizing harvesting.
    harvest = function(value) {
      if (is.null(self$template_model) || "harvest" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.harvest
        } else {
          private$.harvest <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$harvest
        } else {
          self$template_model$harvest <- value
        }
      }
    },

    #' @field harvest_max Proportion harvested per year (annual time scale - not generational).
    harvest_max = function(value) {
      if (is.null(self$template_model) || "harvest_max" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.harvest_max
        } else {
          private$.harvest_max <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$harvest_max
        } else {
          self$template_model$harvest_max <- value
        }
      }
    },

    #' @field harvest_g The "G" parameter in the harvest function.
    harvest_g = function(value) {
      if (is.null(self$template_model) || "harvest_g" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.harvest_g
        } else {
          private$.harvest_g <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$harvest_g
        } else {
          self$template_model$harvest_g <- value
        }
      }
    },

    #' @field harvest_z The "Z" parameter in the harvest function.
    harvest_z = function(value) {
      if (is.null(self$template_model) || "harvest_z" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.harvest_z
        } else {
          private$.harvest_z <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$harvest_z
        } else {
          self$template_model$harvest_z <- value
        }
      }
    },

    #' @field harvest_max_n Maximum density per grid cell.
    harvest_max_n = function(value) {
      if (is.null(self$template_model) || "harvest_max_n" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.harvest_max_n
        } else {
          if (is.numeric(value)) { # rounding due to alias/dual usage
            private$.harvest_max_n <- round(value)
          } else {
            private$.harvest_max_n <- value
          }
        }
      } else {
        if (missing(value)) {
          self$template_model$harvest_max_n
        } else {
          if (is.numeric(value)) {
            self$template_model$harvest_max_n <- round(value)
          } else {
            self$template_model$harvest_max_n <- value
          }
        }
      }
    },

    #' @field human_density Matrix of human density (fraction) ($populations rows by $time_steps columns).
    human_density = function(value) {
      if (is.null(self$template_model) || "human_density" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.human_density
        } else {
          private$.human_density <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$human_density
        } else {
          self$template_model$human_density <- value
        }
      }
    },

    #' @field abundance_threshold Abundance threshold (that needs to be exceeded) for each population to persist.
    abundance_threshold = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "abundance_threshold" %in% self$sample_attributes) {
          private$.abundance_threshold
        } else {
          self$template_model$abundance_threshold
        }
      } else {
        if (is.null(self$template_model) || "abundance_threshold" %in% self$sample_attributes) {
          private$.abundance_threshold <- value
        } else {
          self$template_model$abundance_threshold <- value
        }
      }
    },

    #' @field occupancy_threshold Threshold for the number of populations occupied (that needs to be exceeded) for all populations to persist.
    occupancy_threshold = function(value) {
      if (is.null(self$template_model) || "occupancy_threshold" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.occupancy_threshold
        } else {
          private$.occupancy_threshold <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$occupancy_threshold
        } else {
          self$template_model$occupancy_threshold <- value
        }
      }
    },

    #' @field results_selection List of results selection from ("abundance", "ema", "extirpation", "harvested", "occupancy").
    results_selection = function(value) { # use inherited
      if (missing(value)) {
        super$results_selection
      } else {
        super$results_selection <- value
      }
    },

    # Dynamic attribute accessors #

    #' @field attribute_aliases A list of alternative alias names for model attributes (form: \code{alias = "attribute"}) to be used with the set and get attributes methods.
    attribute_aliases = function(value) { # use inherited with key aliases appended
      if (missing(value)) {
        super$attribute_aliases
      } else {
        if (!is.null(value) && !is.list(value)) {
          stop("Attribute aliases should be a list (form: alias = \"attribute\")", call. = FALSE)
        }
        if (is.null(self$template_model)) {
          value <- c(value, list(density = "harvest_max_n"))
        }
        super$attribute_aliases <- value
      }
    },

    #' @field template_model Nested template model for fixed (non-sampled) attributes for shallow cloning.
    template_model = function(value) { # inherited
      if (missing(value)) {
        super$template_model
      } else {
        super$template_model <- value
      }
    },

    #' @field sample_attributes Vector of sample attribute names (only).
    sample_attributes = function(value) { # inherited
      if (missing(value)) {
        super$sample_attributes
      } else {
        super$sample_attributes <- value
      }
    },

    #' @field required_attributes Vector of required attribute names (only), i.e. those needed to run a simulation.
    required_attributes = function(value) { # inherited
      if (missing(value)) {
        super$required_attributes
      } else {
        super$required_attributes <- value
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
