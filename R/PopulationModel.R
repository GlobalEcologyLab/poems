#' R6 class representing a population model
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class representing a spatially-explicit
#' demographic-based population model. It extends the \code{\link{SimulationModel}}
#' class with parameters for the \code{\link{population_simulator}} function. It
#' inherits functionality for creating a nested model, whereby a nested template model
#' with fixed parameters is maintained when a model is cloned for various sampled
#' parameters. Also provided are extensions to the methods for checking the consistency
#' and completeness of model parameters.
#'
#' @examples
#' # U Island example region
#' coordinates <- data.frame(x = rep(seq(177.01, 177.05, 0.01), 5),
#'                           y = rep(seq(-18.01, -18.05, -0.01), each = 5))
#' template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
#' template_raster[][-c(7, 9, 12, 14, 17:19)] <- NA # make U Island
#' region <- Region$new(template_raster = template_raster)
#' # Harvest function
#' harvest <- list(rate = NA, # set later
#'                 function(params) round(params$stage_abundance*(1 - params$rate)))
#' harvest_rate_alias <- list(harvest_rate = "harvest$rate")
#' # Template model
#' stage_matrix <- matrix(c(0,   2.5, # Leslie/Lefkovitch matrix
#'                          0.8, 0.5), nrow = 2, ncol = 2, byrow = TRUE)
#' template_model <- PopulationModel$new(region = region,
#'                                       time_steps = 10, # years
#'                                       populations = region$region_cells, # 7
#'                                       stage_matrix = stage_matrix,
#'                                       harvest = harvest,
#'                                       results_selection = c("abundance", "harvested"),
#'                                       attribute_aliases = harvest_rate_alias)
#' template_model$model_attributes
#' template_model$required_attributes
#' # Nested model
#' nested_model <- PopulationModel$new(template_model = template_model)
#' nested_model$incomplete_attributes()
#' nested_model$set_sample_attributes(initial_abundance = rep(10, 7),
#'                                    carrying_capacity = array(70:1, c(10, 7)),
#'                                    harvest_rate = 0.3)
#' nested_model$inconsistent_attributes()
#' nested_model$carrying_capacity <- array(70:1, c(7, 10))
#' nested_model$is_consistent()
#' nested_model$is_complete()
#' nested_model$harvest$rate
#'
#' @importFrom R6 R6Class
#' @include SimulationModel.R
#' @export PopulationModel

PopulationModel <- R6Class("PopulationModel",
  inherit = SimulationModel,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericClass, GenericModel, SpatialModel, & SimulationModel) #
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
    #   list_completeness()
    #   incomplete_attributes(include_nas = FALSE)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets default aliases and given attributes individually and/or from a list.
    #' @param attribute_aliases A list of alternative alias names for model attributes (form: \code{alias = "attribute"}) to be used with the set and get attributes methods.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(attribute_aliases = NULL, ...) {
      if (!"dispersal" %in% names(c(list(...), list(...)$params))) { # set default alias for dispersal
        attribute_aliases <- c(attribute_aliases, list(dispersal_data = "dispersal"))
      }
      if (!"dispersal_source_n_k" %in% names(c(list(...), list(...)$params))) { # set default aliases for source n/k
        attribute_aliases <- c(attribute_aliases, list(dispersal_n_k_cutoff = "dispersal_source_n_k$cutoff",
                                                       dispersal_n_k_threshold = "dispersal_source_n_k$threshold"))
      }
      if (!"dispersal_target_k" %in% names(c(list(...), list(...)$params))) { # set default alias for target k
        attribute_aliases <- c(attribute_aliases, list(dispersal_k_threshold = "dispersal_target_k"))
      }
      if (!"dispersal_target_n" %in% names(c(list(...), list(...)$params))) { # set default aliases for target n
        attribute_aliases <- c(attribute_aliases, list(dispersal_n_threshold = "dispersal_target_n$threshold",
                                                       dispersal_n_cutoff = "dispersal_target_n$cutoff"))
      }
      if (!"dispersal_target_n_k" %in% names(c(list(...), list(...)$params))) { # set default aliases for target n/k
        attribute_aliases <- c(attribute_aliases, list(dispersal_target_n_k_threshold = "dispersal_target_n_k$threshold",
                                                       dispersal_target_n_k_cutoff = "dispersal_target_n_k$cutoff"))
      }
      super$initialize(attribute_aliases = attribute_aliases, ...)
    },

    #' @description
    #' Returns a boolean to indicate if (optionally selected or all) model attributes (such as dimensions) are consistent.
    #' @param params Optional array of parameter/attribute names.
    #' @return List of booleans (or NAs) to indicate consistency of selected/all attributes.
    list_consistency = function(params = NULL) {
      if (is.null(params)) { # all model attributes
        super$list_consistency()
      } else { # listed attributes
        params <- c(params)
        local_params <- params[which(params %in% c("populations", "initial_abundance", "stage_matrix", "fecundity_mask",
                                                   "demographic_stochasticity", "standard_deviation", "correlation",
                                                   "carrying_capacity", "density_dependence", "growth_rate_max",
                                                   "density_affects", "density_stages", "translocation", "harvest",
                                                   "mortality", "dispersal", "dispersal_stages", "dispersal_source_n_k",
                                                   "dispersal_target_k", "dispersal_target_n", "dispersal_target_n",
                                                   "abundance_threshold", "result_stages"))]
        consistent_list <- super$list_consistency(params[which(!params %in% local_params)])
        for (param in local_params) {
          param_value <- self$get_attribute(param)
          if (is.null(param_value)) { # ignore incomplete attributes
            consistent_list[[param]] <- NA
          } else {
            consistent_list[[param]] <-
              switch(param,
                     populations = (is.numeric(param_value) && param_value > 0) &&
                       if (!is.null(self$region) && is.numeric(self$region$region_cells)) {
                         (param_value == self$region$region_cells)
                       } else TRUE,
                     initial_abundance =
                       if (is.numeric(self$populations) && is.numeric(self$stages)) {
                         if (any(class(param_value) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
                           if (!is.null(self$region) && self$region$use_raster && !is.null(self$region$region_raster)) {
                             (self$region$raster_is_consistent(param_value) &&
                                self$region$region_cells == self$populations &&
                                raster::nlayers(param_value) %in% c(1, self$stages))
                           } else {
                             NA
                           }
                         } else { # assume matrix or array
                           (is.numeric(param_value) &&
                              nrow(as.matrix(param_value)) == self$populations &&
                              ncol(as.matrix(param_value)) %in% c(1, self$stages))
                         }
                       } else {
                         NA
                       },
                     stage_matrix = , # or
                     fecundity_mask = , # or
                     density_affects =
                       if (is.numeric(param_value) && is.numeric(self$stages)) {
                         all(dim(as.matrix(param_value)) == self$stages)
                       } else {
                         NA
                       },
                     density_stages = , # or
                     dispersal_stages =
                       if (is.numeric(param_value) && is.numeric(self$stages)) {
                         (length(param_value) == self$stages && all(+param_value >= 0) && all(+param_value <= 1))
                       } else {
                         NA
                       },
                     result_stages =
                       if (is.numeric(param_value) && is.numeric(self$stages)) {
                         (length(param_value) == self$stages)
                       } else {
                         NA
                       },
                     demographic_stochasticity = is.logical(param_value),
                     standard_deviation =
                       if (length(param_value) == 1) {
                         TRUE
                       } else {
                         if (is.numeric(param_value) && is.numeric(self$stages)) {
                           all(dim(as.matrix(param_value)) == self$stages)
                         } else {
                           NA
                         }
                       },
                     correlation = NA,
                     carrying_capacity =
                       if (is.numeric(self$populations) && is.numeric(self$time_steps)) {
                         if (any(class(param_value) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
                           if (!is.null(self$region) && self$region$use_raster && !is.null(self$region$region_raster)) {
                             (self$region$raster_is_consistent(param_value) &&
                                self$region$region_cells == self$populations &&
                                raster::nlayers(param_value) %in% c(1, self$time_steps))
                           } else {
                             NA
                           }
                         } else { # assume matrix or array
                           (is.numeric(param_value) &&
                              nrow(as.matrix(param_value)) == self$populations &&
                              ncol(as.matrix(param_value)) %in% c(1, self$time_steps))
                         }
                       } else {
                         NA
                       },
                     density_dependence = NA,
                     growth_rate_max = , # or
                     dispersal_source_n_k =
                       if (is.list(param_value) && all(c("cutoff", "threshold") %in% names(param_value))) {
                         consistent <- list(cutoff = NA, threshold = NA)
                         for (name in c("cutoff", "threshold")) {
                           if (length(param_value[[name]]) == 1) {
                             consistent[[name]] <- TRUE
                           } else if (length(param_value[[name]]) > 1 && is.numeric(self$populations)) {
                             consistent[[name]] <- (is.numeric(param_value[[name]]) &&
                                                      length(param_value[[name]]) == self$populations)
                           }
                         }
                         all(unlist(consistent))
                       } else {
                         NA
                       },
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
                     dispersal_target_n =
                       if (is.list(param_value) && all(c("threshold", "cutoff") %in% names(param_value))) {
                         consistent <- list(threshold = NA, cutoff = NA)
                         for (name in c("threshold", "cutoff")) {
                           if (length(param_value[[name]]) == 1) {
                             consistent[[name]] <- TRUE
                           } else if (length(param_value[[name]]) > 1 && is.numeric(self$populations)) {
                             consistent[[name]] <- (is.numeric(param_value[[name]]) &&
                                                      length(param_value[[name]]) == self$populations)
                           }
                         }
                         all(unlist(consistent))
                       } else {
                         NA
                       },
                     dispersal_target_n_k =
                       if (is.list(param_value) && all(c("threshold", "cutoff") %in% names(param_value))) {
                         consistent <- list(threshold = NA, cutoff = NA)
                         for (name in c("threshold", "cutoff")) {
                           if (length(param_value[[name]]) == 1) {
                             consistent[[name]] <- TRUE
                           } else if (length(param_value[[name]]) > 1 && is.numeric(self$populations)) {
                             consistent[[name]] <- (is.numeric(param_value[[name]]) &&
                                                      length(param_value[[name]]) == self$populations)
                           }
                         }
                         all(unlist(consistent))
                       } else {
                         NA
                       },
                     translocation = NA,
                     harvest = NA,
                     mortality = NA,
                     dispersal = NA,
                     abundance_threshold =
                       if (length(param_value) == 1) {
                         TRUE
                       } else {
                         if (is.numeric(self$populations)) {
                           (is.numeric(param_value) && length(param_value) == self$populations)
                         } else {
                           NA
                         }
                       }
              )
          }
        }
        return(consistent_list)
      }
    }

  ), # end public

  private = list(

    ## Attributes ##

    # Associated (default) simulation function #
    .simulation_function = "population_simulator",

    # Model attributes #
    .model_attributes = c("region", "coordinates", "random_seed", "replicates", "time_steps", "years_per_step",
                          "populations", "stages", "initial_abundance", "stage_matrix", "fecundity_mask",
                          "fecundity_max", "demographic_stochasticity", "standard_deviation", "correlation",
                          "carrying_capacity", "density_dependence", "growth_rate_max", "density_affects",
                          "density_stages", "translocation", "harvest", "mortality", "dispersal",
                          "dispersal_stages", "dispersal_source_n_k", "dispersal_target_k",
                          "dispersal_target_n", "dispersal_target_n_k", "abundance_threshold",
                          "simulation_order", "results_selection", "result_stages"),
    # .region             [inherited]
    # .random_seed        [inherited]
    # .replicates         [inherited]
    # .time_steps         [inherited]
    # .years_per_step     [inherited]
    .populations = NULL,
    .initial_abundance = NULL,
    .stage_matrix = NULL,
    .fecundity_mask = NULL,
    .fecundity_max = NULL,
    .demographic_stochasticity = TRUE, # default for poems simulator
    .standard_deviation = NULL,
    .correlation = NULL,
    .carrying_capacity = NULL,
    .density_dependence = NULL,
    .growth_rate_max = NULL,
    .density_affects = "all",          # default for poems simulator
    .density_stages = NULL,
    .translocation = NULL,
    .harvest = NULL,
    .mortality = NULL,
    .dispersal = NULL,
    .dispersal_stages = NULL,
    .dispersal_source_n_k = list(cutoff = NULL, threshold = NULL), # default for poems simulator
    .dispersal_target_k = NULL,
    .dispersal_target_n = list(threshold = NULL, cutoff = NULL), # default for poems simulator
    .dispersal_target_n_k = list(threshold = NULL, cutoff = NULL), # default for poems simulator
    .abundance_threshold = NULL,
    .simulation_order = c("transition", "translocation", "harvest", "mortality", "dispersal", "results"), # default
    # .results_selection [inherited]
    .result_stages = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("region", "coordinates", "random_seed", "replicates", "time_steps", "years_per_step",
                           "populations", "stages", "initial_abundance", "stage_matrix", "fecundity_mask",
                           "fecundity_max", "demographic_stochasticity", "standard_deviation", "correlation",
                           "carrying_capacity", "density_dependence", "growth_rate_max", "density_affects",
                           "density_stages", "translocation", "harvest", "mortality", "dispersal",
                           "dispersal_stages", "dispersal_source_n_k", "dispersal_target_k", "dispersal_target_n",
                           "dispersal_target_n_k", "abundance_threshold", "simulation_order", "results_selection",
                           "result_stages"),

    # Dynamic attributes #
    # .attribute_aliases  [inherited]
    # .template_model     [inherited]
    # .sample_attributes  [inherited]

    # Vector of required attributes (names) - below are those required by the default poems simulator
    .required_attributes = c("time_steps", "initial_abundance", "stage_matrix", "carrying_capacity")

    # Errors and warnings #
    # .error_messages     [inherited]
    # .warning_messages   [inherited]

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

    #' @field coordinates Data frame (or matrix) of X-Y population (WGS84) coordinates in longitude (degrees West) and latitude (degrees North) (get and set), or distance-based coordinates dynamically returned by region raster (get only).
    coordinates = function(value) { # inherited
      if (missing(value)) {
        super$coordinates
      } else {
        super$coordinates <- value
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

    #' @field replicates Number of replicate simulation runs.
    replicates = function(value) { # inherited
      if (missing(value)) {
        super$replicates
      } else {
        super$replicates <- value
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

    #' @field stages Number of life cycle stages.
    stages = function(value) { # accessor for stage matrix dimension
      if (missing(value)) {
        if (is.matrix(self$stage_matrix)) {
          nrow(self$stage_matrix)
        } else {
          NULL
        }
      }
    },

    #' @field initial_abundance Array (matrix) or raster (stack) of initial abundance values at each population cell (for each age/stage).
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

    #' @field stage_matrix Matrix of transition (fecundity & survival) rates between stages at each time step (Leslie/Lefkovitch matrix).
    stage_matrix = function(value) { # matrix accessor for transition rate
      if (missing(value)) {
        if (is.null(self$template_model) || "stage_matrix" %in% self$sample_attributes) {
          private$.stage_matrix
        } else {
          self$template_model$stage_matrix
        }
      } else {
        if (is.numeric(value)) {
          value <- as.matrix(value)
        }
        if (is.null(self$template_model) || "stage_matrix" %in% self$sample_attributes) {
          private$.stage_matrix <- value
        } else {
          self$template_model$stage_matrix <- value
        }
      }
    },

    #' @field fecundity_mask Matrix of 0-1 to indicate which (proportions) of transition rates refer to fecundity.
    fecundity_mask = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "fecundity_mask" %in% self$sample_attributes) {
          if (is.null(private$.fecundity_mask) && is.matrix(self$stage_matrix)) {
            if (nrow(self$stage_matrix) > 1) {
              +upper.tri(self$stage_matrix)
            } else {
              +(self$stage_matrix > 0)
            }
          } else {
            private$.fecundity_mask
          }
        } else {
          self$template_model$fecundity_mask
        }
      } else {
        if (is.null(self$template_model) || "fecundity_mask" %in% self$sample_attributes) {
          private$.fecundity_mask <- value
        } else {
          self$template_model$fecundity_mask <- value
        }
      }
    },

    #' @field fecundity_max Maximum transition fecundity rate (in Leslie/Lefkovitch matrix).
    fecundity_max = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "fecundity_max" %in% self$sample_attributes) {
          private$.fecundity_max
        } else {
          self$template_model$fecundity_max
        }
      } else {
        if (is.null(self$template_model) || "fecundity_max" %in% self$sample_attributes) {
          private$.fecundity_max <- value
        } else {
          self$template_model$fecundity_max <- value
        }
      }
    },

    #' @field demographic_stochasticity Boolean for choosing demographic stochasticity for transition, dispersal, harvest and/or other processes.
    demographic_stochasticity = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "demographic_stochasticity" %in% self$sample_attributes) {
          private$.demographic_stochasticity
        } else {
          self$template_model$demographic_stochasticity
        }
      } else {
        if (is.null(self$template_model) || "demographic_stochasticity" %in% self$sample_attributes) {
          private$.demographic_stochasticity <- value
        } else {
          self$template_model$demographic_stochasticity <- value
        }
      }
    },

    #' @field standard_deviation Standard deviation matrix (or single value) for applying environmental stochasticity to transition rates.
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

    #' @field correlation Simulator-dependent attribute or list of attributes for describing/parameterizing the correlation strategy utilized when applying environmental stochasticity and/or other processes (see \code{\link{population_simulator}}).
    correlation = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "correlation" %in% self$sample_attributes) {
          private$.correlation
        } else {
          self$template_model$correlation
        }
      } else {
        if (is.null(self$template_model) || "correlation" %in% self$sample_attributes) {
          private$.correlation <- value
        } else {
          self$template_model$correlation <- value
        }
      }
    },

    #' @field carrying_capacity Array (matrix), or raster (stack) of carrying capacity values at each population cell (across time).
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

    #' @field density_dependence Simulator-dependent function, attribute or list of attributes for describing/parameterizing the density dependence strategy utilized (see \code{\link{population_simulator}}).
    density_dependence = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "density_dependence" %in% self$sample_attributes) {
          private$.density_dependence
        } else {
          self$template_model$density_dependence
        }
      } else {
        if (is.null(self$template_model) || "density_dependence" %in% self$sample_attributes) {
          private$.density_dependence <- value
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

    #' @field density_affects Transition vital rates that are affected by density, including \emph{"fecundity"}, \emph{"survival"}, or a matrix of booleans or numeric (0-1) indicating vital rates affected (default is all).
    density_affects = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "density_affects" %in% self$sample_attributes) {
          value <- private$.density_affects
        } else {
          value <- self$template_model$density_affects
        }
        if (is.character(value)) {
          if (value == "fecundity" && is.numeric(self$fecundity_mask)) {
            value <- self$fecundity_mask
          } else if (value == "survival" && is.numeric(self$stage_matrix) && is.numeric(self$fecundity_mask)) {
            value <- +(self$stage_matrix > 0)*(1 - self$fecundity_mask)
          } else if (value == "all" && is.numeric(self$stage_matrix)) {
            value <- +(self$stage_matrix > 0)
          }
        }
        value
      } else {
        if (is.null(self$template_model) || "density_affects" %in% self$sample_attributes) {
          private$.density_affects <- value
        } else {
          self$template_model$density_affects <- value
        }
      }
    },

    #' @field density_stages Array of booleans or numeric (0-1) for each stage to indicate (the degree to) which stages are affected by density (default is 1 for all stages).
    density_stages = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "density_stages" %in% self$sample_attributes) {
          value <- private$.density_stages
        } else {
          value <- self$template_model$density_stages
        }
        if (is.null(value) && is.numeric(self$stages)) {
          value <- array(1, self$stages)
        }
        value
      } else {
        if (is.null(self$template_model) || "density_stages" %in% self$sample_attributes) {
          private$.density_stages <- value
        } else {
          self$template_model$density_stages <- value
        }
      }
    },

    #' @field translocation Simulator-dependent function, attribute or list of attributes for describing/parameterizing translocation (management) strategies utilized (see \code{\link{population_simulator}}).
    translocation = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "translocation" %in% self$sample_attributes) {
          private$.translocation
        } else {
          self$template_model$translocation
        }
      } else {
        if (is.null(self$template_model) || "translocation" %in% self$sample_attributes) {
          private$.translocation <- value
        } else {
          self$template_model$translocation <- value
        }
      }
    },

    #' @field harvest Simulator-dependent function, attribute or list of attributes for describing/parameterizing a harvest (organism removal/hunting) strategy (see \code{\link{population_simulator}}).
    harvest = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "harvest" %in% self$sample_attributes) {
          private$.harvest
        } else {
          self$template_model$harvest
        }
      } else {
        if (is.null(self$template_model) || "harvest" %in% self$sample_attributes) {
          private$.harvest <- value
        } else {
          self$template_model$harvest <- value
        }
      }
    },

    #' @field mortality Simulator-dependent function, attribute or list of attributes to describe/parameterize a spatio-temporal mortality strategy (see \code{\link{population_simulator}}).
    mortality = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "mortality" %in% self$sample_attributes) {
          private$.mortality
        } else {
          self$template_model$mortality
        }
      } else {
        if (is.null(self$template_model) || "mortality" %in% self$sample_attributes) {
          private$.mortality <- value
        } else {
          self$template_model$mortality <- value
        }
      }
    },

    #' @field dispersal Simulator-dependent function, attribute or list of attributes for describing/parameterizing the dispersal (migration) strategy utilized (see \code{\link{population_simulator}}).
    dispersal = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "dispersal" %in% self$sample_attributes) {
          private$.dispersal
        } else {
          self$template_model$dispersal
        }
      } else {
        if (is.null(self$template_model) || "dispersal" %in% self$sample_attributes) {
          private$.dispersal <- value
        } else {
          self$template_model$dispersal <- value
        }
      }
    },

    #' @field dispersal_stages Array of relative dispersal (0-1) for each stage to indicate the degree to which each stage participates in dispersal (default is 1 for all stages).
    dispersal_stages = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "dispersal_stages" %in% self$sample_attributes) {
          value <- private$.dispersal_stages
        } else {
          value <- self$template_model$dispersal_stages
        }
        if (is.null(value) && is.numeric(self$stages)) {
          value <- array(1, self$stages)
        }
        value
      } else {
        if (is.null(self$template_model) || "dispersal_stages" %in% self$sample_attributes) {
          private$.dispersal_stages <- value
        } else {
          self$template_model$dispersal_stages <- value
        }
      }
    },

    #' @field dispersal_source_n_k Simulator-dependent attribute for describing/parameterizing dispersal dependent on source population abundance divided by carrying capacity (see \code{\link{population_simulator}}).
    dispersal_source_n_k = function(value) {
      if (is.null(self$template_model) || "dispersal_source_n_k" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.dispersal_source_n_k
        } else {
          private$.dispersal_source_n_k <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$dispersal_source_n_k
        } else {
          self$template_model$dispersal_source_n_k <- value
        }
      }
    },

    #' @field dispersal_target_k Simulator-dependent attribute for describing/parameterizing dispersal dependent on target population carrying capacity (see \code{\link{population_simulator}}).
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

    #' @field dispersal_target_n Simulator-dependent attribute (default is list with \emph{threshold} and \emph{cutoff}) of attributes for describing/parameterizing dispersal dependent on target population abundance (see \code{\link{population_simulator}}).
    dispersal_target_n = function(value) {
      if (is.null(self$template_model) || "dispersal_target_n" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.dispersal_target_n
        } else {
          private$.dispersal_target_n <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$dispersal_target_n
        } else {
          self$template_model$dispersal_target_n <- value
        }
      }
    },

    #' @field dispersal_target_n_k Simulator-dependent attribute (default is list with \emph{threshold} and \emph{cutoff}) of attributes for describing/parameterizing dispersal dependent on target population abundance/capacity (see \code{\link{population_simulator}}).
    dispersal_target_n_k = function(value) {
      if (is.null(self$template_model) || "dispersal_target_n_k" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.dispersal_target_n_k
        } else {
          private$.dispersal_target_n_k <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$dispersal_target_n_k
        } else {
          self$template_model$dispersal_target_n_k <- value
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

    #' @field simulation_order A vector of simulation process names in configured order of execution (default is "transition", "translocation", "harvest", "mortality", "dispersal", "results").
    simulation_order = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "simulation_order" %in% self$sample_attributes) {
          private$.simulation_order
        } else {
          self$template_model$simulation_order
        }
      } else {
        if (is.null(self$template_model) || "simulation_order" %in% self$sample_attributes) {
          private$.simulation_order <- value
        } else {
          self$template_model$simulation_order <- value
        }
      }
    },

    #' @field results_selection List of attributes to be included in the returned results of each simulation run, selected from: "abundance", "ema", "extirpation", "extinction_location", "harvested", "occupancy"; "summarize" or "replicate".
    results_selection = function(value) { # inherited
      if (missing(value)) {
        super$results_selection
      } else {
        super$results_selection <- value
      }
    },

    #' @field result_stages Array of booleans or numeric (0, 1, 2, ...) for each stage to indicate which stages are included/combined (each unique digit > 0; optionally named) in the results (default is 1 for all stages).
    result_stages = function(value) {
      if (missing(value)) {
        if (is.null(self$template_model) || "result_stages" %in% self$sample_attributes) {
          value <- private$.result_stages
        } else {
          value <- self$template_model$result_stages
        }
        if (is.null(value) && is.numeric(self$stages)) {
          value <- array(1, self$stages)
        }
        value
      } else {
        if (is.null(self$template_model) || "result_stages" %in% self$sample_attributes) {
          private$.result_stages <- value
        } else {
          self$template_model$result_stages <- value
        }
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
