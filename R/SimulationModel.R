#' R6 class representing a simulation model
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class representing a spatially-explicit simulation
#' model. It extends the \code{\link{SpatialModel}} class with a range of common
#' simulation parameters and functionality for creating a nested model, whereby a nested
#' template model with fixed parameters is maintained when a model is cloned for various
#' sampled parameters. Also provided are methods for checking the consistency and
#' completeness of model parameters.
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
#' # Model template
#' template_model <- SimulationModel$new(
#'   simulation_function = "test_simulator",
#'   region = region, time_steps = 10
#' )
#' template_model$model_attributes <- c(
#'   template_model$model_attributes,
#'   "a", "b", "c", "d"
#' )
#' template_model$model_attributes
#' template_model$required_attributes <- c(
#'   template_model$required_attributes[1:2],
#'   "a", "b", "c", "d"
#' )
#' template_model$required_attributes
#' template_model$get_attributes(template_model$required_attributes)
#' template_model$simulation_function
#' # Nested model
#' nested_model <- SimulationModel$new(template_model = template_model)
#' nested_model$region$region_cells
#' nested_model$set_sample_attributes(a = 1:7, b = 1:10, c = 1:15)
#' nested_model$sample_attributes
#' nested_model$get_attributes(c("a", "b", "c", "d"))
#' # Completeness and consistency
#' nested_model$is_complete()
#' nested_model$incomplete_attributes()
#' nested_model$is_consistent()
#' nested_model$inconsistent_attributes()
#' nested_model$set_attributes(c = array(1:70, c(7, 10)), d = 15)
#' nested_model$is_complete()
#' nested_model$is_consistent()
#' # Attached attributes
#' nested_model$attached
#' template_model$attached
#'
#' @importFrom R6 R6Class
#' @include SpatialModel.R
#' @export SimulationModel

SimulationModel <- R6Class(
  "SimulationModel",
  inherit = SpatialModel,
  public = list(
    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericModel & SpatialModel) #
    #   get_attribute(param)
    #   get_attribute_aliases(params = NULL)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets template model and sets given attributes individually and/or from a list.
    #' @param template Template simulation model (nested) containing fixed (non-sampled) attributes.
    #' @param required_attributes Vector of required attribute names (only), i.e. those needed to run a simulation.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(template = NULL, required_attributes = NULL, ...) {
      super$initialize(...)
      if (!is.null(template)) {
        # Any passed model attributes and/or aliases will subsequently be accessed via template, but set locally via super.
        if ("model_attributes" %in% names(list(...))) {
          # clear locally
          self$model_attributes <- NULL
        }
        if ("attribute_aliases" %in% names(list(...))) {
          # clear locally
          self$attribute_aliases <- NULL
        }
        self$template_model <- template
        if ("model_attributes" %in% names(list(...))) {
          # set again via template
          self$model_attributes <- list(...)[["model_attributes"]]
        }
        if ("attribute_aliases" %in% names(list(...))) {
          # set again via template
          self$attribute_aliases <- list(...)[["attribute_aliases"]]
        }
      }
      if (!is.null(required_attributes)) {
        self$required_attributes <- required_attributes
      }
    },

    #' @description
    #' Creates a new (re-initialized) object of the current (inherited) object class with optionally passed parameters.
    #' @param ... Parameters passed via the inherited class constructor (defined in initialize and run via new).
    #' @return New object of the current (inherited) class.
    new_clone = function(...) {
      return(super$new_clone(
        required_attributes = self$required_attributes,
        ...
      ))
    },

    #' @description
    #' Returns a list of all attribute names including public and private model attributes, as well as attached attributes (including those from the template model).
    #' @return List of all attribute names.
    get_attribute_names = function() {
      if (!is.null(self$template_model)) {
        return(unique(c(
          self$template_model$get_attribute_names(),
          names(self$attached)
        )))
      } else {
        return(super$get_attribute_names())
      }
    },

    #' @description
    #' Returns a list of values for selected attributes or attribute aliases (when array of parameter names provided) or all attributes (when no params).
    #' @param params Array of attribute names to return (all when NULL).
    #' @return List of selected or all attributes values.
    get_attributes = function(params = NULL) {
      if (!is.null(self$template_model)) {
        # resolve scope via sample attributes
        attribute_list <- list()
        if (is.null(params)) {
          params <- self$get_attribute_names()
        }
        for (param in params) {
          if (param %in% names(self$attribute_aliases)) {
            attribute <- self$attribute_aliases[[param]]
          } else {
            attribute <- param
          }
          attribute_root <- unlist(strsplit(attribute, "$", fixed = TRUE))[1]
          if (
            attribute_root %in%
              self$sample_attributes ||
              !attribute_root %in% self$template_model$get_attribute_names()
          ) {
            attribute_list <- c(attribute_list, super$get_attributes(param))
          } else {
            attribute_list <- c(
              attribute_list,
              self$template_model$get_attributes(param)
            )
          }
        }
        return(attribute_list)
      } else {
        return(super$get_attributes(params))
      }
    },

    #' @description
    #' Sets given attributes (optionally via alias names) individually and/or from a list.
    #' @param params List of parameters/attributes.
    #' @param ... Parameters/attributes passed individually.
    set_attributes = function(params = list(), ...) {
      params <- c(list(...), params) # prioritise individual parameters
      if (!is.null(self$template_model)) {
        # resolve scope via sample attributes
        sample_params <- list()
        template_params <- list()
        for (param in names(params)) {
          if (param %in% names(self$attribute_aliases)) {
            attribute <- self$attribute_aliases[[param]]
          } else {
            attribute <- param
          }
          attribute_root <- unlist(strsplit(attribute, "$", fixed = TRUE))[1]
          if (
            attribute_root %in%
              self$sample_attributes ||
              !attribute_root %in% self$template_model$get_attribute_names()
          ) {
            sample_params[[param]] <- params[[param]]
          } else {
            template_params[[param]] <- params[[param]]
          }
        }
        super$set_attributes(sample_params)
        self$template_model$set_attributes(template_params)
      } else {
        super$set_attributes(params)
      }
    },

    # New methods #

    #' @description
    #' Sets the names (only - when \emph{params} is a vector) and values (when \emph{params} is a list and/or when name-value pairs are provided) of the sample attributes for the model.
    #' @param params List of parameters/attributes (names and values) or array of names only.
    #' @param ... Parameters/attributes passed individually.
    set_sample_attributes = function(params = list(), ...) {
      if (is.list(params)) {
        param_list <- c(list(...), params) # prioritise individual parameters
        params <- unique(names(param_list))
      } else {
        # params is a vector of names
        param_list <- list(...)
        params <- unique(c(names(param_list), params))
        self$sample_attributes <- NULL # redefine
      }
      for (i in 1:length(params)) {
        # substitute aliases
        if (params[i] %in% names(self$attribute_aliases)) {
          param_alias <- params[i]
          split_names <- unlist(strsplit(
            self$attribute_aliases[[param_alias]],
            "$",
            fixed = TRUE
          ))
          params[i] <- split_names[1] # list root only
          self$sample_attributes <- unique(c(self$sample_attributes, params[i]))
          if (
            length(split_names) > 1 &&
              !is.null(self$template_model) &&
              length(self$get_attributes(params[i])) == 0
          ) {
            # copy template values
            self$set_attributes(self$template_model$get_attributes(params[i]))
          }
          if (
            !(params[i] %in%
              self$get_attribute_names() ||
              params[i] %in% names(self$attached) ||
              param_alias %in% self$get_attribute_names() ||
              param_alias %in% names(self$attached))
          ) {
            self$attached[[params[i]]] <- NA # attach any attributes not present
          }
        } else {
          self$sample_attributes <- unique(c(self$sample_attributes, params[i]))
          if (
            !(params[i] %in%
              self$get_attribute_names() ||
              params[i] %in% names(self$attached))
          ) {
            self$attached[[params[i]]] <- NA # attach any attributes not present
          }
        }
      }
      self$set_attributes(params = param_list)
    },

    #' @description
    #' Returns a boolean to indicate if (optionally selected or all) model attributes (such as dimensions) are consistent/valid.
    #' @param params Optional array of parameter/attribute names.
    #' @return Boolean to indicate consistency of selected/all attributes.
    is_consistent = function(params = NULL) {
      all(unlist(self$list_consistency(params)), na.rm = TRUE)
    },

    #' @description
    #' Returns a boolean to indicate if (optionally selected or all) model attributes (such as dimensions) are consistent/valid.
    #' @param params Optional array of parameter/attribute names.
    #' @return List of booleans (or NAs) to indicate consistency of selected/all attributes.
    list_consistency = function(params = NULL) {
      if (is.null(params)) {
        # all model attributes
        return(self$list_consistency(self$model_attributes))
      } else {
        # listed attributes
        params <- c(params)
        consistent_list <- list()
        for (param in params) {
          param_value <- self$get_attributes(param)[[param]]
          if (is.null(param_value)) {
            # ignore incomplete attributes
            consistent_list[[param]] <- NA
          } else {
            consistent_list[[param]] <-
              switch(
                param,
                region = ("Region" %in% class(param_value)),
                time_steps = (is.numeric(param_value) && param_value > 0),
                years_per_step = (is.numeric(param_value) && param_value > 0),
                results_selection = is.character(param_value),
                {
                  # default for other attributes
                  if (!is.list(param_value)) {
                    # place in list
                    param_value <- list(param_value)
                  }
                  values_consistent <- array(NA, length(param_value))
                  for (i in 1:length(param_value)) {
                    if (
                      !is.null(self$region) &&
                        any(
                          class(param_value[[i]]) %in%
                            c("RasterLayer", "RasterStack", "RasterBrick")
                        )
                    ) {
                      values_consistent[
                        i
                      ] <- (self$region$raster_is_consistent(param_value[[
                        i
                      ]]) &&
                        raster::nlayers(param_value[[i]]) %in%
                          c(1, self$time_steps))
                    } else if (
                      !is.null(self$region) && is.numeric(param_value[[i]])
                    ) {
                      values_consistent[i] <- (nrow(as.matrix(param_value[[
                        i
                      ]])) %in%
                        c(1, self$region$region_cells, self$time_steps) &&
                        ncol(as.matrix(param_value[[i]])) %in%
                          c(1, self$region$region_cells, self$time_steps))
                    } else if (is.numeric(param_value[[i]])) {
                      values_consistent[i] <- (nrow(as.matrix(param_value[[
                        i
                      ]])) %in%
                        c(1, self$time_steps) &&
                        ncol(as.matrix(param_value[[i]])) %in%
                          c(1, self$time_steps))
                    }
                  }
                  all(values_consistent)
                }
              )
          }
        }
        return(consistent_list)
      }
    },

    #' @description
    #' Returns a list of attributes necessary to simulate the model that are inconsistent/invalid.
    #' @param include_nas Optional boolean indicating whether of not to include attributes with unknown consistency (NA).
    #' @return List of inconsistent attributes which prevent the model simulation (and optionally those where consistency is not available).
    inconsistent_attributes = function(include_nas = FALSE) {
      consistent <- unlist(self$list_consistency())
      inconsistent <- if (length(which(consistent == FALSE))) {
        names(which(consistent == FALSE))
      } else {
        NULL
      }
      not_available <- if (length(which(is.na(consistent)))) {
        names(which(is.na(consistent)))
      } else {
        NULL
      }
      if (include_nas) {
        return(list(inconsistent = inconsistent, not_available = not_available))
      } else {
        return(inconsistent)
      }
    },

    #' @description
    #' Returns a boolean to indicate if all attributes necessary to simulate the model have been set and are consistent/valid.
    #' @return Boolean to indicate model completeness (and consistency).
    is_complete = function() {
      return(!length(self$incomplete_attributes()))
    },

    #' @description
    #' Returns a list of booleans (or NAs) for each parameter to indicate attributes that are necessary to simulate the model have been set and are consistent/valid.
    #' @return List of booleans (or NAs) for each parameter to indicate to indicate completeness (and consistency).
    list_completeness = function() {
      complete_list <- list()
      for (param in self$model_attributes) {
        if (param %in% self$required_attributes) {
          if (param == "region") {
            complete_list[[param]] <- ifelse(
              is.null(self$region),
              FALSE,
              self$is_consistent(param) &&
                (!is.null(self$region$coordinates) ||
                  !is.null(self$region$region_raster))
            )
          } else {
            complete_list[[param]] <- length(self$get_attributes(param)) > 0 &&
              self$is_consistent(param)
          }
        } else {
          # still must be consistent
          if (length(self$get_attributes(param)) > 0) {
            complete_list[[param]] <- self$is_consistent(param)
          } else {
            complete_list[[param]] <- NA
          }
        }
      }
      return(complete_list)
    },

    #' @description
    #' Returns a list of attributes necessary to simulate the model that are incomplete/inconsistent/invalid.
    #' @param include_nas Optional boolean indicating whether of not to include attributes with unknown completeness (NA).
    #' @return List of incomplete attributes which prevent the model simulation (and optionally those where completeness is not available).
    incomplete_attributes = function(include_nas = FALSE) {
      complete <- unlist(self$list_completeness())
      incomplete <- if (length(which(complete == FALSE))) {
        names(which(complete == FALSE))
      } else {
        NULL
      }
      not_available <- if (length(which(is.na(complete)))) {
        names(which(is.na(complete)))
      } else {
        NULL
      }
      if (include_nas) {
        return(list(incomplete = incomplete, not_available = not_available))
      } else {
        return(incomplete)
      }
    }
  ), # end public

  private = list(
    ## Attributes ##

    # Associated (default) simulation function #
    .simulation_function = NULL,

    # Model attributes #
    .model_attributes = c(
      "region",
      "coordinates",
      "random_seed",
      "replicates",
      "time_steps",
      "years_per_step",
      "results_selection"
    ),
    # .region            [inherited]
    .random_seed = NULL,
    .replicates = 1,
    .time_steps = NULL,
    .years_per_step = 1,
    .results_selection = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c(
      "region",
      "coordinates",
      "random_seed",
      "replicates",
      "time_steps",
      "years_per_step",
      "results_selection"
    ),

    # Dynamic attributes #

    # .attribute_aliases [inherited]

    # Template model for fixed (non-sampled) attributes for shallow cloning
    .template_model = NULL,

    # Vector of sample attributes (names)
    .sample_attributes = NULL,

    # Vector of required attributes (names)
    .required_attributes = c("region", "time_steps", "results_selection")

    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]
  ), # end private

  # Active binding accessors for private model attributes (above) #
  active = list(
    # Associated (default) simulation function #

    #' @field simulation_function Name (character string) or source path of the default simulation function, which takes a model as an input and returns the simulation results.
    simulation_function = function(value) {
      if (missing(value)) {
        private$.simulation_function
      } else {
        private$.simulation_function <- value
      }
    },

    # Model attribute accessors #

    #' @field model_attributes A vector of model attribute names.
    model_attributes = function(value) {
      if (missing(value)) {
        if (!is.null(self$template_model)) {
          self$template_model$model_attributes
        } else {
          private$.model_attributes
        }
      } else {
        if (!is.null(self$template_model)) {
          self$template_model$model_attributes <- value
        } else {
          private$.model_attributes <- value
        }
      }
    },

    #' @field region A \code{\link{Region}} (or inherited class) object specifying the study region.
    region = function(value) {
      if (missing(value)) {
        if (!is.null(self$template_model)) {
          self$template_model$region
        } else {
          private$.region
        }
      } else {
        if (!is.null(self$template_model)) {
          self$template_model$region <- value
        } else {
          if ("Region" %in% class(value)) {
            if (
              (value$use_raster && is.null(value$region_raster)) ||
                (!value$use_raster && is.null(value$coordinates))
            ) {
              warning(
                "Spatial region has not been defined within the region object",
                call. = FALSE
              )
            }
          } else if (!is.null(value)) {
            stop(
              "Region should be a Region (or inherited class) object",
              call. = FALSE
            )
          }
          private$.region <- value
        }
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

    #' @field random_seed Number to seed the random number generation for stochasticity.
    random_seed = function(value) {
      if (
        is.null(self$template_model) ||
          "random_seed" %in% self$sample_attributes
      ) {
        if (missing(value)) {
          private$.random_seed
        } else {
          private$.random_seed <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$random_seed
        } else {
          self$template_model$random_seed <- value
        }
      }
    },

    #' @field replicates Number of replicate simulation runs.
    replicates = function(value) {
      if (missing(value)) {
        if (
          is.null(self$template_model) ||
            "replicates" %in% self$sample_attributes
        ) {
          private$.replicates
        } else {
          self$template_model$replicates
        }
      } else {
        if (
          is.null(self$template_model) ||
            "replicates" %in% self$sample_attributes
        ) {
          private$.replicates <- value
        } else {
          self$template_model$replicates <- value
        }
      }
    },

    #' @field time_steps Number of simulation time steps.
    time_steps = function(value) {
      if (missing(value)) {
        if (
          is.null(self$template_model) ||
            "time_steps" %in% self$sample_attributes
        ) {
          private$.time_steps
        } else {
          self$template_model$time_steps
        }
      } else {
        if (
          is.null(self$template_model) ||
            "time_steps" %in% self$sample_attributes
        ) {
          private$.time_steps <- value
        } else {
          self$template_model$time_steps <- value
        }
      }
    },

    #' @field years_per_step Number of years per time step.
    years_per_step = function(value) {
      if (missing(value)) {
        if (
          is.null(self$template_model) ||
            "years_per_step" %in% self$sample_attributes
        ) {
          private$.years_per_step
        } else {
          self$template_model$years_per_step
        }
      } else {
        if (
          is.null(self$template_model) ||
            "years_per_step" %in% self$sample_attributes
        ) {
          private$.years_per_step <- value
        } else {
          self$template_model$years_per_step <- value
        }
      }
    },

    #' @field results_selection List of simulator-dependent attributes to be included in the returned results of each simulation run.
    results_selection = function(value) {
      if (missing(value)) {
        if (
          is.null(self$template_model) ||
            "results_selection" %in% self$sample_attributes
        ) {
          private$.results_selection
        } else {
          self$template_model$results_selection
        }
      } else {
        if (
          is.null(self$template_model) ||
            "results_selection" %in% self$sample_attributes
        ) {
          private$.results_selection <- value
        } else {
          self$template_model$results_selection <- value
        }
      }
    },

    # Dynamic attribute accessors #

    #' @field attribute_aliases A list of alternative alias names for model attributes (form: \code{alias = "attribute"}) to be used with the set and get attributes methods.
    attribute_aliases = function(value) {
      if (missing(value)) {
        if (!is.null(self$template_model)) {
          self$template_model$attribute_aliases
        } else {
          private$.attribute_aliases
        }
      } else {
        if (!is.null(value) && !is.list(value)) {
          stop(
            "Attribute aliases should be a list (form: alias = \"attribute\")",
            call. = FALSE
          )
        }
        if (!is.null(self$template_model)) {
          self$template_model$attribute_aliases <- value
        } else {
          private$.attribute_aliases <- value
        }
      }
    },

    #' @field template_model Nested template model for fixed (non-sampled) attributes for shallow cloning.
    template_model = function(value) {
      if (missing(value)) {
        private$.template_model
      } else {
        if ("SimulationModel" %in% class(value)) {
          private$.template_model <- value
        } else {
          # construct a model having the same (potentially inherited) class as the template
          private$.template_model <- self$object_generator$new(
            params = as.list(value)
          )
        }
      }
    },

    #' @field sample_attributes Vector of sample attribute names (only).
    sample_attributes = function(value) {
      if (missing(value)) {
        private$.sample_attributes
      } else {
        private$.sample_attributes <- value
      }
    },

    #' @field required_attributes Vector of required attribute names (only), i.e. those needed to run a simulation.
    required_attributes = function(value) {
      if (missing(value)) {
        if (!is.null(self$template_model)) {
          self$template_model$required_attributes
        } else {
          private$.required_attributes
        }
      } else {
        if (!is.null(self$template_model)) {
          self$template_model$required_attributes <- value
        } else {
          private$.required_attributes <- value
        }
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
