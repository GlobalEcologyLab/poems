#' R6 class representing a model simulator.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class for running individual model simulations via a
#' simulation function, storing results, and generating success/error statuses.
#'
#' @examples
#' # Simulation model
#' model1 <- SimulationModel$new(
#'   time_steps = 10,
#'   model_attributes = c("time_steps", "a", "b"),
#'   params = list(a = 1:7)
#' )
#' model1$required_attributes <- model1$model_attributes
#' # Simulation function
#' test_simulator <- function(model) {
#'   sum(unlist(model$get_attributes(model$required_attributes)))
#' }
#' # Model simulator
#' simulator1 <- ModelSimulator$new(
#'   simulation_model = model1,
#'   simulation_function = test_simulator
#' )
#' simulator1$run()
#' model1$set_attributes(a = 1:10, b = 15)
#' model1$get_attributes(model1$required_attributes)
#' simulator1$run()
#' simulator1$results
#'
#' @importFrom R6 R6Class
#' @include GenericClass.R
#' @export ModelSimulator

ModelSimulator <- R6Class("ModelSimulator",
  inherit = GenericClass,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets the population model, and optionally the simulation function, the sample ID, and any attached attributes listed individually.
    #' @param simulation_model A \code{\link{SimulationModel}} (or inherited class) object (can be set later).
    #' @param simulation_function Optional name (character string) or direct assignment (assigned or loaded via source path) of the simulation function, which takes a \code{\link{SimulationModel}} (or inherited class) as an input and returns the simulation results.
    #' @param sample_id Optional identifier for the simulation sample.
    #' @param ... Additional parameters passed individually are attached.
    initialize = function(simulation_model = NULL, simulation_function = NULL, sample_id = NULL, ...) {
      super$initialize(...)
      self$simulation_model <- simulation_model
      self$simulation_function <- simulation_function
      self$sample_id <- sample_id
      self$attached <- list(...)
      self$attached$object_generator <- NULL
    },

    #' @description
    #' Creates a new (re-initialized) object of the current (inherited) object class with optionally passed parameters.
    #' @param ... Parameters passed via the inherited class constructor (defined in initialize and run via new).
    #' @return New object of the current (inherited) class.
    new_clone = function(...) {
      return(super$new_clone(simulation_function = self$simulation_function, ...))
    },

    # New methods #

    #' @description
    #' Returns selected named simulator or attached attribute.
    #' @param param Name of the parameter/attribute.
    #' @return Selected parameter/attribute value.
    get_attribute = function(param) {
      if (param %in% private$.simulator_attributes) {
        return(eval(parse(text = sprintf("self$%s", param))))
      } else if (param %in% names(self$attached)) {
        return(self$attached[[param]])
      } else {
        return(NULL)
      }
    },

    #' @description
    #' Runs a model simulator (function), stores the results, and creates a status log entry as a list.
    #' @return A list representing a simulation log entry with a \emph{successful} boolean and a status message template (with a placeholder for the sample identifier).
    run = function() {
      self$results <- NULL
      self$attached$warnings <- NULL
      if (is.null(self$simulation_function)) {
        stop("The simulation function needs to be set before the simulation can be run", call. = FALSE)
      }
      if (is.null(self$simulation_model)) {
        stop("The simulation model needs to be set before the simulation can be run", call. = FALSE)
      }
      if (self$simulation_model$is_complete()) {
        run_status <- NULL
        run_status <- tryCatch(
          {
            suppressWarnings(
              withCallingHandlers(
                {
                  if (is.function(self$simulation_function)) {
                    self$results <- self$simulation_function(self$simulation_model)
                  } else { # assume character name
                    self$results <- eval(parse(text = as.character(self$simulation_function)))(self$simulation_model)
                  }
                },
                warning = function(w) {
                  self$attached$warnings <- c(
                    self$attached$warnings,
                    gsub("simpleWarning", "Warning",
                      gsub("\n", "", as.character(w), fixed = TRUE),
                      fixed = TRUE
                    )
                  )
                }
              )
            )
            if (!is.null(self$attached$warnings)) {
              list(
                successful = TRUE, message = "Model %s simulation ran successfully with warnings",
                warnings = self$attached$warnings
              )
            } else {
              list(successful = TRUE, message = "Model %s simulation ran successfully")
            }
          },
          error = function(e) {
            list(
              successful = FALSE, message = "Model %s simulation ran unsuccessfully with errors",
              errors = c(as.character(e))
            )
          }
        )
        if (is.null(run_status)) {
          run_status <- list(successful = FALSE, message = "Model %s simulation had unknown failure without errors")
        }
        return(run_status)
      } else {
        incomplete_message <- "Model %s attributes are incomplete"
        if (!self$simulation_model$is_consistent()) {
          incomplete_message <- paste(incomplete_message, "/inconsistent", sep = "")
        }
        incomplete_message <- paste0(incomplete_message, ": ", paste(self$simulation_model$incomplete_attributes(), collapse = ", "))
        return(list(successful = FALSE, message = incomplete_message))
      }
    }
  ), # end public

  private = list(

    ## Attributes ##

    # Simulator attributes #
    .simulator_attributes = c("simulation_model", "simulation_function", "sample_id", "results"),
    .simulation_model = NULL,
    .simulation_function = NULL,
    .sample_id = NULL,
    .results = NULL
  ), # end private

  # Active binding accessors for private simulator attributes (above) #
  active = list(

    #' @field simulation_model A SimulationModel object or an inherited class object.
    simulation_model = function(value) {
      if (missing(value)) {
        private$.simulation_model
      } else {
        if (!is.null(value) && !("SimulationModel" %in% class(value))) {
          stop("Model must be a SimulationModel or inherited class object", call. = FALSE)
        } else {
          private$.simulation_model <- value
        }
      }
    },

    #' @field simulation_function Name (character string) or direct assignment (assigned or loaded via source path) of the simulation function, which takes a \code{\link{SimulationModel}} (or inherited class) as an input and returns the simulation results.
    simulation_function = function(value) {
      if (missing(value)) {
        private$.simulation_function
      } else {
        if (is.character(value) && file.exists(value) && length(grep(".R", toupper(value), fixed = TRUE))) {
          tryCatch(
            {
              simulation_function <- source(value)$value # direct assignment from a file
            },
            error = function(e) {
              stop(paste("Error loading function from file", value, ":", as.character(e)), call. = FALSE)
            }
          )
          if (is.function(simulation_function)) {
            private$.simulation_function <- simulation_function
          } else {
            stop(paste("Could not assign function", value), call. = FALSE)
          }
        } else { # character or direct assignment
          if (is.null(value) || is.function(value) ||
            (is.character(value) &&
              tryCatch(is.function(eval(parse(text = value))), error = function(e) FALSE))) {
            if (is.character(value)) {
              value <- eval(parse(text = value))
            }
            private$.simulation_function <- value
          } else {
            stop(paste("Could not assign function", value), call. = FALSE)
          }
        }
      }
    },

    #' @field sample_id An identifier for the simulation sample.
    sample_id = function(value) {
      if (missing(value)) {
        private$.sample_id
      } else {
        private$.sample_id <- value
      }
    },

    #' @field results A list of result structures.
    results = function(value) {
      if (missing(value)) {
        private$.results
      } else {
        private$.results <- value
      }
    }
  ) # end active
)
