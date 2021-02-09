#' R6 class representing a pattern-oriented validator.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class for pattern-oriented validation and simulation
#' model ensemble selection. The class wraps functionality for the validation approach,
#' typically utilizing an external library, the default being the approximate Bayesian
#' computation (ABC) \code{\link[abc:abc]{abc}} library, and includes methods for
#' resolving non-finite metrics, centering and scaling the validator inputs, running
#' the validator analysis, and generating diagnostics (see \code{\link[abc:abc]{abc}}).
#'
#' @examples
#' # Example parameter sample data
#' sample_data <- data.frame(growth_rate_max = round(log(seq(1.11, 1.30, 0.01)), 3),
#'                           harvest_rate = seq(0.11, 0.30, 0.01),
#'                           initial_n = seq(105, 200, 5),
#'                           density_max = seq(132, 170, 2))
#' # Example simulation result summary metrics
#' summary_metric_data <- data.frame(trend_n = seq(10, -9, -1),
#'                                   total_h = seq(70, 355, 15))
#' # Create a validator for selecting the 'best' example models
#' validator <- Validator$new(simulation_parameters = sample_data,
#'                            simulation_summary_metrics = summary_metric_data,
#'                            observed_metric_targets = c(trend_n = 0, total_h = 250),
#'                            output_dir = tempdir())
#' suppressWarnings(validator$run(tolerance = 0.25, output_diagnostics = TRUE))
#' dir(tempdir(), "*.pdf") # plus validation diagnostics (see abc library documentation)
#' validator$selected_simulations # top 5 models
#'
#' @importFrom R6 R6Class
#' @include GenericModel.R
#' @export Validator

Validator <- R6Class("Validator",
  inherit = GenericModel,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericClass & GenericModel) #
    #   new_clone(...)
    #   get_attribute_names()
    #   get_attributes(params = NULL)
    #   get_attribute(param)
    #   get_attribute_aliases(params = NULL)
    #   set_attributes((params = list(), ...))

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets given attributes individually and/or from a list.
    #' @param template Template population model containing fixed (non-sampled) attributes.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(...) {
      self$input_center_scale_values <- list()
      super$initialize(...)
      if (is.null(self$validation_call_function)) {
        self$validation_call_function <- function(observed_metric_targets = NULL, simulation_parameters = NULL,
                                                  simulation_summary_metrics = NULL, tolerance = NULL, method = NULL, ...) {
          abc::abc(
            target = observed_metric_targets,
            param = simulation_parameters,
            sumstat = simulation_summary_metrics,
            tol = tolerance, method = method, ...)
        }
      }
    },

    # New methods #

    #' @description
    #' Pre-processes inputs, runs validator function for input parameters, and stores the function (and optionally diagnostic) outputs (see \code{\link[abc:abc]{abc}} documentation if using the default).
    #' @param simulation_parameters A data frame of sample model parameters for each simulation.
    #' @param simulation_summary_metrics A data frame of result summary metrics for each simulation.
    #' @param observed_metric_targets A vector of observed targets for each summary metric.
    #' @param tolerance Tolerance or proportion of models to select.
    #' @param method Validator algorithm to be applied (default is a neural network algorithm - see \code{\link[abc:abc]{abc}} documentation) .
    #' @param output_diagnostics Boolean to indicate whether or not to output diagnostics (PDF file - default is FALSE).
    #' @param ... Additional validator parameters passed individually (see \code{\link[abc:abc]{abc}} documentation if using default).
    run = function(simulation_parameters = NULL, simulation_summary_metrics = NULL, observed_metric_targets = NULL, tolerance = 0.01, method = "neuralnet", output_diagnostics = FALSE, ...) {

      # Ensure sample model parameters, result summary metrics and observed targets are present
      if (!is.null(simulation_parameters)) {
        self$simulation_parameters <- simulation_parameters
      }
      if (!is.null(simulation_summary_metrics)) {
        self$simulation_summary_metrics <- simulation_summary_metrics
      }
      if (!is.null(observed_metric_targets)) {
        self$observed_metric_targets <- observed_metric_targets
      }
      missing_parameters <- names(which(unlist(list(simulation_parameters = is.null(self$simulation_parameters),
                                                    simulation_summary_metrics = is.null(self$simulation_summary_metrics),
                                                    observed_metric_targets = is.null(self$observed_metric_targets)))))
      if (length(missing_parameters)) {
        stop(paste("Validator run requires parameters to be set first:", paste(missing_parameters, collapse = ", ")), call. = FALSE)
      }

      # Attempt to resolve any non-finite simulation summary metric values
      resolved_status <- self$resolve_nonfinite_metrics()
      if ("stop" %in% names(resolved_status)) {
        stop(resolved_status$stop, call. = FALSE)
      }

      # Center and scale the model parameters, result summary metrics and observed targets
      self$center_scale_inputs()

      # Randomize the simulation sample indices for the validator inputs
      if (!is.null(self$random_seed)) {
        set.seed(self$random_seed)
        self$random_indices <- sample(x = 1:nrow(self$simulation_parameters), size = nrow(self$simulation_parameters), replace = FALSE)
      } else {
        self$random_indices <- 1:nrow(self$simulation_parameters)
      }

      # Run the validator function
      run_status <- tryCatch({
        suppressWarnings(
          withCallingHandlers({
            self$validator_return_object <- self$validation_call_function(
              observed_metric_targets = self$observed_metric_targets,
              simulation_parameters = self$simulation_parameters[self$random_indices,],
              simulation_summary_metrics = self$simulation_summary_metrics[self$random_indices,],
              tolerance = tolerance, method = method, ...)
          }, warning = function(w) {
            self$warning_messages <- gsub("simpleWarning", "Validation function generated warning",
                                          gsub("\n", "", as.character(w), fixed = TRUE),
                                          fixed = TRUE)
          })
        )
      },
      error = function(e){
        list(stop = sprintf("Validation function failed. %s", as.character(e)))
      })
      if (!is.null(self$warning_messages)) {
        warning(self$warning_messages, call. = FALSE)
        self$warning_messages <- NULL
      }
      if ("stop" %in% names(run_status)) {
        stop(run_status$stop, call. = FALSE)
      }

      # Generate diagnostics
      if (output_diagnostics) {
        if (!is.null(self$output_dir)) {
          if (length(grep("abc::abc", deparse(self$validation_call_function), fixed = TRUE)) > 0) {
            if (method %in% c("loclinear", "neuralnet")) {
              self$generate_diagnostics()
            } else {
              warning("ABC diagnostics can only be generated with ABC method neuralnet or loclinear", call. = FALSE)
            }
          } else {
            self$generate_diagnostics()
          }
        } else {
          warning("The output directory needs to be set before diagnostics can be generated", call. = FALSE)
        }
      }

    },

    #' @description
    #' Attempts to resolve any non-finite simulation summary metric values (and optionally changing them to NAs) via the non finite replacements parameter (a list of values/functions for replacing non-finite values).
    #' @param use_nas Boolean to indicate whether or not to replace all non-finite values with NAs (default is TRUE).
    resolve_nonfinite_metrics = function (use_nas = TRUE) {

      # Check for non-finite values in the simulation summary metrics
      noninfinite_indices <- which(!is.finite(as.matrix(self$simulation_summary_metrics)), arr.ind = TRUE)
      nonfinite_columns <- names(self$simulation_summary_metrics)[unique(noninfinite_indices[, 2])]

      # Attempt to resolve non-finite values
      if (length(nonfinite_columns)) {

        # Check if appropriate replacements are present
        if (is.null(self$non_finite_replacements)) {
          unresolvable_columns <- nonfinite_columns
        } else {
          unresolvable_columns <- nonfinite_columns[which(!nonfinite_columns %in% names(self$non_finite_replacements))]
        }
        if (length(unresolvable_columns)) {
          return(list(stop = paste("Non-finite simulation summary metric values need to be resolved (via appropriate replacements) for:", paste(unresolvable_columns, collapse = ", "))))
        }

        # First replace all non-finite values with NAs
        if (use_nas) {
          self$simulation_summary_metrics[noninfinite_indices] <- NA
        }

        # Apply non-finite replacements
        for (column in nonfinite_columns) {
          replacement <- self$non_finite_replacements[[column]]
          if (is.character(replacement)) { # apply function as a string
            resolved_status <- tryCatch({
              if (length(grep("Primitive", deparse(eval(parse(text = replacement))), fixed = TRUE))) {
                replacement_value <- eval(parse(text = replacement))(self$simulation_summary_metrics[, column], na.rm = TRUE)
              } else {
                replacement_value <- eval(parse(text = replacement))(self$simulation_summary_metrics[, column])
              }
            },
            error = function(e){
              list(stop = sprintf("Could not apply non-finite replacement for metric %s. %s", column, as.character(e)))
            })
            if ("stop" %in% names(resolved_status)) {
              return(resolved_status)
            }
          } else if (is.function(replacement)) { # apply function directly to column
            resolved_status <- tryCatch({
              replacement_value <- replacement(self$simulation_summary_metrics[, column])
            },
            error = function(e){
              list(stop = sprintf("Could not apply non-finite replacement for metric %s. %s", column, as.character(e)))
            })
            if ("stop" %in% names(resolved_status)) {
              return(resolved_status)
            }
          } else { # use value (assume numeric)
            replacement_value <- replacement
          }
          self$simulation_summary_metrics[which(!is.finite(self$simulation_summary_metrics[, column])), column] <- replacement_value
        }
      }
    },

    #' @description
    #' Centers and scales the model parameters, result summary metrics and observed targets.
    center_scale_inputs = function() {

      # Scale and center simulation parameters (if not already scaled and centered)
      if (!("simulation_parameters" %in% names(self$input_center_scale_values)) ||
          !all(c("center", "scale") %in% names(self$input_center_scale_values$simulation_parameters))) {
        parameters <- scale(self$simulation_parameters, center = TRUE, scale = TRUE)
        self$simulation_parameters <- parameters[,]
        self$input_center_scale_values$simulation_parameters <- list(center = attr(parameters, "scaled:center"), scale = attr(parameters, "scaled:scale"))
      }

      # Scale and center simulation summary metrics and observed metric targets together (if not already scaled and centered)
      if (!all(c("simulation_summary_metrics", "observed_metric_targets") %in% names(self$input_center_scale_values)) ||
          !all(c("center", "scale") %in% names(self$input_center_scale_values$simulation_summary_metrics)) ||
          !all(c("center", "scale") %in% names(self$input_center_scale_values$observed_metric_targets))) {
        metrics <- scale(rbind(self$simulation_summary_metrics, self$observed_metric_targets), center = TRUE, scale = TRUE)
        self$simulation_summary_metrics <- metrics[-nrow(metrics),]
        self$observed_metric_targets <- metrics[nrow(metrics),]
        self$input_center_scale_values$simulation_summary_metrics <- list(center = attr(metrics, "scaled:center"), scale = attr(metrics, "scaled:scale"))
        self$input_center_scale_values$observed_metric_targets <- list(center = attr(metrics, "scaled:center"), scale = attr(metrics, "scaled:scale"))
      }

    },

    #' @description
    #' Generates the validation diagnostics (see \code{\link[abc:abc]{abc}} documentation if using default) as a PDF file in the output directory.
    #' @param output_dir Output directory path for the diagnostics PDF file (must be present if not already set within validator class object).
    generate_diagnostics = function(output_dir = NULL) {

      # Ensure the validator return object and output directory are set
      if (!is.null(output_dir)) {
        self$output_dir <- output_dir
      }
      missing_messages <- c("validation function to be run", "and the", "output directory to be set")
      missing_messages <- paste(missing_messages[which(c(is.null(self$validator_return_object),
                                                         is.null(self$validator_return_object) && is.null(self$output_dir),
                                                         is.null(self$output_dir)))], collapse = " ")
      if (missing_messages != "") {
        stop(sprintf("Diagnostics generation requires the %s first", missing_messages), call. = FALSE)
      }

      if (length(grep("abc::abc", deparse(self$validation_call_function), fixed = TRUE)) > 0) {
        # Diagnostics plot can only be generated when ABC method is "loclinear" or "neuralnet"
        if (!(self$validator_return_object$method %in% c("loclinear", "neuralnet"))) {
          stop("Validation diagnostics can only be generated when ABC methods neuralnet or loclinear were utilized", call. = FALSE)
        }
      }

      # Plot diagnostics to PDF file
      diag_status <- tryCatch({
        suppressWarnings(
          withCallingHandlers({
            grDevices::pdf(file = file.path(self$output_dir, "validation_diagnostics.pdf"), onefile = TRUE)
            plot(self$validator_return_object, self$simulation_parameters, subsample = nrow(self$simulation_parameters), ask = FALSE)
            pdf_device_off <- dev.off()
          }, warning = function(w) {
            self$warning_messages <- gsub("simpleWarning", "Validation diagnostics generation warning",
                                          gsub("\n", "", as.character(w), fixed = TRUE),
                                          fixed = TRUE)
          })
        )
      },
      error = function(e){
        list(stop = sprintf("Validation diagnostics generation failed. %s", as.character(e)))
      })
      if (!is.null(self$warning_messages)) {
        warning(self$warning_messages, call. = FALSE)
        self$warning_messages <- NULL
      }
      if ("stop" %in% names(diag_status)) {
        stop(diag_status$stop, call. = FALSE)
      }

    }

  ), # end public

  private = list(

    ## Attributes ##

    # Model attributes #
    .model_attributes = c("simulation_parameters", "simulation_summary_metrics", "observed_metric_targets",
                          "random_seed", "random_indices", "non_finite_replacements",
                          "input_center_scale_values", "output_dir", "validation_call_function",
                          "validator_return_object", "selected_simulations"),
    .simulation_parameters = NULL,
    .simulation_summary_metrics = NULL,
    .observed_metric_targets = NULL,
    .random_seed = NULL,
    .random_indices = NULL,
    .non_finite_replacements = NULL,
    .input_center_scale_values = NULL,
    .output_dir = NULL,
    .validation_call_function = NULL,
    .validator_return_object = NULL,
    .selected_simulations = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("simulation_parameters", "simulation_summary_metrics", "observed_metric_targets",
                           "random_seed", "random_indices", "non_finite_replacements",
                           "input_center_scale_values", "output_dir", "validation_call_function",
                           "validator_return_object", "selected_simulations")

    # Dynamic attributes #
    # .attribute_aliases [inherited]

    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]

  ), # end private

  # Active binding accessors for private model attributes (above)
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

    #' @field simulation_parameters A data frame of sample model parameters for each simulation.
    simulation_parameters = function(value) {
      if (missing(value)) {
        private$.simulation_parameters
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            value <- utils::read.csv(file = value)
          } else if (length(grep(".RDATA", toupper(value), fixed = TRUE)) || length(grep(".RDS", toupper(value), fixed = TRUE))) {
            value <- readRDS(file = value)
          } else {
            value <- utils::read.table(file = value)
          }
        }
        if (!is.null(value) && (is.data.frame(value) || is.matrix(value))) {
          if (!is.null(self$simulation_summary_metrics) &&
              nrow(self$simulation_summary_metrics) != nrow(value)) {
            stop("The simulation parameters must be have the same number of rows as the existing simulation summary metrics", call. = FALSE)
          }
          if (!is.null(self$simulation_parameters) && "simulation_parameters" %in% names(self$input_center_scale_values)) {
            self$input_center_scale_values$simulation_parameters <- NULL
          }
          private$.simulation_parameters <- as.data.frame(value)
        } else if (is.null(value)) {
          self$input_center_scale_values$simulation_parameters <- NULL
          private$.simulation_parameters <- value
        } else {
          stop("The simulation parameters must be a data frame (or matrix)", call. = FALSE)
        }
      }
    },

    #' @field simulation_summary_metrics A data frame of result summary metrics for each simulation.
    simulation_summary_metrics = function(value) {
      if (missing(value)) {
        private$.simulation_summary_metrics
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            value <- utils::read.csv(file = value)
          } else if (length(grep(".RDATA", toupper(value), fixed = TRUE)) || length(grep(".RDS", toupper(value), fixed = TRUE))) {
            value <- readRDS(file = value)
          } else {
            value <- utils::read.table(file = value)
          }
        }
        if (!is.null(value) && (is.data.frame(value) || is.matrix(value))) {
          if (!is.null(self$simulation_parameters) && nrow(self$simulation_parameters) != nrow(value)) {
            stop("The simulation summary metrics must be have the same number of rows as the existing simulation parameters", call. = FALSE)
          } else if (!is.null(self$observed_metric_targets) && length(self$observed_metric_targets) != ncol(value)) {
            stop("The simulation summary metrics must be have the same number of metrics (columns) as the existing observed metric targets", call. = FALSE)
          } else if (!is.null(self$observed_metric_targets)) {
            if (!is.null(names(value)) && !is.null(names(self$observed_metric_targets))) {
              if (!all(names(value) %in% names(self$observed_metric_targets))) {
                stop("The simulation summary metric names must correspond to those in the existing observed target metrics", call. = FALSE)
              } else if (!all(names(value) == names(self$observed_metric_targets))) {
                # Re-order existing observed targets to match summary metrics
                private$.observed_metric_targets <- private$.observed_metric_targets[names(value)]
              }
            } else if (is.null(names(value)) && !is.null(names(self$observed_metric_targets))) {
              # Name targets via simulation summary metrics
              names(value) <- names(self$observed_metric_targets)
            } else if (!is.null(names(value)) && is.null(names(self$observed_metric_targets))) {
              # Name simulation summary metrics via targets
              names(private$.observed_metric_targets) <- names(value)
            } else {
              stop("Metrics have not been named", call. = FALSE)
            }
          }
          if (!is.null(self$simulation_summary_metrics) && "simulation_summary_metrics" %in% names(self$input_center_scale_values)) {
            self$input_center_scale_values$simulation_summary_metrics <- NULL
          }
          private$.simulation_summary_metrics <- as.data.frame(value)
        } else if (is.null(value)) {
          self$input_center_scale_values$simulation_summary_metrics <- NULL
          private$.simulation_summary_metrics <- value
        } else {
          stop("The simulation summary metrics must be a data frame (or matrix)", call. = FALSE)
        }
      }
    },

    #' @field observed_metric_targets A vector of observed targets for each summary metric.
    observed_metric_targets = function(value) {
      if (missing(value)) {
        private$.observed_metric_targets
      } else {
        if (!is.null(value)) {
          if (is.list(value)) {
            value <- unlist(value)
          }
          if (!is.null(self$simulation_summary_metrics) && ncol(self$simulation_summary_metrics) != length(value)) {
            stop("The observed metric targets must be have the same number of metrics as the existing simulation summary metrics (columns)", call. = FALSE)
          } else if (!is.null(self$simulation_summary_metrics)) {
            if (!is.null(names(value)) && !is.null(names(self$simulation_summary_metrics))) {
              if (!all(names(value) %in% names(self$simulation_summary_metrics))) {
                stop("The observed target metric names must correspond to those in the existing simulation summary metrics", call. = FALSE)
              } else if (!all(names(value) == names(self$simulation_summary_metrics))) {
                # Re-order targets to match simulation summary metrics
                value <- value[names(self$simulation_summary_metrics)]
              }
            } else if (is.null(names(value)) && !is.null(names(self$simulation_summary_metrics))) {
              # Name targets via simulation summary metrics
              names(value) <- names(self$simulation_summary_metrics)
            } else if (!is.null(names(value)) && is.null(names(self$simulation_summary_metrics))) {
              # Name simulation summary metrics via targets
              names(private$.simulation_summary_metrics) <- names(value)
            } else {
              stop("Metrics have not been named", call. = FALSE)
            }
          }
          if (any(c("scaled:center", "scaled:scale") %in% names(attributes(value)))) {
            center_scale_values <- list(center = attr(value, "scaled:center"), scale = attr(value, "scaled:scale"))
            # Ensure summary metrics have the same centering and scaling when present
            if ("simulation_summary_metrics" %in% self$input_center_scale_values &&
                !all(unlist(center_scale_values) == unlist(self$input_center_scale_values$simulation_summary_metrics))) {
              self$input_center_scale_values$observed_metric_targets <- NULL
            } else {
              self$input_center_scale_values$observed_metric_targets <- center_scale_values
            }
            attributes(value)[c("scaled:center", "scaled:scale")] <- NULL
          }
          if (!is.null(self$observed_metric_targets) && "observed_metric_targets" %in% names(self$input_center_scale_values)) {
            self$input_center_scale_values$observed_metric_targets <- NULL
          }
          private$.observed_metric_targets <- value
        } else { # NULL
          self$input_center_scale_values$observed_metric_targets <- NULL
          private$.observed_metric_targets <- value
        }
      }
    },

    #' @field random_seed A seed for randomizing the order of the simulation samples (no randomization is utilized when left NULL).
    random_seed = function(value) {
      if (missing(value)) {
        private$.random_seed
      } else {
        private$.random_seed <- value
      }
    },

    #' @field random_indices Randomized simulation sample indices for the validator inputs and consequently the validator results when random seed is used.
    random_indices = function(value) {
      if (missing(value)) {
        private$.random_indices
      } else {
        private$.random_indices <- value
      }
    },

    #' @field non_finite_replacements A list of numeric values or function names (character strings) or direct assignments (assigned or loaded via source paths) for replacing NAs in specified (list names) summary metrics.
    non_finite_replacements = function(value) {
      if (missing(value)) {
        private$.non_finite_replacements
      } else {
        if (is.null(value)) {
          private$.non_finite_replacements <- value
        } else if (!is.list(value)) {
          stop("Non-finite replacements should be a list", call. = FALSE)
        }
        if (!is.null(self$simulation_summary_metrics)) { # only include those matching summary metrics
          nonmatching_metrics <- names(value)[which(!(names(value) %in% names(self$simulation_summary_metrics)))]
          if (length(nonmatching_metrics)) {
            warning(paste("Ignoring replacements not found in summary metrics:", paste(nonmatching_metrics, collapse = ", ")), call. = FALSE)
            value[nonmatching_metrics] <- NULL
          }
        }
        replacements <- list()
        for (metric in names(value)) {
          metric_value <- value[[metric]]
          if (is.character(metric_value) && file.exists(metric_value) && length(grep(".R", toupper(metric_value), fixed = TRUE))) {
            tryCatch({
              replacement_function <- source(metric_value)$value # direct assignment from a file
            },
            error = function(e){
              stop(paste("Error loading function from file", metric_value, ":", as.character(e)), call. = FALSE)
            })
            if (is.function(replacement_function)) {
              replacements[[metric]] <- replacement_function
            } else {
              stop(paste("Could not assign function", metric_value), call. = FALSE)
            }
          } else { # numeric, character or direct assignment
            if (is.numeric(metric_value) || is.function(metric_value) ||
                (is.character(metric_value) &&
                 tryCatch(is.function(eval(parse(text = metric_value))), error = function(e) FALSE))) {
              if (is.function(metric_value) && length(grep("Primitive", deparse(metric_value), fixed = TRUE))) {
                metric_value <- strsplit(deparse(metric_value), "\"")[[1]][2] # convert primitives to string
              }
              replacements[[metric]] <- metric_value
            } else {
              stop(paste("Could not assign function", metric_value), call. = FALSE)
            }
          }
        }
        private$.non_finite_replacements <- replacements
      }
    },

    #' @field input_center_scale_values A nested list of center and scale values for validator input parameters/metrics.
    input_center_scale_values = function(value) {
      if (missing(value)) {
        private$.input_center_scale_values
      } else {
        private$.input_center_scale_values <- value
      }
    },

    #' @field output_dir Directory path for validator (default: \code{\link[abc:abc]{abc}}) regression diagnostic and other outputs.
    output_dir = function(value) {
      if (missing(value)) {
        private$.output_dir
      } else {
        if (!is.null(value)) {
          value <- as.character(value)
          if (!dir.exists(value)) { # create directory
            stop(paste("Could not find output directory", value), call. = FALSE)
          }
        }
        private$.output_dir <- value
      }
    },

    #' @field validation_call_function Dynamically assigned function: \code{function(observed_metric_targets, simulation_parameters, simulation_summary_metrics, tolerance, method, ...)} for calling the validation function (default calls \code{\link[abc:abc]{abc}} library function).
    validation_call_function = function(value) {
      if (missing(value)) {
        private$.validation_call_function
      } else {
        if (is.function(value)) {
          private$.validation_call_function <- value
        } else {
          stop("The validation call function must be a function definition: function(observed_metric_targets, simulation_parameters, simulation_summary_metrics, tolerance, method, ...)", call. = FALSE)
        }
      }
    },

    #' @field validator_return_object Object returned by the validator function (see \code{\link[abc:abc]{abc}} documentation if using default).
    validator_return_object = function(value) {
      if (missing(value)) {
        private$.validator_return_object
      } else {
        private$.validator_return_object <- value
      }
    },

    #' @field selected_simulations A data frame of simulation sample indices and weights selected/assigned by the validation function (\code{\link[abc:abc]{abc}} by default).
    selected_simulations = function(value) {
      if (missing(value)) {
        if (!is.null(self$random_indices) && !is.null(self$validator_return_object) &&
            !is.null(self$validator_return_object$weights) && !is.null(self$validator_return_object$region)) {
          selected_simulations <- data.frame(index = self$random_indices[which(self$validator_return_object$region)],
                                             weight = self$validator_return_object$weights)
          selected_simulations <- selected_simulations[order(selected_simulations$index),]
          row.names(selected_simulations) <- NULL
          selected_simulations
        } else {
          NULL
        }
      } else {
        private$.selected_simulations <- value
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
