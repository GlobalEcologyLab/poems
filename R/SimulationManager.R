#' R6 class representing a simulation manager.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class to represent a manager for running multiple model
#' simulations and saving results.
#'
#' @examplesIf interactive()
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
#' # Example population model template
#' model_template <- PopulationModel$new(
#'   region = region,
#'   time_steps = 10, # years
#'   populations = region$region_cells, # 7
#'   stage_matrix = 1
#' )
#' # Example generators for initial abundance and carrying capacity
#' hs_matrix <- c(0.5, 0.3, 0.7, 0.9, 0.6, 0.7, 0.8)
#' initial_gen <- Generator$new(
#'   description = "initial abundance",
#'   region = region,
#'   hs_matrix = hs_matrix, # template attached
#'   inputs = c("initial_n"),
#'   outputs = c("initial_abundance")
#' )
#' initial_gen$add_generative_requirements(list(initial_abundance = "function"))
#' initial_gen$add_function_template("initial_abundance",
#'   function_def = function(params) {
#'     stats::rmultinom(1,
#'       size = params$initial_n,
#'       prob = params$hs_matrix
#'     )[, 1]
#'   },
#'   call_params = c("initial_n", "hs_matrix")
#' )
#' capacity_gen <- Generator$new(
#'   description = "carrying capacity",
#'   region = region,
#'   hs_matrix = hs_matrix, # template attached
#'   inputs = c("density_max"),
#'   outputs = c("carrying_capacity")
#' )
#' capacity_gen$add_generative_requirements(list(carrying_capacity = "function"))
#' capacity_gen$add_function_template("carrying_capacity",
#'   function_def = function(params) {
#'     round(params$density_max * params$hs_matrix)
#'   },
#'   call_params = c("density_max", "hs_matrix")
#' )
#' # Sample input parameters
#' sample_data <- data.frame(initial_n = c(40, 60, 80), density_max = c(15, 20, 25))
#' # Simulation manager
#' sim_manager <- SimulationManager$new(
#'   sample_data = sample_data,
#'   model_template = model_template,
#'   generators = list(initial_gen, capacity_gen),
#'   parallel_cores = 2,
#'   results_dir = tempdir()
#' )
#' run_output <- sim_manager$run()
#' run_output$summary
#' dir(tempdir(), "*.RData") # includes 3 result files
#' for (i in 1:3) {
#'   print(paste("Run", i, "results:"))
#'   file_name <- paste0(sim_manager$get_results_filename(i), ".RData")
#'   print(readRDS(file.path(tempdir(), file_name)))
#' }
#' dir(tempdir(), "*.txt") # plus simulation log
#'
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom R6 R6Class
#' @importFrom doParallel registerDoParallel
#' @importFrom doParallel stopImplicitCluster
#' @include GenericManager.R
#' @include SimulationModel.R
#' @include ModelSimulator.R
#' @export SimulationManager

SimulationManager <- R6Class("SimulationManager",
  inherit = GenericManager,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericClass & GenericManager) #
    #   new_clone(...)
    #   get_attribute(param)
    #   get_message_sample(status_message, sample_index)
    #   get_results_filename(sample_index)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets any included attributes (\emph{sample_data}, \emph{model_template}, \emph{generators}, \emph{model_simulator}, \emph{parallel_cores}, \emph{results_dir}, \emph{results_filename_attributes}) and attaches other attributes individually listed.
    #' @param model_template A SimulationModel (or inherited class) object with parameters common to all simulations.
    #' @param ... Parameters listed individually.
    initialize = function(model_template = NULL, ...) {
      self$model_template <- model_template
      if (!("model_simulator" %in% names(list(...)))) {
        if (!is.null(model_template)) {
          self$model_simulator <- ModelSimulator$new(simulation_function = model_template$simulation_function)
        } else {
          self$model_simulator <- ModelSimulator$new()
        }
      }
      super$initialize(...)
    },

    # New methods #

    #' @description
    #' Runs the multiple population simulations (via the set function), stores the results, and creates/writes a simulation log.
    #' @param results_dir Results directory path (must be present if not already set within manager class object).
    #' @return Simulator log as a list.
    run = function(results_dir = NULL) {
      # Check for error messages
      if (!is.null(self$error_messages)) {
        error_messages <- self$error_messages
        self$error_messages <- NULL
        stop(error_messages, call. = FALSE)
      }

      # Check that model and sample data is present
      if (is.null(self$model_template) || length(self$sample_data) == 0) {
        stop("No model samples to run", call. = FALSE)
      }

      # Check that model simulator/function is present
      if (is.null(self$model_simulator)) {
        stop("The model simulator has not been set", call. = FALSE)
      } else if (is.null(self$model_simulator$simulation_function)) {
        stop("The model simulator function has not been set", call. = FALSE)
      }

      # Check that the results directory is present and exists
      if (!is.null(results_dir)) {
        self$results_dir <- results_dir
      }
      if (is.null(self$results_dir)) {
        stop("No output directory set for results", call. = FALSE)
      }
      if (!dir.exists(self$results_dir)) {
        stop(paste("Could not find results directory", self$results_dir), call. = FALSE)
      }
      if (is.null(self$results_ext)) {
        self$results_ext <- ".RData" # reinstate default
      }

      # Create a nested simulation (or descendant) model for cloning
      self$nested_model <- self$model_template$new_clone(template = self$model_template)

      # Allow extra attachments to be passed
      if ("nested_model" %in% names(self$attached)) {
        self$nested_model$attached <- self$attached$nested_model
      }

      # Resolve sample attributes and attach them separately (as aliases) to the model to avoid repetition
      model_sample_columns <- which(names(self$sample_data) %in% self$nested_model$get_attribute_aliases())
      if (length(model_sample_columns) > 0) {
        self$nested_model$attached$sample_model_names <- names(self$sample_data)[model_sample_columns]
        self$nested_model$sample_attributes <- self$nested_model$attached$sample_model_names
      }
      if (!is.null(self$generators)) {
        self$nested_model$attached$sample_generative_names <- list()
        for (i in 1:length(self$generators)) {
          if (all(self$generators[[i]]$inputs %in% names(self$sample_data))) {
            self$nested_model$attached$sample_generative_names[[i]] <- self$generators[[i]]$outputs
            self$nested_model$sample_attributes <- unique(c(self$nested_model$sample_attributes, self$generators[[i]]$outputs))
          }
        }
      }

      # Check the completeness/consistency of the first sample only
      model <- self$nested_model$clone()
      self$set_model_sample(model, 1)
      if (length(model$error_messages)) {
        stop(c("Error(s) setting model sample attributes: ", model$error_messages), call. = FALSE)
      }
      if (!model$is_complete()) {
        incomplete_message <- "Model attributes are incomplete"
        if (!model$is_consistent()) {
          incomplete_message <- paste(incomplete_message, "/inconsistent", sep = "")
        }
        incomplete_message <- paste0(incomplete_message, ": ", paste(model$incomplete_attributes(), collapse = ", "))
        stop(incomplete_message, call. = FALSE)
      }
      model <- NULL # release from memory

      # Run sample simulations in parallel
      doParallel::registerDoParallel(cores = self$parallel_cores)
      simulation_log <- foreach(
        i = 1:nrow(self$sample_data),
        .packages = c("raster"),
        .errorhandling = c("pass")
      ) %dopar% {
        # Clone the model
        model <- self$nested_model$clone()

        # Set the model sample attributes
        self$set_model_sample(model, i)
        if (length(model$error_messages)) {
          return(list(successful = FALSE, message = self$get_message_sample("Error(s) setting model %s sample attributes", i), errors = model$error_messages))
        }

        # Create and run the simulator
        simulator <- self$model_simulator$new_clone(simulation_model = model, sample_id = i)
        simulator_run_status <- simulator$run()

        # Substitute sample details into the simulator run status message
        simulator_run_status$message <- self$get_message_sample(simulator_run_status$message, i)

        # Save results
        if (!is.null(simulator$results)) {
          results_file <- file.path(self$results_dir, paste0(self$get_results_filename(i), self$results_ext))
          suppressWarnings(try(
            saveRDS(simulator$results, file = results_file),
            silent = TRUE
          ))
          if (file.exists(results_file)) {
            simulator_run_status$message <- paste0(simulator_run_status$message, " and the results were saved")
          } else {
            simulator_run_status$successful <- FALSE
            simulator_run_status$message <- paste0(simulator_run_status$message, ", but the results could not be saved in ", results_file)
          }
        }

        return(simulator_run_status)
      }
      doParallel::stopImplicitCluster()

      # Summarize and write log to a file
      simulation_log <- self$log_simulation(simulation_log)

      return(simulation_log)
    },

    #' @description
    #' Sets the model sample attributes via the sample data frame and the generators.
    #' @param model \code{\link{SimulationModel}} (or inherited class) object (clone) to receive sample attributes.
    #' @param sample_index Index of sample from data frame.
    set_model_sample = function(model, sample_index) {
      sample_list <- as.list(self$sample_data[sample_index, ])
      names(sample_list) <- names(self$sample_data)
      if (!is.null(model$attached$sample_model_names)) {
        model$set_sample_attributes(params = sample_list[model$attached$sample_model_names])
      }
      if (!is.null(self$generators)) {
        for (i in 1:length(self$generators)) {
          if (!is.null(self$nested_model$attached$sample_generative_names[[i]])) {
            tryCatch(
              {
                model$set_sample_attributes(params = self$generators[[i]]$generate(input_values = sample_list[self$generators[[i]]$inputs]))
              },
              error = function(e) {
                stop(paste("produced when generating", self$generators[[i]]$description, ":", as.character(e)), call. = FALSE)
              }
            )
          }
        }
      }
    },

    #' @description
    #' Summarizes the simulation log generated within the run method and writes it to a text file in the results directory.
    #' @param simulation_log Nested list of simulation log entries generated via the run method.
    log_simulation = function(simulation_log) {
      # Determine which simulations were successful and collect any warnings
      successful_array <- array(FALSE, length(simulation_log))
      warning_indices <- c()
      for (i in 1:length(simulation_log)) {
        if (is.null(simulation_log[[i]]$successful)) {
          simulation_log[[i]] <- list(message = as.character(simulation_log[[i]]), successful = FALSE)
        }
        successful_array[i] <- simulation_log[[i]]$successful
        if (!is.null(simulation_log[[i]]$warnings)) {
          warning_indices <- c(warning_indices, i)
        }
      }
      # Add a summary and failure & warning indices to the log
      simulation_log <- list(
        summary = sprintf(
          "%s of %s sample models ran and saved results successfully",
          length(which(successful_array)), length(simulation_log)
        ),
        failed_indices = which(!successful_array),
        warning_indices = warning_indices,
        full_log = simulation_log
      )
      if (length(warning_indices)) {
        simulation_log$summary <- paste(simulation_log$summary, "with warnings")
      }
      # Write a log file
      log_file <- file.path(self$results_dir, "simulation_log.txt")
      suppressWarnings(try(
        {
          file_con <- file(log_file, "w")
          writeLines(c(simulation_log$summary), con = file_con)
          if (length(simulation_log$failed_indices)) {
            writeLines(c("", paste(length(simulation_log$failed_indices), "failed runs/errors:")), con = file_con)
            for (i in simulation_log$failed_indices) {
              writeLines(c("", paste("Sample", i, ":"), simulation_log$full_log[[i]]$message), con = file_con)
              if (!is.null(simulation_log$full_log[[i]]$errors)) {
                writeLines(simulation_log$full_log[[i]]$errors, con = file_con)
              }
            }
          }
          if (length(warning_indices)) {
            writeLines(c("", paste(length(warning_indices), "warnings:")), con = file_con)
            for (i in warning_indices) {
              writeLines(c("", paste("Sample", i, ":"), simulation_log$full_log[[i]]$message), con = file_con)
              writeLines(simulation_log$full_log[[i]]$warnings, con = file_con)
            }
          }
          close(file_con)
        },
        silent = TRUE
      ))
      return(simulation_log)
    }
  ), # end public

  private = list(

    ## Attributes ##

    # Manager attributes #
    .manager_attributes = c(
      "sample_data", "model_template", "nested_model", "generators", "model_simulator",
      "parallel_cores", "results_dir", "results_ext", "results_filename_attributes"
    ),
    # .sample_data                   [inherited]
    .model_template = NULL,
    .nested_model = NULL,
    # .generators             [inherited]
    .model_simulator = NULL
    # .parallel_cores                [inherited]
    # .results_dir                   [inherited]
    # .results_ext                   [inherited]
    # .results_filename_attributes   [inherited]

    # Errors and warnings #
    # .error_messages                [inherited]
    # .warning_messages              [inherited]
  ), # end private

  # Active binding accessors for private manager attributes (above) #
  active = list(

    #' @field sample_data A data frame of sampled parameters for each simulation/result.
    sample_data = function(value) { # inherited
      if (missing(value)) {
        super$sample_data
      } else {
        super$sample_data <- value
      }
    },

    #' @field model_template A \code{\link{SimulationModel}} (or inherited class) object with parameters common to all simulations.
    model_template = function(value) {
      if (missing(value)) {
        private$.model_template
      } else {
        if (!is.null(value) && !("SimulationModel" %in% class(value))) {
          stop("Model template must be a SimulationModel or inherited class object", call. = FALSE)
        } else {
          if (!is.null(value) && !is.null(self$model_simulator) && is.null(self$model_simulator$simulation_function)) {
            self$model_simulator$simulation_function <- value$simulation_function
          }
          private$.model_template <- value
        }
      }
    },

    #' @field nested_model A \code{\link{SimulationModel}} (or inherited class) object with empty sample parameters and a nested model template common to all simulations.
    nested_model = function(value) {
      if (missing(value)) {
        private$.nested_model
      } else {
        if (!is.null(value) && !("SimulationModel" %in% class(value))) {
          stop("Nested model must be a SimulationModel or inherited class object", call. = FALSE)
        } else {
          private$.nested_model <- value
        }
      }
    },

    #' @field generators A list of generators (\code{\link{Generator}} or inherited class) objects for generating simulation model values.
    generators = function(value) { # inherited
      if (missing(value)) {
        super$generators
      } else {
        super$generators <- value
      }
    },

    #' @field model_simulator A \code{\link{ModelSimulator}} (or inherited class) object for running the simulations.
    model_simulator = function(value) {
      if (missing(value)) {
        private$.model_simulator
      } else {
        if (!is.null(value) && !("ModelSimulator" %in% class(value))) {
          stop("Model simulator must be a ModelSimulator or inherited class object", call. = FALSE)
        } else {
          private$.model_simulator <- value
        }
      }
    },

    #' @field parallel_cores Number of cores for running the simulations in parallel.
    parallel_cores = function(value) { # inherited
      if (missing(value)) {
        super$parallel_cores
      } else {
        super$parallel_cores <- value
      }
    },

    #' @field results_dir Results directory path.
    results_dir = function(value) { # inherited
      if (missing(value)) {
        super$results_dir
      } else {
        super$results_dir <- value
      }
    },

    #' @field results_ext Result file extension (default is .RData).
    results_ext = function(value) { # inherited
      if (missing(value)) {
        super$results_ext
      } else {
        super$results_ext <- value
      }
    },

    #' @field results_filename_attributes A vector of: prefix (optional); attribute names (from the sample data frame); postfix (optional); utilized to construct results filenames.
    results_filename_attributes = function(value) { # inherited
      if (missing(value)) {
        super$results_filename_attributes
      } else {
        super$results_filename_attributes <- value
      }
    },

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
