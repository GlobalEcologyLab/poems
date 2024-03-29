#' R6 class representing a results manager.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class to represent a manager for generating summary
#' metrics and/or matrices from simulation results, as well as optionally regenerating
#' values via generators.
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
#' # Results manager
#' results_manager <- ResultsManager$new(
#'   sample_data = data.frame(index = 1:3),
#'   simulation_results = PopulationResults$new(region = region),
#'   summary_metrics = c("trend_n", "total_h"),
#'   summary_matrices = c("n", "h"),
#'   summary_functions = list(
#'     trend_n = function(results) {
#'       round(results$all$abundance_trend, 2)
#'     },
#'     total_h = function(results) {
#'       sum(results$harvested)
#'     },
#'     n = "all$abundance", # string
#'     h = "all$harvested"
#'   ),
#'   parallel_cores = 2,
#'   results_dir = tempdir()
#' )
#' # Write example result files
#' results <- list()
#' for (i in 1:3) {
#'   results[[i]] <- list(abundance = t(apply(
#'     matrix(11:17), 1,
#'     function(n) round(n * exp(-(0:9) / i))
#'   )))
#'   results[[i]]$harvested <- round(results[[i]]$abundance * i / 7)
#'   file_name <- paste0(results_manager$get_results_filename(i), ".RData")
#'   saveRDS(results[[i]], file.path(tempdir(), file_name))
#' }
#' # Generate result metrics and matrices
#' gen_output <- results_manager$generate()
#' gen_output$summary
#' dir(tempdir(), "*.txt") # plus generation log
#' results_manager$summary_metric_data
#' results_manager$summary_matrix_list
#'
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom R6 R6Class
#' @importFrom doParallel registerDoParallel
#' @importFrom doParallel stopImplicitCluster
#' @include GenericManager.R
#' @export ResultsManager

ResultsManager <- R6Class("ResultsManager",
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
    #' Initialization method optionally copies attributes from a simulation (results) manager, sets any included attributes (\emph{sample_data}, \emph{simulation_results}, \emph{generators}, \emph{result_attachment_functions}, \emph{summary_metrics}, \emph{summary_functions}, \emph{parallel_cores}, \emph{results_dir}, \emph{results_ext}, \emph{results_filename_attributes}), and attaches other attributes individually listed.
    #' @param simulation_manager Optional \code{\link{SimulationManager}} object (or an object inherited from the \code{\link{GenericManager}} class), from which simulation attributes can be copied.
    #' @param ... Parameters listed individually.
    initialize = function(simulation_manager = NULL, ...) {
      if (!is.null(simulation_manager)) { # copy attributes
        if (!("GenericManager" %in% class(simulation_manager))) {
          stop("Simulation manager must be a GenericManager or inherited class object", call. = FALSE)
        }
        self$sample_data <- simulation_manager$sample_data
        self$generators <- simulation_manager$generators
        self$parallel_cores <- simulation_manager$parallel_cores
        self$results_dir <- simulation_manager$results_dir
        self$results_ext <- simulation_manager$results_ext
        self$results_filename_attributes <- simulation_manager$results_filename_attributes
      }
      super$initialize(...)
    },

    # New methods #

    #' @description
    #' Generates the summary metric data and/or matrix list via the summary functions for each simulation sample, and creates/writes a generation log.
    #' @param results_dir Results directory path (must be present if not already set within manager class object).
    #' @return Generation log as a list.
    generate = function(results_dir = NULL) {
      # Check for error messages
      if (!is.null(self$error_messages)) {
        error_messages <- self$error_messages
        self$error_messages <- NULL
        stop(error_messages, call. = FALSE)
      }

      # Ensure sample data, simulation results, and summary metrics and/or matrices, and functions are present
      missing_parameters <- names(which(unlist(list(
        sample_data = is.null(self$sample_data),
        simulation_results = is.null(self$simulation_results),
        summary_metrics = is.null(self$summary_metrics) && is.null(self$summary_matrices),
        summary_matrices = is.null(self$summary_matrices) && is.null(self$summary_metrics),
        summary_functions = is.null(self$summary_functions)
      ))))
      if (length(missing_parameters)) {
        stop(paste("Summary metrics generation requires parameters to be set first:", paste(missing_parameters, collapse = ", ")), call. = FALSE)
      }

      # Check that the results directory is present and exists
      if (!is.null(results_dir)) {
        self$results_dir <- results_dir
      }
      if (is.null(self$results_dir)) {
        stop("No directory set for reading results", call. = FALSE)
      }
      if (!dir.exists(self$results_dir)) {
        stop(paste("Could find the results directory", self$results_dir), call. = FALSE)
      }
      if (is.null(self$results_ext)) {
        self$results_ext <- ".RData" # reinstate default
      }

      # Check that the results are present for the first sample (thus checking results_filename_attributes)
      results_file <- file.path(self$results_dir, paste0(self$get_results_filename(1), self$results_ext))
      if (!file.exists(results_file)) {
        stop(sprintf("Could not find (first) results file %s. Ensure results_filename_attributes is set to match the filename pattern", file.path(self$results_dir, paste0(self$get_results_filename(1), self$results_ext))), call. = FALSE)
      }

      # Generate summary metrics in parallel
      registerDoParallel(cores = self$parallel_cores)
      self <- self # pass object to parallel
      generation_log <- foreach(
        i = 1:nrow(self$sample_data),
        .packages = c("raster"),
        .errorhandling = c("pass")
      ) %dopar% {
        # Initialize the summary metric data for the sample to NAs
        if (length(self$summary_metrics)) {
          summary_metric_data <- data.frame(index = i, array(NA, c(1, length(self$summary_metrics))))
          names(summary_metric_data)[-1] <- self$summary_metrics
        } else {
          summary_metric_data <- data.frame(index = i)
        }

        # Initialize the summary matrix list for the sample to NAs
        if (length(self$summary_matrices)) {
          summary_matrix_list <- as.list(array(NA, c(1, length(self$summary_matrices))))
          names(summary_matrix_list) <- self$summary_matrices
        } else {
          summary_matrix_list <- NULL
        }

        # Read the results file for the sample
        results <- NULL
        results_file <- file.path(self$results_dir, paste0(self$get_results_filename(i), self$results_ext))
        if (file.exists(results_file)) {
          suppressWarnings(try(
            results <- readRDS(file = results_file),
            silent = TRUE
          ))
          if (is.null(results)) {
            return(list(
              successful = FALSE,
              message = self$get_message_sample("Could not read result file for %s", i),
              errors = "Result file reading error",
              summary_metric_data = summary_metric_data,
              summary_matrix_list = summary_matrix_list
            ))
          }
        } else {
          return(list(
            successful = FALSE,
            message = self$get_message_sample("Could not find result file for %s", i),
            errors = "Result file not found",
            summary_metric_data = summary_metric_data,
            summary_matrix_list = summary_matrix_list
          ))
        }

        # Clone and populate the simulation results and copy any additional attached attributes
        simulation_results <- self$simulation_results$new_clone(results = results)
        additional_attributes <- self$simulation_results$get_attribute_names()[which(!(self$simulation_results$get_attribute_names() %in% simulation_results$get_attribute_names()))]
        simulation_results$set_attributes(self$simulation_results$get_attributes(additional_attributes))
        additional_attributes <- self$simulation_results$all$get_attribute_names()[which(!(self$simulation_results$all$get_attribute_names() %in% simulation_results$all$get_attribute_names()))]
        simulation_results$all$set_attributes(self$simulation_results$all$get_attributes(additional_attributes))

        # Add optional simulation inputs to the simulation results via generators
        sample_list <- as.list(self$sample_data[i, ])
        if (!is.null(self$generators)) {
          for (j in 1:length(self$generators)) {
            tryCatch(
              {
                simulation_results$set_attributes(params = self$generators[[j]]$generate(input_values = sample_list[self$generators[[j]]$inputs]))
              },
              error = function(e) {
                stop(paste("produced when generating", self$generators[[j]]$description, ":", as.character(e)), call. = FALSE)
              }
            )
          }
        }

        # Apply result attachment functions to the results (model)
        if (!is.null(self$result_attachment_functions)) {
          self$calculate_result_attachments(simulation_results)
        }

        # Apply the summary functions to the results (model)
        return(self$calculate_summaries(simulation_results, i))
      }
      stopImplicitCluster()

      # Merge summary metric data and matrix list
      self$summary_metric_data <- data.frame(
        index = 1:nrow(self$sample_data),
        array(NA, c(nrow(self$sample_data), length(self$summary_metrics)))
      )
      names(self$summary_metric_data)[-1] <- self$summary_metrics
      if (!is.null(self$summary_matrices)) {
        summary_matrix_columns <- as.list(array(1, c(1, length(self$summary_matrices))))
        names(summary_matrix_columns) <- self$summary_matrices
      }
      for (i in 1:nrow(self$sample_data)) {
        # Merge summary metric data
        if (!is.null(generation_log[[i]]$summary_metric_data) && generation_log[[i]]$summary_metric_data$index == i) {
          self$summary_metric_data[i, ] <- generation_log[[i]]$summary_metric_data
        }
        # Determine matrix (max) dimensions
        for (matrix_name in self$summary_matrices) {
          if (!is.null(generation_log[[i]]$summary_matrix_list) && matrix_name %in% names(generation_log[[i]]$summary_matrix_list)) {
            summary_matrix_columns[[matrix_name]] <- max(length(generation_log[[i]]$summary_matrix_list[[matrix_name]]), summary_matrix_columns[[matrix_name]])
          }
        }
      }
      if (!is.null(self$summary_matrices)) {
        summary_matrix_list <- list()
        for (matrix_name in self$summary_matrices) {
          summary_matrix_list[[matrix_name]] <- array(NA, c(nrow(self$sample_data), summary_matrix_columns[[matrix_name]]))
        }
        for (i in 1:nrow(self$sample_data)) {
          # Merge summary matrix list rows
          for (matrix_name in self$summary_matrices) {
            if (!is.null(generation_log[[i]]$summary_matrix_list) && matrix_name %in% names(generation_log[[i]]$summary_matrix_list)) {
              values <- generation_log[[i]]$summary_matrix_list[[matrix_name]]
              summary_matrix_list[[matrix_name]][i, 1:length(values)] <- values
            }
          }
        }
        self$summary_matrix_list <- summary_matrix_list
        summary_matrix_list <- NULL # release from memory
      }

      # Summarize and write log to a file
      generation_log <- self$log_generation(generation_log)

      return(generation_log)
    },


    #' @description
    #' Calculates and attaches intermediate values to the sample result model (via the result attachment functions).
    #' @param simulation_results The sample simulation results, an object of a class inherited from \code{\link{SimulationResults}}, to which the intermediate results are attached.
    calculate_result_attachments = function(simulation_results) {
      # Calculate each result attachment
      for (attachment in names(self$result_attachment_functions)) {
        # Assign attachment function and initilize attachment value
        attachment_function <- self$result_attachment_functions[[attachment]]
        attachment_value <- NULL

        # Apply the function
        tryCatch(
          {
            suppressWarnings(
              withCallingHandlers(
                {
                  if (is.character(attachment_function)) {
                    attachment_function <- eval(parse(text = attachment_function))
                  }
                  if (length(grep("Primitive", deparse(attachment_function), fixed = TRUE))) { # use default
                    attachment_value <- attachment_function(simulation_results$default)
                  } else { # apply function to results
                    attachment_value <- attachment_function(simulation_results)
                  }
                },
                warning = function(w) {
                  simulation_results$warning_messages <- c(
                    simulation_results$warning_messages,
                    paste(
                      sprintf("Warning encountered attaching %s to results.", attachment),
                      gsub("simpleWarning", "Warning",
                        gsub("\n", "", as.character(w), fixed = TRUE),
                        fixed = TRUE
                      )
                    )
                  )
                }
              )
            )
          },
          error = function(e) {
            simulation_results$error_messages <- paste(sprintf("Error encountered attaching %s to results.", attachment), as.character(e))
          }
        )

        # Attach the returned function value to the simulation results
        if (!is.null(attachment_value)) {
          simulation_results$attached[[attachment]] <- attachment_value
        }
      }
    },

    #' @description
    #' Calculates the summary metrics and/or matrices for the results of a sample simulation (via the summary functions).
    #' @param simulation_results The sample simulation results, an object of a class inherited from \code{\link{SimulationResults}}.
    #' @param sample_index Index of sample from data frame.
    #' @return Generation log entry as a (nested) list, including generated summary metric data and (optionally) matrices.
    calculate_summaries = function(simulation_results, sample_index) {
      # Initialize the summary metric data for the sample to NAs
      if (length(self$summary_metrics)) {
        summary_metric_data <- data.frame(index = sample_index, array(NA, c(1, length(self$summary_metrics))))
        names(summary_metric_data)[-1] <- self$summary_metrics
      } else {
        summary_metric_data <- data.frame(index = sample_index)
      }

      # Initialize the summary matrix list for the sample to NAs
      if (length(self$summary_matrices)) {
        summary_matrix_list <- as.list(array(NA, c(1, length(self$summary_matrices))))
        names(summary_matrix_list) <- self$summary_matrices
      } else {
        summary_matrix_list <- NULL
      }

      # Calculate each summary metric and matrix
      for (name in c(self$summary_metrics, self$summary_matrices)) {
        type <- ifelse(name %in% self$summary_matrices, "matrix", "metric")
        if (!(name %in% names(self$summary_functions))) {
          simulation_results$error_messages <- sprintf("No %s function defined for %s", type, name)
        } else {
          function_value <- self$summary_functions[[name]]
          data_value <- NULL
          if (is.numeric(function_value)) { # constant value
            data_value <- function_value
          } else if (is.character(function_value) && function_value %in% simulation_results$get_attribute_names(all = TRUE)) { # results attribute
            data_value <- eval(parse(text = paste0("simulation_results$", function_value)))
          } else { # apply a function
            tryCatch(
              {
                suppressWarnings(
                  withCallingHandlers(
                    {
                      if (is.character(function_value)) {
                        function_value <- eval(parse(text = function_value))
                      }
                      if (length(grep("Primitive", deparse(function_value), fixed = TRUE))) { # use default
                        data_value <- function_value(simulation_results$default)
                      } else { # apply function to results
                        data_value <- function_value(simulation_results)
                      }
                    },
                    warning = function(w) {
                      simulation_results$warning_messages <- c(
                        simulation_results$warning_messages,
                        paste(
                          sprintf("Warning encountered setting %s %s.", type, name),
                          gsub("simpleWarning", "Warning",
                            gsub("\n", "", as.character(w), fixed = TRUE),
                            fixed = TRUE
                          )
                        )
                      )
                    }
                  )
                )
              },
              error = function(e) {
                simulation_results$error_messages <- paste(sprintf("Error encountered setting %s %s.", type, name), as.character(e))
              }
            )
          }
          if (length(data_value)) {
            if (type == "metric") {
              if (length(data_value[]) == 1) {
                summary_metric_data[1, name] <- data_value
              } else if (length(data_value[]) > 1) {
                simulation_results$error_messages <- paste("Metric function defined for", name, "produces multiple values")
              }
            } else { # matrix
              summary_matrix_list[[name]] <- as.vector(data_value[])
            }
          }
        }
      }

      # Construct return list
      generation <- list(successful = (length(simulation_results$error_messages) == 0))
      error_warning <- paste0(c("errors", "warnings")[which(c((length(simulation_results$error_messages) > 0), (length(simulation_results$warning_messages) > 0)))], collapse = " and ")
      if (length(simulation_results$error_messages) || length(simulation_results$warning_messages)) {
        generation$message <- self$get_message_sample(paste("Summaries calculated with", error_warning, "for %s"), sample_index)
      } else {
        generation$message <- self$get_message_sample("Summaries calculated for %s", sample_index)
      }
      if (length(simulation_results$error_messages)) {
        generation$errors <- simulation_results$error_messages
      }
      if (length(simulation_results$warning_messages)) {
        generation$warnings <- simulation_results$warning_messages
      }
      generation$summary_metric_data <- summary_metric_data
      generation$summary_matrix_list <- summary_matrix_list
      return(generation)
    },

    #' @description
    #' Summarizes the log generated within the generate method and writes it to a text file in the results directory.
    #' @param generation_log Nested list of log entries generated via the generate method.
    #' @return Extended generation log as a nested with added summary and failure/warning indices.
    log_generation = function(generation_log) {
      # Determine which generations were successful and collect any warnings
      successful_array <- array(FALSE, length(generation_log))
      warning_indices <- c()
      for (i in 1:length(generation_log)) {
        if (is.null(generation_log[[i]]$successful)) {
          generation_log[[i]] <- list(message = as.character(generation_log[[i]]), successful = FALSE)
        }
        successful_array[i] <- generation_log[[i]]$successful
        if (!is.null(generation_log[[i]]$warnings)) {
          warning_indices <- c(warning_indices, i)
        }
      }
      # Add a summary and failure & warning indices to the log
      generation_log <- list(
        summary = sprintf(
          "%s of %s summary metrics/matrices generated from sample results successfully",
          length(which(successful_array)), length(generation_log)
        ),
        failed_indices = which(!successful_array),
        warning_indices = warning_indices,
        full_log = generation_log
      )
      if (length(warning_indices)) {
        generation_log$summary <- paste(generation_log$summary, "with warnings")
      }
      # Write a log file
      log_file <- file.path(self$results_dir, "generation_log.txt")
      suppressWarnings(try(
        {
          file_con <- file(log_file, "w")
          writeLines(c(generation_log$summary), con = file_con)
          if (length(generation_log$failed_indices)) {
            writeLines(c("", paste(length(generation_log$failed_indices), "failed generations/errors:")), con = file_con)
            for (i in generation_log$failed_indices) {
              writeLines(c("", paste("Sample", i, ":"), generation_log$full_log[[i]]$message), con = file_con)
              if (!is.null(generation_log$full_log[[i]]$errors)) {
                writeLines(generation_log$full_log[[i]]$errors, con = file_con)
              }
            }
          }
          if (length(warning_indices)) {
            writeLines(c("", paste(length(warning_indices), "warnings:")), con = file_con)
            for (i in warning_indices) {
              writeLines(c("", paste("Sample", i, ":"), generation_log$full_log[[i]]$message), con = file_con)
              writeLines(generation_log$full_log[[i]]$warnings, con = file_con)
            }
          }
          close(file_con)
        },
        silent = TRUE
      ))
      return(generation_log)
    },

    #' @description
    #' Calculates the weighted averages for each of the summary matrices (providing the sample data has a \emph{weight} column).
    #' @param na_replacements List of values or functions (form: \code{modified_matrix <- function(matrix)}) for dealing with NA values in each summary matrix (default NULL will ignore NAs).
    calculate_summary_weighted_averages = function(na_replacements = NULL) {
      # Ensure summary matrix list is present and sample data has a weight column
      if (is.null(self$summary_matrix_list)) {
        stop("No summary matrices have been generated", call. = FALSE)
      }
      if (is.null(self$sample_data) || !("weight" %in% names(self$sample_data))) {
        stop("Sample data with a weight column is required", call. = FALSE)
      }

      # Copy the matrix list (maintains NAs)
      summary_matrix_list <- self$summary_matrix_list

      # Apply NA replacements
      if (!is.null(na_replacements)) {
        if (is.list(na_replacements)) {
          for (param in names(na_replacements)) {
            if (param %in% names(summary_matrix_list)) {
              if (is.function(na_replacements[[param]])) {
                summary_matrix_list[[param]] <- na_replacements[[param]](summary_matrix_list[[param]])
              } else { # assume value
                summary_matrix_list[[param]][which(is.na(summary_matrix_list[[param]]))] <- na_replacements[[param]]
              }
            }
          }
        } else {
          stop("NA replacements must be a list of values or functions", call. = FALSE)
        }
      }

      # Normalize weights
      normalized_weights <- self$sample_data$weight / sum(self$sample_data$weight)

      # Calculate the weighted averages
      for (param in names(summary_matrix_list)) {
        self$summary_matrix_weighted_averages[[param]] <- .colSums(summary_matrix_list[[param]] * array(normalized_weights, dim(summary_matrix_list[[param]])),
          m = length(normalized_weights), n = ncol(summary_matrix_list[[param]]), na.rm = TRUE
        )
      }
    }
  ), # end public

  private = list(

    ## Attributes ##

    # Manager attributes #
    .manager_attributes = c(
      "sample_data", "simulation_results", "generators", "result_attachment_functions",
      "summary_metrics", "summary_matrices", "summary_functions", "summary_metric_data",
      "summary_matrix_list", "summary_matrix_weighted_averages", "parallel_cores",
      "results_dir", "results_ext", "results_filename_attributes"
    ),
    # .sample_data                   [inherited]
    .simulation_results = NULL,
    # .generators             [inherited]
    .result_attachment_functions = NULL,
    .summary_metrics = NULL,
    .summary_matrices = NULL,
    .summary_functions = NULL,
    .summary_metric_data = NULL,
    .summary_matrix_list = NULL,
    .summary_matrix_weighted_averages = NULL
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

    #' @field simulation_results An object of a class inherited from the \code{\link{SimulationResults}} class for encapsulating and dynamically generating simulation results.
    simulation_results = function(value) {
      if (missing(value)) {
        private$.simulation_results
      } else {
        if (!is.null(value) && !("SimulationResults" %in% class(value))) {
          stop("The simulation results must be a object of a class inherited from the SimulationResults class", call. = FALSE)
        } else {
          private$.simulation_results <- value
        }
      }
    },

    #' @field generators A list of generators (\code{\link{Generator}} or inherited class) objects for (optionally) regenerating simulation model values.
    generators = function(value) { # inherited
      if (missing(value)) {
        super$generators
      } else {
        super$generators <- value
      }
    },

    #' @field result_attachment_functions A list of functions for attaching intermediate values to the simulation results prior to generation.
    result_attachment_functions = function(value) {
      if (missing(value)) {
        private$.result_attachment_functions
      } else {
        if (is.null(value)) {
          private$.result_attachment_functions <- value
        } else if (!is.list(value)) {
          stop("Result attachment functions should be a list", call. = FALSE)
        }
        attachment_functions <- list()
        for (attachment in names(value)) {
          attachment_value <- value[[attachment]]
          if (is.character(attachment_value) && file.exists(attachment_value) && length(grep(".R", toupper(attachment_value), fixed = TRUE))) {
            tryCatch(
              {
                attachment_function <- source(attachment_value)$value # direct assignment from a file
              },
              error = function(e) {
                stop(paste("Error loading function from file", attachment_value, ":", as.character(e)), call. = FALSE)
              }
            )
            if (is.function(attachment_function)) {
              attachment_functions[[attachment]] <- attachment_function
            } else {
              stop(paste("Could not assign function", attachment_value), call. = FALSE)
            }
          } else { # numeric, character or direct assignment
            if (is.function(attachment_value) ||
              (is.character(attachment_value) &&
                tryCatch(is.function(eval(parse(text = attachment_value))), error = function(e) FALSE))) {
              if (is.function(attachment_value) && length(grep("Primitive", deparse(attachment_value), fixed = TRUE))) {
                attachment_value <- strsplit(deparse(attachment_value), "\"")[[1]][2] # convert primitives to string
              }
              attachment_functions[[attachment]] <- attachment_value
            } else {
              stop(paste("Could not assign function", attachment_value), call. = FALSE)
            }
          }
        }
        private$.result_attachment_functions <- attachment_functions
      }
    },

    #' @field summary_metrics An array of names for summary metrics, each of which are calculated as single values for each simulation. These should refer to list names for the summary functions.
    summary_metrics = function(value) {
      if (missing(value)) {
        private$.summary_metrics
      } else {
        private$.summary_metrics <- value
      }
    },

    #' @field summary_matrices An array of names for summary matrices, each of which are calculated as a single matrix row for each simulation. These should refer to list names for the summary functions.
    summary_matrices = function(value) {
      if (missing(value)) {
        private$.summary_matrices
      } else {
        private$.summary_matrices <- value
      }
    },

    #' @field summary_functions A list of functions, result attributes, or constants for transforming individual simulation results to single summary metric values stored in the metric data frame, or to matrix rows stored in the summary matrix list.
    summary_functions = function(value) {
      if (missing(value)) {
        private$.summary_functions
      } else {
        if (is.null(value)) {
          private$.summary_functions <- value
        } else if (!is.list(value)) {
          stop("Summary metric functions should be a list", call. = FALSE)
        }
        metric_functions <- list()
        for (metric in names(value)) {
          metric_value <- value[[metric]]
          if (is.character(metric_value) && !is.null(self$simulation_results) && metric_value %in% self$simulation_results$get_attribute_names(all = TRUE)) {
            metric_functions[[metric]] <- metric_value # direct simulation results attribute
          } else if (is.character(metric_value) && file.exists(metric_value) && length(grep(".R", toupper(metric_value), fixed = TRUE))) {
            tryCatch(
              {
                metric_function <- source(metric_value)$value # direct assignment from a file
              },
              error = function(e) {
                stop(paste("Error loading function from file", metric_value, ":", as.character(e)), call. = FALSE)
              }
            )
            if (is.function(metric_function)) {
              metric_functions[[metric]] <- metric_function
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
              metric_functions[[metric]] <- metric_value
            } else {
              stop(paste("Could not assign function", metric_value), call. = FALSE)
            }
          }
        }
        # Set any function names not already listed as metrics or matrices as metrics
        self$summary_metrics <- unique(c(self$summary_metrics, names(metric_functions)[which(!names(metric_functions) %in% self$summary_matrices)]))
        private$.summary_functions <- metric_functions
      }
    },

    #' @field summary_metric_data A data frame of generated summary metrics (one row per simulation).
    summary_metric_data = function(value) {
      if (missing(value)) {
        private$.summary_metric_data
      } else {
        private$.summary_metric_data <- value
      }
    },

    #' @field summary_matrix_list A list of generated matrices of summary results (each having one row per simulation).
    summary_matrix_list = function(value) {
      if (missing(value)) {
        private$.summary_matrix_list
      } else {
        private$.summary_matrix_list <- value
      }
    },

    #' @field summary_matrix_weighted_averages A list of calculated weighted averages for each of the summary matrices (using the sample data \emph{weight} column).
    summary_matrix_weighted_averages = function(value) {
      if (missing(value)) {
        private$.summary_matrix_weighted_averages
      } else {
        private$.summary_matrix_weighted_averages <- value
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
