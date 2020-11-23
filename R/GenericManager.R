#' R6 class representing a generic manager.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class to represent a generic (abstract) manager for
#' generating or processing simulation results, as well as optionally generating values
#' via generators.
#'
#' @importFrom R6 R6Class
#' @include GenericClass.R
#' @include Generator.R
#' @export GenericManager

GenericManager <- R6Class("GenericManager",
  inherit = GenericClass,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericClass) #
    #   new_clone(...)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets any included attributes (\emph{sample_data}, \emph{generators}, \emph{parallel_cores}, \emph{results_dir}, \emph{results_filename_attributes}) and attaches other attributes individually listed.
    #' @param ... Parameters listed individually.
    initialize = function(...) {
      params <- list(...)
      params$object_generator <- NULL
      for (param in names(params)) {
        if (param %in% private$.manager_attributes) {
          eval(parse(text=sprintf("self$%s <- params$%s", param, param)))
        } else { # attach
          eval(parse(text=sprintf("self$attached$%s <- params$%s", param, param)))
        }
      }
      if (!is.null(self$error_messages)) {
        stop(self$error_messages, call. = FALSE)
      }
      super$initialize(...)
    },

    # New methods #

    #' @description
    #' Returns a named manager or attached attribute.
    #' @param param Character string name of the attribute.
    #' @return Selected attribute value.
    get_attribute = function(param) {
      if (param %in% private$.manager_attributes) {
        return(eval(parse(text=sprintf("self$%s", param))))
      } else if (param %in% names(self$attached)) {
        return(self$attached[[param]])
      } else {
        return(NULL)
      }
    },

    #' @description
    #' Substitutes the specified sample details into a status message (using sprintf) and returns the result.
    #' @param status_message Character string message with a \emph{\%s} placeholder for sample details.
    #' @param sample_index Row index of sample data frame containing details of substitution parameters.
    #' @return Status message with substituted sample details.
    get_message_sample = function(status_message, sample_index) {
      sample_vector <- c()
      if (!is.null(self$results_filename_attributes)) {
        sample_attributes <- self$results_filename_attributes[which(self$results_filename_attributes %in% names(self$sample_data))]
        if (length(sample_attributes)) {
          for (i in 1:length(sample_attributes)) {
            sample_vector <- c(sample_vector, gsub("_", " ", sample_attributes[i], fixed = TRUE),
                               as.character(self$sample_data[sample_index, sample_attributes[i]]))
          }
        }
      }
      if (length(sample_vector) == 0) {
        sample_vector <- c("sample", as.character(sample_index))
      }
      return(sprintf(status_message, paste(sample_vector, collapse = " ")))
    },

    #' @description
    #' Constructs and returns the results filename based on the sample data frame index and results filename attributes.
    #' @param sample_index Row index of sample data frame containing details of substitution parameters.
    #' @return Results filename with substituted sample details.
    get_results_filename = function(sample_index) {
      if (!is.null(self$results_filename_attributes) && !is.null(self$sample_data)) {
        filename_vector <- c()
        postfix <- "results"
        pre_postfix_present <- c(FALSE, FALSE)
        for (i in 1:length(self$results_filename_attributes)) {
          if (i == 1 && !(self$results_filename_attributes[i] %in% names(self$sample_data))) { # add prefix
            filename_vector <- c(filename_vector, as.character(self$results_filename_attributes[i]))
            pre_postfix_present[1] <- TRUE
          } else if (self$results_filename_attributes[i] %in% names(self$sample_data)) { # add attribute and value
            filename_vector <- c(filename_vector, self$results_filename_attributes[i], as.character(self$sample_data[sample_index, self$results_filename_attributes[i]]))
          } else if (i == length(self$results_filename_attributes) && !(self$results_filename_attributes[i] %in% names(self$sample_data))) { # set postfix
            postfix <- as.character(self$results_filename_attributes[i])
            pre_postfix_present[2] <- TRUE
          } else { # add "mid-fix" anyway
            filename_vector <- c(filename_vector, as.character(self$results_filename_attributes[i]))
          }
        }
        if (length(self$results_filename_attributes) == length(which(pre_postfix_present))) { # insert sample index
          filename_vector <- c(filename_vector, as.character(sample_index))
        }
        filename_vector <- c(filename_vector, postfix); paste(filename_vector, collapse = "_")
        return(paste(filename_vector, collapse = "_"))
      } else {
        return(sprintf("sample_%s_results", sample_index))
      }
    }

  ), # end public

  private = list(

  ## Attributes ##

  # Manager attributes #
  .manager_attributes = c("sample_data", "generators", "parallel_cores", "results_dir", "results_ext",
                          "results_filename_attributes"),
  .sample_data = NULL,
  .generators = NULL,
  .parallel_cores = 1,
  .results_dir = NULL,
  .results_ext = ".RData",
  .results_filename_attributes = NULL,

  # Errors and warnings #
  .error_messages = NULL,
  .warning_messages = NULL

  ), # end private

  # Active binding accessors for private manager attributes (above) #
  active = list(

    #' @field sample_data A data frame of sampled parameters for each simulation/result.
    sample_data = function(value) {
      if (missing(value)) {
        private$.sample_data
      } else {
        if (is.data.frame(value) || is.matrix(value)) {
          private$.sample_data <- data.frame(value)
        } else if (is.null(value)) {
          private$.sample_data <- value
        } else {
          stop("Sample data should be a data frame", call. = FALSE)
        }
      }
    },

    #' @field generators A list of generators (\code{\link{Generator}} or inherited class) objects for generating simulation model values.
    generators = function(value) {
      if (missing(value)) {
        private$.generators
      } else {
        if (!is.null(value) && (!is.list(value) ||
                                !all(unlist(lapply(value, function(value) "Generator" %in% class(value)))))){
          stop("Generators must be a list of Generator or inherited class objects", call. = FALSE)
        } else {
          private$.generators <- value
        }
      }
    },

    #' @field parallel_cores Number of cores for running the simulations in parallel.
    parallel_cores = function(value) {
      if (missing(value)) {
        private$.parallel_cores
      } else {
        private$.parallel_cores <- value
      }
    },

    #' @field results_dir Results directory path.
    results_dir = function(value) {
      if (missing(value)) {
        private$.results_dir
      } else {
        private$.results_dir <- value
      }
    },

    #' @field results_ext Result file extension (default is .RData).
    results_ext = function(value) {
      if (missing(value)) {
        private$.results_ext
      } else {
        private$.results_ext <- value
      }
    },

    #' @field results_filename_attributes A vector of: prefix (optional); attribute names (from the sample data frame); postfix (optional); utilized to construct results filenames.
    results_filename_attributes = function(value) {
      if (missing(value)) {
        private$.results_filename_attributes
      } else {
        private$.results_filename_attributes <- value
      }
    },

    #' @field error_messages A vector of error messages encountered.
    error_messages = function(value) {
      if (missing(value)) {
        private$.error_messages
      } else {
        if (!is.null(value)) {
          private$.error_messages <- unique(c(private$.error_messages, value))
        } else {
          private$.error_messages <- value
        }
      }
    },

    #' @field warning_messages A vector of warning messages encountered.
    warning_messages = function(value) {
      if (missing(value)) {
        private$.warning_messages
      } else {
        if (!is.null(value)) {
          private$.warning_messages <- unique(c(private$.warning_messages, value))
        } else {
          private$.warning_messages <- value
        }
      }
    }

  ) # end active
)
