#' R6 class representing a dynamic attribute generator
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class representing a model that dynamically generates
#' attribute values (\emph{outputs}) via reading data from files, running assigned
#' functions, generating sample distributions, or built-in functions (assigned as
#' \emph{default} in inherited classes), using simulation sample parameters
#' (\emph{inputs}).
#'
#' @examples
#' # U Island example region
#' coordinates <- data.frame(
#'   x = rep(seq(177.01, 177.05, 0.01), 5),
#'   y = rep(seq(-18.01, -18.05, -0.01), each = 5)
#' )
#' coordinates <- coordinates[c(7, 9, 12, 14, 17:19), ]
#' region <- Region$new(coordinates = coordinates, use_raster = FALSE)
#' # Spatial correlation
#' spatial_correlation <- SpatialCorrelation$new(
#'   region = region, correlation_amplitude = 0.6,
#'   correlation_breadth = 300
#' )
#' spatial_correlation$calculate_compact_decomposition(decimals = 4)
#' # Example habitat suitability in file
#' saveRDS(
#'   array(c(0.5, 0.3, 0.7, 0.9, 0.6, 0.7, 0.8), c(7, 5)),
#'   file.path(tempdir(), "hs_mean_1.RData")
#' )
#' # Generator
#' capacity_gen <- Generator$new(
#'   description = "capacity",
#'   region = region,
#'   time_steps = 5,
#'   spatial_correlation = spatial_correlation,
#'   temporal_correlation = 0.9,
#'   hs_sd = 0.1, # template attached
#'   inputs = c("hs_file", "density_max", "initial_n"),
#'   outputs = c("initial_abundance", "carrying_capacity")
#' )
#' capacity_gen$add_generative_requirements(list(
#'   hs_mean = "file",
#'   hs_sample = "distribution",
#'   carrying_capacity = "function",
#'   initial_abundance = "function"
#' ))
#' # File template for mean habitat suitability
#' capacity_gen$add_file_template("hs_mean",
#'   path_template = file.path(tempdir(), "hs_mean_%s.RData"),
#'   path_params = c("hs_file"), file_type = "RDS"
#' )
#' # Distribution template for sampling habitat suitability
#' capacity_gen$add_distribution_template("hs_sample",
#'   distr_type = "beta",
#'   distr_params = list(
#'     mean = "hs_mean",
#'     sd = "hs_sd"
#'   )
#' )
#' # Function templates for initial abundance and carrying capacity
#' capacity_gen$add_function_template("initial_abundance",
#'   function_def = function(params) {
#'     stats::rmultinom(1,
#'       size = params$initial_n,
#'       prob = params$hs_sample[, 1]
#'     )
#'   },
#'   call_params = c("initial_n", "hs_sample")
#' )
#' capacity_gen$add_function_template("carrying_capacity",
#'   function_def = function(params) {
#'     round(params$density_max * params$hs_sample)
#'   },
#'   call_params = c("density_max", "hs_sample")
#' )
#' # Generation
#' capacity_gen$generate(input_values = list(
#'   hs_file = 1,
#'   initial_n = 400,
#'   density_max = 100
#' ))
#'
#' @importFrom R6 R6Class
#' @importFrom metRology qtri
#' @importFrom qs qread
#' @include SpatialModel.R
#' @include GenerativeTemplate.R
#' @export Generator

Generator <- R6Class("Generator",
  inherit = SpatialModel,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericModel & SpatialModel) #
    #   get_attribute_names()
    #   get_attribute(param)
    #   get_attribute_aliases(params = NULL)
    #   set_attributes(params = list(), ...)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets the generative template and requirements as well as any attributes passed via a \emph{params} list or individually.
    #' @param generative_template A \code{\link{GenerativeTemplate}} (or inherited class) object containing the file, function and/or distribution templates utilized (facilitates shallow cloning).
    #' @param generative_requirements A list of attribute names and the template setting ("file", "function", or "distribution") that is required to generate their values.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(generative_template = NULL, generative_requirements = NULL, ...) {
      if (!is.null(generative_template)) {
        self$generative_template <- generative_template
      } else {
        self$generative_template <- GenerativeTemplate$new()
      }
      if (is.null(generative_requirements)) {
        self$generative_requirements <- list()
      } else {
        self$generative_requirements <- generative_requirements
      }
      if (is.null(generative_template)) { # Set any user-named parameters to attach to the template (via aliases)
        param_names <- names(c(list(...), list(...)$params))
        self$attribute_aliases <- c(self$attribute_aliases, list())
        for (param in param_names[!param_names %in% c("attribute_aliases", "params", private$.active_attributes)]) {
          self$attribute_aliases[[param]] <- paste0("template_attached$", param)
        }
      }
      super$initialize(...)
      if (is.null(self$description)) {
        self$description <- "unnamed"
      }
    },

    #' @description
    #' Creates a new (re-initialized) object of the current (inherited) object class with optionally passed parameters.
    #' @param ... Parameters passed via the inherited class constructor (defined in initialize and run via new).
    #' @return New object of the current (inherited) class.
    new_clone = function(...) {
      return(super$new_clone(
        generative_template = self$generative_template,
        generative_requirements = self$generative_requirements, ...
      ))
    },

    #' @description
    #' Returns a list of existing and template-generated values for selected attributes or attribute aliases (when array of parameter names provided), or all existing attributes (when no params).
    #' @param params Array of attribute names to return, including those to be template-generated (all when NULL).
    #' @return List of selected or all attributes values.
    get_attributes = function(params = NULL) {
      attribute_list <- super$get_attributes(params)
      if (any(c("error_messages", "warning_messages") %in% names(attribute_list))) {
        attribute_list$error_messages <- NULL
        attribute_list$warning_messages <- NULL
      }
      if (!is.null(params)) {
        for (param in params[!params %in% names(attribute_list)]) {
          required_type <- self$generative_requirements[[param]]
          if (param %in% names(self$attribute_aliases) && is.null(required_type) &&
            !(param %in% c(names(self$file_templates), names(self$function_templates), names(self$distribution_templates)))) {
            attribute <- self$attribute_aliases[[param]]
            attribute_root <- unlist(strsplit(attribute, "$", fixed = TRUE))[1]
            attribute_root <- unlist(strsplit(attribute_root, "[", fixed = TRUE))[1]
            required_type <- self$generative_requirements[[attribute]]
          } else {
            attribute_root <- attribute <- param
          }
          if ((is.null(required_type) || required_type == "file") && attribute_root %in% names(self$file_templates)) {
            eval(parse(text = sprintf("%s <- self$read_file(attribute_root)", attribute_root)))
            attribute_list[[param]] <- eval(parse(text = attribute))
          } else if ((is.null(required_type) || required_type == "function") && attribute_root %in% names(self$function_templates)) {
            eval(parse(text = sprintf("%s <- self$run_function(attribute_root)", attribute_root)))
            attribute_list[[param]] <- eval(parse(text = attribute))
          } else if ((is.null(required_type) || required_type == "distribution") && attribute_root %in% names(self$distribution_templates)) {
            eval(parse(text = sprintf("%s <- self$sample_distribution(attribute_root)", attribute_root)))
            attribute_list[[param]] <- eval(parse(text = attribute))
          }
        }
      }
      for (param in names(attribute_list)) {
        # Check consistency of any rasters
        if (!is.null(self$region) && any(class(attribute_list[[param]])[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")) &&
          !self$region$raster_is_consistent(attribute_list[[param]])) {
          self$error_messages <- paste("Region is inconsistent with template generated raster for attribute: ", param)
        }
        if (!is.null(attribute_list[[param]]) && any(c(param, self$attribute_aliases[[param]]) %in% self$outputs) &&
          (is.numeric(attribute_list[[param]]) ||
            class(attribute_list[[param]])[1] %in% c("data.frame", "RasterLayer", "RasterStack", "RasterBrick"))) {
          # Apply decimal rounding
          if (!is.null(self$decimals)) {
            attribute_list[[param]][] <- round(attribute_list[[param]][], self$decimals)
          }
          # Generate raster or array?
          if (!is.null(self$region)) {
            if (class(attribute_list[[param]])[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
              if (!self$generate_rasters) { # raster to array
                attribute_list[[param]] <- attribute_list[[param]][self$region$region_indices]
              }
            } else {
              if (self$generate_rasters && nrow(as.matrix(attribute_list[[param]])) == self$region$region_cells) { # array to raster
                attribute_list[[param]] <- self$region$raster_from_values(attribute_list[[param]])
              }
            }
          }
          # Apply occupancy mask
          if (!is.null(self$occupancy_mask)) {
            if (length(as.matrix(attribute_list[[param]][])) < length(as.matrix(self$occupancy_mask[]))) {
              if (nrow(as.matrix(attribute_list[[param]][])) == nrow(as.matrix(self$occupancy_mask[])) &&
                ncol(as.matrix(attribute_list[[param]][])) == 1) { # use first column of mask
                attribute_list[[param]][] <- attribute_list[[param]][] * self$occupancy_mask[][, 1]
              } # else don't apply mask
            } else {
              attribute_list[[param]][] <- as.matrix(attribute_list[[param]][] * self$occupancy_mask[])
            }
          }
        }
      }
      if (any(c("error_messages", "warning_messages") %in% params)) {
        attribute_list <- c(attribute_list, super$get_attributes(c("error_messages", "warning_messages")))
      }
      return(attribute_list)
    },

    # New methods #

    #' @description
    #' Returns a list of generated output values (attributes) corresponding to the sample input values (attributes).
    #' @param input_values List of sample input values for generator attributes.
    #' @return List containing generated model output attributes and/or any error/warning messages.
    generate = function(input_values = list()) {
      if ((is.null(self$inputs) && length(input_values)) || is.null(self$outputs)) {
        missing <- c("inputs", "outputs")[which(c(is.null(self$inputs) && length(input_values), is.null(self$outputs)))]
        return(list(error_messages = sprintf("%s generation requires settings for: %s", self$description, paste(missing, collapse = ", "))))
      }
      inputs_present <- 0
      for (param in self$inputs) {
        if (any(c(param, self$get_attribute_aliases(param)) %in% names(input_values))) {
          inputs_present <- inputs_present + 1
        }
      }
      if (inputs_present == length(self$inputs)) {
        generative_requirements_satisfied <- self$generative_requirements_satisfied()
        if (all(unlist(generative_requirements_satisfied))) { # ensure the required file, function, and/or distribution templates are set for specified outputs
          return(self$new_clone(params = input_values)$get_attributes(c(self$outputs, "error_messages", "warning_messages")))
        } else {
          return(list(error_messages = sprintf("%s generation requires further settings for output(s): %s", self$description, paste(names(which(!unlist(generative_requirements_satisfied))), collapse = ", "))))
        }
      } else {
        return(list(error_messages = sprintf("%s generation requires inputs: %s", self$description, paste(self$inputs, collapse = ", "))))
      }
    },

    #' @description
    #' Adds a file template for reading raster/RData(RDS)/CSV files for a given
    #'  model attribute.
    #' @param param Name of model attribute to be read from a file.
    #' @param path_template Template string for the file path with placeholders
    #'  (see \code{\link{sprintf}}) for simulation sample parameters.
    #' @param path_params Array of the names of the simulation sample 
    #' parameters to be substituted (in order) into the path template.
    #' @param file_type File type raster \emph{"GRD"} (default), \emph{"TIF"},
    #'  \emph{"RData/RDS"}, \emph{"QS"}, or \emph{"CSV"} to be read.
    add_file_template = function(param, path_template, path_params = c(), file_type = "GRD") {
      if (is.character(param) && is.character(path_template)) {
        if (length(path_params) == (length(strsplit(paste0("#", path_template, "#"), "%s")[[1]]) - 1)) {
          self$file_templates[[param]] <- list()
          self$file_templates[[param]]$path_template <- path_template
          self$file_templates[[param]]$path_params <- c(path_params)
          if (toupper(file_type) == "GRD" || toupper(file_type) == "RDS" || 
          toupper(file_type) == "CSV" || toupper(file_type) == "TIF" || 
          toupper(file_type) == "QS"
          ) {
            self$file_templates[[param]]$file_type <- toupper(file_type)
          } else {
            stop("The file type should be GRD (raster), TIF (raster), RDS, QS, or CSV", call. = FALSE)
          }
        } else {
          stop("Ensure the path template contains a corresponding %s for each path parameter", call. = FALSE)
        }
      } else {
        stop("The parameter name and path template should be strings", call. = FALSE)
      }
    },

    #' @description
    #' Adds a function template for running a user-defined function to calculate a given model attribute.
    #' @param param Name of model attribute to be generated using a function.
    #' @param function_def Function definition (or path to the file containing the function) in form: \code{function(params)}, where \emph{params} is a list passed to the function.
    #' @param call_params Array of the names of the model parameters/attributes to be passed into the function via a list: \emph{params}.
    add_function_template = function(param, function_def, call_params = c()) {
      if (!is.character(param)) {
        stop("The parameter name should be a string", call. = FALSE)
      }
      # Assign function definition from a file or directly
      if (is.character(function_def)) {
        if (file.exists(function_def) && length(grep(".R", toupper(function_def), fixed = TRUE))) { # from file
          tryCatch(
            {
              function_def <- source(function_def)$value # direct assignment from a file
            },
            error = function(e) {
              stop(paste("Error loading function from file", function_def, ":", as.character(e)), call. = FALSE)
            }
          )
        } else { # named function
          tryCatch(
            {
              function_def <- eval(parse(text = function_def))
            },
            error = function(e) {
              stop(paste("Could not assign function", function_def, ":", as.character(e)), call. = FALSE)
            }
          )
        }
      }
      if (!is.function(function_def)) {
        stop(paste("Could not assign function", function_def), call. = FALSE)
      } else if (length(formals(function_def)) != 1) {
        stop("The function definition should have one argument, e.g. function(params)", call. = FALSE)
      }
      self$function_templates[[param]] <- list(function_def = function_def, call_params = call_params)
    },

    #' @description
    #' Adds a distribution template for generating a given model attribute via sampling a distribution.
    #' @param param Name of model attribute to be generated via sampling a distribution.
    #' @param distr_type Distribution type to sample from (uniform, normal, lognormal, beta or triangular).
    #' @param distr_params List of distribution parameters and their values or associated model attributes (uniform: lower, upper; normal: mean, sd; lognormal: meanlog, sdlog (or mean, sd); beta: alpha, beta (or mean, sd); triangular: lower, mode, upper).
    #' @param sample Model attribute(s) name(s) or values associated with single sample probabilities (0-1), or bounds as a vector (e.g. \code{sample = c("p_lower", "p_upper")}), or as a list (e.g. \code{sample = list(mid = "p", window = 0.2)} for bounds p +/- 0.1).
    #' @param random_seed Random seed utilized when sample probability is generated internally, via bounds, and/or correlated deviates.
    #' @param normalize_threshold Optional normalization threshold is utilized when generated values are to be normalized with a fixed upper limit/threshold.
    add_distribution_template = function(param, distr_type = c("uniform", "normal", "lognormal", "beta", "triangular"), distr_params = list(), sample = NULL, random_seed = NULL, normalize_threshold = NULL) {
      if (!is.character(param)) {
        stop("The parameter name should be a string", call. = FALSE)
      }
      if (!is.null(distr_type) && (!is.character(distr_type) || !(distr_type %in% c("uniform", "normal", "lognormal", "beta", "triangular")))) {
        stop("The distribution type should be one of: \"uniform\", \"normal\", \"lognormal\", \"beta\", \"triangular\"", call. = FALSE)
      }
      distr_type <- match.arg(distr_type)
      expected_distr_params <- switch(distr_type,
        uniform = c("lower", "upper"),
        normal = c("mean", "sd"),
        lognormal = list(c("meanlog", "sdlog"), c("mean", "sd")),
        beta = list(c("alpha", "beta"), c("mean", "sd")),
        triangular = c("lower", "mode", "upper")
      )
      if (is.list(expected_distr_params)) {
        if (!is.list(distr_params) || !(all(expected_distr_params[[1]] %in% names(distr_params)) ||
          all(expected_distr_params[[2]] %in% names(distr_params)))) {
          stop(sprintf(
            "The distribution parameters should be a list containing %s distribution parameters: %s (or %s)", distr_type,
            paste(expected_distr_params[[1]], collapse = ", "), paste(expected_distr_params[[2]], collapse = ", ")
          ), call. = FALSE)
        }
      } else if (!is.list(distr_params) || !all(expected_distr_params %in% names(distr_params))) {
        stop(sprintf("The distribution parameters should be a list containing %s distribution parameters: %s", distr_type, paste(expected_distr_params, collapse = ", ")), call. = FALSE)
      }
      if (!is.null(sample) && !(all(unlist(lapply(sample, is.numeric)) | unlist(lapply(sample, is.character))))) { #
        stop("The sample attribute(s) should be parameter name string(s) or numeric", call. = FALSE)
      }
      if (!is.null(random_seed) && !is.numeric(random_seed)) {
        stop("The random seed should be numeric", call. = FALSE)
      }
      self$distribution_templates[[param]] <- list(
        distr_type = distr_type,
        distr_params = distr_params,
        sample = sample,
        random_seed = random_seed,
        normalize_threshold = normalize_threshold
      )
    },

    #' @description
    #' Reads and returns the value of a model attribute from a file using the corresponding file template and simulation sample parameters.
    #' @param param Name of model attribute to be read from the file.
    #' @return Model attribute value read from a file.
    read_file = function(param) {
      if (param %in% names(self$file_templates)) {
        path_template <- self$file_templates[[param]]$path_template
        path_params <- self$file_templates[[param]]$path_params
        path_param_list <- self$get_attributes(path_params)
        if (all(path_params %in% names(path_param_list))) {
          file_path <- eval(parse(text = paste0("sprintf(path_template, ", paste0(sprintf("path_param_list[[%d]]", 1:length(path_param_list)), collapse = ", "), ")")))
          if (is.character(file_path) && file.exists(file_path)) {
            value_list <- list()
            if (self$file_templates[[param]]$file_type == "CSV") {
              value_list[[param]] <- utils::read.csv(file = file_path)
            } else if (self$file_templates[[param]]$file_type == "RDS") { # RDS (RData)
              value_list[[param]] <- readRDS(file = file_path)
            } else if (self$file_templates[[param]]$file_type == "QS") {
              value_list[[param]] <- qread(file = file_path)
            } else { # raster
              value_list[[param]] <- raster::brick(file_path)
            }
            if (!param %in% self$model_attributes) {
              self$set_attributes(value_list)
            }
            return(value_list[[param]])
          } else {
            self$error_messages <- paste("Error reading file", file_path)
            return(NULL)
          }
        } else {
          missing_params <- path_params[which(!path_params %in% names(path_param_list))]
          self$error_messages <- paste("Missing parameter(s) for file template: ", paste(missing_params, collapse = ", "))
          return(NULL)
        }
      }
    },

    #' @description
    #' Returns the calculated value of a model attribute using the corresponding function template and model simulation sample parameters.
    #' @param param Name of model attribute to be calculated using a function.
    #' @return Model attribute value calculated using a function.
    run_function = function(param) {
      if (param %in% names(self$function_templates)) {
        call_params <- self$function_templates[[param]]$call_params
        call_param_list <- self$get_attributes(call_params)
        if (all(call_params %in% names(call_param_list))) {
          value_list <- list()
          value_list[[param]] <- self$function_templates[[param]]$function_def(call_param_list)
          if (!param %in% self$model_attributes) {
            self$set_attributes(value_list)
          }
          return(value_list[[param]])
        } else {
          missing_params <- call_params[which(!call_params %in% names(call_param_list))]
          self$error_messages <- paste("Missing parameter(s) for function template: ", paste(missing_params, collapse = ", "))
          return(NULL)
        }
      }
    },

    #' @description
    #' Returns the calculated value of a model attribute using the corresponding distribution template and simulation sample parameters.
    #' @param param Name of model attribute to be calculated using a sampling distribution.
    #' @return Model attribute value calculated via distribution sampling.
    sample_distribution = function(param) {
      if (param %in% names(self$distribution_templates)) {
        # Unpack template parameters
        distr_type <- self$distribution_templates[[param]][["distr_type"]]
        distr_params <- self$distribution_templates[[param]][["distr_params"]]
        sample <- self$distribution_templates[[param]][["sample"]]
        sample_names <- names(sample)
        if (is.list(sample) && all(sample_names %in% c("mid", "window"))) { # window-based
          if (is.character(sample$mid)) sample$mid <- self$get_attribute(sample$mid)
          if (is.character(sample$window)) sample$window <- self$get_attribute(sample$window)
          if (is.numeric(sample$mid) && is.numeric(sample$window)) {
            sample <- c(max(sample$mid - sample$window / 2, 0), min(sample$mid + sample$window / 2, 1))
          } else {
            self$error_messages <- sprintf(
              "The distribution sample for %s utilizes missing parameter(s): %s", param,
              paste(unlist(self$distribution_templates[[param]][["sample"]][which(!sample_names %in% names(sample))]), collapse = ", ")
            )
            return(NULL)
          }
        } else if (!is.null(sample)) { # single or c(lower, upper) bounds
          for (i in 1:length(sample)) {
            if (is.character(sample[i])) {
              sample_value <- self$get_attribute(sample[i])
              if (length(sample_value) > length(sample[i])) {
                sample <- sample_value
              } else if (!is.null(sample_value)) {
                sample[i] <- sample_value
              }
            }
          }
          sample <- suppressWarnings(as.numeric(sample))
          if (any(is.na(sample))) {
            self$error_messages <- sprintf(
              "The distribution sample for %s utilizes missing parameter(s): %s", param,
              paste(self$distribution_templates[[param]][["sample"]][which(is.na(sample))])
            )
            return(NULL)
          }
        }
        random_seed <- self$distribution_templates[[param]][["random_seed"]]
        normalize_threshold <- self$distribution_templates[[param]][["normalize_threshold"]]
        if (is.character(normalize_threshold)) {
          normalize_threshold <- self$get_attribute(normalize_threshold)
        }

        # Ensure the spatial correlation object is set when required
        if (self$uses_correlations && is.null(self$spatial_correlation)) {
          self$error_messages <- "The spatial correlation object for generating sample deviates needs to be set first"
          return(NULL)
        }

        # Check sample
        if (!is.null(sample) && !all(sample >= 0 & sample <= 1)) {
          self$error_messages <- sprintf("The distribution sample for %s should utilize existing parameters with values between 0 and 1 (inclusive)", param)
          return(NULL)
        }

        # Generate (correlated) random samples
        if (self$uses_correlations) {
          # Use a spatial correlation object to generate correlated normal deviates using the random seed when present
          if (self$temporal_correlation < 1) {
            correlated_deviates <- self$spatial_correlation$generate_correlated_normal_deviates(
              random_seed = random_seed,
              temporal_correlation = self$temporal_correlation,
              time_steps = self$time_steps
            )
          } else {
            correlated_deviates <- self$spatial_correlation$generate_correlated_normal_deviates(random_seed = random_seed)
          }
          if (is.character(correlated_deviates)) { # return with error message
            self$error_messages <- correlated_deviates
            return(NULL)
          }
          # Convert normal correlated deviates to correlated uniform samples between lower and upper values if present
          if (length(sample) > 1) {
            generated_samples <- sample[1] + pnorm(correlated_deviates) * (sample[2] - sample[1])
          } else { # use 0-1
            generated_samples <- pnorm(correlated_deviates)
          }
        } else { # not correlated
          if (length(sample) == 1) {
            generated_samples <- sample
          } else {
            if (is.null(sample)) {
              sample <- c(0, 1)
            }
            if (!is.null(random_seed)) {
              set.seed(random_seed)
            }
            sample_number <- 1
            if (!is.null(self$region)) {
              sample_number <- self$region$region_cells
            }
            generated_samples <- stats::runif(sample_number, min = sample[1], max = sample[2])
          }
        }

        # Substitute attribute values, unpack rasters, and replace non-finites where necessary
        distr_param_values <- list()
        for (distr_param in names(distr_params)) {
          if (is.character(distr_params[[distr_param]])) {
            distr_param_values[[distr_param]] <- self$get_attributes(distr_params[[distr_param]])[[distr_params[[distr_param]]]]
          } else {
            distr_param_values[[distr_param]] <- distr_params[[distr_param]]
          }
          if (!is.null(distr_param_values[[distr_param]])) {
            if (!is.null(self$region) && self$region$use_raster &&
              any(class(distr_param_values[[distr_param]]) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
              distr_param_values[[distr_param]] <- distr_param_values[[distr_param]][self$region$region_indices]
            }
            if (distr_type %in% c("uniform", "triangular")) {
              distr_param_values[[distr_param]][which(!is.finite(distr_param_values[[distr_param]]))] <- NA
            } else {
              distr_param_values[[distr_param]][which(is.nan(distr_param_values[[distr_param]]))] <- NA
            }
          }
        }

        # Ensure all distribution parameters have values
        if (!all(names(distr_params) %in% names(distr_param_values))) {
          missing_values <- names(distr_params)[which(!names(distr_params) %in% names(distr_param_values))]
          self$error_messages <- sprintf(
            "Values for %s distribution not obtained for parameters: %s (via %s)",
            distr_type, paste(missing_values, collapse = ", "),
            paste(unlist(distr_params[missing_values]), collapse = ", ")
          )
          return(NULL)
        }

        # Generate attribute sampled from distribution
        switch(distr_type,
          uniform = generated_values <- stats::qunif(generated_samples,
            min = distr_param_values$lower[],
            max = distr_param_values$upper[]
          ),
          normal = generated_values <- stats::qnorm(generated_samples,
            mean = distr_param_values$mean[],
            sd = distr_param_values$sd[]
          ),
          lognormal = {
            if (all(c("mean", "sd") %in% names(distr_param_values))) { # transform
              distr_param_values$meanlog <- log(distr_param_values$mean^2 / sqrt(distr_param_values$mean^2 + distr_param_values$sd^2))
              distr_param_values$sdlog <- sqrt(log(1 + distr_param_values$sd^2 / distr_param_values$mean^2))
            }
            # Get shape of generated values and indices of available values
            generated_values <- (generated_samples + distr_param_values$meanlog[] + distr_param_values$sdlog[])
            available_indices <- which(!is.na(generated_values))
            generated_values[] <- 1
            # Set the shape of the samples and distribution parameters consistently
            generated_samples <- generated_samples * generated_values
            distr_param_values <- lapply(distr_param_values, function(x) x * generated_values)
            if (all(c("mean", "sd") %in% names(distr_param_values))) {
              generated_values <- distr_param_values$mean
            } else {
              generated_values <- array(NA, dim(distr_param_values$meanlog))
            }
            # Generate finite values (eliminates warnings)
            generated_values[available_indices] <- stats::qlnorm(generated_samples[available_indices],
              meanlog = distr_param_values$meanlog[available_indices],
              sdlog = distr_param_values$sdlog[available_indices]
            )
          },
          beta = {
            if (all(c("mean", "sd") %in% names(distr_param_values))) { # transform
              distr_param_values$sd <- array(distr_param_values$sd, dim(as.matrix(distr_param_values$mean * distr_param_values$sd)))
              finite_indices <- which(is.finite(distr_param_values$mean * distr_param_values$sd))
              # Limit sd to < sqrt(mean*(1 - mean))
              distr_param_values$sd[finite_indices] <- pmin(
                distr_param_values$sd[finite_indices],
                sqrt(distr_param_values$mean[finite_indices] * (1 - distr_param_values$mean[finite_indices])) * 0.999
              )
              distr_param_values$alpha <- distr_param_values$mean * (distr_param_values$mean * (1 - distr_param_values$mean) / distr_param_values$sd^2 - 1)
              distr_param_values$beta <- (1 - distr_param_values$mean) * (distr_param_values$mean * (1 - distr_param_values$mean) / distr_param_values$sd^2 - 1)
            }
            # Get shape of generated values and indices of available values
            generated_values <- (generated_samples + distr_param_values$alpha[] + distr_param_values$beta[])
            available_indices <- which(!is.na(generated_values))
            generated_values[] <- 1
            # Set the shape of the samples and distribution parameters consistently
            generated_samples <- generated_samples * generated_values
            distr_param_values <- lapply(distr_param_values, function(x) x * generated_values)
            generated_values <- array(NA, dim(generated_values))
            generated_values[available_indices] <- stats::qbeta(generated_samples[available_indices],
              shape1 = distr_param_values$alpha[available_indices],
              shape2 = distr_param_values$beta[available_indices]
            )
          },
          triangular = {
            # Get shape of generated values and indices of any expected NAs
            generated_values <- (generated_samples + distr_param_values$lower[] +
              distr_param_values$upper[] + distr_param_values$mode[])
            finite_indices <- which(is.finite(generated_values))
            generated_values[] <- 1
            # Set the shape of the samples and distribution parameters consistently
            generated_samples <- generated_samples * generated_values
            distr_param_values <- lapply(distr_param_values, function(x) x * generated_values)
            # Generate finite values (eliminates NAs which cause qtri errors)
            finite_values <- qtri(generated_samples[finite_indices],
              min = distr_param_values$lower[finite_indices],
              max = distr_param_values$upper[finite_indices],
              mode = distr_param_values$mode[finite_indices]
            )
            # Correct flat cases (lower >= upper)
            flat_indices <- which(distr_param_values$lower[finite_indices] >= distr_param_values$upper[finite_indices])
            finite_values[flat_indices] <- distr_param_values$upper[finite_indices][flat_indices]
            # Merge the finite values in the expected shape
            generated_values[] <- NA
            generated_values[finite_indices] <- finite_values
          }
        )

        # Normalize with threshold
        if (is.numeric(normalize_threshold)) {
          generated_values <- generated_values / normalize_threshold
          generated_values[which(generated_values > 1)] <- 1
        }

        # Return as raster or values
        value_list <- list()
        if (!is.null(self$region) && self$generate_rasters) {
          value_list[[param]] <- self$region$raster_from_values(generated_values)
        } else {
          value_list[[param]] <- generated_values
        }
        if (!param %in% self$model_attributes) {
          self$set_attributes(value_list)
        }
        return(value_list[[param]])
      }
    },

    #' @description
    #' Adds attribute names and the template setting (\emph{"file"}, \emph{"function"} or \emph{"distribution"}) that is required to generate their values (via a \emph{params} list or individually).
    #' @param params Parameters passed via a list (e.g. \code{params = list(attr1 = "file", attr2 = "function", attr3 = "distribution")}).
    #' @param ... Parameters passed individually (e.g. \code{attr3 = "file"}).
    add_generative_requirements = function(params = list(), ...) {
      params <- c(list(...), params) # prioritise individual parameters
      invalid_params <- list()
      # Set via params list when valid
      for (param in names(params)) {
        if (params[[param]] %in% c("file", "function", "distribution")) {
          self$generative_requirements[[param]] <- params[[param]]
        } else {
          invalid_params[[param]] <- params[[param]]
        }
      }
      if (length(invalid_params)) { # return a message for invalid parameters
        return_message <- "Generative requirements must be added to model attributes only as 'file', 'function' or 'distribution':"
        for (param in names(invalid_params)) {
          return_message <- paste0(return_message, (sprintf(" %s = '%s' not added;", param, invalid_params[[param]])))
        }
        warning(return_message, call. = FALSE)
      }
    },

    #' @description
    #' Returns a boolean to indicate that all the file, function and/or distribution template settings that are required for attribute generation are present.
    #' @return Boolean to indicate that the required settings for attribute generation are present.
    generative_requirements_satisfied = function() {
      if (length(self$generative_requirements)) {
        satisfied <- self$generative_requirements
        satisfied[] <- FALSE
        for (param in names(self$generative_requirements)) {
          template <- self$generative_requirements[[param]]
          if (template == "file") {
            satisfied[[param]] <- param %in% names(self$file_templates)
            if (!satisfied[[param]]) {
              satisfied$file_templates <- FALSE
            }
          } else if (template == "function") {
            satisfied[[param]] <- param %in% names(self$function_templates)
            if (!satisfied[[param]]) {
              satisfied$function_templates <- FALSE
            }
          } else if (template == "distribution") {
            satisfied[[param]] <- param %in% names(self$distribution_templates)
            if (!satisfied[[param]]) {
              satisfied$distribution_templates <- FALSE
            }
          }
        }
        return(satisfied)
      } else { # no requirements
        return(TRUE)
      }
    }
  ), # end public

  private = list(

    ## Attributes ##

    # Model attributes #
    # .model_attributes  [inherited]
    # .region            [inherited]

    # Attributes accessible via model get/set methods #
    .active_attributes = c(
      "region", "coordinates", "description", "inputs", "outputs", "file_templates", "function_templates",
      "distribution_templates", "uses_correlations", "spatial_correlation", "temporal_correlation",
      "time_steps", "generate_rasters", "decimals", "occupancy_mask", "template_attached"
    ),

    # Dynamic attributes #
    # .attribute_aliases [inherited]

    # Generative attributes #
    .generative_template = NULL,
    .generative_requirements = NULL

    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]
  ), # end private

  # Active binding accessors for private attributes (above and template nested) #
  active = list(

    # Model attributes accessors #

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

    # Template-nested model attribute accessors #

    #' @field description A brief description of what the generator generates.
    description = function(value) {
      if (missing(value)) {
        self$generative_template$description
      } else {
        self$generative_template$description <- value
      }
    },

    #' @field inputs An array of input attribute names for the generator.
    inputs = function(value) {
      if (missing(value)) {
        self$generative_template$inputs
      } else {
        self$generative_template$inputs <- value
      }
    },

    #' @field outputs An array of output attribute names for the generator.
    outputs = function(value) {
      if (missing(value)) {
        self$generative_template$outputs
      } else {
        self$generative_template$outputs <- value
      }
    },

    #' @field file_templates A nested list of file template attributes.
    file_templates = function(value) {
      if (missing(value)) {
        self$generative_template$file_templates
      } else {
        self$generative_template$file_templates <- value
      }
    },

    #' @field function_templates A nested list of function template attributes.
    function_templates = function(value) {
      if (missing(value)) {
        self$generative_template$function_templates
      } else {
        self$generative_template$function_templates <- value
      }
    },

    #' @field distribution_templates A list of distribution template attributes.
    distribution_templates = function(value) {
      if (missing(value)) {
        self$generative_template$distribution_templates
      } else {
        self$generative_template$distribution_templates <- value
      }
    },

    #' @field uses_correlations A boolean to indicate that a \code{\link{SpatialCorrelation}} (or inherited class) object is used for generating correlated random deviates.
    uses_correlations = function(value) {
      if (missing(value)) {
        self$generative_template$uses_correlations
      } else {
        self$generative_template$uses_correlations <- value
      }
    },

    #' @field spatial_correlation A \code{\link{SpatialCorrelation}} (or inherited class) object for generating correlated random deviates.
    spatial_correlation = function(value) {
      if (missing(value)) {
        self$generative_template$spatial_correlation
      } else {
        if (!is.null(value) && !("SpatialCorrelation" %in% class(value))) {
          stop("Spatial correlation must be a SpatialCorrelation or inherited class object", call. = FALSE)
        } else {
          if (!is.null(value)) {
            if (!is.null(value$region) && !is.null(self$region) && !identical(value$region, self$region)) {
              stop("The spatial correlation object must have the same region class object as this generator", call. = FALSE)
            } else if (!is.null(value$region) && is.null(self$region)) {
              self$region <- value$region
            } else if (is.null(value$region) && !is.null(self$region)) {
              value$region <- self$region
            }
            self$uses_correlations <- TRUE
          } else {
            self$uses_correlations <- FALSE
          }
          self$generative_template$spatial_correlation <- value
        }
      }
    },

    #' @field temporal_correlation Absolute correlation coefficient between simulation time steps for all grid cells (0-1; default = 1).
    temporal_correlation = function(value) {
      if (missing(value)) {
        self$generative_template$temporal_correlation
      } else {
        self$generative_template$temporal_correlation <- value
      }
    },

    #' @field time_steps Number of simulation time steps.
    time_steps = function(value) {
      if (missing(value)) {
        self$generative_template$time_steps
      } else {
        self$generative_template$time_steps <- value
      }
    },

    #' @field generate_rasters Boolean to indicate if rasters should be generated (defaults to TRUE when region uses rasters).
    generate_rasters = function(value) {
      if (missing(value)) {
        if (is.null(self$generative_template$generate_rasters)) {
          if (!is.null(self$region)) {
            self$region$use_raster
          } else {
            FALSE
          }
        } else {
          self$generative_template$generate_rasters
        }
      } else {
        if (!is.null(value)) {
          value <- as.logical(value)
          if (value && is.null(self$region)) {
            stop("Rasters can only be generated when a region is defined that uses rasters", call. = FALSE)
          }
        }
        self$generative_template$generate_rasters <- value
      }
    },

    #' @field decimals Number of decimal places applied to generated data outputs (default: NULL = no rounding).
    decimals = function(value) {
      if (missing(value)) {
        self$generative_template$decimals
      } else {
        self$generative_template$decimals <- value
      }
    },

    #' @field occupancy_mask Optional binary mask array (matrix), data frame, or raster (stack) for generated (time-series) data outputs.
    occupancy_mask = function(value) {
      if (missing(value)) {
        self$generative_template$occupancy_mask
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            value <- utils::read.csv(file = value)
          } else if (length(grep(".RDATA", toupper(value), fixed = TRUE)) || length(grep(".RDS", toupper(value), fixed = TRUE))) {
            value <- readRDS(file = value)
          } else if (length(grep(".GRD", toupper(value), fixed = TRUE))) {
            value <- raster::brick(value)
          } else if (length(grep(".TIF", toupper(value), fixed = TRUE))) {
            value <- raster::brick(value)
          } else {
            value <- utils::read.table(file = value)
          }
        }
        if (!is.null(self$region)) {
          if (any(class(value) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
            if (!self$region$raster_is_consistent(value)) {
              stop("Occupancy mask raster must be consistent with the defined region raster", call. = FALSE)
            }
            if (!self$generate_rasters) {
              value <- value[self$region$region_indices]
            }
          } else if (self$generate_rasters) {
            stop("Occupancy mask must be a raster layer, stack or brick consistent with the defined region", call. = FALSE)
          }
        }
        self$generative_template$occupancy_mask <- value
      }
    },

    #' @field template_attached A list of template-nested dynamically attached model attributes that are maintained via shallow or \emph{new} cloning.
    template_attached = function(value) {
      if (missing(value)) {
        self$generative_template$attached
      } else {
        self$generative_template$attached <- value
      }
    },

    # Local (non-nested) model attribute accessors #
    #   in inherited classes

    # Dynamic attribute accessors #

    #' @field attribute_aliases A list of alternative alias names for model attributes (form: \code{alias = "attribute"}) to be used with the set and get attributes methods.
    attribute_aliases = function(value) { # inherited
      if (missing(value)) {
        super$attribute_aliases
      } else {
        super$attribute_aliases <- value
      }
    },

    # Generative attribute accessors #

    #' @field generative_template A nested \code{\link{GenerativeTemplate}} (or inherited class) object for model attributes that are maintained via shallow or \emph{new} cloning.
    generative_template = function(value) {
      if (missing(value)) {
        private$.generative_template
      } else {
        private$.generative_template <- value
      }
    },

    #' @field generative_requirements A list of attribute names and the template setting (\emph{"file"}, \emph{"function"}, or \emph{"default"}) that is required to generate their values.
    generative_requirements = function(value) {
      if (missing(value)) {
        private$.generative_requirements
      } else {
        private$.generative_requirements <- value
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
