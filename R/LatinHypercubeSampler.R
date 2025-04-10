#' R6 class to represent a Latin hypercube sampler.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class that generates Latin hypercube samples (using
#' \code{\link[lhs:randomLHS]{randomLHS}}) for parameters drawn from configured
#' distributions: \code{\link[stats:Uniform]{uniform}},
#' \code{\link[stats:Poisson]{Poisson}},
#' \code{\link[stats:Normal]{normal}},
#' \code{\link[stats:Lognormal]{lognormal}},
#' \code{\link[stats:Beta]{beta}},
#' \code{\link[truncnorm:qtruncnorm]{truncated normal}} or
#' \code{\link[metRology:qtri]{triangular}}.
#' It generates a data frame of sample values.
#'
#' @examples
#' lhs_gen <- LatinHypercubeSampler$new(parameter_names = c("size", "age", "km", "price"))
#' lhs_gen$set_class_parameter("size", c("small", "medium", "large"))
#' lhs_gen$set_uniform_parameter("age", lower = 18, upper = 70, decimals = 0)
#' lhs_gen$set_poisson_parameter("offspring", lambda = 2)
#' lhs_gen$set_normal_parameter("km", mean = 50000, sd = 20000, decimals = 0)
#' lhs_gen$set_truncnorm_parameter("kg", mean = 75, sd = 20, lower = 0, upper = Inf, decimals = 2)
#' lhs_gen$set_lognormal_parameter("price", mean = 30000, sd = 10000, decimals = 0)
#' lhs_gen$set_beta_parameter("tread", mean = 0.7, sd = 0.1, decimals = 2)
#' lhs_gen$set_triangular_parameter("rating",
#'   lower = 0, upper = 10, mode = 5,
#'   decimals = 1
#' )
#' lhs_gen$generate_samples(number = 10, random_seed = 123)
#'
#' @importFrom R6 R6Class
#' @importFrom truncnorm qtruncnorm
#' @importFrom metRology qtri
#' @importFrom lhs randomLHS
#' @include GenericClass.R
#' @export LatinHypercubeSampler

LatinHypercubeSampler <- R6Class(
  "LatinHypercubeSampler",
  inherit = GenericClass,
  public = list(
    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    # See private/active for sample generation attributes

    ## Methods ##

    # Inherited methods (from GenericClass) #
    #   new_clone(...)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets parameter names when provided.
    #' @param parameter_names Optional vector of sample parameter names.
    #' @param ... Additional parameters passed individually.
    initialize = function(parameter_names = NULL, ...) {
      super$initialize(...)
      self$parameter_names <- parameter_names
      self$parameter_distributions <- list()
    },

    # New methods #

    #' @description
    #' Sets a parameter to sampled from a vector of classes.
    #' @param parameter_name Character string name of sample parameter.
    #' @param classes Vector of class values.
    set_class_parameter = function(parameter_name, classes) {
      if (!(parameter_name %in% self$parameter_names)) {
        # add parameter name
        self$parameter_names <- c(self$parameter_names, parameter_name)
      }
      self$parameter_distributions[[parameter_name]] <- list(
        type = "class",
        classes = classes
      )
    },

    #' @description
    #' Sets a parameter to be sampled from a \code{\link[stats:Uniform]{uniform}} distribution with lower and upper bounds, optionally rounded to a specified number of decimal places.
    #' @param parameter_name Character string name of sample parameter.
    #' @param lower Lower bound of the uniform distribution (default = 0).
    #' @param upper Upper bound of the uniform distribution (default = 1).
    #' @param decimals Optional number of decimals applied to generated samples.
    set_uniform_parameter = function(
      parameter_name,
      lower = 0,
      upper = 1,
      decimals = NULL
    ) {
      if (lower > upper) {
        stop(
          "Uniform distribution lower must not greater than upper",
          call. = FALSE
        )
      }
      if (!(parameter_name %in% self$parameter_names)) {
        # add parameter name
        self$parameter_names <- c(self$parameter_names, parameter_name)
      }
      self$parameter_distributions[[parameter_name]] <- list(
        type = "uniform",
        lower = lower,
        upper = upper,
        decimals = decimals
      )
    },

    #' @description
    #' Sets a parameter to be sampled from a \code{\link[stats:Normal]{normal}} distribution with mean and standard deviation, optionally rounded to a specified number of decimal places.
    #' @param parameter_name Character string name of sample parameter.
    #' @param mean Mean parameter for the normal distribution (default = 0).
    #' @param sd Standard deviation parameter for the normal distribution (default = 1).
    #' @param decimals Optional number of decimals applied to generated samples.
    set_normal_parameter = function(
      parameter_name,
      mean = 0,
      sd = 1,
      decimals = NULL
    ) {
      if (sd < 0) {
        stop(
          "Normal distribution standard deviation (sd) parameter must be non-negative",
          call. = FALSE
        )
      }
      if (!(parameter_name %in% self$parameter_names)) {
        # add parameter name
        self$parameter_names <- c(self$parameter_names, parameter_name)
      }
      self$parameter_distributions[[parameter_name]] <- list(
        type = "normal",
        mean = mean,
        sd = sd,
        decimals = decimals
      )
    },

    #' @description
    #' Sets a parameter to be sampled from a \code{\link[stats:Poisson]{Poisson}} distribution with lambda parameter. Produces integers.
    #' @param parameter_name Character string name of sample parameter.
    #' @param lambda Lambda parameter for the Poisson distribution. Must be positive (default = 1).
    set_poisson_parameter = function(parameter_name, lambda = 1) {
      if (lambda < 0) {
        stop(
          "Poisson distribution lambda parameter must be non-negative",
          call. = FALSE
        )
      }
      if (!(parameter_name %in% self$parameter_names)) {
        # add parameter name
        self$parameter_names <- c(self$parameter_names, parameter_name)
      }
      self$parameter_distributions[[parameter_name]] <- list(
        type = "poisson",
        lambda = lambda
      )
    },

    #' @description
    #' Sets a parameter to be sampled from a \code{\link[stats:Lognormal]{lognormal}} distribution with log mean and log standard deviation, optionally expressed as regular mean and SD (overriding log mean/sd), and optionally rounded to a specified number of decimal places.
    #' @param parameter_name Character string name of sample parameter.
    #' @param meanlog Log mean parameter for the lognormal distribution (default = 0).
    #' @param sdlog Log standard deviation parameter for the lognormal distribution (default = 1).
    #' @param mean Optional (overriding) regular mean parameter for the lognormal distribution (default = NULL).
    #' @param sd Optional (overriding) standard deviation parameter for the lognormal distribution (default = NULL).
    #' @param decimals Optional number of decimals applied to generated samples.
    set_lognormal_parameter = function(
      parameter_name,
      meanlog = 0,
      sdlog = 1,
      mean = NULL,
      sd = NULL,
      decimals = NULL
    ) {
      if (sdlog < 0) {
        stop(
          "Lognormal distribution standard deviation (sdlog) parameter must be non-negative",
          call. = FALSE
        )
      }
      if (!is.null(mean) && !is.null(sd)) {
        # override/transform
        if (mean > 0 && sd > 0) {
          meanlog <- log(mean^2 / sqrt(mean^2 + sd^2))
          sdlog <- sqrt(log(1 + sd^2 / mean^2))
        } else {
          stop(
            "Lognormal distribution regular (overriding) mean and standard deviation parameters must be greater than zero",
            call. = FALSE
          )
        }
      }
      if (!(parameter_name %in% self$parameter_names)) {
        # add parameter name
        self$parameter_names <- c(self$parameter_names, parameter_name)
      }
      self$parameter_distributions[[parameter_name]] <- list(
        type = "lognormal",
        meanlog = meanlog,
        sdlog = sdlog,
        decimals = decimals
      )
    },

    #' @description
    #' Sets a parameter to be sampled from a \code{\link[stats:Beta]{beta}} distribution configured with alpha and beta parameters, or optionally with mean and standard deviation (overriding alpha and beta), and optionally rounded to a specified number of decimal places.
    #' @param parameter_name Character string name of sample parameter.
    #' @param alpha Shaping (towards 1) parameter (> 0) for the beta distribution (default = 1).
    #' @param beta Shaping (towards 0) parameter (> 0) for the beta distribution (default = 1).
    #' @param mean Optional (overriding) mean parameter for the beta distribution (default = NULL).
    #' @param sd Optional (overriding) standard deviation parameter for the beta distribution (default = NULL).
    #' @param decimals Optional number of decimals applied to generated samples.
    set_beta_parameter = function(
      parameter_name,
      alpha = 1,
      beta = 1,
      mean = NULL,
      sd = NULL,
      decimals = NULL
    ) {
      if (alpha <= 0 || beta <= 0) {
        stop(
          "Beta distribution alpha and beta shaping parameters must be greater than zero",
          call. = FALSE
        )
      }
      if (!is.null(mean) && !is.null(sd)) {
        # override/transform
        if (mean > 0 && sd > 0 && mean < 1 && sd < 1) {
          sd <- pmin(sd, sqrt(mean * (1 - mean)) * 0.999) # Limit sd to < sqrt(mean*(1 - mean))
          alpha <- mean * (mean * (1 - mean) / sd^2 - 1)
          beta <- (1 - mean) * (mean * (1 - mean) / sd^2 - 1)
        } else {
          stop(
            "Beta distribution (overriding) mean and standard deviation parameters must be between zero and one (exclusively)",
            call. = FALSE
          )
        }
      }
      if (!(parameter_name %in% self$parameter_names)) {
        # add parameter name
        self$parameter_names <- c(self$parameter_names, parameter_name)
      }
      self$parameter_distributions[[parameter_name]] <- list(
        type = "beta",
        alpha = alpha,
        beta = beta,
        decimals = decimals
      )
    },

    #' @description
    #' Sets a parameter to be sampled from a \code{\link[truncnorm:qtruncnorm]{truncated normal}} distribution with mean, standard deviation, and lower and upper bounds, optionally rounded to a specified number of decimal places.
    #' @param parameter_name Character string name of sample parameter.
    #' @param mean Mean parameter of the truncated normal distribution (default = 0).
    #' @param sd Standard deviation of the truncated normal distribution (default = 1).
    #' @param lower Lower bound of the truncated normal distribution (default = -Inf, meaning no lower bound).
    #' @param upper Upper bound of the truncated normal distribution (default = Inf, meaning no upper bound).
    #' @param decimals Optional number of decimals that generated samples are rounded to.
    set_truncnorm_parameter = function(
      parameter_name,
      mean = 0,
      sd = 1,
      lower = -Inf,
      upper = Inf,
      decimals = NULL
    ) {
      if (lower > upper || mean < lower || mean > upper) {
        stop(
          "Truncated normal distribution parameters must comply with: lower <= mean <= upper",
          call. = FALSE
        )
      }
      if (!(parameter_name %in% self$parameter_names)) {
        # add parameter name
        self$parameter_names <- c(self$parameter_names, parameter_name)
      }
      self$parameter_distributions[[parameter_name]] <- list(
        type = "truncated normal",
        mean = mean,
        sd = sd,
        lower = lower,
        upper = upper,
        decimals = decimals
      )
    },

    #' @description
    #' Sets a parameter to be sampled from a \code{\link[metRology:qtri]{triangular}} distribution with lower and upper bounds and mode (peak), optionally rounded to a specified number of decimal places.
    #' @param parameter_name Character string name of sample parameter.
    #' @param lower Lower bound of the triangular distribution (default = 0).
    #' @param upper Upper bound of the triangular distribution (default = 1).
    #' @param mode Mode (or peak) of the triangular distribution (default = (lower + upper)/2).
    #' @param decimals Optional number of decimals applied to generated samples.
    set_triangular_parameter = function(
      parameter_name,
      lower = 0,
      upper = 1,
      mode = (lower + upper) / 2,
      decimals = NULL
    ) {
      if (lower > upper || mode < lower || mode > upper) {
        stop(
          "Triangular distribution parameters must comply with: lower <= mode <= upper",
          call. = FALSE
        )
      }
      if (!(parameter_name %in% self$parameter_names)) {
        # add parameter name
        self$parameter_names <- c(self$parameter_names, parameter_name)
      }
      self$parameter_distributions[[parameter_name]] <- list(
        type = "triangular",
        lower = lower,
        upper = upper,
        mode = mode,
        decimals = decimals
      )
    },

    #' @description
    #' Generates Latin hypercube sample data (via \code{\link[lhs:randomLHS]{randomLHS}}) for the set parameters using corresponding distributions.
    #' @param number Number of samples to generate (default = 10).
    #' @param random_seed Optional seed for the random generation of samples.
    #' @return A data frame of generated sample values.
    generate_samples = function(number = 10, random_seed = NULL) {
      # Set random seed when present
      if (!is.null(random_seed)) {
        set.seed(random_seed)
      }

      # Ensure distributions are set for all parameters
      if (
        !is.null(self$parameter_names) &&
          !is.null(self$parameter_distributions) &&
          all(self$parameter_names %in% names(self$parameter_distributions))
      ) {
        # Generate uniform 0-1 LHS
        sample_data <- as.data.frame(randomLHS(
          number,
          length(self$parameter_names)
        ))
        names(sample_data) <- self$parameter_names

        # Apply distributions for each parameter
        for (param in self$parameter_names) {
          distribution <- self$parameter_distributions[[param]]
          if (distribution$type == "class") {
            sample_data[[param]] <- distribution$classes[as.numeric(cut(
              sample_data[[param]],
              breaks = c(
                0,
                (1:length(distribution$classes) / length(distribution$classes))
              )
            ))]
          } else if (distribution$type == "uniform") {
            sample_data[[param]] <- distribution$lower +
              sample_data[[param]] * (distribution$upper - distribution$lower)
          } else if (distribution$type == "normal") {
            sample_data[[param]] <- stats::qnorm(
              sample_data[[param]],
              mean = distribution$mean,
              sd = distribution$sd
            )
          } else if (distribution$type == "lognormal") {
            sample_data[[param]] <- stats::qlnorm(
              sample_data[[param]],
              meanlog = distribution$meanlog,
              sdlog = distribution$sdlog
            )
          } else if (distribution$type == "beta") {
            sample_data[[param]] <- stats::qbeta(
              sample_data[[param]],
              shape1 = distribution$alpha,
              shape2 = distribution$beta
            )
          } else if (distribution$type == "triangular") {
            sample_data[[param]] <- qtri(
              sample_data[[param]],
              min = distribution$lower,
              max = distribution$upper,
              mode = distribution$mode
            )
          } else if (distribution$type == "poisson") {
            sample_data[[param]] <- qpois(
              sample_data[[param]],
              lambda = distribution$lambda
            )
          } else if (distribution$type == "truncated normal") {
            sample_data[[param]] <- qtruncnorm(
              sample_data[[param]],
              a = distribution$lower,
              b = distribution$upper,
              mean = distribution$mean,
              sd = distribution$sd
            )
          }
          if (!is.null(distribution$decimals)) {
            sample_data[[param]] <- round(
              sample_data[[param]],
              distribution$decimals
            )
          }
        }

        return(sample_data)
      } else {
        parameters_not_set <- self$parameter_names[which(
          !(self$parameter_names %in% names(self$parameter_distributions))
        )]
        if (length(parameters_not_set)) {
          stop(
            sprintf(
              "Parameter distributions need to be set before generating samples: %s",
              paste(parameters_not_set, collapse = ", ")
            ),
            call. = FALSE
          )
        } else {
          stop(
            "Parameter distributions need to be set before generating samples.",
            call. = FALSE
          )
        }
      }
    }
  ), # end public

  private = list(
    ## Attributes ##

    # Sample generation attributes #
    .parameter_names = c(),
    .parameter_distributions = NULL
  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(
    #' @field parameter_names A vector of sample parameter names.
    parameter_names = function(value) {
      if (missing(value)) {
        private$.parameter_names
      } else {
        private$.parameter_names <- value
      }
    },

    #' @field parameter_distributions A list of sample distribution values (nested list with appropriate parameters).
    parameter_distributions = function(value) {
      if (missing(value)) {
        private$.parameter_distributions
      } else {
        private$.parameter_distributions <- value
      }
    }
  ) # end active
)
