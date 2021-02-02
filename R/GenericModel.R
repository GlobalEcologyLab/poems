#' R6 class representing a generic model.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class with generic (abstract) functionality for toolset
#' models, including model attribute get and set methods that resolve attribute scope
#' (\emph{public}, \emph{active}, \emph{attached}), attribute aliases, attribute
#' attachment, and error and warning message attributes.
#'
#' @examples
#' model1 <- GenericModel$new(model_attributes = c("a", "b", "c"),
#'                            attribute_aliases = list(A = "a"),
#'                            params = list(a = 1, b = 2), c = 3)
#' # Get/set attributes
#' model1$get_attribute_names()
#' model1$set_attributes(d = 4)
#' model1$get_attributes()
#' model1$get_attribute("A")
#' model1$get_attribute("B")
#' model1$get_attribute_aliases() # all attribute names
#' # New cloning
#' model2 <- model1$new_clone(e = 5)
#' model2$get_attributes()
#' model2$modelattributes
#' model2$attribute_aliases
#'
#' @importFrom R6 R6Class
#' @include GenericClass.R
#' @export GenericModel

GenericModel <- R6Class("GenericModel",
  inherit = GenericClass,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets given attributes individually and/or from a list.
    #' @param model_attributes A vector of model attribute names.
    #' @param attribute_aliases A list of alternative alias names for model attributes (form: \code{alias = "attribute"}) to be used with the set and get attributes methods.
    #' @param params Parameters passed via a list.
    #' @param ... Parameters passed individually.
    initialize = function(model_attributes = NULL, attribute_aliases = NULL, params = list(), ...) {
      super$initialize(...)
      if (!is.null(model_attributes)) {
        self$model_attributes <- model_attributes
      }
      if (!is.null(attribute_aliases)) {
        self$attribute_aliases <- c(self$attribute_aliases, attribute_aliases)
      }
      self$set_attributes(params = params, ...)
    },

    #' @description
    #' Creates a new (re-initialized) object of the current (inherited) object class with optionally passed parameters.
    #' @param ... Parameters passed via the inherited class constructor (defined in initialize and run via new).
    #' @return New object of the current (inherited) class.
    new_clone = function(...) {
      return(super$new_clone(model_attributes = self$model_attributes,
                             attribute_aliases = self$attribute_aliases, ...))
    },

    # New methods #

    #' @description
    #' Returns an array of all attribute names including public and private model attributes, as well as attached attributes, error and warning messages.
    #' @return Array of all attribute names.
    get_attribute_names = function() {
      return(unique(c(self$model_attributes, names(self$attached), "error_messages", "warning_messages")))
    },

    #' @description
    #' Returns a list of values for selected attributes or attribute aliases (when array of parameter names provided) or all attributes (when no params).
    #' @param params Array of attribute names to return (all when NULL).
    #' @return List of selected or all attributes values.
    get_attributes = function(params = NULL) {
      attribute_list <- list()
      if (is.null(params)) {
        params <- self$get_attribute_names()
      }
      for (param in params) {
        param <- as.character(param)
        if (param %in% names(self$attribute_aliases)) {
          attribute <- self$attribute_aliases[[param]]
        } else {
          attribute <- param
        }
        attribute_root <- unlist(strsplit(attribute, "$", fixed = TRUE))[1]
        attribute_root <- unlist(strsplit(attribute_root, "[", fixed = TRUE))[1]
        if (attribute_root %in% names(self) || attribute_root %in% private$.active_attributes) {
          eval(parse(text=sprintf("attribute_list$%s <- self$%s", param, attribute)))
        } else if (attribute_root %in% names(private)) {
          eval(parse(text=sprintf("attribute_list$%s <- private$%s", param, attribute)))
        } else if (paste0(".", attribute_root) %in% names(private)) {
          eval(parse(text=sprintf("attribute_list$%s <- private$.%s", param, attribute)))
        } else if (attribute_root %in% c("model_attributes", "attribute_aliases", "error_messages", "warning_messages")) {
          eval(parse(text=sprintf("attribute_list$%s <- self$%s", param, attribute)))
        } else if (attribute_root %in% names(self$attached)) {
          eval(parse(text=sprintf("attribute_list$%s <- self$attached$%s", param, attribute)))
        } else {
          attribute_list[[param]] <- NULL
        }
      }
      return(attribute_list)
    },

    #' @description
    #' Returns the value of an attribute via character name or attribute alias.
    #' @param param Character string name of the attribute.
    #' @return Attribute value.
    get_attribute = function(param) {
      param <- as.character(param)
      return(self$get_attributes(param)[[param]])
    },

    #' @description
    #' Returns an array of attribute names and aliases for specified or all attributes.
    #' @param params Array of attribute names for names/aliases to return (all when NULL).
    #' @return Array of selected or all attribute names and aliases.
    get_attribute_aliases = function(params = NULL) {
      if (is.null(params)) {
        params <- self$get_attribute_names()
      } else {
        params <- params[which(params %in% self$get_attribute_names())]
      }
      return(c(params, unique(names(self$attribute_aliases[unlist(lapply(params, grep, unlist(self$attribute_aliases), fixed = TRUE))]))))
    },

    #' @description
    #' Sets given attributes (optionally via alias names) individually and/or from a list.
    #' @param params List of parameters/attributes.
    #' @param ... Parameters/attributes passed individually.
    set_attributes = function(params = list(), ...) {
      params <- c(list(...), params) # prioritise individual parameters
      for (param in names(params)) {
        if (param %in% names(self$attribute_aliases)) {
          attribute <- self$attribute_aliases[[param]]
        } else {
          attribute <- param
        }
        attribute_root <- unlist(strsplit(attribute, "$", fixed = TRUE))[1]
        attribute_root <- unlist(strsplit(attribute_root, "[", fixed = TRUE))[1]
        if (attribute_root %in% names(self) || attribute_root %in% private$.active_attributes) {
          eval(parse(text = sprintf("self$%s <- params$%s", attribute, param)))
        } else if (attribute_root %in% names(private)) {
          eval(parse(text = sprintf("private$%s <- params$%s", attribute, param)))
        } else if (paste0(".", attribute_root) %in% names(private)) {
          eval(parse(text = sprintf("private$.%s <- params$%s", attribute, param)))
        } else if (attribute_root %in% c("model_attributes", "attribute_aliases", "error_messages", "warning_messages")) {
          eval(parse(text = sprintf("self$%s <- params$%s", attribute, param)))
        } else if (attribute %in% c("object_generator")) {
          # ignore - handled in generic class
        } else { # attach
          eval(parse(text = sprintf("self$attached$%s <- params$%s", attribute, param)))
        }
      }
    }

  ), # end public

  private = list(

    ## Attributes ##

    # Model attributes #
    .model_attributes = c(), # none in base class

    # Attributes accessible via model get/set methods #
    .active_attributes = c(),

    # Dynamic attributes #
    .attribute_aliases = list(),

    # Errors and warnings #
    .error_messages = NULL,
    .warning_messages = NULL

  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    # Model attribute accessors #

    #' @field model_attributes A vector of model attribute names.
    model_attributes = function(value) {
      if (missing(value)) {
        private$.model_attributes
      } else {
        private$.model_attributes <- value
      }
    },

    # Dynamic attribute accessors #

    #' @field attribute_aliases A list of alternative alias names for model attributes (form: \code{alias = "attribute"}) to be used with the set and get attributes methods.
    attribute_aliases = function(value) {
      if (missing(value)) {
        private$.attribute_aliases
      } else {
        if (!is.null(value) && !is.list(value)) {
          stop("Attribute aliases should be a list (form: alias = \"attribute\")", call. = FALSE)
        }
        private$.attribute_aliases <- value
      }
    },

    # Errors and warnings accessors #

    #' @field error_messages A vector of error messages encountered when setting model attributes.
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

    #' @field warning_messages A vector of warning messages encountered when setting model attributes.
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
