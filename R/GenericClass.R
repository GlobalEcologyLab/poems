#' R6 class with generic reusable functionality
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class with generic (abstract) new cloning functionality.
#'
#' @examples
#' object1 <- GenericClass$new()
#' class(object1)
#' # Referencing
#' object_ref <- object1
#' object_ref$attached$a <- 1
#' object1$attached
#' # Cloning
#' object2 <- object1$clone()
#' object2$attached$b <- 2
#' object1$attached
#' object2$attached
#' # New cloning
#' object3 <- object1$new_clone()
#' object3$attached$c <- 3
#' object1$attached
#' object3$attached
#'
#' @importFrom R6 R6Class
#' @export GenericClass

GenericClass <- R6Class(
  "GenericClass",
  public = list(
    ## Attributes ##

    #' @field object_generator Class object generator used to create new clones, particularly for user inheritance.
    object_generator = NULL,

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method saves an object generator for new cloning.
    #' @param object_generator Class object generator used to create new clones, particularly for user inheritance.
    #' @param ... Parameters passed individually (ignored).
    initialize = function(object_generator = NULL, ...) {
      if (!is.null(object_generator)) {
        self$object_generator <- object_generator
      } else {
        self$object_generator <- eval(parse(text = class(self)[1]))
      }
    },

    # New methods #

    #' @description
    #' Creates a new (re-initialized) object of the current (inherited) object class with optionally passed parameters.
    #' @param ... Parameters passed via the inherited class constructor (defined in initialize and run via new).
    #' @return New object of the current (inherited) class.
    new_clone = function(...) {
      return(self$object_generator$new(
        object_generator = self$object_generator,
        ...
      ))
    }
  ), # end public

  private = list(
    ## Attributes ##
  ) # end private
)
