# R6 class representing a generic model.

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class with generic
(abstract) functionality for toolset models, including model attribute
get and set methods that resolve attribute scope (*public*, *active*,
*attached*), attribute aliases, attribute attachment, and error and
warning message attributes.

## Super class

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\> `GenericModel`

## Public fields

- `attached`:

  A list of dynamically attached attributes (name-value pairs).

## Active bindings

- `model_attributes`:

  A vector of model attribute names.

- `attribute_aliases`:

  A list of alternative alias names for model attributes (form:
  `alias = "attribute"`) to be used with the set and get attributes
  methods.

- `error_messages`:

  A vector of error messages encountered when setting model attributes.

- `warning_messages`:

  A vector of warning messages encountered when setting model
  attributes.

## Methods

### Public methods

- [`GenericModel$new()`](#method-GenericModel-new)

- [`GenericModel$new_clone()`](#method-GenericModel-new_clone)

- [`GenericModel$get_attribute_names()`](#method-GenericModel-get_attribute_names)

- [`GenericModel$get_attributes()`](#method-GenericModel-get_attributes)

- [`GenericModel$get_attribute()`](#method-GenericModel-get_attribute)

- [`GenericModel$get_attribute_aliases()`](#method-GenericModel-get_attribute_aliases)

- [`GenericModel$set_attributes()`](#method-GenericModel-set_attributes)

- [`GenericModel$clone()`](#method-GenericModel-clone)

------------------------------------------------------------------------

### Method `new()`

Initialization method sets given attributes individually and/or from a
list.

#### Usage

    GenericModel$new(
      model_attributes = NULL,
      attribute_aliases = NULL,
      params = list(),
      ...
    )

#### Arguments

- `model_attributes`:

  A vector of model attribute names.

- `attribute_aliases`:

  A list of alternative alias names for model attributes (form:
  `alias = "attribute"`) to be used with the set and get attributes
  methods.

- `params`:

  Parameters passed via a list.

- `...`:

  Parameters passed individually.

------------------------------------------------------------------------

### Method `new_clone()`

Creates a new (re-initialized) object of the current (inherited) object
class with optionally passed parameters.

#### Usage

    GenericModel$new_clone(...)

#### Arguments

- `...`:

  Parameters passed via the inherited class constructor (defined in
  initialize and run via new).

#### Returns

New object of the current (inherited) class.

------------------------------------------------------------------------

### Method `get_attribute_names()`

Returns an array of all attribute names including public and private
model attributes, as well as attached attributes, error and warning
messages.

#### Usage

    GenericModel$get_attribute_names()

#### Returns

Array of all attribute names.

------------------------------------------------------------------------

### Method `get_attributes()`

Returns a list of values for selected attributes or attribute aliases
(when array of parameter names provided) or all attributes (when no
params).

#### Usage

    GenericModel$get_attributes(params = NULL)

#### Arguments

- `params`:

  Array of attribute names to return (all when NULL).

#### Returns

List of selected or all attributes values.

------------------------------------------------------------------------

### Method `get_attribute()`

Returns the value of an attribute via character name or attribute alias.

#### Usage

    GenericModel$get_attribute(param)

#### Arguments

- `param`:

  Character string name of the attribute.

#### Returns

Attribute value.

------------------------------------------------------------------------

### Method `get_attribute_aliases()`

Returns an array of attribute names and aliases for specified or all
attributes.

#### Usage

    GenericModel$get_attribute_aliases(params = NULL)

#### Arguments

- `params`:

  Array of attribute names for names/aliases to return (all when NULL).

#### Returns

Array of selected or all attribute names and aliases.

------------------------------------------------------------------------

### Method `set_attributes()`

Sets given attributes (optionally via alias names) individually and/or
from a list.

#### Usage

    GenericModel$set_attributes(params = list(), ...)

#### Arguments

- `params`:

  List of parameters/attributes.

- `...`:

  Parameters/attributes passed individually.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GenericModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model1 <- GenericModel$new(
  model_attributes = c("a", "b", "c"),
  attribute_aliases = list(A = "a"),
  params = list(a = 1, b = 2), c = 3
)
# Get/set attributes
model1$get_attribute_names()
#> [1] "a"                "b"                "c"                "error_messages"  
#> [5] "warning_messages"
model1$set_attributes(d = 4)
model1$get_attributes()
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 3
#> 
#> $d
#> [1] 4
#> 
model1$get_attribute("A")
#> [1] 1
model1$get_attribute("B")
#> NULL
model1$get_attribute_aliases() # all attribute names
#> [1] "a"                "b"                "c"                "d"               
#> [5] "error_messages"   "warning_messages" "A"               
# New cloning
model2 <- model1$new_clone(e = 5)
model2$get_attributes()
#> $e
#> [1] 5
#> 
model2$modelattributes
#> NULL
model2$attribute_aliases
#> $A
#> [1] "a"
#> 
```
