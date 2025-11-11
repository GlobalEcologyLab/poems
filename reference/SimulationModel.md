# R6 class representing a simulation model

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class representing a
spatially-explicit simulation model. It extends the
[`SpatialModel`](https://globalecologylab.github.io/poems/reference/SpatialModel.md)
class with a range of common simulation parameters and functionality for
creating a nested model, whereby a nested template model with fixed
parameters is maintained when a model is cloned for various sampled
parameters. Also provided are methods for checking the consistency and
completeness of model parameters.

## Super classes

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\>
[`poems::GenericModel`](https://globalecologylab.github.io/poems/reference/GenericModel.md)
-\>
[`poems::SpatialModel`](https://globalecologylab.github.io/poems/reference/SpatialModel.md)
-\> `SimulationModel`

## Public fields

- `attached`:

  A list of dynamically attached attributes (name-value pairs).

## Active bindings

- `simulation_function`:

  Name (character string) or source path of the default simulation
  function, which takes a model as an input and returns the simulation
  results.

- `model_attributes`:

  A vector of model attribute names.

- `region`:

  A
  [`Region`](https://globalecologylab.github.io/poems/reference/Region.md)
  (or inherited class) object specifying the study region.

- `coordinates`:

  Data frame (or matrix) of X-Y population (WGS84) coordinates in
  longitude (degrees West) and latitude (degrees North) (get and set),
  or distance-based coordinates dynamically returned by region raster
  (get only).

- `random_seed`:

  Number to seed the random number generation for stochasticity.

- `replicates`:

  Number of replicate simulation runs.

- `time_steps`:

  Number of simulation time steps.

- `years_per_step`:

  Number of years per time step.

- `results_selection`:

  List of simulator-dependent attributes to be included in the returned
  results of each simulation run.

- `attribute_aliases`:

  A list of alternative alias names for model attributes (form:
  `alias = "attribute"`) to be used with the set and get attributes
  methods.

- `template_model`:

  Nested template model for fixed (non-sampled) attributes for shallow
  cloning.

- `sample_attributes`:

  Vector of sample attribute names (only).

- `required_attributes`:

  Vector of required attribute names (only), i.e. those needed to run a
  simulation.

- `error_messages`:

  A vector of error messages encountered when setting model attributes.

- `warning_messages`:

  A vector of warning messages encountered when setting model
  attributes.

## Methods

### Public methods

- [`SimulationModel$new()`](#method-SimulationModel-new)

- [`SimulationModel$new_clone()`](#method-SimulationModel-new_clone)

- [`SimulationModel$get_attribute_names()`](#method-SimulationModel-get_attribute_names)

- [`SimulationModel$get_attributes()`](#method-SimulationModel-get_attributes)

- [`SimulationModel$set_attributes()`](#method-SimulationModel-set_attributes)

- [`SimulationModel$set_sample_attributes()`](#method-SimulationModel-set_sample_attributes)

- [`SimulationModel$is_consistent()`](#method-SimulationModel-is_consistent)

- [`SimulationModel$list_consistency()`](#method-SimulationModel-list_consistency)

- [`SimulationModel$inconsistent_attributes()`](#method-SimulationModel-inconsistent_attributes)

- [`SimulationModel$is_complete()`](#method-SimulationModel-is_complete)

- [`SimulationModel$list_completeness()`](#method-SimulationModel-list_completeness)

- [`SimulationModel$incomplete_attributes()`](#method-SimulationModel-incomplete_attributes)

- [`SimulationModel$clone()`](#method-SimulationModel-clone)

Inherited methods

- [`poems::GenericModel$get_attribute()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute)
- [`poems::GenericModel$get_attribute_aliases()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_aliases)

------------------------------------------------------------------------

### Method `new()`

Initialization method sets template model and sets given attributes
individually and/or from a list.

#### Usage

    SimulationModel$new(template = NULL, required_attributes = NULL, ...)

#### Arguments

- `template`:

  Template simulation model (nested) containing fixed (non-sampled)
  attributes.

- `required_attributes`:

  Vector of required attribute names (only), i.e. those needed to run a
  simulation.

- `...`:

  Parameters passed via a *params* list or individually.

------------------------------------------------------------------------

### Method `new_clone()`

Creates a new (re-initialized) object of the current (inherited) object
class with optionally passed parameters.

#### Usage

    SimulationModel$new_clone(...)

#### Arguments

- `...`:

  Parameters passed via the inherited class constructor (defined in
  initialize and run via new).

#### Returns

New object of the current (inherited) class.

------------------------------------------------------------------------

### Method `get_attribute_names()`

Returns a list of all attribute names including public and private model
attributes, as well as attached attributes (including those from the
template model).

#### Usage

    SimulationModel$get_attribute_names()

#### Returns

List of all attribute names.

------------------------------------------------------------------------

### Method `get_attributes()`

Returns a list of values for selected attributes or attribute aliases
(when array of parameter names provided) or all attributes (when no
params).

#### Usage

    SimulationModel$get_attributes(params = NULL)

#### Arguments

- `params`:

  Array of attribute names to return (all when NULL).

#### Returns

List of selected or all attributes values.

------------------------------------------------------------------------

### Method `set_attributes()`

Sets given attributes (optionally via alias names) individually and/or
from a list.

#### Usage

    SimulationModel$set_attributes(params = list(), ...)

#### Arguments

- `params`:

  List of parameters/attributes.

- `...`:

  Parameters/attributes passed individually.

------------------------------------------------------------------------

### Method `set_sample_attributes()`

Sets the names (only - when *params* is a vector) and values (when
*params* is a list and/or when name-value pairs are provided) of the
sample attributes for the model.

#### Usage

    SimulationModel$set_sample_attributes(params = list(), ...)

#### Arguments

- `params`:

  List of parameters/attributes (names and values) or array of names
  only.

- `...`:

  Parameters/attributes passed individually.

------------------------------------------------------------------------

### Method `is_consistent()`

Returns a boolean to indicate if (optionally selected or all) model
attributes (such as dimensions) are consistent/valid.

#### Usage

    SimulationModel$is_consistent(params = NULL)

#### Arguments

- `params`:

  Optional array of parameter/attribute names.

#### Returns

Boolean to indicate consistency of selected/all attributes.

------------------------------------------------------------------------

### Method `list_consistency()`

Returns a boolean to indicate if (optionally selected or all) model
attributes (such as dimensions) are consistent/valid.

#### Usage

    SimulationModel$list_consistency(params = NULL)

#### Arguments

- `params`:

  Optional array of parameter/attribute names.

#### Returns

List of booleans (or NAs) to indicate consistency of selected/all
attributes.

------------------------------------------------------------------------

### Method `inconsistent_attributes()`

Returns a list of attributes necessary to simulate the model that are
inconsistent/invalid.

#### Usage

    SimulationModel$inconsistent_attributes(include_nas = FALSE)

#### Arguments

- `include_nas`:

  Optional boolean indicating whether of not to include attributes with
  unknown consistency (NA).

#### Returns

List of inconsistent attributes which prevent the model simulation (and
optionally those where consistency is not available).

------------------------------------------------------------------------

### Method `is_complete()`

Returns a boolean to indicate if all attributes necessary to simulate
the model have been set and are consistent/valid.

#### Usage

    SimulationModel$is_complete()

#### Returns

Boolean to indicate model completeness (and consistency).

------------------------------------------------------------------------

### Method `list_completeness()`

Returns a list of booleans (or NAs) for each parameter to indicate
attributes that are necessary to simulate the model have been set and
are consistent/valid.

#### Usage

    SimulationModel$list_completeness()

#### Returns

List of booleans (or NAs) for each parameter to indicate to indicate
completeness (and consistency).

------------------------------------------------------------------------

### Method `incomplete_attributes()`

Returns a list of attributes necessary to simulate the model that are
incomplete/inconsistent/invalid.

#### Usage

    SimulationModel$incomplete_attributes(include_nas = FALSE)

#### Arguments

- `include_nas`:

  Optional boolean indicating whether of not to include attributes with
  unknown completeness (NA).

#### Returns

List of incomplete attributes which prevent the model simulation (and
optionally those where completeness is not available).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SimulationModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# U Island example region
coordinates <- data.frame(
  x = rep(seq(177.01, 177.05, 0.01), 5),
  y = rep(seq(-18.01, -18.05, -0.01), each = 5)
)
template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
template_raster[][-c(7, 9, 12, 14, 17:19)] <- NA # make U Island
region <- Region$new(template_raster = template_raster)
# Model template
template_model <- SimulationModel$new(
  simulation_function = "test_simulator",
  region = region, time_steps = 10
)
template_model$model_attributes <- c(
  template_model$model_attributes,
  "a", "b", "c", "d"
)
template_model$model_attributes
#>  [1] "region"            "coordinates"       "random_seed"      
#>  [4] "replicates"        "time_steps"        "years_per_step"   
#>  [7] "results_selection" "a"                 "b"                
#> [10] "c"                 "d"                
template_model$required_attributes <- c(
  template_model$required_attributes[1:2],
  "a", "b", "c", "d"
)
template_model$required_attributes
#> [1] "region"     "time_steps" "a"          "b"          "c"         
#> [6] "d"         
template_model$get_attributes(template_model$required_attributes)
#> $region
#> <Region>
#>   Inherits from: <GenericClass>
#>   Public:
#>     attached: list
#>     clone: function (deep = FALSE) 
#>     coordinates: active binding
#>     initialize: function (coordinates = NULL, template_raster = NULL, region_raster = NULL, 
#>     new_clone: function (...) 
#>     object_generator: R6ClassGenerator
#>     raster_from_values: function (values) 
#>     raster_is_consistent: function (check_raster) 
#>     region_cells: active binding
#>     region_indices: active binding
#>     region_raster: active binding
#>     strict_consistency: active binding
#>     use_raster: active binding
#>   Private:
#>     .coordinates: NULL
#>     .region_raster: RasterLayer
#>     .strict_consistency: TRUE
#>     .use_raster: TRUE
#> 
#> $time_steps
#> [1] 10
#> 
template_model$simulation_function
#> [1] "test_simulator"
# Nested model
nested_model <- SimulationModel$new(template_model = template_model)
nested_model$region$region_cells
#> [1] 7
nested_model$set_sample_attributes(a = 1:7, b = 1:10, c = 1:15)
nested_model$sample_attributes
#> [1] "a" "b" "c"
nested_model$get_attributes(c("a", "b", "c", "d"))
#> $a
#> [1] 1 2 3 4 5 6 7
#> 
#> $b
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $c
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
#> 
# Completeness and consistency
nested_model$is_complete()
#> [1] FALSE
nested_model$incomplete_attributes()
#> [1] "c" "d"
nested_model$is_consistent()
#> [1] FALSE
nested_model$inconsistent_attributes()
#> [1] "c"
nested_model$set_attributes(c = array(1:70, c(7, 10)), d = 15)
nested_model$is_complete()
#> [1] TRUE
nested_model$is_consistent()
#> [1] TRUE
# Attached attributes
nested_model$attached
#> $a
#> [1] 1 2 3 4 5 6 7
#> 
#> $b
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $c
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    1    8   15   22   29   36   43   50   57    64
#> [2,]    2    9   16   23   30   37   44   51   58    65
#> [3,]    3   10   17   24   31   38   45   52   59    66
#> [4,]    4   11   18   25   32   39   46   53   60    67
#> [5,]    5   12   19   26   33   40   47   54   61    68
#> [6,]    6   13   20   27   34   41   48   55   62    69
#> [7,]    7   14   21   28   35   42   49   56   63    70
#> 
template_model$attached
#> $d
#> [1] 15
#> 
```
