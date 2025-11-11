# R6 class representing simulation results.

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class for
encapsulating and dynamically generating spatially-explicit simulation
results, as well as optional re-generated
[`Generator`](https://globalecologylab.github.io/poems/reference/Generator.md)
outputs.

## Super classes

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\>
[`poems::GenericModel`](https://globalecologylab.github.io/poems/reference/GenericModel.md)
-\>
[`poems::SpatialModel`](https://globalecologylab.github.io/poems/reference/SpatialModel.md)
-\> `SimulationResults`

## Public fields

- `attached`:

  A list of dynamically attached attributes (name-value pairs).

## Active bindings

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

- `time_steps`:

  Number of simulation time steps.

- `burn_in_steps`:

  Optional number of initial 'burn-in' time steps to be ignored.

- `occupancy_mask`:

  Optional binary mask array (matrix), data frame, or raster (stack) for
  each cell at each time-step of the simulation including burn-in.

- `all`:

  Nested simulation results for all cells.

- `parent`:

  Parent simulation results for individual cells.

- `default`:

  Default value/attribute utilized when applying primitive metric
  functions (e.g. max) to the results.

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

- [`SimulationResults$new()`](#method-SimulationResults-new)

- [`SimulationResults$new_clone()`](#method-SimulationResults-new_clone)

- [`SimulationResults$get_attribute_names()`](#method-SimulationResults-get_attribute_names)

- [`SimulationResults$get_attributes()`](#method-SimulationResults-get_attributes)

- [`SimulationResults$set_attributes()`](#method-SimulationResults-set_attributes)

- [`SimulationResults$clone()`](#method-SimulationResults-clone)

Inherited methods

- [`poems::GenericModel$get_attribute()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute)
- [`poems::GenericModel$get_attribute_aliases()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_aliases)

------------------------------------------------------------------------

### Method `new()`

Initialization method sets attributes from a results list or file, and
sets object attributes individually and/or from a list.

#### Usage

    SimulationResults$new(results = NULL, parent = NULL, ...)

#### Arguments

- `results`:

  A list containing results or a file path to simulation results.

- `parent`:

  Parent simulation results for individual cells (used when nesting a
  simulation results clone for all cells).

- `...`:

  Parameters passed via a *params* list or individually.

------------------------------------------------------------------------

### Method `new_clone()`

Creates a new (re-initialized) object of the current (inherited) object
class with optionally passed parameters.

#### Usage

    SimulationResults$new_clone(...)

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

    SimulationResults$get_attribute_names(all = FALSE)

#### Arguments

- `all`:

  Boolean to indicate if a nested list for all cells (when present)
  should be also listed (default is FALSE).

#### Returns

Array of all attribute names with optional inclusion of attribute names
of nested results for all cells.

------------------------------------------------------------------------

### Method `get_attributes()`

Returns a list of values for selected attributes or attribute aliases
(when array of parameter names provided) or all attributes (when no
params).

#### Usage

    SimulationResults$get_attributes(params = NULL, remove_burn_in = TRUE)

#### Arguments

- `params`:

  Array of attribute names to return (all when NULL).

- `remove_burn_in`:

  Boolean to indicate whether or not to remove burn-in steps from the
  attribute values (default = TRUE; mostly for internal use).

#### Returns

List of selected or all attributes values.

------------------------------------------------------------------------

### Method `set_attributes()`

Sets given attributes (optionally via alias names) individually and/or
from a list.

#### Usage

    SimulationResults$set_attributes(params = list(), ...)

#### Arguments

- `params`:

  List of parameters/attributes.

- `...`:

  Parameters/attributes passed individually.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SimulationResults$clone(deep = FALSE)

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
raster::plot(region$region_raster,
  main = "Example region (indices)",
  xlab = "Longitude (degrees)", ylab = "Latitude (degrees)",
  colNA = "blue"
)

# Sample results occupancy (ignore cell 2 in last 3 time steps)
occupancy_raster <- region$raster_from_values(array(1, c(7, 13)))
occupancy_raster[region$region_indices][2, 11:13] <- 0
occupancy_raster[region$region_indices]
#>      layer.1 layer.2 layer.3 layer.4 layer.5 layer.6 layer.7 layer.8 layer.9
#> [1,]       1       1       1       1       1       1       1       1       1
#> [2,]       1       1       1       1       1       1       1       1       1
#> [3,]       1       1       1       1       1       1       1       1       1
#> [4,]       1       1       1       1       1       1       1       1       1
#> [5,]       1       1       1       1       1       1       1       1       1
#> [6,]       1       1       1       1       1       1       1       1       1
#> [7,]       1       1       1       1       1       1       1       1       1
#>      layer.10 layer.11 layer.12 layer.13
#> [1,]        1        1        1        1
#> [2,]        1        0        0        0
#> [3,]        1        1        1        1
#> [4,]        1        1        1        1
#> [5,]        1        1        1        1
#> [6,]        1        1        1        1
#> [7,]        1        1        1        1
# Simulation example results
example_results <- list(abundance = region$raster_from_values(
  t(apply(
    matrix(11:17), 1,
    function(n) c(rep(n, 3), round(n * exp(-(0:9) / log(n))))
  ))
))
example_results$abundance[region$region_indices]
#>      layer.1 layer.2 layer.3 layer.4 layer.5 layer.6 layer.7 layer.8 layer.9
#> [1,]      11      11      11      11       7       5       3       2       1
#> [2,]      12      12      12      12       8       5       4       2       2
#> [3,]      13      13      13      13       9       6       4       3       2
#> [4,]      14      14      14      14      10       7       4       3       2
#> [5,]      15      15      15      15      10       7       5       3       2
#> [6,]      16      16      16      16      11       8       5       4       3
#> [7,]      17      17      17      17      12       8       6       4       3
#>      layer.10 layer.11 layer.12 layer.13
#> [1,]        1        1        0        0
#> [2,]        1        1        0        0
#> [3,]        1        1        1        0
#> [4,]        1        1        1        0
#> [5,]        2        1        1        1
#> [6,]        2        1        1        1
#> [7,]        2        1        1        1
# Simulation results object
sim_results <- SimulationResults$new(
  region = region,
  time_steps = 13,
  burn_in_steps = 3,
  occupancy_mask = occupancy_raster
)
# Clone (for each simulation results)
results_clone <- sim_results$new_clone(results = example_results)
results_clone$get_attribute("abundance")
#> class      : RasterBrick 
#> dimensions : 5, 5, 25, 10  (nrow, ncol, ncell, nlayers)
#> resolution : 0.01, 0.01  (x, y)
#> extent     : 177.005, 177.055, -18.055, -18.005  (xmin, xmax, ymin, ymax)
#> crs        : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
#> source     : memory
#> names      : layer.4, layer.5, layer.6, layer.7, layer.8, layer.9, layer.10, layer.11, layer.12, layer.13 
#> min values :      11,       7,       5,       3,       2,       1,        1,        0,        0,        0 
#> max values :      17,      12,       8,       6,       4,       3,        2,        1,        1,        1 
#> 
results_clone$get_attribute("abundance")[region$region_indices]
#>      layer.4 layer.5 layer.6 layer.7 layer.8 layer.9 layer.10 layer.11 layer.12
#> [1,]      11       7       5       3       2       1        1        1        0
#> [2,]      12       8       5       4       2       2        1        0        0
#> [3,]      13       9       6       4       3       2        1        1        1
#> [4,]      14      10       7       4       3       2        1        1        1
#> [5,]      15      10       7       5       3       2        2        1        1
#> [6,]      16      11       8       5       4       3        2        1        1
#> [7,]      17      12       8       6       4       3        2        1        1
#>      layer.13
#> [1,]        0
#> [2,]        0
#> [3,]        0
#> [4,]        0
#> [5,]        1
#> [6,]        1
#> [7,]        1
results_clone$all$get_attribute("abundance")
#>  [1] 98 67 46 31 21 15 10  6  5  3
results_clone$get_attribute("all$abundance")
#>  [1] 98 67 46 31 21 15 10  6  5  3
```
