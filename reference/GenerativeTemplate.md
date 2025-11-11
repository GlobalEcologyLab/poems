# R6 class representing a nested container for generator attributes

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class representing a
nested container for
[`Generator`](https://globalecologylab.github.io/poems/reference/Generator.md)
attributes that are maintained when new model clones are created. The
container maintains *input* and *output* attribute names, file, function
and distribution templates, correlation parameters (for distribution
generation), rounding decimals, occupancy mask, and any inherited class
model attributes that need to be maintained when cloning.

## Public fields

- `attached`:

  A list of dynamically attached attributes (name-value pairs).

## Active bindings

- `description`:

  A brief description of what the generator generates.

- `inputs`:

  An array of input attribute names for the generator.

- `outputs`:

  An array of output attribute names for the generator.

- `file_templates`:

  A list of file template attributes.

- `function_templates`:

  A list of function template attributes.

- `distribution_templates`:

  A list of distribution template attributes.

- `uses_correlations`:

  A boolean to indicate that a
  [`SpatialCorrelation`](https://globalecologylab.github.io/poems/reference/SpatialCorrelation.md)
  (or inherited class) object is used for generating correlated random
  deviates.

- `spatial_correlation`:

  A
  [`SpatialCorrelation`](https://globalecologylab.github.io/poems/reference/SpatialCorrelation.md)
  (or inherited class) object for generating correlated random deviates.

- `temporal_correlation`:

  Absolute correlation coefficient between simulation time steps for all
  grid cells (0-1; default = 1).

- `time_steps`:

  Number of simulation time steps (default = 1).

- `generate_rasters`:

  Boolean to indicate if rasters should be generated (default: NULL).

- `decimals`:

  Number of decimal places applied to the generated values (default:
  NULL = no rounding).

- `occupancy_mask`:

  Optional binary mask array (matrix), data frame, or raster (stack) for
  generated (time-series) data.

## Methods

### Public methods

- [`GenerativeTemplate$new()`](#method-GenerativeTemplate-new)

- [`GenerativeTemplate$clone()`](#method-GenerativeTemplate-clone)

------------------------------------------------------------------------

### Method `new()`

Initialization method initializes the generator templates.

#### Usage

    GenerativeTemplate$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GenerativeTemplate$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
gen_template <- GenerativeTemplate$new()
gen_template$occupancy_mask <- array(c(1, 1, 0, 0, 1, 1, 1))
gen_template$decimals <- 4
gen_template$description <- "Test generator"

coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))

generator <- Generator$new(
  region = Region$new(coordinates = coordinates), attr1 = 1,
  template_attached = gen_template
)
generator$description
#> [1] "unnamed"
generator$occupancy_mask
#> NULL
generator$decimals
#> NULL
```
