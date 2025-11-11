# R6 class representing a nested container for dispersal generator attributes

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class representing a
nested container for
[`DispersalGenerator`](https://globalecologylab.github.io/poems/reference/DispersalGenerator.md)
attributes that are maintained when new model clones are created. The
container maintains *input* and *output* attribute names, file, function
and distribution templates, correlation parameters (for distribution
generation), rounding decimals, occupancy mask, and other
[`DispersalGenerator`](https://globalecologylab.github.io/poems/reference/DispersalGenerator.md)
attributes that need to be maintained when cloning.

## Super class

[`poems::GenerativeTemplate`](https://globalecologylab.github.io/poems/reference/GenerativeTemplate.md)
-\> `DispersalTemplate`

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

  A nested list of file template attributes.

- `function_templates`:

  A nested list of function template attributes.

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

  Number of simulation time steps.

- `decimals`:

  Number of decimal places applied to generated data outputs (default:
  NULL = no rounding).

- `occupancy_mask`:

  Optional binary mask array (matrix), data frame, or raster (stack) for
  generated (time-series) data outputs.

- `dispersal_friction`:

  A
  [`DispersalFriction`](https://globalecologylab.github.io/poems/reference/DispersalFriction.md)
  (or inherited class) object for dispersal distance multiplier data.

- `distance_classes`:

  Vector of distance interval boundaries (in km) for calculating
  discrete dispersal rates.

- `max_distance_classes`:

  The maximum number of distance classes when they are calculated
  automatically via the maximum distance (default: 1000).

- `distance_scale`:

  Scale of distance values in meters (default = 1). Usage: set to 1 for
  values in meters, or to 1000 for values in kilometers.

- `distance_data`:

  Data frame of distance classes including indices for the construction
  of compact matrices (columns: target_pop, source_pop, compact_row,
  distance_class).

- `dispersal_function_data`:

  Data frame of discrete dispersal function values. Optional first
  column may provide distance intervals (non-inclusive lower bounds).

- `dispersal_proportion`:

  Dispersal function: *p\*exp(-distance/b)* *p* parameter. Represents
  the proportion and limit of dispersers between model cells. This
  represents a maximum potential proportion of dispersers; other factors
  such as population density and carrying capacity may limit the actual
  proportion of dispersers.

- `dispersal_breadth`:

  Dispersal function: *p\*exp(-distance/b)* *b* parameter. Represents
  the breadth of the dispersal between model cells. Typically estimated
  via average migration distance.

- `dispersal_max_distance`:

  Dispersal maximum distance or range (*r*) parameter limits the use of
  the dispersal function: *p\*exp(-distance/b)*. The function is
  utilized when *distance \<= r* otherwise the dispersal rate is set to
  zero.

## Methods

### Public methods

- [`DispersalTemplate$clone()`](#method-DispersalTemplate-clone)

Inherited methods

- [`poems::GenerativeTemplate$initialize()`](https://globalecologylab.github.io/poems/reference/GenerativeTemplate.html#method-initialize)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DispersalTemplate$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
dispersal_template <- DispersalTemplate$new()
dispersal_template$dispersal_breadth <- 130
dispersal_template$dispersal_proportion <- 0.4
coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
dispersal_gen <- DispersalGenerator$new(
  coordinates = coordinates, inputs = c("dispersal_r"),
  generative_template = dispersal_template
)
dispersal_gen$dispersal_breadth
#> [1] 130
dispersal_gen$dispersal_proportion
#> [1] 0.4
```
