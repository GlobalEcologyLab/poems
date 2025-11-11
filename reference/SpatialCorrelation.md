# R6 class representing a spatial correlation.

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class functionality
for modeling spatial correlations within a spatially-explicit model. It
provides functionality for calculating correlations between region cells
using a distance-based function: *a\*exp(-distance/b)*, where *a*
(amplitude) and *b* (breadth) are configurable model attributes. It then
calculates the Cholesky decomposition of the correlation matrix (via
[`chol`](https://rdrr.io/r/base/chol.html)), which is utilized to
generate (optionally temporal) correlated normal deviates. A compacted
version of the decomposed matrix can also generated for computational
efficiency.

## Super classes

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\>
[`poems::GenericModel`](https://globalecologylab.github.io/poems/reference/GenericModel.md)
-\>
[`poems::SpatialModel`](https://globalecologylab.github.io/poems/reference/SpatialModel.md)
-\> `SpatialCorrelation`

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

- `distance_scale`:

  Scale of distance values in meters (default = 1). Usage: set to 1 for
  values in meters, or to 1000 for values in kilometers.

- `correlation_amplitude`:

  Correlation function: *a\*exp(-distance/b)* *a* parameter. Represents
  the amplitude or maximum magnitude of correlation values between model
  cells.

- `correlation_breadth`:

  Correlation function: *a\*exp(-distance/b)* *b* parameter. Represents
  the breadth of the correlation between region cells. Typically
  estimated via average distance between correlated region cells.

- `correlation_matrix`:

  Correlation matrix calculated via correlation function:
  *a\*exp(-distance/b)*.

- `t_decomposition_matrix`:

  The transposed Cholesky decomposition of the correlation matrix (see
  [`chol`](https://rdrr.io/r/base/chol.html)).

- `compact_only`:

  Boolean to indicate that only the compact versions of matrices will be
  maintained once calculated.

- `t_decomposition_compact_matrix`:

  A compact (rows) version of the transposed Cholesky decomposition of
  the correlation matrix.

- `t_decomposition_compact_map`:

  A map of the original region cell rows for the compact transposed
  decomposition matrix.

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

- [`SpatialCorrelation$new()`](#method-SpatialCorrelation-new)

- [`SpatialCorrelation$calculate_distance_matrix()`](#method-SpatialCorrelation-calculate_distance_matrix)

- [`SpatialCorrelation$calculate_correlations()`](#method-SpatialCorrelation-calculate_correlations)

- [`SpatialCorrelation$calculate_cholesky_decomposition()`](#method-SpatialCorrelation-calculate_cholesky_decomposition)

- [`SpatialCorrelation$calculate_compact_decomposition()`](#method-SpatialCorrelation-calculate_compact_decomposition)

- [`SpatialCorrelation$get_compact_decomposition()`](#method-SpatialCorrelation-get_compact_decomposition)

- [`SpatialCorrelation$generate_correlated_normal_deviates()`](#method-SpatialCorrelation-generate_correlated_normal_deviates)

- [`SpatialCorrelation$clone()`](#method-SpatialCorrelation-clone)

Inherited methods

- [`poems::GenericModel$get_attribute()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute)
- [`poems::GenericModel$get_attribute_aliases()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_aliases)
- [`poems::GenericModel$get_attribute_names()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_names)
- [`poems::GenericModel$get_attributes()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attributes)
- [`poems::GenericModel$set_attributes()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-set_attributes)
- [`poems::SpatialModel$new_clone()`](https://globalecologylab.github.io/poems/reference/SpatialModel.html#method-new_clone)

------------------------------------------------------------------------

### Method `new()`

Initialization method sets given attributes individually and/or from a
list.

#### Usage

    SpatialCorrelation$new(compact_only = TRUE, attribute_aliases = NULL, ...)

#### Arguments

- `compact_only`:

  Boolean to indicate that only the compact versions of matrices will be
  maintained once calculated.

- `attribute_aliases`:

  Optional list of extra alias names for model attributes (form:
  `alias = "attribute"`) to be used with the set and get attributes
  methods.

- `...`:

  Parameters passed via a *params* list or individually.

------------------------------------------------------------------------

### Method `calculate_distance_matrix()`

Returns a matrix with the calculated distance (in meters by default)
between each pair of region cells.

#### Usage

    SpatialCorrelation$calculate_distance_matrix(use_longlat = NULL)

#### Arguments

- `use_longlat`:

  Optional boolean indicating use of (WGS84) coordinates in longitude
  (degrees West) and latitude (degrees North).

#### Returns

Matrix with distances between region cells.

------------------------------------------------------------------------

### Method `calculate_correlations()`

Calculates the correlation matrix by applying the distance-based
correlation function.

#### Usage

    SpatialCorrelation$calculate_correlations(
      distance_matrix = NULL,
      decimals = NULL,
      threshold = 1e-07,
      ...
    )

#### Arguments

- `distance_matrix`:

  Optional pre-calculated matrix with distances between region cells.

- `decimals`:

  Optional number of decimal places for correlation values.

- `threshold`:

  Optional threshold (minimum value) for correlation values (default
  0.0000001).

- `...`:

  Parameters passed via a *params* list or individually.

------------------------------------------------------------------------

### Method `calculate_cholesky_decomposition()`

Calculates the transposed Cholesky decomposition of the correlation
matrix (via [`chol`](https://rdrr.io/r/base/chol.html)).

#### Usage

    SpatialCorrelation$calculate_cholesky_decomposition(
      distance_matrix = NULL,
      decimals = NULL,
      threshold = 1e-07,
      ...
    )

#### Arguments

- `distance_matrix`:

  Optional pre-calculated matrix with distances between region cells.

- `decimals`:

  Optional number of decimal places for correlation values.

- `threshold`:

  Optional threshold (minimum value) for correlation values (default
  0.0000001).

- `...`:

  Parameters passed via a *params* list or individually.

------------------------------------------------------------------------

### Method `calculate_compact_decomposition()`

Compacts the transposed Cholesky decomposition of the correlation matrix
into the minimal number of rows, which are mapped to the original
matrix.

#### Usage

    SpatialCorrelation$calculate_compact_decomposition(distance_matrix = NULL, ...)

#### Arguments

- `distance_matrix`:

  Optional pre-calculated matrix with distances between region cells.

- `...`:

  Parameters passed via a *params* list or individually.

------------------------------------------------------------------------

### Method `get_compact_decomposition()`

Returns a compact transposed Cholesky decomposition of the correlation
matrix and a corresponding map of region cell indices in a list with
names: matrix, map.

#### Usage

    SpatialCorrelation$get_compact_decomposition(distance_matrix = NULL, ...)

#### Arguments

- `distance_matrix`:

  Optional pre-calculated matrix with distances between region cells.

- `...`:

  Parameters passed via a *params* list or individually.

#### Returns

List containing a compact Cholesky decomposition matrix and a
corresponding map of region cell indices (for the compacted rows).

------------------------------------------------------------------------

### Method `generate_correlated_normal_deviates()`

Generates correlated normal deviates using the spatial correlation,
utilizing the optional random seed and optional temporal correlation
across time steps.

#### Usage

    SpatialCorrelation$generate_correlated_normal_deviates(
      random_seed = NULL,
      temporal_correlation = 1,
      time_steps = 1
    )

#### Arguments

- `random_seed`:

  Optional seed for the random generation of correlated deviates.

- `temporal_correlation`:

  Optional temporal correlation coefficient (0-1; default = 1).

- `time_steps`:

  Optional number of time steps for temporal correlation (default = 1 or
  none).

#### Returns

Array (non-temporal) or matrix (temporal) of correlated normal deviates.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SpatialCorrelation$clone(deep = FALSE)

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
# Spatial correlation
env_corr <- SpatialCorrelation$new(region = region, amplitude = 0.4, breadth = 500)
env_corr$calculate_distance_matrix() # m
#>          [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]
#> [1,]    0.000 2115.410 1112.263 2389.944 2224.526 2463.154 3069.685
#> [2,] 2115.410    0.000 2389.944 1112.263 3069.685 2463.154 2224.526
#> [3,] 1112.263 2389.944    0.000 2115.290 1112.263 1534.822 2389.838
#> [4,] 2389.944 1112.263 2115.290    0.000 2389.838 1534.822 1112.263
#> [5,] 2224.526 3069.685 1112.263 2389.838    0.000 1057.585 2115.170
#> [6,] 2463.154 2463.154 1534.822 1534.822 1057.585    0.000 1057.585
#> [7,] 3069.685 2224.526 2389.838 1112.263 2115.170 1057.585    0.000
env_corr$calculate_correlations(decimals = 5)
env_corr$correlation_matrix
#>         [,1]    [,2]    [,3]    [,4]    [,5]    [,6]    [,7]
#> [1,] 1.00000 0.00582 0.04325 0.00336 0.00468 0.00290 0.00086
#> [2,] 0.00582 1.00000 0.00336 0.04325 0.00086 0.00290 0.00468
#> [3,] 0.04325 0.00336 1.00000 0.00582 0.04325 0.01858 0.00336
#> [4,] 0.00336 0.04325 0.00582 1.00000 0.00336 0.01858 0.04325
#> [5,] 0.00468 0.00086 0.04325 0.00336 1.00000 0.04825 0.00582
#> [6,] 0.00290 0.00290 0.01858 0.01858 0.04825 1.00000 0.04825
#> [7,] 0.00086 0.00468 0.00336 0.04325 0.00582 0.04825 1.00000
env_corr$calculate_cholesky_decomposition(decimals = 2)
env_corr$t_decomposition_matrix
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#> [1,]    1 0.01 0.04 0.00 0.00 0.00 0.00
#> [2,]    0 1.00 0.00 0.04 0.00 0.00 0.00
#> [3,]    0 0.00 1.00 0.01 0.04 0.02 0.00
#> [4,]    0 0.00 0.00 1.00 0.00 0.02 0.04
#> [5,]    0 0.00 0.00 0.00 1.00 0.05 0.01
#> [6,]    0 0.00 0.00 0.00 0.00 1.00 0.05
#> [7,]    0 0.00 0.00 0.00 0.00 0.00 1.00
env_corr$get_compact_decomposition()
#> $matrix
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#> [1,]    1 0.01 0.04 0.04 0.04 0.02 0.04
#> [2,]    0 1.00 1.00 0.01 1.00 0.02 0.01
#> [3,]    0 0.00 0.00 1.00 0.00 0.05 0.05
#> [4,]    0 0.00 0.00 0.00 0.00 1.00 1.00
#> 
#> $map
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#> [1,]    1    1    1    2    3    3    4
#> [2,]   NA    2    3    3    5    4    5
#> [3,]   NA   NA   NA    4   NA    5    6
#> [4,]   NA   NA   NA   NA   NA    6    7
#> 
# Scale to km
env_corr$distance_scale <- 1000
env_corr$calculate_distance_matrix() # km
#>          [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]
#> [1,] 0.000000 2.115410 1.112263 2.389944 2.224526 2.463154 3.069685
#> [2,] 2.115410 0.000000 2.389944 1.112263 3.069685 2.463154 2.224526
#> [3,] 1.112263 2.389944 0.000000 2.115290 1.112263 1.534822 2.389838
#> [4,] 2.389944 1.112263 2.115290 0.000000 2.389838 1.534822 1.112263
#> [5,] 2.224526 3.069685 1.112263 2.389838 0.000000 1.057585 2.115170
#> [6,] 2.463154 2.463154 1.534822 1.534822 1.057585 0.000000 1.057585
#> [7,] 3.069685 2.224526 2.389838 1.112263 2.115170 1.057585 0.000000
```
