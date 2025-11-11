# R6 class representing a dispersal friction.

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class functionality
for modeling sea, ice and other frictional barriers to dispersal within
a spatially-explicit population model. The dispersal friction model
utilizes the
[`gdistance`](https://AgrDataSci.github.io/gdistance/reference/gdistance.html)
package functionality to calculate distance multipliers to modify
distance-based dispersal rates for simulated migrations in a
spatio-temporal frictional landscape. The frictional landscape is
defined via conductance/permeability values, the inverse of friction,
which ranges from zero (barrier) to one (no friction) with values
in-between representing some friction. For example, a conductance value
of 1/5 = 0.2 represents a landscape in which simulated animals move 5
times slower than a non-friction landscape. In this example the
resultant distance multiplier would be 5, thus reducing the effective
dispersal range.

## Super classes

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\>
[`poems::GenericModel`](https://globalecologylab.github.io/poems/reference/GenericModel.md)
-\>
[`poems::SpatialModel`](https://globalecologylab.github.io/poems/reference/SpatialModel.md)
-\> `DispersalFriction`

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

- `parallel_cores`:

  Number of cores for running the simulations in parallel.

- `write_to_dir`:

  Directory path for storing distance multipliers when memory
  performance is an issue.

- `transition_directions`:

  Number of transition directions or neighbors in which cells are
  connected: usually 4, 8 (default), or 16 (see
  [`gdistance::transition`](https://AgrDataSci.github.io/gdistance/reference/transition.html)).

- `conductance`:

  Matrix/raster of conductance (inverse friction) values (range: 0 =
  barrier; 0 \< some friction \< 1; 1 = no friction) for each grid cell
  (rows/cells) at each simulation time step (columns/layers).

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

- [`DispersalFriction$calculate_distance_multipliers()`](#method-DispersalFriction-calculate_distance_multipliers)

- [`DispersalFriction$clone()`](#method-DispersalFriction-clone)

Inherited methods

- [`poems::GenericModel$get_attribute()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute)
- [`poems::GenericModel$get_attribute_aliases()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_aliases)
- [`poems::GenericModel$get_attribute_names()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_names)
- [`poems::GenericModel$get_attributes()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attributes)
- [`poems::GenericModel$set_attributes()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-set_attributes)
- [`poems::SpatialModel$initialize()`](https://globalecologylab.github.io/poems/reference/SpatialModel.html#method-initialize)
- [`poems::SpatialModel$new_clone()`](https://globalecologylab.github.io/poems/reference/SpatialModel.html#method-new_clone)

------------------------------------------------------------------------

### Method `calculate_distance_multipliers()`

Calculates and returns spatio-temporal dispersal distance multipliers
for each in-range migration.

#### Usage

    DispersalFriction$calculate_distance_multipliers(dispersal_indices, ...)

#### Arguments

- `dispersal_indices`:

  Two-column integer matrix, data.frame, or array representing the
  target and source coordinate index for each in-range migration.

- `...`:

  Parameters passed via a *params* list or individually.

#### Returns

Temporal list of dispersal distance multiplier arrays with values for
each in-range migration.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DispersalFriction$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # interactive()
#' U Island example region
coordinates <- data.frame(
  x = rep(seq(177.01, 177.05, 0.01), 5),
  y = rep(seq(-18.01, -18.05, -0.01), each = 5)
)
template_raster <- Region$new(coordinates = coordinates)$region_raster #' full extent
template_raster[][-c(7, 9, 12, 14, 17:19)] <- NA #' make U Island
region <- Region$new(template_raster = template_raster)
raster::plot(region$region_raster,
  main = "Example region (indices)",
  xlab = "Longitude (degrees)", ylab = "Latitude (degrees)",
  colNA = "blue"
)

#' Dispersal distances
dispersal_gen <- DispersalGenerator$new(region = region)
dispersal_gen$set_attributes(params = list(p = 0.5, b = 700, r = 3000))
distances <- round(dispersal_gen$calculate_distance_matrix()) #' in m
dispersal_gen$calculate_distance_data()
dispersal_indices <- as.matrix(dispersal_gen$distance_data$base[, 1:2])

#' Distance multipliers with friction in cell 4
dispersal_friction <- DispersalFriction$new(
  region = region,
  conductance = c(1, 1, 1, 0.5, 1, 1, 1)
)
multipliers <- dispersal_friction$calculate_distance_multipliers(dispersal_indices)
cbind(dispersal_indices,
  distance = distances[dispersal_indices],
  multiplier = multipliers[[1]]
)

#' Note that crossing the water is avoided.
}
```
