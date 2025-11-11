# R6 class representing a spatial model

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class representing a
generic (abstract) spatially-explicit model. It extends
[`GenericModel`](https://globalecologylab.github.io/poems/reference/GenericModel.md)
with the addition of a study region specification.

## Super classes

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\>
[`poems::GenericModel`](https://globalecologylab.github.io/poems/reference/GenericModel.md)
-\> `SpatialModel`

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

- [`SpatialModel$new()`](#method-SpatialModel-new)

- [`SpatialModel$new_clone()`](#method-SpatialModel-new_clone)

- [`SpatialModel$clone()`](#method-SpatialModel-clone)

Inherited methods

- [`poems::GenericModel$get_attribute()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute)
- [`poems::GenericModel$get_attribute_aliases()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_aliases)
- [`poems::GenericModel$get_attribute_names()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_names)
- [`poems::GenericModel$get_attributes()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attributes)
- [`poems::GenericModel$set_attributes()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-set_attributes)

------------------------------------------------------------------------

### Method `new()`

Initialization method sets given attributes individually and/or from a
list.

#### Usage

    SpatialModel$new(region = NULL, ...)

#### Arguments

- `region`:

  A
  [`Region`](https://globalecologylab.github.io/poems/reference/Region.md)
  (or inherited class) object specifying the study region.

- `...`:

  Parameters passed individually.

------------------------------------------------------------------------

### Method `new_clone()`

Creates a new (re-initialized) object of the current (inherited) object
class with optionally passed parameters.

#### Usage

    SpatialModel$new_clone(...)

#### Arguments

- `...`:

  Parameters passed via the inherited class constructor (defined in
  initialize and run via new).

#### Returns

New object of the current (inherited) class.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SpatialModel$clone(deep = FALSE)

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
# Example spatial model
model1 <- SpatialModel$new(region = region, a_layers = 3)
model1$coordinates
#>        x      y
#> 1 177.02 -18.02
#> 2 177.04 -18.02
#> 3 177.02 -18.03
#> 4 177.04 -18.03
#> 5 177.02 -18.04
#> 6 177.03 -18.04
#> 7 177.04 -18.04
model1$set_attributes(a_values = array(8:28, c(7, 3)))
model1$region$raster_from_values(model1$get_attribute("a_values"))
#> class      : RasterBrick 
#> dimensions : 5, 5, 25, 3  (nrow, ncol, ncell, nlayers)
#> resolution : 0.01, 0.01  (x, y)
#> extent     : 177.005, 177.055, -18.055, -18.005  (xmin, xmax, ymin, ymax)
#> crs        : +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
#> source     : memory
#> names      : layer.1, layer.2, layer.3 
#> min values :       8,      15,      22 
#> max values :      14,      21,      28 
#> 
```
