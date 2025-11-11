# R6 class representing population simulator results.

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class encapsulating
and dynamically generating spatially-explicit
[`population_simulator`](https://globalecologylab.github.io/poems/reference/population_simulator.md)
results, as well as optional re-generated
[`Generator`](https://globalecologylab.github.io/poems/reference/Generator.md)
outputs.

## Super classes

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\>
[`poems::GenericModel`](https://globalecologylab.github.io/poems/reference/GenericModel.md)
-\>
[`poems::SpatialModel`](https://globalecologylab.github.io/poems/reference/SpatialModel.md)
-\>
[`poems::SimulationResults`](https://globalecologylab.github.io/poems/reference/SimulationResults.md)
-\> `PopulationResults`

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

- `trend_interval`:

  Optional time-step range (indices) for trend calculations (assumes
  indices begin after the burn-in when utilized).

- `abundance`:

  Population abundance across simulation time-steps (summary list or
  replicate array).

- `abundance_stages`:

  Population abundance for combined stages across simulation time-steps
  (list of summary lists or replicate arrays for each combined stage).

- `abundance_trend`:

  Trend or average Sen's
  [`slope`](https://rdrr.io/pkg/trend/man/sens.slope.html) of abundance
  (optionally across a time-step interval).

- `ema`:

  Array of population expected minimum abundance (EMA) across simulation
  time-steps.

- `extirpation`:

  Array of population extirpation times.

- `extinction_location`:

  The weighted centroid of cells occupied in the time-step prior to the
  extirpation of all populations (if it occurred).

- `harvested`:

  Number of animals harvested from each population across simulation
  time-steps (summary list or replicate array).

- `harvested_stages`:

  Number of animals harvested from each population for combined stages
  across simulation time-steps (list of summary lists or replicate
  arrays for each combined stage).

- `occupancy`:

  Array of the number of populations occupied at each time-step.

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

- [`PopulationResults$clone()`](#method-PopulationResults-clone)

Inherited methods

- [`poems::GenericModel$get_attribute()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute)
- [`poems::GenericModel$get_attribute_aliases()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_aliases)
- [`poems::SimulationResults$get_attribute_names()`](https://globalecologylab.github.io/poems/reference/SimulationResults.html#method-get_attribute_names)
- [`poems::SimulationResults$get_attributes()`](https://globalecologylab.github.io/poems/reference/SimulationResults.html#method-get_attributes)
- [`poems::SimulationResults$initialize()`](https://globalecologylab.github.io/poems/reference/SimulationResults.html#method-initialize)
- [`poems::SimulationResults$new_clone()`](https://globalecologylab.github.io/poems/reference/SimulationResults.html#method-new_clone)
- [`poems::SimulationResults$set_attributes()`](https://globalecologylab.github.io/poems/reference/SimulationResults.html#method-set_attributes)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PopulationResults$clone(deep = FALSE)

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

# Sample results occupancy (ignore cell 2 in last 5 time steps)
occupancy_raster <- region$raster_from_values(array(1, c(7, 13)))
occupancy_raster[region$region_indices][2, 9:13] <- 0
occupancy_raster[region$region_indices]
#>      layer.1 layer.2 layer.3 layer.4 layer.5 layer.6 layer.7 layer.8 layer.9
#> [1,]       1       1       1       1       1       1       1       1       1
#> [2,]       1       1       1       1       1       1       1       1       0
#> [3,]       1       1       1       1       1       1       1       1       1
#> [4,]       1       1       1       1       1       1       1       1       1
#> [5,]       1       1       1       1       1       1       1       1       1
#> [6,]       1       1       1       1       1       1       1       1       1
#> [7,]       1       1       1       1       1       1       1       1       1
#>      layer.10 layer.11 layer.12 layer.13
#> [1,]        1        1        1        1
#> [2,]        0        0        0        0
#> [3,]        1        1        1        1
#> [4,]        1        1        1        1
#> [5,]        1        1        1        1
#> [6,]        1        1        1        1
#> [7,]        1        1        1        1
# Population simulation example results
example_results <- list(abundance = t(apply(matrix(11:17), 1, function(n) {
  c(rep(n, 3), round(n * exp(-(0:9) / 2)))
})))
example_results$harvested <- round(example_results$abundance * 0.3)
example_results
#> $abundance
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
#> [1,]   11   11   11   11    7    4    2    1    1     1     0     0     0
#> [2,]   12   12   12   12    7    4    3    2    1     1     0     0     0
#> [3,]   13   13   13   13    8    5    3    2    1     1     0     0     0
#> [4,]   14   14   14   14    8    5    3    2    1     1     0     0     0
#> [5,]   15   15   15   15    9    6    3    2    1     1     0     0     0
#> [6,]   16   16   16   16   10    6    4    2    1     1     0     0     0
#> [7,]   17   17   17   17   10    6    4    2    1     1     1     0     0
#> 
#> $harvested
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
#> [1,]    3    3    3    3    2    1    1    0    0     0     0     0     0
#> [2,]    4    4    4    4    2    1    1    1    0     0     0     0     0
#> [3,]    4    4    4    4    2    2    1    1    0     0     0     0     0
#> [4,]    4    4    4    4    2    2    1    1    0     0     0     0     0
#> [5,]    4    4    4    4    3    2    1    1    0     0     0     0     0
#> [6,]    5    5    5    5    3    2    1    1    0     0     0     0     0
#> [7,]    5    5    5    5    3    2    1    1    0     0     0     0     0
#> 
# Population results object
pop_results <- PopulationResults$new(
  region = region,
  time_steps = 13,
  burn_in_steps = 3,
  occupancy_mask = occupancy_raster,
  trend_interval = 1:5
)
pop_results$get_attribute_names(all = TRUE)
#>  [1] "region"                  "coordinates"            
#>  [3] "time_steps"              "burn_in_steps"          
#>  [5] "occupancy_mask"          "trend_interval"         
#>  [7] "abundance"               "abundance_stages"       
#>  [9] "abundance_trend"         "ema"                    
#> [11] "extirpation"             "extinction_location"    
#> [13] "harvested"               "harvested_stages"       
#> [15] "occupancy"               "error_messages"         
#> [17] "warning_messages"        "all$region"             
#> [19] "all$coordinates"         "all$time_steps"         
#> [21] "all$burn_in_steps"       "all$occupancy_mask"     
#> [23] "all$trend_interval"      "all$abundance"          
#> [25] "all$abundance_stages"    "all$abundance_trend"    
#> [27] "all$ema"                 "all$extirpation"        
#> [29] "all$extinction_location" "all$harvested"          
#> [31] "all$harvested_stages"    "all$occupancy"          
#> [33] "all$error_messages"      "all$warning_messages"   
# Clone (for each population simulation results)
results_clone <- pop_results$new_clone(results = example_results)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'i' in selecting a method for function '[': object 'region' not found
results_clone$all$get_attribute("abundance")
#> Error: object 'results_clone' not found
results_clone$get_attributes(c(
  "abundance", "all$abundance",
  "abundance_trend", "all$abundance_trend",
  "all$ema", # only defined for all
  "extirpation", "all$extirpation",
  "all$extinction_location", # only defined for all
  "harvested", "all$harvested",
  "occupancy", "all$occupancy"
))
#> Error: object 'results_clone' not found
```
