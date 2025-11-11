# R6 class representing a population model

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class representing a
spatially-explicit demographic-based population model. It extends the
[`SimulationModel`](https://globalecologylab.github.io/poems/reference/SimulationModel.md)
class with parameters for the
[`population_simulator`](https://globalecologylab.github.io/poems/reference/population_simulator.md)
function. It inherits functionality for creating a nested model, whereby
a nested template model with fixed parameters is maintained when a model
is cloned for various sampled parameters. Also provided are extensions
to the methods for checking the consistency and completeness of model
parameters.

## Super classes

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\>
[`poems::GenericModel`](https://globalecologylab.github.io/poems/reference/GenericModel.md)
-\>
[`poems::SpatialModel`](https://globalecologylab.github.io/poems/reference/SpatialModel.md)
-\>
[`poems::SimulationModel`](https://globalecologylab.github.io/poems/reference/SimulationModel.md)
-\> `PopulationModel`

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

- `populations`:

  Number of population cells.

- `stages`:

  Number of life cycle stages.

- `initial_abundance`:

  Array (matrix) or raster (stack) of initial abundance values at each
  population cell (for each age/stage).

- `stage_matrix`:

  Matrix of transition (fecundity & survival) rates between stages at
  each time step (Leslie/Lefkovitch matrix).

- `fecundity_mask`:

  Matrix of 0-1 to indicate which (proportions) of transition rates
  refer to fecundity.

- `fecundity_max`:

  Maximum transition fecundity rate (in Leslie/Lefkovitch matrix).

- `demographic_stochasticity`:

  Boolean for choosing demographic stochasticity for transition,
  dispersal, harvest and/or other processes.

- `standard_deviation`:

  Standard deviation matrix (or single value) for applying environmental
  stochasticity to transition rates.

- `correlation`:

  Simulator-dependent attribute or list of attributes for
  describing/parameterizing the correlation strategy utilized when
  applying environmental stochasticity and/or other processes (see
  [`population_simulator`](https://globalecologylab.github.io/poems/reference/population_simulator.md)).

- `carrying_capacity`:

  Array (matrix), or raster (stack) of carrying capacity values at each
  population cell (across time).

- `density_dependence`:

  Simulator-dependent function, attribute or list of attributes for
  describing/parameterizing the density dependence strategy utilized
  (see
  [`population_simulator`](https://globalecologylab.github.io/poems/reference/population_simulator.md)).

- `growth_rate_max`:

  Maximum growth rate (utilized by density dependence processes).

- `density_affects`:

  Transition vital rates that are affected by density, including
  *"fecundity"*, *"survival"*, or a matrix of booleans or numeric (0-1)
  indicating vital rates affected (default is all).

- `density_stages`:

  Array of booleans or numeric (0-1) for each stage to indicate (the
  degree to) which stages are affected by density (default is 1 for all
  stages).

- `translocation`:

  Simulator-dependent function, attribute or list of attributes for
  describing/parameterizing translocation (management) strategies
  utilized (see
  [`population_simulator`](https://globalecologylab.github.io/poems/reference/population_simulator.md)).

- `harvest`:

  Simulator-dependent function, attribute or list of attributes for
  describing/parameterizing a harvest (organism removal/hunting)
  strategy (see
  [`population_simulator`](https://globalecologylab.github.io/poems/reference/population_simulator.md)).

- `mortality`:

  Simulator-dependent function, attribute or list of attributes to
  describe/parameterize a spatio-temporal mortality strategy (see
  [`population_simulator`](https://globalecologylab.github.io/poems/reference/population_simulator.md)).

- `dispersal`:

  Simulator-dependent function, attribute or list of attributes for
  describing/parameterizing the dispersal (migration) strategy utilized
  (see
  [`population_simulator`](https://globalecologylab.github.io/poems/reference/population_simulator.md)).

- `dispersal_stages`:

  Array of relative dispersal (0-1) for each stage to indicate the
  degree to which each stage participates in dispersal (default is 1 for
  all stages). This factor modifies dispersal proportion, not dispersal
  rate.

- `dispersal_source_n_k`:

  Simulator-dependent attribute for describing/parameterizing dispersal
  dependent on source population abundance divided by carrying capacity
  (see
  [`population_simulator`](https://globalecologylab.github.io/poems/reference/population_simulator.md)).

- `dispersal_target_k`:

  Simulator-dependent attribute for describing/parameterizing dispersal
  dependent on target population carrying capacity (see
  [`population_simulator`](https://globalecologylab.github.io/poems/reference/population_simulator.md)).

- `dispersal_target_n`:

  Simulator-dependent attribute (default is list with *threshold* and
  *cutoff*) of attributes for describing/parameterizing dispersal
  dependent on target population abundance (see
  [`population_simulator`](https://globalecologylab.github.io/poems/reference/population_simulator.md)).

- `dispersal_target_n_k`:

  Simulator-dependent attribute (default is list with *threshold* and
  *cutoff*) of attributes for describing/parameterizing dispersal
  dependent on target population abundance/capacity (see
  [`population_simulator`](https://globalecologylab.github.io/poems/reference/population_simulator.md)).

- `abundance_threshold`:

  Abundance threshold (that needs to be exceeded) for each population to
  persist.

- `simulation_order`:

  A vector of simulation process names in configured order of execution
  (default is "transition", "translocation", "harvest", "mortality",
  "dispersal", "results").

- `results_selection`:

  List of attributes to be included in the returned results of each
  simulation run, selected from: "abundance", "ema", "extirpation",
  "extinction_location", "harvested", "occupancy"; "summarize" or
  "replicate".

- `result_stages`:

  Array of booleans or numeric (0, 1, 2, ...) for each stage to indicate
  which stages are included/combined (each unique digit \> 0; optionally
  named) in the results (default is 1 for all stages).

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

- [`PopulationModel$new()`](#method-PopulationModel-new)

- [`PopulationModel$list_consistency()`](#method-PopulationModel-list_consistency)

- [`PopulationModel$clone()`](#method-PopulationModel-clone)

Inherited methods

- [`poems::GenericModel$get_attribute()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute)
- [`poems::GenericModel$get_attribute_aliases()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_aliases)
- [`poems::SimulationModel$get_attribute_names()`](https://globalecologylab.github.io/poems/reference/SimulationModel.html#method-get_attribute_names)
- [`poems::SimulationModel$get_attributes()`](https://globalecologylab.github.io/poems/reference/SimulationModel.html#method-get_attributes)
- [`poems::SimulationModel$incomplete_attributes()`](https://globalecologylab.github.io/poems/reference/SimulationModel.html#method-incomplete_attributes)
- [`poems::SimulationModel$inconsistent_attributes()`](https://globalecologylab.github.io/poems/reference/SimulationModel.html#method-inconsistent_attributes)
- [`poems::SimulationModel$is_complete()`](https://globalecologylab.github.io/poems/reference/SimulationModel.html#method-is_complete)
- [`poems::SimulationModel$is_consistent()`](https://globalecologylab.github.io/poems/reference/SimulationModel.html#method-is_consistent)
- [`poems::SimulationModel$list_completeness()`](https://globalecologylab.github.io/poems/reference/SimulationModel.html#method-list_completeness)
- [`poems::SimulationModel$new_clone()`](https://globalecologylab.github.io/poems/reference/SimulationModel.html#method-new_clone)
- [`poems::SimulationModel$set_attributes()`](https://globalecologylab.github.io/poems/reference/SimulationModel.html#method-set_attributes)
- [`poems::SimulationModel$set_sample_attributes()`](https://globalecologylab.github.io/poems/reference/SimulationModel.html#method-set_sample_attributes)

------------------------------------------------------------------------

### Method `new()`

Initialization method sets default aliases and given attributes
individually and/or from a list.

#### Usage

    PopulationModel$new(attribute_aliases = NULL, ...)

#### Arguments

- `attribute_aliases`:

  A list of alternative alias names for model attributes (form:
  `alias = "attribute"`) to be used with the set and get attributes
  methods.

- `...`:

  Parameters passed via a *params* list or individually.

------------------------------------------------------------------------

### Method `list_consistency()`

Returns a boolean to indicate if (optionally selected or all) model
attributes (such as dimensions) are consistent.

#### Usage

    PopulationModel$list_consistency(params = NULL)

#### Arguments

- `params`:

  Optional array of parameter/attribute names.

#### Returns

List of booleans (or NAs) to indicate consistency of selected/all
attributes.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PopulationModel$clone(deep = FALSE)

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
# Harvest function
harvest <- list(
  rate = NA, # set later
  function(params) round(params$stage_abundance * (1 - params$rate))
)
harvest_rate_alias <- list(harvest_rate = "harvest$rate")
# Template model
stage_matrix <- matrix(c(
  0, 2.5, # Leslie/Lefkovitch matrix
  0.8, 0.5
), nrow = 2, ncol = 2, byrow = TRUE)
template_model <- PopulationModel$new(
  region = region,
  time_steps = 10, # years
  populations = region$region_cells, # 7
  stage_matrix = stage_matrix,
  harvest = harvest,
  results_selection = c("abundance", "harvested"),
  attribute_aliases = harvest_rate_alias
)
template_model$model_attributes
#>  [1] "region"                    "coordinates"              
#>  [3] "random_seed"               "replicates"               
#>  [5] "time_steps"                "years_per_step"           
#>  [7] "populations"               "stages"                   
#>  [9] "initial_abundance"         "stage_matrix"             
#> [11] "fecundity_mask"            "fecundity_max"            
#> [13] "demographic_stochasticity" "standard_deviation"       
#> [15] "correlation"               "carrying_capacity"        
#> [17] "density_dependence"        "growth_rate_max"          
#> [19] "density_affects"           "density_stages"           
#> [21] "translocation"             "harvest"                  
#> [23] "mortality"                 "dispersal"                
#> [25] "dispersal_stages"          "dispersal_source_n_k"     
#> [27] "dispersal_target_k"        "dispersal_target_n"       
#> [29] "dispersal_target_n_k"      "abundance_threshold"      
#> [31] "simulation_order"          "results_selection"        
#> [33] "result_stages"            
template_model$required_attributes
#> [1] "time_steps"        "initial_abundance" "stage_matrix"     
#> [4] "carrying_capacity"
# Nested model
nested_model <- PopulationModel$new(template_model = template_model)
nested_model$incomplete_attributes()
#> [1] "initial_abundance" "carrying_capacity"
nested_model$set_sample_attributes(
  initial_abundance = rep(10, 7),
  carrying_capacity = array(70:1, c(10, 7)),
  harvest_rate = 0.3
)
nested_model$inconsistent_attributes()
#> [1] "carrying_capacity"
nested_model$carrying_capacity <- array(70:1, c(7, 10))
nested_model$is_consistent()
#> [1] TRUE
nested_model$is_complete()
#> [1] TRUE
nested_model$harvest$rate
#> [1] 0.3
```
