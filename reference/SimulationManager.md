# R6 class representing a simulation manager.

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class to represent a
manager for running multiple model simulations and saving results.

## Super classes

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\>
[`poems::GenericManager`](https://globalecologylab.github.io/poems/reference/GenericManager.md)
-\> `SimulationManager`

## Public fields

- `attached`:

  A list of dynamically attached attributes (name-value pairs).

## Active bindings

- `sample_data`:

  A data frame of sampled parameters for each simulation/result.

- `model_template`:

  A
  [`SimulationModel`](https://globalecologylab.github.io/poems/reference/SimulationModel.md)
  (or inherited class) object with parameters common to all simulations.

- `nested_model`:

  A
  [`SimulationModel`](https://globalecologylab.github.io/poems/reference/SimulationModel.md)
  (or inherited class) object with empty sample parameters and a nested
  model template common to all simulations.

- `generators`:

  A list of generators
  ([`Generator`](https://globalecologylab.github.io/poems/reference/Generator.md)
  or inherited class) objects for generating simulation model values.

- `model_simulator`:

  A
  [`ModelSimulator`](https://globalecologylab.github.io/poems/reference/ModelSimulator.md)
  (or inherited class) object for running the simulations.

- `parallel_cores`:

  Number of cores for running the simulations in parallel.

- `results_dir`:

  Results directory path.

- `results_ext`:

  Result file extension (default is .RData).

- `results_filename_attributes`:

  A vector of: prefix (optional); attribute names (from the sample data
  frame); postfix (optional); utilized to construct results filenames.

- `error_messages`:

  A vector of error messages encountered when setting model attributes.

- `warning_messages`:

  A vector of warning messages encountered when setting model
  attributes.

## Methods

### Public methods

- [`SimulationManager$new()`](#method-SimulationManager-new)

- [`SimulationManager$run()`](#method-SimulationManager-run)

- [`SimulationManager$set_model_sample()`](#method-SimulationManager-set_model_sample)

- [`SimulationManager$log_simulation()`](#method-SimulationManager-log_simulation)

- [`SimulationManager$clone()`](#method-SimulationManager-clone)

Inherited methods

- [`poems::GenericClass$new_clone()`](https://globalecologylab.github.io/poems/reference/GenericClass.html#method-new_clone)
- [`poems::GenericManager$get_attribute()`](https://globalecologylab.github.io/poems/reference/GenericManager.html#method-get_attribute)
- [`poems::GenericManager$get_message_sample()`](https://globalecologylab.github.io/poems/reference/GenericManager.html#method-get_message_sample)
- [`poems::GenericManager$get_results_filename()`](https://globalecologylab.github.io/poems/reference/GenericManager.html#method-get_results_filename)

------------------------------------------------------------------------

### Method `new()`

Initialization method sets any included attributes (*sample_data*,
*model_template*, *generators*, *model_simulator*, *parallel_cores*,
*results_dir*, *results_filename_attributes*) and attaches other
attributes individually listed.

#### Usage

    SimulationManager$new(model_template = NULL, ...)

#### Arguments

- `model_template`:

  A SimulationModel (or inherited class) object with parameters common
  to all simulations.

- `...`:

  Parameters listed individually.

------------------------------------------------------------------------

### Method `run()`

Runs the multiple population simulations (via the set function), stores
the results, and creates/writes a simulation log.

#### Usage

    SimulationManager$run(results_dir = NULL)

#### Arguments

- `results_dir`:

  Results directory path (must be present if not already set within
  manager class object).

#### Returns

Simulator log as a list.

------------------------------------------------------------------------

### Method `set_model_sample()`

Sets the model sample attributes via the sample data frame and the
generators.

#### Usage

    SimulationManager$set_model_sample(model, sample_index)

#### Arguments

- `model`:

  [`SimulationModel`](https://globalecologylab.github.io/poems/reference/SimulationModel.md)
  (or inherited class) object (clone) to receive sample attributes.

- `sample_index`:

  Index of sample from data frame.

------------------------------------------------------------------------

### Method `log_simulation()`

Summarizes the simulation log generated within the run method and writes
it to a text file in the results directory.

#### Usage

    SimulationManager$log_simulation(simulation_log)

#### Arguments

- `simulation_log`:

  Nested list of simulation log entries generated via the run method.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SimulationManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # interactive()
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
# Example population model template
model_template <- PopulationModel$new(
  region = region,
  time_steps = 10, # years
  populations = region$region_cells, # 7
  stage_matrix = 1
)
# Example generators for initial abundance and carrying capacity
hs_matrix <- c(0.5, 0.3, 0.7, 0.9, 0.6, 0.7, 0.8)
initial_gen <- Generator$new(
  description = "initial abundance",
  region = region,
  hs_matrix = hs_matrix, # template attached
  inputs = c("initial_n"),
  outputs = c("initial_abundance")
)
initial_gen$add_generative_requirements(list(initial_abundance = "function"))
initial_gen$add_function_template("initial_abundance",
  function_def = function(params) {
    stats::rmultinom(1,
      size = params$initial_n,
      prob = params$hs_matrix
    )[, 1]
  },
  call_params = c("initial_n", "hs_matrix")
)
capacity_gen <- Generator$new(
  description = "carrying capacity",
  region = region,
  hs_matrix = hs_matrix, # template attached
  inputs = c("density_max"),
  outputs = c("carrying_capacity")
)
capacity_gen$add_generative_requirements(list(carrying_capacity = "function"))
capacity_gen$add_function_template("carrying_capacity",
  function_def = function(params) {
    round(params$density_max * params$hs_matrix)
  },
  call_params = c("density_max", "hs_matrix")
)
# Sample input parameters
sample_data <- data.frame(initial_n = c(40, 60, 80), density_max = c(15, 20, 25))
# Simulation manager
sim_manager <- SimulationManager$new(
  sample_data = sample_data,
  model_template = model_template,
  generators = list(initial_gen, capacity_gen),
  parallel_cores = 2,
  results_dir = tempdir()
)
run_output <- sim_manager$run()
run_output$summary
dir(tempdir(), "*.RData") # includes 3 result files
for (i in 1:3) {
  print(paste("Run", i, "results:"))
  file_name <- paste0(sim_manager$get_results_filename(i), ".RData")
  print(readRDS(file.path(tempdir(), file_name)))
}
dir(tempdir(), "*.txt") # plus simulation log
}
```
