# R6 class representing a model simulator.

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class for running
individual model simulations via a simulation function, storing results,
and generating success/error statuses.

## Super class

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\> `ModelSimulator`

## Public fields

- `attached`:

  A list of dynamically attached attributes (name-value pairs).

## Active bindings

- `simulation_model`:

  A SimulationModel object or an inherited class object.

- `simulation_function`:

  Name (character string) or direct assignment (assigned or loaded via
  source path) of the simulation function, which takes a
  [`SimulationModel`](https://globalecologylab.github.io/poems/reference/SimulationModel.md)
  (or inherited class) as an input and returns the simulation results.

- `sample_id`:

  An identifier for the simulation sample.

- `results`:

  A list of result structures.

## Methods

### Public methods

- [`ModelSimulator$new()`](#method-ModelSimulator-new)

- [`ModelSimulator$new_clone()`](#method-ModelSimulator-new_clone)

- [`ModelSimulator$get_attribute()`](#method-ModelSimulator-get_attribute)

- [`ModelSimulator$run()`](#method-ModelSimulator-run)

- [`ModelSimulator$clone()`](#method-ModelSimulator-clone)

------------------------------------------------------------------------

### Method `new()`

Initialization method sets the population model, and optionally the
simulation function, the sample ID, and any attached attributes listed
individually.

#### Usage

    ModelSimulator$new(
      simulation_model = NULL,
      simulation_function = NULL,
      sample_id = NULL,
      ...
    )

#### Arguments

- `simulation_model`:

  A
  [`SimulationModel`](https://globalecologylab.github.io/poems/reference/SimulationModel.md)
  (or inherited class) object (can be set later).

- `simulation_function`:

  Optional name (character string) or direct assignment (assigned or
  loaded via source path) of the simulation function, which takes a
  [`SimulationModel`](https://globalecologylab.github.io/poems/reference/SimulationModel.md)
  (or inherited class) as an input and returns the simulation results.

- `sample_id`:

  Optional identifier for the simulation sample.

- `...`:

  Additional parameters passed individually are attached.

------------------------------------------------------------------------

### Method `new_clone()`

Creates a new (re-initialized) object of the current (inherited) object
class with optionally passed parameters.

#### Usage

    ModelSimulator$new_clone(...)

#### Arguments

- `...`:

  Parameters passed via the inherited class constructor (defined in
  initialize and run via new).

#### Returns

New object of the current (inherited) class.

------------------------------------------------------------------------

### Method `get_attribute()`

Returns selected named simulator or attached attribute.

#### Usage

    ModelSimulator$get_attribute(param)

#### Arguments

- `param`:

  Name of the parameter/attribute.

#### Returns

Selected parameter/attribute value.

------------------------------------------------------------------------

### Method `run()`

Runs a model simulator (function), stores the results, and creates a
status log entry as a list.

#### Usage

    ModelSimulator$run()

#### Returns

A list representing a simulation log entry with a *successful* boolean
and a status message template (with a placeholder for the sample
identifier).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ModelSimulator$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Simulation model
model1 <- SimulationModel$new(
  time_steps = 10,
  model_attributes = c("time_steps", "a", "b"),
  params = list(a = 1:7)
)
model1$required_attributes <- model1$model_attributes
# Simulation function
test_simulator <- function(model) {
  sum(unlist(model$get_attributes(model$required_attributes)))
}
# Model simulator
simulator1 <- ModelSimulator$new(
  simulation_model = model1,
  simulation_function = test_simulator
)
simulator1$run()
#> $successful
#> [1] FALSE
#> 
#> $message
#> [1] "Model %s attributes are incomplete/inconsistent: a, b"
#> 
model1$set_attributes(a = 1:10, b = 15)
model1$get_attributes(model1$required_attributes)
#> $time_steps
#> [1] 10
#> 
#> $a
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $b
#> [1] 15
#> 
simulator1$run()
#> $successful
#> [1] TRUE
#> 
#> $message
#> [1] "Model %s simulation ran successfully"
#> 
simulator1$results
#> [1] 80
```
