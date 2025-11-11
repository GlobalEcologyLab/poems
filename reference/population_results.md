# Nested functions for initializing, calculating and collecting population simulator results.

Modular functions for the population simulator for initializing,
calculating and collecting simulator results.

## Usage

``` r
population_results(
  replicates,
  time_steps,
  coordinates,
  initial_abundance,
  results_selection = NULL,
  result_stages = NULL
)
```

## Arguments

- replicates:

  Number of replicate simulation runs.

- time_steps:

  Number of simulation time steps.

- coordinates:

  Data frame (or matrix) of X-Y population coordinates.

- initial_abundance:

  Matrix of initial abundances at each stage (in rows) for each
  population (in columns).

- results_selection:

  List of results selection from: "abundance" (default), "ema",
  "extirpation", "extinction_location", "harvested", "occupancy";
  "summarize" (default) or "replicate".

- result_stages:

  Array of booleans or numeric (0, 1, 2, ...) for each stage to indicate
  which stages are included/combined (each unique digit \> 0; optionally
  named) in the results (default is 1 for all stages).

## Value

List of result functions:

- `initialize_attributes = function())`:

  Constructs and returns an initialized nested list for the selected
  result attributes.

- `initialize_replicate = function(results)`:

  Initializes and returns nested result attributes at the start of each
  replicate.

- `calculate_at_timestep = function(r, tm, stage_abundance, harvested, results)`:

  Appends and calculates (non-NULL) results and returns nested result
  attributes at the end of each time step (tm) within replicate (r).

- `finalize_attributes = function(results)`:

  Finalizes result calculations at the end of the simulation.

## Examples

``` r
coordinates <- array(c(1:4, 4:1), c(7, 2))
initial_abundance <- matrix(c(
  7, 13, 0, 26, 0, 39, 47,
  2, 0, 6, 8, 0, 12, 13,
  0, 3, 4, 6, 0, 9, 10
), nrow = 3, ncol = 7, byrow = TRUE)
results_selection <- c(
  "abundance", "ema", "extirpation",
  "extinction_location", "harvested", "occupancy"
)
result_functions <- population_results(
  replicates = 1, time_steps = 10, coordinates, initial_abundance,
  results_selection = results_selection, result_stages = c(0, 1, 1)
)
result_functions$initialize_attributes()
#> $all
#> $all$abundance
#>  [1] 0 0 0 0 0 0 0 0 0 0
#> 
#> $all$ema
#>  [1] 0 0 0 0 0 0 0 0 0 0
#> 
#> $all$extirpation
#> [1] NA
#> 
#> $all$extinction_location
#>       x  y
#> [1,] NA NA
#> 
#> $all$harvested
#>  [1] 0 0 0 0 0 0 0 0 0 0
#> 
#> $all$occupancy
#>  [1] 0 0 0 0 0 0 0 0 0 0
#> 
#> 
#> $abundance
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    0    0    0    0    0    0    0    0    0     0
#> [2,]    0    0    0    0    0    0    0    0    0     0
#> [3,]    0    0    0    0    0    0    0    0    0     0
#> [4,]    0    0    0    0    0    0    0    0    0     0
#> [5,]    0    0    0    0    0    0    0    0    0     0
#> [6,]    0    0    0    0    0    0    0    0    0     0
#> [7,]    0    0    0    0    0    0    0    0    0     0
#> 
#> $extirpation
#> [1] NA NA NA NA  0 NA NA
#> 
#> $last_occupied_abundance_count
#> [1]  2  3 10 14  0 21 23
#> 
#> $harvested
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    0    0    0    0    0    0    0    0    0     0
#> [2,]    0    0    0    0    0    0    0    0    0     0
#> [3,]    0    0    0    0    0    0    0    0    0     0
#> [4,]    0    0    0    0    0    0    0    0    0     0
#> [5,]    0    0    0    0    0    0    0    0    0     0
#> [6,]    0    0    0    0    0    0    0    0    0     0
#> [7,]    0    0    0    0    0    0    0    0    0     0
#> 
```
