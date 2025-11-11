# Nested functions for population dispersal.

Modular functions for the population simulator for performing dispersal
of stage abundance at a specified time step via dispersal rates
provided.

## Usage

``` r
population_dispersal(
  replicates,
  time_steps,
  years_per_step,
  populations,
  demographic_stochasticity,
  density_stages,
  dispersal,
  dispersal_stages,
  dispersal_source_n_k = NULL,
  dispersal_target_k = NULL,
  dispersal_target_n = NULL,
  dispersal_target_n_k = NULL,
  simulator
)
```

## Arguments

- replicates:

  Number of replicate simulation runs.

- time_steps:

  Number of simulation time steps.

- years_per_step:

  Number of years per time step.

- populations:

  Number of populations.

- demographic_stochasticity:

  Boolean for optionally choosing demographic stochasticity for the
  transformation.

- density_stages:

  Array of booleans or numeric (0,1) for each stage to indicate which
  stages are affected by density.

- dispersal:

  Either a matrix of dispersal rates between populations (source columns
  to target rows) or a list of data frames of non-zero dispersal rates
  and indices for constructing a compact dispersal matrix, and optional
  changing rates over time (as per class
  [`DispersalGenerator`](https://globalecologylab.github.io/poems/reference/DispersalGenerator.md)
  *dispersal_data* attribute). Alternatively a user-defined function
  (optionally nested in a list with additional attributes) may be used:
  `function(params)`, where *params* is a list passed to the function
  containing:

  `replicates`

  :   Number of replicate simulation runs.

  `time_steps`

  :   Number of simulation time steps.

  `years_per_step`

  :   Number of years per time step.

  `populations`

  :   Number of populations.

  `stages`

  :   Number of life cycle stages.

  `demographic_stochasticity`

  :   Boolean for optionally choosing demographic stochasticity for the
      transformation.

  `density_stages`

  :   Array of booleans or numeric (0,1) for each stage to indicate
      which stages are affected by density.

  `dispersal_stages`

  :   Array of relative dispersal (0-1) for each stage to indicate the
      degree to which each stage participates in dispersal. This factor
      modifies dispersal proportion, not dispersal rate.

  `dispersal_source_n_k`

  :   Dispersal proportion (p) density dependence via source population
      abundance divided by carrying capacity (n/k), where p is reduced
      via a linear slope (defined by two list items) from n/k \<=
      *cutoff* (p = 0) to n/k \>= *threshold*.

  `dispersal_target_k`

  :   Dispersal rate (r) density dependence via target population
      carrying capacity (k), where r is reduced via a linear slope
      (through the origin) when k \<= *threshold*.

  `dispersal_target_n`

  :   Dispersal rate (r) density dependence via target population
      abundance (n), where r is reduced via a linear slope (defined by
      two list items) from n \>= *threshold* to n \<= *cutoff* (r = 0)
      or vice versa.

  `dispersal_target_n_k`

  :   Dispersal rate (r) density dependence via target population
      abundance divided by carrying capacity (n/k), where r is reduced
      via a linear slope (defined by two list items) from n/k \>=
      *threshold* to n/k \<= *cutoff* (r = 0) or vice versa.

  `r`

  :   Simulation replicate.

  `tm`

  :   Simulation time step.

  `carrying_capacity`

  :   Array of carrying capacity values for each population at time
      step.

  `stage_abundance`

  :   Matrix of abundance for each stage (rows) and population (columns)
      at time step.

  `occupied_indices`

  :   Array of indices for populations occupied at time step.

  `simulator`

  :   [`SimulatorReference`](https://globalecologylab.github.io/poems/reference/SimulatorReference.md)
      object with dynamically accessible *attached* and *results* lists.

  `additional attributes`

  :   Additional attributes when the transformation is optionally nested
      in a list.

  returns the post-dispersal abundance matrix

- dispersal_stages:

  Array of relative dispersal (0-1) for each stage to indicate the
  degree to which each stage participates in dispersal (default is 1 for
  all stages). This factor modifies dispersal proportion, not dispersal
  rate.

- dispersal_source_n_k:

  Dispersal proportion (p) density dependence via source population
  abundance divided by carrying capacity (n/k), where p is reduced via a
  linear slope (defined by two list items) from n/k \<= *cutoff* (p = 0)
  to n/k \>= *threshold* or vice versa.

- dispersal_target_k:

  Dispersal rate (r) density dependence via target population carrying
  capacity (k), where r is reduced via a linear slope (through the
  origin) when k \<= *threshold*.

- dispersal_target_n:

  Dispersal rate (r) density dependence via target population abundance
  (n), where r is reduced via a linear slope (defined by two list items)
  from n \>= *threshold* to n \<= *cutoff* (r = 0) or visa-versa.

- dispersal_target_n_k:

  Dispersal rate (r) density dependence via target population abundance
  divided by carrying capacity (n/k), where r is reduced via a linear
  slope (defined by two list items) from n/k \>= *threshold* to n/k \<=
  *cutoff* (r = 0) or vice versa.

- simulator:

  [`SimulatorReference`](https://globalecologylab.github.io/poems/reference/SimulatorReference.md)
  object with dynamically accessible *attached* and *results* lists.

## Value

Dispersal function:
`function(r, tm, carrying_capacity, stage_abundance, occupied_indices)`,
where:

- `r`:

  Simulation replicate.

- `tm`:

  Simulation time step.

- `carrying_capacity`:

  Array of carrying capacity values for each population at time step.

- `stage_abundance`:

  Matrix of abundance for each stage (rows) and population (columns) at
  time step.

- `occupied_indices`:

  Array of indices for populations occupied at time step.

- `returns`:

  New stage abundance matrix with dispersal applied.

## Examples

``` r
# User-defined dispersal: one-quarter of dispersing stages move one population over
simulator <- SimulatorReference$new()
example_function <- function(params) {
  params$simulator$attached$params <- params # attach to reference object
  emigrants <- round(params$stage_abundance * params$dispersal_stages * 0.25)
  return(params$stage_abundance - emigrants + emigrants[, c(7, 1:6)])
}
dispersal_function <- population_dispersal(
  replicates = 4,
  time_steps = 10,
  years_per_step = 1,
  populations = 7,
  demographic_stochasticity = TRUE,
  density_stages = c(0, 1, 1),
  dispersal = example_function,
  dispersal_stages = c(0, 1, 0.5),
  dispersal_source_n_k = list(cutoff = -0.5, threshold = 1.5),
  dispersal_target_k = 5,
  dispersal_target_n = list(threshold = 10, cutoff = 15),
  simulator = simulator
)
carrying_capacity <- rep(10, 7)
stage_abundance <- matrix(
  c(
    7, 13, 0, 26, 0, 39, 47,
    2, 0, 6, 8, 0, 12, 13,
    0, 3, 4, 6, 0, 9, 10
  ),
  nrow = 3,
  ncol = 7,
  byrow = TRUE
)
occupied_indices <- (1:7)[-5]
dispersal_function(
  r = 2, tm = 6, carrying_capacity, stage_abundance,
  occupied_indices
)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#> [1,]    7   13    0   26    0   39   47
#> [2,]    5    0    4    8    2    9   13
#> [3,]    1    3    4    5    1    8   10
```
