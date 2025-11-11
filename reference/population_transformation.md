# Nested functions for a user-defined population abundance (and capacity) transformation.

Modular functions for the population simulator for performing a
transformation of the stage abundance (and optionally carrying capacity)
at a specified time step via a user-defined function.

## Usage

``` r
population_transformation(
  replicates,
  time_steps,
  years_per_step,
  populations,
  demographic_stochasticity,
  density_stages,
  transformation,
  simulator,
  name = "transformation"
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

- transformation:

  A user-defined function (optionally nested in a list with additional
  attributes) for performing transformation: `function(params)`, where
  *params* is a list passed to the function containing:

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

  `r`

  :   Simulation replicate.

  `tm`

  :   Simulation time step.

  `carrying_capacity`

  :   Array of carrying capacity values for each population at time
      step.

  `stage_abundance`

  :   Matrix of (current) abundance for each stage (rows) and population
      (columns) at time step.

  `occupied_indices`

  :   Array of indices for populations occupied at (current) time step.

  `simulator`

  :   [`SimulatorReference`](https://globalecologylab.github.io/poems/reference/SimulatorReference.md)
      object with dynamically accessible *attached* and *results* lists.

  `additional attributes`

  :   Additional attributes when the transformation is optionally nested
      in a list.

  returns a transformed stage abundance matrix (or a list with stage
  abundance and carrying capacity)

- simulator:

  [`SimulatorReference`](https://globalecologylab.github.io/poems/reference/SimulatorReference.md)
  object with dynamically accessible *attached* and *results* lists.

- name:

  Optional name for the transformation (default is "transformation").

## Value

Abundance (and capacity) transformation function:
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

  List with transformed stage abundance matrix (and optionally carrying
  capacity).

## Examples

``` r
simulator <- SimulatorReference$new()
# Example transformation: a random population is chosen for a severe disturbance event
# (only life cycle stage 3 individuals survive)
disturbance_function <- function(params) {
  params$simulator$attached$params <- params # attach to reference object
  random_population <- sample(params$occupied_indices, 1)
  new_stage_abundance <- params$stage_abundance
  new_stage_abundance[1:2, random_population] <- 0
  return(new_stage_abundance)
}
transformation_function <- population_transformation(
  replicates = 4, time_steps = 10, years_per_step = 1,
  populations = 7, demographic_stochasticity = TRUE,
  density_stages = c(0, 1, 1), transformation = disturbance_function,
  simulator
)
carrying_capacity <- rep(10, 7)
carrying_capacity <- rep(10, 7)
stage_abundance <- matrix(c(
  7, 13, 0, 26, 0, 39, 47,
  2, 0, 6, 8, 0, 12, 13,
  0, 3, 4, 6, 0, 9, 10
), nrow = 3, ncol = 7, byrow = TRUE)
occupied_indices <- (1:7)[-5]
transformation_function(
  r = 2, tm = 6, carrying_capacity, stage_abundance,
  occupied_indices
)
#> $stage_abundance
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#> [1,]    7   13    0   26    0    0   47
#> [2,]    2    0    6    8    0    0   13
#> [3,]    0    3    4    6    0    9   10
#> 
```
