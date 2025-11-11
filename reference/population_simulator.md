# Runs a stage-based demographic population model simulation.

Simulates a stage-based demographic population model and returns
simulation results across multiple replicate runs. Processes run at each
simulation time-step include:

1.  Density dependence calculations (ceiling, logistic, or user-defined)

2.  Environmental stochasticity calculations

3.  Stage transition (stochastic) calculations

4.  Translocation calculations (user-defined)

5.  Harvest calculations (user-defined)

6.  Mortality calculations (user-defined)

7.  Dispersal calculations (default or user-defined)

8.  Results collection

## Usage

``` r
population_simulator(inputs)
```

## Arguments

- inputs:

  Nested list/object with named elements:

  `random_seed`

  :   Number to seed the random number generation for stochasticity.

  `replicates`

  :   Number of replicate simulation runs (default is 1).

  `time_steps`

  :   Number of simulation time steps. Required input.

  `years_per_step`

  :   Number of years per time step (default is 1).

  `populations`

  :   Number of populations. Required input.

  `coordinates`

  :   Data frame (or matrix) of X-Y population coordinates.

  `stages`

  :   Number of life cycle stages.

  `region`

  :   A
      [`Region`](https://globalecologylab.github.io/poems/reference/Region.md)
      object defining the study region.

  `initial_abundance`

  :   Array (or matrix) of initial abundances (at each stage in rows)
      for each population (in columns). If there is only one stage and a
      region object is attached, then initial abundance may be provided
      in the form of a raster with the same specs as the region raster.
      A vector can be provided that will be forced to a matrix. Required
      input.

  `stage_matrix`

  :   Matrix of transition (fecundity & survival) rates between stages
      at each time step (Leslie/Lefkovitch matrix). Required input.

  `fecundity_mask`

  :   Matrix of 0-1 to indicate which (proportions) of transition rates
      refer to fecundity.

  `fecundity_max`

  :   Maximum transition fecundity rate (in Leslie/Lefkovitch matrix).

  `demographic_stochasticity`

  :   Boolean for choosing demographic stochasticity for transition,
      dispersal, harvest and/or other processes (default is TRUE).

  `standard_deviation`

  :   Standard deviation matrix (or single value) for applying
      environmental stochasticity to transition rates.

  `correlation`

  :   List containing either an environmental correlation matrix
      (correlation_matrix), a pre-calculated transposed (Cholesky)
      decomposition matrix (t_decomposition_matrix), or a compact
      transposed (Cholesky) decomposition matrix
      (t_decomposition_compact_matrix) and a corresponding map of
      population indices (t_decomposition_compact_map), as per
      [`SpatialCorrelation`](https://globalecologylab.github.io/poems/reference/SpatialCorrelation.md)
      class attributes.

  `carrying_capacity`

  :   Array (matrix) of carrying capacity values at each population cell
      (*populations* rows by *time_steps* columns when across time).
      Required input.

  `density_dependence`

  :   Density dependence can be "ceiling" (default), "logistic"
      (Ricker), or a user-defined function (optionally nested in a list
      with additional attributes) for adjusting transition rates:
      `function(params)`, where *params* is a list passed to the
      function containing:

      `transition_array`

      :   3D array of transition rates: stages by stages by populations.

      `fecundity_mask`

      :   Matrix of 0-1 to indicate which (proportions) of transition
          rates refer to fecundity.

      `fecundity_max`

      :   Maximum transition fecundity rate (in Leslie/Lefkovitch
          matrix).

      `carrying_capacity`

      :   Array of carrying capacity values for each population.

      `stage_abundance`

      :   Matrix of abundances for each stage (rows) and population
          (columns).

      `population_abundance`

      :   Array of summed population abundances for all stages.

      `density_abundance`

      :   Array of summed population abundances for stages affected by
          density.

      `growth_rate_max`

      :   Maximum growth rate value or array for populations.

      `occupied_indices`

      :   Array of indices for populations occupied at (current) time
          step.

      `calculate_multipliers`

      :   Function (`function(growth_rates)`) for finding multipliers
          (when stages \> 1) to apply to affected transitions that
          result in target growth rates (dominant eigenvalues).

      `apply_multipliers`

      :   Function (`function(transition_array, multipliers`) for
          applying multipliers (when stages \> 1) to the affected
          transition rates within a transition array (returns multiplied
          array).

      `simulator`

      :   [`SimulatorReference`](https://globalecologylab.github.io/poems/reference/SimulatorReference.md)
          object with dynamically accessible *attached* and *results*
          lists.

      `optional attributes`

      :   Additional numeric attributes when density dependence is
          optionally nested in a list.

      returns a transformed transition 3D array

  `growth_rate_max`

  :   Maximum growth rate (utilized by density dependence processes).

  `density_affects`

  :   Matrix of booleans or numeric (0-1) indicating the transition
      vital rates affected by density (default is all).

  `density_stages`

  :   Array of booleans or numeric (0,1) for each stage to indicate
      which stages are affected by density (default is all).

  `density_precision`

  :   Numeric precision of the calculated multipliers (used when stages
      \> 1) applied to affected transition rates (default is 3 decimal
      places).

  `translocation`

  :   An optional user-defined function (optionally nested in a list
      with additional attributes) for applying translocation or
      spatio-temporal management (to abundances): `function(params)`,
      where *params* is a list passed to the function containing:

      `replicates`

      :   Number of replicate simulation runs.

      `time_steps`

      :   Number of simulation time steps.

      `years_per_step`

      :   Number of years per time step.

      `populations`

      :   Number of populations.

      `stages`

      :   Number of lifecycle stages.

      `demographic_stochasticity`

      :   Boolean for optionally choosing demographic stochasticity for
          the transformation.

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

      :   Matrix of (current) abundance for each stage (rows) and
          population (columns) at time step.

      `occupied_indices`

      :   Array of indices for populations occupied at (current) time
          step.

      `simulator`

      :   [`SimulatorReference`](https://globalecologylab.github.io/poems/reference/SimulatorReference.md)
          object with dynamically accessible *attached* and *results*
          lists.

      `additional attributes`

      :   Additional attributes when the transformation is optionally
          nested in a list.

      returns a transformed stage abundance matrix (or a list with stage
      abundance and carrying capacity)

  `harvest`

  :   An optional user-defined function (optionally nested in a list
      with additional attributes) for applying harvesting (to
      abundances): `function(params)` as per translocation.

  `mortality`

  :   An optional user-defined function (optionally nested in a list
      with additional attributes) for applying mortality (to
      abundances): `function(params)` as per translocation.

  `dispersal`

  :   Either a matrix of dispersal rates between populations (source
      columns to target rows) or a list of data frames of non-zero
      dispersal rates and indices for constructing a compact dispersal
      matrix, and optional changing rates over time (as per class
      [`DispersalGenerator`](https://globalecologylab.github.io/poems/reference/DispersalGenerator.md)
      *dispersal_data* attribute). Alternatively a user-defined function
      (optionally nested in a list with additional attributes) may be
      used: `function(params)`, where *params* is a list passed to the
      function containing:

      `replicates`

      :   Number of replicate simulation runs.

      `time_steps`

      :   Number of simulation time steps.

      `years_per_step`

      :   Number of years per time step.

      `populations`

      :   Number of populations.

      `demographic_stochasticity`

      :   Boolean for optionally choosing demographic stochasticity for
          the transformation.

      `density_stages`

      :   Array of booleans or numeric (0,1) for each stage to indicate
          which stages are affected by density.

      `dispersal_stages`

      :   Array of relative dispersal (0-1) for each stage to indicate
          the degree to which each stage participates in dispersal. This
          factor modifies dispersal proportion, not dispersal rate.

      `r`

      :   Simulation replicate.

      `tm`

      :   Simulation time step.

      `carrying_capacity`

      :   Array of carrying capacity values for each population at time
          step.

      `stage_abundance`

      :   Matrix of abundance for each stage (rows) and population
          (columns) at time step.

      `occupied_indices`

      :   Array of indices for populations occupied at time step.

      `simulator`

      :   [`SimulatorReference`](https://globalecologylab.github.io/poems/reference/SimulatorReference.md)
          object with dynamically accessible *attached* and *results*
          lists.

      `additional attributes`

      :   Additional attributes when the transformation is optionally
          nested in a list.

      returns the post-dispersal abundance matrix

  `dispersal_stages`

  :   Array of relative dispersal (0-1) for each stage to indicate the
      degree to which each stage participates in dispersal (default is 1
      for all stages). This factor modifies dispersal proportion, not
      dispersal rate.

  `dispersal_source_n_k`

  :   Dispersal proportion (p) density dependence via source population
      abundance divided by carrying capacity (n/k), where p is reduced
      via a linear slope (defined by two list items) from n/k \<=
      *cutoff* (p = 0) to n/k \>= *threshold* (aliases:
      *dispersal_n_k_cutoff* & *dispersal_n_k_threshold*).

  `dispersal_target_k`

  :   Dispersal rate (r) density dependence via target population
      carrying capacity (k), where r is reduced via a linear slope
      (through the origin) when k \<= *threshold* (alias:
      *dispersal_k_threshold*).

  `dispersal_target_n`

  :   Dispersal rate (r) density dependence via target population
      abundance (n), where r is reduced via a linear slope (defined by
      two list items) from n \>= *threshold* to n \<= *cutoff* (r = 0)
      or vice versa (aliases: *dispersal_n_threshold* &
      *dispersal_n_cutoff*).

  `dispersal_target_n_k`

  :   Dispersal rate (r) density dependence via target population
      abundance divided by carrying capacity (n/k), where r is reduced
      via a linear slope (defined by two list items) from n/k \>=
      *threshold* to n/k \<= *cutoff* (r = 0) or vica versa.

  `abundance_threshold`

  :   Abundance threshold (that needs to be exceeded) for each
      population to persist.

  `simulation_order`

  :   A vector of simulation process names in configured order of
      execution (default is "transition", "translocation", "harvest"
      (plus harvested results), "mortality", "dispersal", "results"
      (except harvested).

  `additional transformation functions`

  :   Additional user-defined abundance transformation functions
      (optionally nested in lists with additional attributes) are
      utilised when listed in *simulation_order* (function as per
      translocation).

  `results_selection`

  :   List of results selection from: "abundance" (default), "ema",
      "extirpation", "extinction_location", "harvested", "occupancy";
      "summarize" (default) or "replicate".

  `result_stages`

  :   Array of booleans or numeric (0, 1, 2, ...) for each stage to
      indicate which stages are included/combined (each unique digit \>
      0; optionally named) in the results (default is 1 for all stages).

## Value

Selected simulation results as a nested list summarized (mean, sd, min,
max) across multiple replicates (default), or 2-3D arrays including
results for each replicate:

- `abundance`:

  Matrix or 3D array of simulation abundance: *populations* rows by
  *time_steps* columns (by *replicates* deep).

- `abundance_stages`:

  List of matrices or 3D arrays of simulation abundance for unique stage
  combinations when present: each *populations* rows by *time_steps*
  columns (by *replicates* deep).

- `all$abundance`:

  Array or matrix of total abundance across populations: *time_steps*
  (rows by *replicates* columns).

- `all$abundance_stages`:

  List of arrays or matrices of total abundance across populations for
  unique stage combinations when present: each *time_steps* (rows by
  *replicates* columns).

- `all$ema`:

  Array of expected minimum abundance at each time step (averaged across
  replicates).

- `extirpation`:

  Array or matrix of extirpation times: *populations* (rows by
  *replicates* columns).

- `all$extirpation`:

  Array of extirpation time across populations for each replicate.

- `all$extinction_location`:

  The weighted centroid of cells occupied in the time-step prior to the
  extirpation of all populations (if it occurred) for each replicate.

- `harvested`:

  Matrix or 3D array of individuals harvested: *populations* rows by
  *time_steps* columns (by *replicates* deep).

- `harvested_stages`:

  List of matrices or 3D arrays of individuals harvested for unique
  stage combinations when present: each *populations* rows by
  *time_steps* columns (by *replicates* deep).

- `all$harvested`:

  Array or matrix of individuals harvested across populations:
  *time_steps* (rows by *replicates* columns).

- `all$harvested_stages`:

  List of arrays or matrices of individuals harvested across populations
  for unique stage combinations when present: each *time_steps* (rows by
  *replicates* columns).

- `all$occupancy`:

  Array or matrix of the number of populations occupied at each
  time-step: *time_steps* (rows by *replicates* columns).

- `additional results`:

  Additional results may be attached via user-defined functions (using
  `params$simulator$results`).

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
  rate = 0.3,
  function(params) round(params$stage_abundance * (1 - params$rate))
)
# Population model
stage_matrix <- matrix(c(
  0, 2.5, # Leslie/Lefkovitch matrix
  0.8, 0.5
), nrow = 2, ncol = 2, byrow = TRUE)
pop_model <- PopulationModel$new(
  region = region,
  time_steps = 10, # years
  populations = region$region_cells, # 7
  stage_matrix = stage_matrix,
  initial_abundance = rep(10, 7),
  carrying_capacity = array(70:1, c(7, 10)),
  harvest = harvest,
  results_selection = c("abundance", "harvested")
)
# Simulations
population_simulator(pop_model) # model
#> $all
#> $all$abundance
#>  [1]  86 112 125 175 170 148 121  89  55  19
#> 
#> $all$harvested
#>  [1] 37 47 50 74 72 64 54 37 22  9
#> 
#> 
#> $abundance
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]   14   19   24   30   30   25   19   15   10     5
#> [2,]   11   16   16   31   29   24   19   14    9     4
#> [3,]   10   16   14   13   13   14   18   14    8     3
#> [4,]   16   22   23   31   27   22   17   13    8     2
#> [5,]   13   14   16   21   27   22   17   12    7     2
#> [6,]   14   16   23   31   25   21   16   11    7     2
#> [7,]    8    9    9   18   19   20   15   10    6     1
#> 
#> $harvested
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    6    8   11   13   12   10    9    6    4     2
#> [2,]    4    6    6   13   12   10    8    6    4     2
#> [3,]    5    7    6    5    5    7    8    5    4     2
#> [4,]    8   10    9   14   12   10    8    5    3     2
#> [5,]    5    5    6    8   11    9    7    5    3     1
#> [6,]    6    7    9   13   12    9    7    5    2     0
#> [7,]    3    4    3    8    8    9    7    5    2     0
#> 
inputs <- pop_model$get_attributes()
population_simulator(inputs) # list
#> $all
#> $all$abundance
#>  [1]  86 100  92  98 123 122 115  90  56  20
#> 
#> $all$harvested
#>  [1] 36 44 38 44 52 55 47 36 21  8
#> 
#> 
#> $abundance
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]   13   16   15   13   20   21   20   15   10     5
#> [2,]   10   14   12   14   11   13   16   14    9     4
#> [3,]   15   19   19   23   27   19   19   13    9     3
#> [4,]   14   17   18   14   27   23   17   13    8     3
#> [5,]   14   15    8   12    7   11   11   12    7     2
#> [6,]    9    8    8   10   12   16   16   12    7     2
#> [7,]   11   11   12   12   19   19   16   11    6     1
#> 
#> $harvested
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    6    7    7    6    8   10    8    6    4     2
#> [2,]    4    6    4    7    5    6    7    6    4     2
#> [3,]    6    8    8   10   12    9    7    6    3     2
#> [4,]    6    8    8    5   12    9    8    5    3     1
#> [5,]    5    7    2    6    3    5    4    5    3     1
#> [6,]    4    3    4    5    4    7    7    4    2     0
#> [7,]    5    5    5    5    8    9    6    4    2     0
#> 
```
