# Nested functions for stage-based population transitions.

Modular functions for the population simulator for performing
staged-based (Leslie/Lefkovitch matrix) transitions via 3D survival and
fecundity arrays.

## Usage

``` r
population_transitions(
  populations,
  demographic_stochasticity,
  fecundity_matrix,
  fecundity_max,
  survival_matrix
)
```

## Arguments

- populations:

  Number of populations.

- demographic_stochasticity:

  Boolean for choosing demographic stochasticity for transitions.

- fecundity_matrix:

  Matrix of transition fecundity rates (Leslie/Lefkovitch matrix with
  non-zero fecundities only).

- fecundity_max:

  Maximum transition fecundity rate (in Leslie/Lefkovitch matrix).

- survival_matrix:

  Matrix of transition survival rates (Leslie/Lefkovitch matrix with
  non-zero survivals only).

## Value

Transition calculation function:
`function(fecundity_array, survival_array, stage_abundance, occupied_indices)`,
where:

- `fecundity_array`:

  3D array of fecundity rates (*stages* rows by *stages* columns by
  *populations* deep).

- `survival_array`:

  3D array of survival rates (*stages* rows by *stages* columns by
  *populations* deep).

- `stage_abundance`:

  Matrix of stage abundances for each population at time step (*stages*
  rows by *populations* columns).

- `occupied_indices`:

  Array of indices for those populations occupied.

- `returns`:

  Transitioned stage abundances.

## Examples

``` r
# Deterministic transition (no stochasticity)
fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
survival_matrix <- array(c(0, 0.5, 0, 0, 0, 0.7, 0, 0, 0.8), c(3, 3))
variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
fecundity_array <- array(fecundity_matrix, c(3, 3, 7)) * variation_array
survival_array <- array(survival_matrix, c(3, 3, 7)) * variation_array
stage_abundance <- matrix(c(
  7, 13, 0, 26, 0, 39, 47,
  2, 0, 6, 8, 0, 12, 13,
  0, 3, 4, 6, 0, 9, 10
), nrow = 3, ncol = 7, byrow = TRUE)
occupied_indices <- (1:7)[-5]
transition_function <- population_transitions(
  populations = 7, demographic_stochasticity = FALSE,
  fecundity_matrix = fecundity_matrix, fecundity_max = NULL,
  survival_matrix = survival_matrix
)
transition_function(
  fecundity_array, survival_array, stage_abundance,
  occupied_indices
)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#> [1,]    5   11   32   48    0   80   91
#> [2,]    3    6    0   13    0   21   27
#> [3,]    1    2    7   11    0   17   19
```
