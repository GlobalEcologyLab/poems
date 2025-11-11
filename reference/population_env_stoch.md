# Nested functions for population environmental stochasticity.

Modular functions for the population simulator for performing correlated
environmentally stochastic adjustments to transition rates.

## Usage

``` r
population_env_stoch(
  populations,
  fecundity_matrix,
  fecundity_max,
  survival_matrix,
  standard_deviation,
  correlation
)
```

## Arguments

- populations:

  Number of populations.

- fecundity_matrix:

  Matrix of transition fecundity rates (Leslie/Lefkovitch matrix with
  non-zero fecundities only).

- fecundity_max:

  Maximum transition fecundity rate (in Leslie/Lefkovitch matrix).

- survival_matrix:

  Matrix of transition survival rates (Leslie/Lefkovitch matrix with
  non-zero survivals only).

- standard_deviation:

  Standard deviation matrix for applying environmental stochasticity to
  transition rates.

- correlation:

  List containing either an environmental correlation matrix
  (correlation_matrix), a pre-calculated transposed (Cholesky)
  decomposition matrix (t_decomposition_matrix), or a compact transposed
  (Cholesky) decomposition matrix (t_decomposition_compact_matrix) and a
  corresponding map of population indices (t_decomposition_compact_map),
  as per *SpatialCorrelation* class attributes.

## Value

Environmental stochasticity calculation function:
`function(fecundity_array, survival_array, occupied_indices)`, where:

- `fecundity_array`:

  3D array of fecundity rates (*stages* rows by *stages* columns by
  *populations* deep).

- `survival_array`:

  3D array of survival rates (*stages* rows by *stages* columns by
  *populations* deep).

- `occupied_indices`:

  Array of indices for those populations occupied.

- `returns`:

  List containing stochastically varied fecundity and survival arrays.

## Examples

``` r
fecundity_matrix <- array(c(0, 0, 0, 3, 0, 0, 4, 0, 0), c(3, 3))
survival_matrix <- array(c(0, 0.5, 0, 0, 0, 0.7, 0, 0, 0.8), c(3, 3))
standard_deviation <- (fecundity_matrix + survival_matrix) * 0.3
variation_array <- array(rep(seq(0.85, 1.15, 0.05), each = 9), c(3, 3, 7))
fecundity_array <- array(fecundity_matrix, c(3, 3, 7)) * variation_array
survival_array <- array(survival_matrix, c(3, 3, 7)) * variation_array
stage_abundance <- matrix(c(
  7, 13, 0, 26, 0, 39, 47,
  2, 0, 6, 8, 0, 12, 13,
  0, 3, 4, 6, 0, 9, 10
), nrow = 3, ncol = 7, byrow = TRUE)
occupied_indices <- (1:7)[-5]
env_stoch_function <- population_env_stoch(
  populations = 7, fecundity_matrix, fecundity_max = NULL, survival_matrix,
  standard_deviation, correlation = NULL
)
env_stoch_function(fecundity_array, survival_array, occupied_indices)
#> $fecundity_array
#> , , 1
#> 
#>      [,1]     [,2]     [,3]
#> [1,]    0 1.864996 2.486662
#> [2,]    0 0.000000 0.000000
#> [3,]    0 0.000000 0.000000
#> 
#> , , 2
#> 
#>      [,1]     [,2]     [,3]
#> [1,]    0 1.347192 1.796257
#> [2,]    0 0.000000 0.000000
#> [3,]    0 0.000000 0.000000
#> 
#> , , 3
#> 
#>      [,1]     [,2]     [,3]
#> [1,]    0 3.394361 4.525815
#> [2,]    0 0.000000 0.000000
#> [3,]    0 0.000000 0.000000
#> 
#> , , 4
#> 
#>      [,1]     [,2]    [,3]
#> [1,]    0 3.899423 5.19923
#> [2,]    0 0.000000 0.00000
#> [3,]    0 0.000000 0.00000
#> 
#> , , 5
#> 
#>      [,1] [,2] [,3]
#> [1,]    0 3.15  4.2
#> [2,]    0 0.00  0.0
#> [3,]    0 0.00  0.0
#> 
#> , , 6
#> 
#>      [,1]     [,2]     [,3]
#> [1,]    0 2.508835 3.345113
#> [2,]    0 0.000000 0.000000
#> [3,]    0 0.000000 0.000000
#> 
#> , , 7
#> 
#>      [,1]     [,2]     [,3]
#> [1,]    0 4.594126 6.125501
#> [2,]    0 0.000000 0.000000
#> [3,]    0 0.000000 0.000000
#> 
#> 
#> $survival_array
#> , , 1
#> 
#>           [,1]      [,2]      [,3]
#> [1,] 0.0000000 0.0000000 0.0000000
#> [2,] 0.3050735 0.0000000 0.0000000
#> [3,] 0.0000000 0.4256026 0.4897736
#> 
#> , , 2
#> 
#>           [,1]      [,2]      [,3]
#> [1,] 0.0000000 0.0000000 0.0000000
#> [2,] 0.1708776 0.0000000 0.0000000
#> [3,] 0.0000000 0.1891723 0.1620844
#> 
#> , , 3
#> 
#>           [,1]      [,2]      [,3]
#> [1,] 0.0000000 0.0000000 0.0000000
#> [2,] 0.5888976 0.0000000 0.0000000
#> [3,] 0.0000000 0.8444792 0.9650061
#> 
#> , , 4
#> 
#>           [,1]      [,2]      [,3]
#> [1,] 0.0000000 0.0000000 0.0000000
#> [2,] 0.6629824 0.0000000 0.0000000
#> [3,] 0.0000000 0.9229945 0.9969679
#> 
#> , , 5
#> 
#>       [,1]  [,2] [,3]
#> [1,] 0.000 0.000 0.00
#> [2,] 0.525 0.000 0.00
#> [3,] 0.000 0.735 0.84
#> 
#> , , 6
#> 
#>           [,1]      [,2]      [,3]
#> [1,] 0.0000000 0.0000000 0.0000000
#> [2,] 0.4101316 0.0000000 0.0000000
#> [3,] 0.0000000 0.5778957 0.8007894
#> 
#> , , 7
#> 
#>           [,1]      [,2] [,3]
#> [1,] 0.0000000 0.0000000    0
#> [2,] 0.7643113 0.0000000    0
#> [3,] 0.0000000 0.9951333    1
#> 
#> 
```
