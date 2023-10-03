
<!-- README.md is generated from README.Rmd. Please edit that file -->

# poems: Pattern-oriented ensemble modeling system (for spatially explicit populations)

<!-- badges: start -->

[![CRAN_status](https://www.r-pkg.org/badges/version/poems)](https://CRAN.R-project.org/package=poems)
[![Download_count](https://cranlogs.r-pkg.org/badges/grand-total/poems)](https://CRAN.R-project.org/package=poems)
[![Paper_doi](https://img.shields.io/badge/doi-10.1111/2041--210X.13720-orange.svg)](https://doi.org/10.1111/2041-210X.13720)
[![Codecov test
coverage](https://codecov.io/gh/GlobalEcologyLab/poems/branch/main/graph/badge.svg)](https://app.codecov.io/gh/GlobalEcologyLab/poems?branch=main)
[![R-CMD-check](https://github.com/GlobalEcologyLab/poems/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GlobalEcologyLab/poems/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The poems package provides a framework of interoperable *R6* (Chang,
2020) classes for building ensembles of viable models via the
pattern-oriented modeling (POM) approach (Grimm et al., 2005). The
package includes classes for encapsulating and generating model
parameters, and managing the POM workflow. The workflow includes:

1.  Model setup including generated spatial layers and demographic
    population model parameters.
2.  Generating model parameters via Latin hypercube sampling (Iman &
    Conover, 1980).
3.  Running multiple sampled model simulations.
4.  Collating summary results metrics via user-defined functions.
5.  Validating and selecting an ensemble of models that best match known
    patterns.

By default, model validation and selection utilizes an approximate
Bayesian computation (ABC) approach (Beaumont et al., 2002) using the
*abc* package (Csillery et al., 2015). However, alternative user-defined
functionality could be employed.

The package includes a spatially explicit demographic population model
simulation engine, which incorporates default functionality for density
dependence, correlated environmental stochasticity, stage-based
transitions, and distance-based dispersal. The user may customize the
simulator by defining functionality for trans-locations, harvesting,
mortality, and other processes, as well as defining the sequence order
for the simulator processes. The framework could also be adapted for use
with other model simulators by utilizing its extendable (inheritable)
base classes.

## Installation

You can install the released version of poems from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("poems")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("GlobalEcologyLab/poems")
```

## Example

The following simple example demonstrates how to run a single spatially
explicit demographic population model using *poems*:

``` r
library(poems)

# Demonstration example region (U Island) and initial abundance
coordinates <- data.frame(x = rep(seq(177.01, 177.05, 0.01), 5),
                          y = rep(seq(-18.01, -18.05, -0.01), each = 5))
template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
template_raster[][-c(7, 9, 12, 14, 17:19)] <- NA # make U Island
region <- Region$new(template_raster = template_raster)
initial_abundance <- seq(0, 300, 50)
raster::plot(region$raster_from_values(initial_abundance), 
             main = "Initial abundance", xlab = "Longitude (degrees)", 
             ylab = "Latitude (degrees)", zlim = c(0, 300), colNA = "blue")
```

<img src="man/figures/README-example-1.png" width="100%" style="display: block; margin: auto;" />

``` r

# Set population model
pop_model <- PopulationModel$new(
  region = region,
  time_steps = 5,
  populations = 7,
  initial_abundance = initial_abundance,
  stage_matrix = matrix(c(0, 2.5, # Leslie/Lefkovitch matrix
                          0.8, 0.5), nrow = 2, ncol = 2, byrow = TRUE),
  carrying_capacity = rep(200, 7),
  density_dependence = "logistic",
  dispersal = (!diag(nrow = 7, ncol = 7))*0.05,
  result_stages = c(1, 2))

# Run single simulation
results <- population_simulator(pop_model)
results # examine
#> $all
#> $all$abundance
#> [1] 1010 1181 1236 1359 1335
#> 
#> $all$abundance_stages
#> $all$abundance_stages[[1]]
#> [1] 589 743 699 858 780
#> 
#> $all$abundance_stages[[2]]
#> [1] 421 438 537 501 555
#> 
#> 
#> 
#> $abundance
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   54  101  155  192  188
#> [2,]  106  158  175  209  171
#> [3,]  127  127  157  173  197
#> [4,]  172  202  185  212  210
#> [5,]  190  222  200  177  182
#> [6,]  171  177  186  205  185
#> [7,]  190  194  178  191  202
#> 
#> $abundance_stages
#> $abundance_stages[[1]]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   34   55   87  128  114
#> [2,]   59  100   91  137  103
#> [3,]   82   74   93  105  119
#> [4,]   83  146   85  140  118
#> [5,]  113  147  116  106  116
#> [6,]  107   99  110  124   99
#> [7,]  111  122  117  118  111
#> 
#> $abundance_stages[[2]]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   20   46   68   64   74
#> [2,]   47   58   84   72   68
#> [3,]   45   53   64   68   78
#> [4,]   89   56  100   72   92
#> [5,]   77   75   84   71   66
#> [6,]   64   78   76   81   86
#> [7,]   79   72   61   73   91
raster::plot(region$raster_from_values(results$abundance[,5]),
             main = "Final abundance", xlab = "Longitude (degrees)", 
             ylab = "Latitude (degrees)", zlim = c(0, 300), colNA = "blue")
```

<img src="man/figures/README-example-2.png" width="100%" style="display: block; margin: auto;" />

Further examples utilizing the POM workflow and more advanced features
of *poems* can be found in the accompanying vignettes.

## References

Beaumont, M. A., Zhang, W., & Balding, D. J. (2002). ‘Approximate
Bayesian computation in population genetics’. *Genetics*, vol. 162, no.
4, pp, 2025–2035. <doi:10.1093/genetics/162.4.2025>

Chang, W. (2020). ‘R6: Encapsulated Classes with Reference Semantics’.
*R package version 2.5.0*. Retrieved from
<https://CRAN.R-project.org/package=R6>

Csillery, K., Lemaire L., Francois O., & Blum M. (2015). ‘abc: Tools for
Approximate Bayesian Computation (ABC)’. *R package version 2.1*.
Retrieved from <https://CRAN.R-project.org/package=abc>

Grimm, V., Revilla, E., Berger, U., Jeltsch, F., Mooij, W. M.,
Railsback, S. F., Thulke, H. H., Weiner, J., Wiegand, T., DeAngelis, D.
L., (2005). ‘Pattern-Oriented Modeling of Agent-Based Complex Systems:
Lessons from Ecology’. *Science* vol. 310, no. 5750, pp. 987–991.
<doi:10.1126/science.1116681>

Iman R. L., Conover W. J. (1980). ‘Small sample sensitivity analysis
techniques for computer models, with an application to risk assessment’.
*Commun Stat Theor Methods* A9, pp. 1749–1842.
<doi:10.1080/03610928008827996>
