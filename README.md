
<!-- README.md is generated from README.Rmd. Please edit that file -->

# poems: Pattern-oriented ensemble modelling system for spatially explicit population simulations

<!-- badges: start -->

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

The package includes a customizable spatially explicit demographic
population model simulation engine, which incorporates default
functionality for density dependence, correlated environmental
stochasiticity, stage-based transitions, and distance-based dispersal.
The user may also define functionality for translocations, harvesting,
mortality, and other processes, as well as define the sequence order for
the simulator processes. The framework could also be adapted for use
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
#> [1] 1071 1064 1218 1272 1277
#> 
#> $all$abundance_stages
#> $all$abundance_stages[[1]]
#> [1] 666 620 743 781 764
#> 
#> $all$abundance_stages[[2]]
#> [1] 405 444 475 491 513
#> 
#> 
#> 
#> $abundance
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   48  101  140  176  196
#> [2,]   96  136  152  192  177
#> [3,]  152  156  198  177  186
#> [4,]  183  165  188  186  196
#> [5,]  165  171  171  202  181
#> [6,]  201  176  191  163  166
#> [7,]  226  159  178  176  175
#> 
#> $abundance_stages
#> $abundance_stages[[1]]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   27   64   81  104  114
#> [2,]   49   83   92  124  109
#> [3,]  102   86  124  101  116
#> [4,]  117   92  118  117  114
#> [5,]   92  102  102  122  115
#> [6,]  132   98  123  103   92
#> [7,]  147   95  103  110  104
#> 
#> $abundance_stages[[2]]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   21   37   59   72   82
#> [2,]   47   53   60   68   68
#> [3,]   50   70   74   76   70
#> [4,]   66   73   70   69   82
#> [5,]   73   69   69   80   66
#> [6,]   69   78   68   60   74
#> [7,]   79   64   75   66   71
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
4, pp, 2025–2035.

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

Iman R. L., Conover W. J. (1980). ‘Small sample sensitivity analysis
techniques for computer models, with an application to risk assessment’.
*Commun Stat Theor Methods* A9, pp. 1749–1842.
