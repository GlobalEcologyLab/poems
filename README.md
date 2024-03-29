
<!-- README.md is generated from README.Rmd. Please edit that file -->

# poems: Pattern-oriented ensemble modeling system (for spatially explicit populations)

<!-- badges: start -->

[![Paper_doi](https://img.shields.io/badge/doi-10.1111/2041--210X.13720-orange.svg)](https://doi.org/10.1111/2041-210X.13720)
[![Codecov test
coverage](https://codecov.io/gh/GlobalEcologyLab/poems/branch/main/graph/badge.svg)](https://app.codecov.io/gh/GlobalEcologyLab/poems?branch=main)
[![Last
commit](https://img.shields.io/github/last-commit/GlobalEcologyLab/poems.svg)](https://github.com/GlobalEcologyLab/poems/commits/main)
[![R-CMD-check](https://github.com/GlobalEcologyLab/poems/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GlobalEcologyLab/poems/actions/workflows/R-CMD-check.yaml)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
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

You can install poems from [GitHub](https://github.com/) using:

``` r
# install.packages("devtools")
remotes::install_github("GlobalEcologyLab/poems")
```

## Example

The following simple example demonstrates how to run a single spatially
explicit demographic population model using *poems*:

``` r
library(poems)

# Demonstration example region (U Island) and initial abundance
coordinates <- data.frame(
  x = rep(seq(177.01, 177.05, 0.01), 5),
  y = rep(seq(-18.01, -18.05, -0.01), each = 5)
)
template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
template_raster[][-c(7, 9, 12, 14, 17:19)] <- NA # make U Island
region <- Region$new(template_raster = template_raster)
initial_abundance <- seq(0, 300, 50)
raster::plot(region$raster_from_values(initial_abundance),
  main = "Initial abundance", xlab = "Longitude (degrees)",
  ylab = "Latitude (degrees)", zlim = c(0, 300), colNA = "blue"
)
```

<img src="man/figures/README-example-1.png" width="100%" style="display: block; margin: auto;" />

``` r

# Set population model
pop_model <- PopulationModel$new(
  region = region,
  time_steps = 5,
  populations = 7,
  initial_abundance = initial_abundance,
  stage_matrix = matrix(c(
    0, 2.5, # Leslie/Lefkovitch matrix
    0.8, 0.5
  ), nrow = 2, ncol = 2, byrow = TRUE),
  carrying_capacity = rep(200, 7),
  density_dependence = "logistic",
  dispersal = (!diag(nrow = 7, ncol = 7)) * 0.05,
  result_stages = c(1, 2)
)

# Run single simulation
results <- population_simulator(pop_model)
results # examine
#> $all
#> $all$abundance
#> [1] 1041 1157 1269 1306 1421
#> 
#> $all$abundance_stages
#> $all$abundance_stages[[1]]
#> [1] 624 686 767 767 865
#> 
#> $all$abundance_stages[[2]]
#> [1] 417 471 502 539 556
#> 
#> 
#> 
#> $abundance
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   64  147  153  189  184
#> [2,]   99  135  166  173  190
#> [3,]  152  171  188  164  208
#> [4,]  170  183  202  216  225
#> [5,]  172  170  179  202  193
#> [6,]  182  169  195  172  206
#> [7,]  202  182  186  190  215
#> 
#> $abundance_stages
#> $abundance_stages[[1]]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   31   96   90  121  106
#> [2,]   66   77  113  101  118
#> [3,]   91  102  119   87  126
#> [4,]  109  109  118  124  149
#> [5,]   91  102  109  111  117
#> [6,]  110   96  117  108  122
#> [7,]  126  104  101  115  127
#> 
#> $abundance_stages[[2]]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   33   51   63   68   78
#> [2,]   33   58   53   72   72
#> [3,]   61   69   69   77   82
#> [4,]   61   74   84   92   76
#> [5,]   81   68   70   91   76
#> [6,]   72   73   78   64   84
#> [7,]   76   78   85   75   88
raster::plot(region$raster_from_values(results$abundance[, 5]),
  main = "Final abundance", xlab = "Longitude (degrees)",
  ylab = "Latitude (degrees)", zlim = c(0, 300), colNA = "blue"
)
```

<img src="man/figures/README-example-2.png" width="100%" style="display: block; margin: auto;" />

Further examples utilizing the POM workflow and more advanced features
of *poems* can be found in the accompanying vignettes.

## Citation

You may cite poems in publications using our software paper in *Methods
in Ecology and Evolution*:

Fordham, D. A., Haythorne, S., Brown, S. C., Buettel, J. C., & Brook, B.
W. (2021). poems: R package for simulating species’ range dynamics using
pattern‐oriented validation. *Methods in Ecology and Evolution*,
*12*(12), 2364-2371.

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
