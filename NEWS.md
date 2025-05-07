# version 1.3.3
## Bug fixes
- Fixed a bug that caused the `population_density` function to fail when population density was logistic and there were no growth rates (that is, a stagnant population).
- Fixed a bug in `population_transitions` in which excess survivals were "overdrawn" from populations that were already at zero.

# version 1.3.2

## Bug fixes
One of the tests for the ResultsManager object was failing on Fedora and Debian R-devel. This test has now been fixed.

# version 1.3.1

## Bug fixes
- `poems` no longer depends on `geosphere`. I am hopeful that this will clear up the test failures on CRAN's m1mac.

# version 1.3.0

Every update is an enhancement this time around:

- Changed two of the vignettes to HTML vignettes.
- These vignettes should now run much faster on CRAN servers.
- Added a CONTRIBUTING.md file.
- Added examples for all functions, classes, and data objects in the package.
- Added a logo to the README.
- `poems` is now documented compliant with `srr` standards G1.0 through G1.6.

# version 1.2.0

## Bug fixes
- The CRS of the tasmania_modifier raster dataset in `/data` has been changed from NA to the appropriate equal area projection.
- The vignettes now handle the objects in `/data` appropriately.

## Enhancements
- Test coverage has been increased for `LatinHypercubeSampler.R`, 
`SimulationManager.R`, and `PopulationModel.R`.
- A CITATION file has been added.
- A ![https://globalecologylab.github.io/poems/](pkgdown documentation site) has been added.
- Code style has been improved across all R files and documentation.
- The dataset documentation has been updated to keep up with the latest roxygen2 style.
- Imports from the `gdistance` package are now more explicit.


# version 1.1.1

- Adjusted some tests so they will work correctly on certain Apple Silicon systems

# version 1.1.0

- Added density dependent dispersal, via density dependence in the source population and the target population
- Added Poisson distributions as a possible parameter distribution to the Latin hypercube sampler

# version 1.0.6

- Patched some errors and notes that came up in the tests for the generator object and the spatial correlation object

# version 1.0.5

- added vignette for translocation/introduction of new populations

# version 1.0.2

## Bug fixes

- updated raster CRS strings in tests to avoid anticipated PROJ 8 errors
  [#1](https://github.com/GlobalEcologyLab/poems/issues/1)

## Enhancements

- added method references in description

# version 1.0.1

- Initial public release
