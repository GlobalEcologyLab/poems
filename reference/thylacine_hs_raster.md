# Thylacine vignette habitat suitability raster

A *raster* dataset defining estimated habitat suitability values for
each grid cells of the Tasmanian study region of the Thylacine example
vignette.

## Format

A *raster::RasterLayer* object:

- dimensions:

  32 rows by 40 columns grid

- resolution:

  0.1 by 0.1 degree grid cells

- extent:

  longitude 144.5 to 148.5 degrees; latitude -43.8025 to -40.6025
  degrees

- CRS:

  WGS84 longitude-latitude

- values:

  Estimated habitat suitability values of 0 to 1

## Source

https://doi.org/10.1111/2041-210X.13720

## Examples

``` r
data(thylacine_hs_raster)
raster::plot(thylacine_hs_raster, colNA = "blue")

```
