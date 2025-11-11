# Thylacine vignette Tasmania raster

A *raster* dataset defining the grid cells of the Tasmanian study region
for the Thylacine example vignette.

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

  region defined by 795 cells with value of 1 (surrounded by non-region
  `NA` values)

## Source

https://doi.org/10.1111/2041-210X.13720

## Examples

``` r
data(tasmania_raster)
tasmania_region <- Region$new(
  template_raster = tasmania_raster
)
raster::plot(tasmania_region$region_raster)

```
