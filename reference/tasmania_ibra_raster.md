# Thylacine vignette Tasmania IBRA raster

A *raster* dataset defining the grid cells of the nine Interim
Bioregionalisation of Australia (IBRA) bioregions for the Tasmanian
study region of the Thylacine example vignette.

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

  IBRA bioregions defined by cells with values 1 to 9 (as per `index` in
  [`tasmania_ibra_data`](https://globalecologylab.github.io/poems/reference/tasmania_ibra_data.md))

## Source

https://doi.org/10.1111/2041-210X.13720

## Examples

``` r
data(tasmania_ibra_raster)
data(tasmania_raster)
tasmania_region <- Region$new(
  template_raster = tasmania_raster
)
tasmania_region$raster_is_consistent(tasmania_ibra_raster)
#> [1] TRUE
raster::plot(tasmania_ibra_raster)

```
