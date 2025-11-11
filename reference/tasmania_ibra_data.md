# Thylacine vignette Tasmania IBRA data

A dataset describing the nine Interim Bioregionalisation of Australia
(IBRA) bioregions for the Tasmanian study region of the Thylacine
example vignette.

## Format

A data frame with 9 rows and 4 variables:

- index:

  Cross-reference index for each bioregion

- key:

  Additional alphabetical cross-reference for each bioregion

- abbr:

  Abbreviated name for each bioregion

- name:

  Full name for each bioregion

## Source

https://doi.org/10.1111/2041-210X.13720

## Examples

``` r
data(tasmania_ibra_data)
data(tasmania_ibra_raster)
raster::values(tasmania_ibra_raster)[!is.na(raster::values(tasmania_ibra_raster))] |>
  table() |>
  as.data.frame() |>
  merge(tasmania_ibra_data, by.x = "Var1", by.y = "index")
#>   Var1 Freq key abbr                        name
#> 1    1   46   A  FUR                    Furneaux
#> 2    2   76   B  BEN                  Ben Lomond
#> 3    3   40   C  TNM Tasmanian Northern Midlands
#> 4    4  147   D  TSE        Tasmanian South East
#> 5    5  188   E   TW              Tasmanian West
#> 6    6   72   F  TNS   Tasmanian Northern Slopes
#> 7    7   93   G  TSR   Tasmanian Southern Ranges
#> 8    8   86   H  TCH Tasmanian Central Highlands
#> 9    9   47   I  KIN                        King
```
