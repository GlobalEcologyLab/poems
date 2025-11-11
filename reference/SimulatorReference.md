# R6 class for a simulator reference

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class for
dynamically attaching simulator attributes and results (passed by
reference).

## Public fields

- `attached`:

  A list of dynamically attached simulator attributes (name-value
  pairs).

- `results`:

  A list of dynamically accessed simulator results (name-value pairs).

## Methods

### Public methods

- [`SimulatorReference$clone()`](#method-SimulatorReference-clone)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SimulatorReference$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
test_class <- SimulatorReference$new()
test_class$attached$attr1 <- "example1"
test_class$results$attr1 <- "example2"
str(test_class)
#> Classes 'SimulatorReference', 'R6' <SimulatorReference>
#>   Public:
#>     attached: list
#>     clone: function (deep = FALSE) 
#>     results: list 
```
