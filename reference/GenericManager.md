# R6 class representing a generic manager.

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class to represent a
generic (abstract) manager for generating or processing simulation
results, as well as optionally generating values via generators.

## Super class

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\> `GenericManager`

## Public fields

- `attached`:

  A list of dynamically attached attributes (name-value pairs).

## Active bindings

- `sample_data`:

  A data frame of sampled parameters for each simulation/result.

- `generators`:

  A list of generators
  ([`Generator`](https://globalecologylab.github.io/poems/reference/Generator.md)
  or inherited class) objects for generating simulation model values.

- `parallel_cores`:

  Number of cores for running the simulations in parallel.

- `results_dir`:

  Results directory path.

- `results_ext`:

  Result file extension (default is .RData).

- `results_filename_attributes`:

  A vector of: prefix (optional); attribute names (from the sample data
  frame); postfix (optional); utilized to construct results filenames.

- `error_messages`:

  A vector of error messages encountered.

- `warning_messages`:

  A vector of warning messages encountered.

## Methods

### Public methods

- [`GenericManager$new()`](#method-GenericManager-new)

- [`GenericManager$get_attribute()`](#method-GenericManager-get_attribute)

- [`GenericManager$get_message_sample()`](#method-GenericManager-get_message_sample)

- [`GenericManager$get_results_filename()`](#method-GenericManager-get_results_filename)

- [`GenericManager$clone()`](#method-GenericManager-clone)

Inherited methods

- [`poems::GenericClass$new_clone()`](https://globalecologylab.github.io/poems/reference/GenericClass.html#method-new_clone)

------------------------------------------------------------------------

### Method `new()`

Initialization method sets any included attributes (*sample_data*,
*generators*, *parallel_cores*, *results_dir*,
*results_filename_attributes*) and attaches other attributes
individually listed.

#### Usage

    GenericManager$new(...)

#### Arguments

- `...`:

  Parameters listed individually.

------------------------------------------------------------------------

### Method `get_attribute()`

Returns a named manager or attached attribute.

#### Usage

    GenericManager$get_attribute(param)

#### Arguments

- `param`:

  Character string name of the attribute.

#### Returns

Selected attribute value.

------------------------------------------------------------------------

### Method `get_message_sample()`

Substitutes the specified sample details into a status message (using
sprintf) and returns the result.

#### Usage

    GenericManager$get_message_sample(status_message, sample_index)

#### Arguments

- `status_message`:

  Character string message with a placeholder for sample details.

- `sample_index`:

  Row index of sample data frame containing details of substitution
  parameters.

#### Returns

Status message with substituted sample details.

------------------------------------------------------------------------

### Method `get_results_filename()`

Constructs and returns the results filename based on the sample data
frame index and results filename attributes.

#### Usage

    GenericManager$get_results_filename(sample_index)

#### Arguments

- `sample_index`:

  Row index of sample data frame containing details of substitution
  parameters.

#### Returns

Results filename with substituted sample details.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GenericManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
generic_manager <- GenericManager$new(
  attr1 = 22:23,
  results_filename_attributes = c("attr1", "example")
)
generic_manager$get_results_filename(1)
#> [1] "sample_1_results"
generic_manager$get_results_filename(2)
#> [1] "sample_2_results"
```
