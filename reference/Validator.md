# R6 class representing a pattern-oriented validator.

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class for
pattern-oriented validation and simulation model ensemble selection.
Pattern-oriented validation is a statistical approach to compare
patterns generated in simulations against observed empirical patterns.

The class wraps functionality for the validation approach, typically
utilizing an external library, the default being the approximate
Bayesian computation (ABC) [`abc`](https://rdrr.io/pkg/abc/man/abc.html)
library, and includes methods for resolving non-finite metrics,
centering and scaling the validator inputs, running the validator
analysis, and generating diagnostics (see
[`abc`](https://rdrr.io/pkg/abc/man/abc.html)).

## Super classes

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\>
[`poems::GenericModel`](https://globalecologylab.github.io/poems/reference/GenericModel.md)
-\> `Validator`

## Public fields

- `attached`:

  A list of dynamically attached attributes (name-value pairs).

## Active bindings

- `model_attributes`:

  A vector of model attribute names.

- `simulation_parameters`:

  A data frame of sample model parameters for each simulation.

- `simulation_summary_metrics`:

  A data frame of result summary metrics for each simulation.

- `observed_metric_targets`:

  A vector of observed targets for each summary metric.

- `random_seed`:

  A seed for randomizing the order of the simulation samples (no
  randomization is utilized when left NULL).

- `random_indices`:

  Randomized simulation sample indices for the validator inputs and
  consequently the validator results when random seed is used.

- `non_finite_replacements`:

  A list of numeric values or function names (character strings) or
  direct assignments (assigned or loaded via source paths) for replacing
  NAs in specified (list names) summary metrics.

- `input_center_scale_values`:

  A nested list of center and scale values for validator input
  parameters/metrics.

- `output_dir`:

  Directory path for validator (default:
  [`abc`](https://rdrr.io/pkg/abc/man/abc.html)) regression diagnostic
  and other outputs.

- `validation_call_function`:

  Dynamically assigned function:
  `function(observed_metric_targets, simulation_parameters, simulation_summary_metrics, tolerance, method, ...)`
  for calling the validation function (default calls
  [`abc`](https://rdrr.io/pkg/abc/man/abc.html) library function).

- `validator_return_object`:

  Object returned by the validator function (see
  [`abc`](https://rdrr.io/pkg/abc/man/abc.html) documentation if using
  default).

- `selected_simulations`:

  A data frame of simulation sample indices and weights
  selected/assigned by the validation function
  ([`abc`](https://rdrr.io/pkg/abc/man/abc.html) by default).

- `attribute_aliases`:

  A list of alternative alias names for model attributes (form:
  `alias = "attribute"`) to be used with the set and get attributes
  methods.

- `error_messages`:

  A vector of error messages encountered when setting model attributes.

- `warning_messages`:

  A vector of warning messages encountered when setting model
  attributes.

## Methods

### Public methods

- [`Validator$new()`](#method-Validator-new)

- [`Validator$run()`](#method-Validator-run)

- [`Validator$resolve_nonfinite_metrics()`](#method-Validator-resolve_nonfinite_metrics)

- [`Validator$center_scale_inputs()`](#method-Validator-center_scale_inputs)

- [`Validator$generate_diagnostics()`](#method-Validator-generate_diagnostics)

- [`Validator$clone()`](#method-Validator-clone)

Inherited methods

- [`poems::GenericModel$get_attribute()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute)
- [`poems::GenericModel$get_attribute_aliases()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_aliases)
- [`poems::GenericModel$get_attribute_names()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_names)
- [`poems::GenericModel$get_attributes()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attributes)
- [`poems::GenericModel$new_clone()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-new_clone)
- [`poems::GenericModel$set_attributes()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-set_attributes)

------------------------------------------------------------------------

### Method `new()`

Initialization method sets given attributes individually and/or from a
list.

#### Usage

    Validator$new(...)

#### Arguments

- `...`:

  Parameters passed via a *params* list or individually.

- `template`:

  Template population model containing fixed (non-sampled) attributes.

------------------------------------------------------------------------

### Method `run()`

Pre-processes inputs, runs validator function for input parameters, and
stores the function (and optionally diagnostic) outputs (see
[`abc`](https://rdrr.io/pkg/abc/man/abc.html) documentation if using the
default).

#### Usage

    Validator$run(
      simulation_parameters = NULL,
      simulation_summary_metrics = NULL,
      observed_metric_targets = NULL,
      tolerance = 0.01,
      method = "neuralnet",
      output_diagnostics = FALSE,
      ...
    )

#### Arguments

- `simulation_parameters`:

  A data frame of sample model parameters for each simulation.

- `simulation_summary_metrics`:

  A data frame of result summary metrics for each simulation.

- `observed_metric_targets`:

  A vector of observed targets for each summary metric.

- `tolerance`:

  Tolerance or proportion of models to select.

- `method`:

  Validator algorithm to be applied (default is a neural network
  algorithm - see [`abc`](https://rdrr.io/pkg/abc/man/abc.html)
  documentation) .

- `output_diagnostics`:

  Boolean to indicate whether or not to output diagnostics (PDF file -
  default is FALSE).

- `...`:

  Additional validator parameters passed individually (see
  [`abc`](https://rdrr.io/pkg/abc/man/abc.html) documentation if using
  default).

------------------------------------------------------------------------

### Method `resolve_nonfinite_metrics()`

Attempts to resolve any non-finite simulation summary metric values (and
optionally changing them to NAs) via the non finite replacements
parameter (a list of values/functions for replacing non-finite values).

#### Usage

    Validator$resolve_nonfinite_metrics(use_nas = TRUE)

#### Arguments

- `use_nas`:

  Boolean to indicate whether or not to replace all non-finite values
  with NAs (default is TRUE).

------------------------------------------------------------------------

### Method `center_scale_inputs()`

Centers and scales the model parameters, result summary metrics and
observed targets.

#### Usage

    Validator$center_scale_inputs()

------------------------------------------------------------------------

### Method `generate_diagnostics()`

Generates the validation diagnostics (see
[`abc`](https://rdrr.io/pkg/abc/man/abc.html) documentation if using
default) as a PDF file in the output directory.

#### Usage

    Validator$generate_diagnostics(output_dir = NULL)

#### Arguments

- `output_dir`:

  Output directory path for the diagnostics PDF file (must be present if
  not already set within validator class object).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Validator$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Example parameter sample data
sample_data <- data.frame(
  growth_rate_max = round(log(seq(1.11, 1.30, 0.01)), 3),
  harvest_rate = seq(0.11, 0.30, 0.01),
  initial_n = seq(105, 200, 5),
  density_max = seq(132, 170, 2)
)
# Example simulation result summary metrics
summary_metric_data <- data.frame(
  trend_n = seq(10, -9, -1),
  total_h = seq(70, 355, 15)
)
# Create a validator for selecting the 'best' example models
validator <- Validator$new(
  simulation_parameters = sample_data,
  simulation_summary_metrics = summary_metric_data,
  observed_metric_targets = c(trend_n = 0, total_h = 250),
  output_dir = tempdir()
)
suppressWarnings(validator$run(tolerance = 0.25, output_diagnostics = TRUE))
#> 12345678910
#> 12345678910
dir(tempdir(), "*.pdf") # plus validation diagnostics (see abc library documentation)
#> [1] "validation_diagnostics.pdf"
validator$selected_simulations # top 5 models
#>   index       weight
#> 1    10 2.220446e-16
#> 2    11 6.000000e-01
#> 3    12 8.000000e-01
#> 4    13 6.000000e-01
#> 5    14 0.000000e+00
```
