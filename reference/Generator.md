# R6 class representing a dynamic attribute generator

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class representing a
model that dynamically generates attribute values (*outputs*) via
reading data from files, running assigned functions, generating sample
distributions, or built-in functions (assigned as *default* in inherited
classes), using simulation sample parameters (*inputs*).

## Super classes

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\>
[`poems::GenericModel`](https://globalecologylab.github.io/poems/reference/GenericModel.md)
-\>
[`poems::SpatialModel`](https://globalecologylab.github.io/poems/reference/SpatialModel.md)
-\> `Generator`

## Public fields

- `attached`:

  A list of dynamically attached attributes (name-value pairs).

## Active bindings

- `model_attributes`:

  A vector of model attribute names.

- `region`:

  A
  [`Region`](https://globalecologylab.github.io/poems/reference/Region.md)
  (or inherited class) object specifying the study region.

- `coordinates`:

  Data frame (or matrix) of X-Y population (WGS84) coordinates in
  longitude (degrees West) and latitude (degrees North) (get and set),
  or distance-based coordinates dynamically returned by region raster
  (get only).

- `description`:

  A brief description of what the generator generates.

- `inputs`:

  An array of input attribute names for the generator.

- `outputs`:

  An array of output attribute names for the generator.

- `file_templates`:

  A nested list of file template attributes.

- `function_templates`:

  A nested list of function template attributes.

- `distribution_templates`:

  A list of distribution template attributes.

- `uses_correlations`:

  A boolean to indicate that a
  [`SpatialCorrelation`](https://globalecologylab.github.io/poems/reference/SpatialCorrelation.md)
  (or inherited class) object is used for generating correlated random
  deviates.

- `spatial_correlation`:

  A
  [`SpatialCorrelation`](https://globalecologylab.github.io/poems/reference/SpatialCorrelation.md)
  (or inherited class) object for generating correlated random deviates.

- `temporal_correlation`:

  Absolute correlation coefficient between simulation time steps for all
  grid cells (0-1; default = 1).

- `time_steps`:

  Number of simulation time steps.

- `generate_rasters`:

  Boolean to indicate if rasters should be generated (defaults to TRUE
  when region uses rasters).

- `decimals`:

  Number of decimal places applied to generated data outputs (default:
  NULL = no rounding).

- `occupancy_mask`:

  Optional binary mask array (matrix), data frame, or raster (stack) for
  generated (time-series) data outputs.

- `template_attached`:

  A list of template-nested dynamically attached model attributes that
  are maintained via shallow or *new* cloning.

- `attribute_aliases`:

  A list of alternative alias names for model attributes (form:
  `alias = "attribute"`) to be used with the set and get attributes
  methods.

- `generative_template`:

  A nested
  [`GenerativeTemplate`](https://globalecologylab.github.io/poems/reference/GenerativeTemplate.md)
  (or inherited class) object for model attributes that are maintained
  via shallow or *new* cloning.

- `generative_requirements`:

  A list of attribute names and the template setting (*"file"*,
  *"function"*, or *"default"*) that is required to generate their
  values.

- `error_messages`:

  A vector of error messages encountered when setting model attributes.

- `warning_messages`:

  A vector of warning messages encountered when setting model
  attributes.

## Methods

### Public methods

- [`Generator$new()`](#method-Generator-new)

- [`Generator$new_clone()`](#method-Generator-new_clone)

- [`Generator$get_attributes()`](#method-Generator-get_attributes)

- [`Generator$generate()`](#method-Generator-generate)

- [`Generator$add_file_template()`](#method-Generator-add_file_template)

- [`Generator$add_function_template()`](#method-Generator-add_function_template)

- [`Generator$add_distribution_template()`](#method-Generator-add_distribution_template)

- [`Generator$read_file()`](#method-Generator-read_file)

- [`Generator$run_function()`](#method-Generator-run_function)

- [`Generator$sample_distribution()`](#method-Generator-sample_distribution)

- [`Generator$add_generative_requirements()`](#method-Generator-add_generative_requirements)

- [`Generator$generative_requirements_satisfied()`](#method-Generator-generative_requirements_satisfied)

- [`Generator$clone()`](#method-Generator-clone)

Inherited methods

- [`poems::GenericModel$get_attribute()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute)
- [`poems::GenericModel$get_attribute_aliases()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_aliases)
- [`poems::GenericModel$get_attribute_names()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-get_attribute_names)
- [`poems::GenericModel$set_attributes()`](https://globalecologylab.github.io/poems/reference/GenericModel.html#method-set_attributes)

------------------------------------------------------------------------

### Method `new()`

Initialization method sets the generative template and requirements as
well as any attributes passed via a *params* list or individually.

#### Usage

    Generator$new(generative_template = NULL, generative_requirements = NULL, ...)

#### Arguments

- `generative_template`:

  A
  [`GenerativeTemplate`](https://globalecologylab.github.io/poems/reference/GenerativeTemplate.md)
  (or inherited class) object containing the file, function and/or
  distribution templates utilized (facilitates shallow cloning).

- `generative_requirements`:

  A list of attribute names and the template setting ("file",
  "function", or "distribution") that is required to generate their
  values.

- `...`:

  Parameters passed via a *params* list or individually.

------------------------------------------------------------------------

### Method `new_clone()`

Creates a new (re-initialized) object of the current (inherited) object
class with optionally passed parameters.

#### Usage

    Generator$new_clone(...)

#### Arguments

- `...`:

  Parameters passed via the inherited class constructor (defined in
  initialize and run via new).

#### Returns

New object of the current (inherited) class.

------------------------------------------------------------------------

### Method `get_attributes()`

Returns a list of existing and template-generated values for selected
attributes or attribute aliases (when array of parameter names
provided), or all existing attributes (when no params).

#### Usage

    Generator$get_attributes(params = NULL)

#### Arguments

- `params`:

  Array of attribute names to return, including those to be
  template-generated (all when NULL).

#### Returns

List of selected or all attributes values.

------------------------------------------------------------------------

### Method `generate()`

Returns a list of generated output values (attributes) corresponding to
the sample input values (attributes).

#### Usage

    Generator$generate(input_values = list())

#### Arguments

- `input_values`:

  List of sample input values for generator attributes.

#### Returns

List containing generated model output attributes and/or any
error/warning messages.

------------------------------------------------------------------------

### Method `add_file_template()`

Adds a file template for reading raster/RData(RDS)/CSV files for a given
model attribute.

#### Usage

    Generator$add_file_template(
      param,
      path_template,
      path_params = c(),
      file_type = "GRD"
    )

#### Arguments

- `param`:

  Name of model attribute to be read from a file.

- `path_template`:

  Template string for the file path with placeholders (see
  [`sprintf`](https://rdrr.io/r/base/sprintf.html)) for simulation
  sample parameters.

- `path_params`:

  Array of the names of the simulation sample parameters to be
  substituted (in order) into the path template.

- `file_type`:

  File type raster *"GRD"* (default), *"TIF"*, *"RData/RDS"*, *"QS2"*,
  or *"CSV"* to be read.

------------------------------------------------------------------------

### Method `add_function_template()`

Adds a function template for running a user-defined function to
calculate a given model attribute.

#### Usage

    Generator$add_function_template(param, function_def, call_params = c())

#### Arguments

- `param`:

  Name of model attribute to be generated using a function.

- `function_def`:

  Function definition (or path to the file containing the function) in
  form: `function(params)`, where *params* is a list passed to the
  function.

- `call_params`:

  Array of the names of the model parameters/attributes to be passed
  into the function via a list: *params*.

------------------------------------------------------------------------

### Method `add_distribution_template()`

Adds a distribution template for generating a given model attribute via
sampling a distribution.

#### Usage

    Generator$add_distribution_template(
      param,
      distr_type = c("uniform", "normal", "lognormal", "beta", "triangular"),
      distr_params = list(),
      sample = NULL,
      random_seed = NULL,
      normalize_threshold = NULL
    )

#### Arguments

- `param`:

  Name of model attribute to be generated via sampling a distribution.

- `distr_type`:

  Distribution type to sample from (uniform, normal, lognormal, beta or
  triangular).

- `distr_params`:

  List of distribution parameters and their values or associated model
  attributes (uniform: lower, upper; normal: mean, sd; lognormal:
  meanlog, sdlog (or mean, sd); beta: alpha, beta (or mean, sd);
  triangular: lower, mode, upper).

- `sample`:

  Model attribute(s) name(s) or values associated with single sample
  probabilities (0-1), or bounds as a vector (e.g.
  `sample = c("p_lower", "p_upper")`), or as a list (e.g.
  `sample = list(mid = "p", window = 0.2)` for bounds p +/- 0.1).

- `random_seed`:

  Random seed utilized when sample probability is generated internally,
  via bounds, and/or correlated deviates.

- `normalize_threshold`:

  Optional normalization threshold is utilized when generated values are
  to be normalized with a fixed upper limit/threshold.

------------------------------------------------------------------------

### Method `read_file()`

Reads and returns the value of a model attribute from a file using the
corresponding file template and simulation sample parameters.

#### Usage

    Generator$read_file(param)

#### Arguments

- `param`:

  Name of model attribute to be read from the file.

#### Returns

Model attribute value read from a file.

------------------------------------------------------------------------

### Method `run_function()`

Returns the calculated value of a model attribute using the
corresponding function template and model simulation sample parameters.

#### Usage

    Generator$run_function(param)

#### Arguments

- `param`:

  Name of model attribute to be calculated using a function.

#### Returns

Model attribute value calculated using a function.

------------------------------------------------------------------------

### Method `sample_distribution()`

Returns the calculated value of a model attribute using the
corresponding distribution template and simulation sample parameters.

#### Usage

    Generator$sample_distribution(param)

#### Arguments

- `param`:

  Name of model attribute to be calculated using a sampling
  distribution.

#### Returns

Model attribute value calculated via distribution sampling.

------------------------------------------------------------------------

### Method `add_generative_requirements()`

Adds attribute names and the template setting (*"file"*, *"function"* or
*"distribution"*) that is required to generate their values (via a
*params* list or individually).

#### Usage

    Generator$add_generative_requirements(params = list(), ...)

#### Arguments

- `params`:

  Parameters passed via a list (e.g.
  `params = list(attr1 = "file", attr2 = "function", attr3 = "distribution")`).

- `...`:

  Parameters passed individually (e.g. `attr3 = "file"`).

------------------------------------------------------------------------

### Method `generative_requirements_satisfied()`

Returns a boolean to indicate that all the file, function and/or
distribution template settings that are required for attribute
generation are present.

#### Usage

    Generator$generative_requirements_satisfied()

#### Returns

Boolean to indicate that the required settings for attribute generation
are present.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Generator$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# U Island example region
coordinates <- data.frame(
  x = rep(seq(177.01, 177.05, 0.01), 5),
  y = rep(seq(-18.01, -18.05, -0.01), each = 5)
)
coordinates <- coordinates[c(7, 9, 12, 14, 17:19), ]
region <- Region$new(coordinates = coordinates, use_raster = FALSE)
# Spatial correlation
spatial_correlation <- SpatialCorrelation$new(
  region = region, correlation_amplitude = 0.6,
  correlation_breadth = 300
)
spatial_correlation$calculate_compact_decomposition(decimals = 4)
# Example habitat suitability in file
saveRDS(
  array(c(0.5, 0.3, 0.7, 0.9, 0.6, 0.7, 0.8), c(7, 5)),
  file.path(tempdir(), "hs_mean_1.RData")
)
# Generator
capacity_gen <- Generator$new(
  description = "capacity",
  region = region,
  time_steps = 5,
  spatial_correlation = spatial_correlation,
  temporal_correlation = 0.9,
  hs_sd = 0.1, # template attached
  inputs = c("hs_file", "density_max", "initial_n"),
  outputs = c("initial_abundance", "carrying_capacity")
)
capacity_gen$add_generative_requirements(list(
  hs_mean = "file",
  hs_sample = "distribution",
  carrying_capacity = "function",
  initial_abundance = "function"
))
# File template for mean habitat suitability
capacity_gen$add_file_template("hs_mean",
  path_template = file.path(tempdir(), "hs_mean_%s.RData"),
  path_params = c("hs_file"), file_type = "RDS"
)
# Distribution template for sampling habitat suitability
capacity_gen$add_distribution_template("hs_sample",
  distr_type = "beta",
  distr_params = list(
    mean = "hs_mean",
    sd = "hs_sd"
  )
)
# Function templates for initial abundance and carrying capacity
capacity_gen$add_function_template("initial_abundance",
  function_def = function(params) {
    stats::rmultinom(1,
      size = params$initial_n,
      prob = params$hs_sample[, 1]
    )
  },
  call_params = c("initial_n", "hs_sample")
)
capacity_gen$add_function_template("carrying_capacity",
  function_def = function(params) {
    round(params$density_max * params$hs_sample)
  },
  call_params = c("density_max", "hs_sample")
)
# Generation
capacity_gen$generate(input_values = list(
  hs_file = 1,
  initial_n = 400,
  density_max = 100
))
#> $initial_abundance
#>      [,1]
#> [1,]   58
#> [2,]   12
#> [3,]   58
#> [4,]   70
#> [5,]   53
#> [6,]   62
#> [7,]   87
#> 
#> $carrying_capacity
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   62   69   64   58   64
#> [2,]   14   10   13   14   16
#> [3,]   68   71   72   69   68
#> [4,]   91   80   72   62   49
#> [5,]   57   55   59   63   62
#> [6,]   65   65   74   75   73
#> [7,]   87   88   87   88   91
#> 
```
