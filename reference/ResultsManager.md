# R6 class representing a results manager.

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class to represent a
manager for generating summary metrics and/or matrices from simulation
results, as well as optionally regenerating values via generators.

## Super classes

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\>
[`poems::GenericManager`](https://globalecologylab.github.io/poems/reference/GenericManager.md)
-\> `ResultsManager`

## Public fields

- `attached`:

  A list of dynamically attached attributes (name-value pairs).

## Active bindings

- `sample_data`:

  A data frame of sampled parameters for each simulation/result.

- `simulation_results`:

  An object of a class inherited from the
  [`SimulationResults`](https://globalecologylab.github.io/poems/reference/SimulationResults.md)
  class for encapsulating and dynamically generating simulation results.

- `generators`:

  A list of generators
  ([`Generator`](https://globalecologylab.github.io/poems/reference/Generator.md)
  or inherited class) objects for (optionally) regenerating simulation
  model values.

- `result_attachment_functions`:

  A list of functions for attaching intermediate values to the
  simulation results prior to generation.

- `summary_metrics`:

  An array of names for summary metrics, each of which are calculated as
  single values for each simulation. These should refer to list names
  for the summary functions.

- `summary_matrices`:

  An array of names for summary matrices, each of which are calculated
  as a single matrix row for each simulation. These should refer to list
  names for the summary functions.

- `summary_functions`:

  A list of functions, result attributes, or constants for transforming
  individual simulation results to single summary metric values stored
  in the metric data frame, or to matrix rows stored in the summary
  matrix list.

- `summary_metric_data`:

  A data frame of generated summary metrics (one row per simulation).

- `summary_matrix_list`:

  A list of generated matrices of summary results (each having one row
  per simulation).

- `summary_matrix_weighted_averages`:

  A list of calculated weighted averages for each of the summary
  matrices (using the sample data *weight* column).

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

  A vector of error messages encountered when setting model attributes.

- `warning_messages`:

  A vector of warning messages encountered when setting model
  attributes.

## Methods

### Public methods

- [`ResultsManager$new()`](#method-ResultsManager-new)

- [`ResultsManager$generate()`](#method-ResultsManager-generate)

- [`ResultsManager$calculate_result_attachments()`](#method-ResultsManager-calculate_result_attachments)

- [`ResultsManager$calculate_summaries()`](#method-ResultsManager-calculate_summaries)

- [`ResultsManager$log_generation()`](#method-ResultsManager-log_generation)

- [`ResultsManager$calculate_summary_weighted_averages()`](#method-ResultsManager-calculate_summary_weighted_averages)

- [`ResultsManager$clone()`](#method-ResultsManager-clone)

Inherited methods

- [`poems::GenericClass$new_clone()`](https://globalecologylab.github.io/poems/reference/GenericClass.html#method-new_clone)
- [`poems::GenericManager$get_attribute()`](https://globalecologylab.github.io/poems/reference/GenericManager.html#method-get_attribute)
- [`poems::GenericManager$get_message_sample()`](https://globalecologylab.github.io/poems/reference/GenericManager.html#method-get_message_sample)
- [`poems::GenericManager$get_results_filename()`](https://globalecologylab.github.io/poems/reference/GenericManager.html#method-get_results_filename)

------------------------------------------------------------------------

### Method `new()`

Initialization method optionally copies attributes from a simulation
(results) manager, sets any included attributes (*sample_data*,
*simulation_results*, *generators*, *result_attachment_functions*,
*summary_metrics*, *summary_functions*, *parallel_cores*, *results_dir*,
*results_ext*, *results_filename_attributes*), and attaches other
attributes individually listed.

#### Usage

    ResultsManager$new(simulation_manager = NULL, ...)

#### Arguments

- `simulation_manager`:

  Optional
  [`SimulationManager`](https://globalecologylab.github.io/poems/reference/SimulationManager.md)
  object (or an object inherited from the
  [`GenericManager`](https://globalecologylab.github.io/poems/reference/GenericManager.md)
  class), from which simulation attributes can be copied.

- `...`:

  Parameters listed individually.

------------------------------------------------------------------------

### Method `generate()`

Generates the summary metric data and/or matrix list via the summary
functions for each simulation sample, and creates/writes a generation
log.

#### Usage

    ResultsManager$generate(results_dir = NULL)

#### Arguments

- `results_dir`:

  Results directory path (must be present if not already set within
  manager class object).

#### Returns

Generation log as a list.

------------------------------------------------------------------------

### Method `calculate_result_attachments()`

Calculates and attaches intermediate values to the sample result model
(via the result attachment functions).

#### Usage

    ResultsManager$calculate_result_attachments(simulation_results)

#### Arguments

- `simulation_results`:

  The sample simulation results, an object of a class inherited from
  [`SimulationResults`](https://globalecologylab.github.io/poems/reference/SimulationResults.md),
  to which the intermediate results are attached.

------------------------------------------------------------------------

### Method `calculate_summaries()`

Calculates the summary metrics and/or matrices for the results of a
sample simulation (via the summary functions).

#### Usage

    ResultsManager$calculate_summaries(simulation_results, sample_index)

#### Arguments

- `simulation_results`:

  The sample simulation results, an object of a class inherited from
  [`SimulationResults`](https://globalecologylab.github.io/poems/reference/SimulationResults.md).

- `sample_index`:

  Index of sample from data frame.

#### Returns

Generation log entry as a (nested) list, including generated summary
metric data and (optionally) matrices.

------------------------------------------------------------------------

### Method `log_generation()`

Summarizes the log generated within the generate method and writes it to
a text file in the results directory.

#### Usage

    ResultsManager$log_generation(generation_log)

#### Arguments

- `generation_log`:

  Nested list of log entries generated via the generate method.

#### Returns

Extended generation log as a nested with added summary and
failure/warning indices.

------------------------------------------------------------------------

### Method `calculate_summary_weighted_averages()`

Calculates the weighted averages for each of the summary matrices
(providing the sample data has a *weight* column).

#### Usage

    ResultsManager$calculate_summary_weighted_averages(na_replacements = NULL)

#### Arguments

- `na_replacements`:

  List of values or functions (form:
  `modified_matrix <- function(matrix)`) for dealing with NA values in
  each summary matrix (default NULL will ignore NAs).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResultsManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # interactive()
# U Island example region
coordinates <- data.frame(
  x = rep(seq(177.01, 177.05, 0.01), 5),
  y = rep(seq(-18.01, -18.05, -0.01), each = 5)
)
template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
template_raster[][-c(7, 9, 12, 14, 17:19)] <- NA # make U Island
region <- Region$new(template_raster = template_raster)
raster::plot(region$region_raster,
  main = "Example region (indices)",
  xlab = "Longitude (degrees)", ylab = "Latitude (degrees)",
  colNA = "blue"
)
# Results manager
results_manager <- ResultsManager$new(
  sample_data = data.frame(index = 1:3),
  simulation_results = PopulationResults$new(region = region),
  summary_metrics = c("trend_n", "total_h"),
  summary_matrices = c("n", "h"),
  summary_functions = list(
    trend_n = function(results) {
      round(results$all$abundance_trend, 2)
    },
    total_h = function(results) {
      sum(results$harvested)
    },
    n = "all$abundance", # string
    h = "all$harvested"
  ),
  parallel_cores = 2,
  results_dir = tempdir()
)
# Write example result files
results <- list()
for (i in 1:3) {
  results[[i]] <- list(abundance = t(apply(
    matrix(11:17), 1,
    function(n) round(n * exp(-(0:9) / i))
  )))
  results[[i]]$harvested <- round(results[[i]]$abundance * i / 7)
  file_name <- paste0(results_manager$get_results_filename(i), ".RData")
  saveRDS(results[[i]], file.path(tempdir(), file_name))
}
# Generate result metrics and matrices
gen_output <- results_manager$generate()
gen_output$summary
dir(tempdir(), "*.txt") # plus generation log
results_manager$summary_metric_data
results_manager$summary_matrix_list
}
```
