# R6 class to represent a Latin hypercube sampler.

[`R6`](https://r6.r-lib.org/reference/R6Class.html) class that generates
Latin hypercube samples (using
[`randomLHS`](https://rdrr.io/pkg/lhs/man/randomLHS.html)) for
parameters drawn from configured distributions:
[`uniform`](https://rdrr.io/r/stats/Uniform.html),
[`Poisson`](https://rdrr.io/r/stats/Poisson.html),
[`normal`](https://rdrr.io/r/stats/Normal.html),
[`lognormal`](https://rdrr.io/r/stats/Lognormal.html),
[`beta`](https://rdrr.io/r/stats/Beta.html),
[`truncated normal`](https://rdrr.io/pkg/truncnorm/man/dtruncnorm.html)
or [`triangular`](https://rdrr.io/pkg/metRology/man/dtri.html). It
generates a data frame of sample values.

## Super class

[`poems::GenericClass`](https://globalecologylab.github.io/poems/reference/GenericClass.md)
-\> `LatinHypercubeSampler`

## Public fields

- `attached`:

  A list of dynamically attached attributes (name-value pairs).

## Active bindings

- `parameter_names`:

  A vector of sample parameter names.

- `parameter_distributions`:

  A list of sample distribution values (nested list with appropriate
  parameters).

## Methods

### Public methods

- [`LatinHypercubeSampler$new()`](#method-LatinHypercubeSampler-new)

- [`LatinHypercubeSampler$set_class_parameter()`](#method-LatinHypercubeSampler-set_class_parameter)

- [`LatinHypercubeSampler$set_uniform_parameter()`](#method-LatinHypercubeSampler-set_uniform_parameter)

- [`LatinHypercubeSampler$set_normal_parameter()`](#method-LatinHypercubeSampler-set_normal_parameter)

- [`LatinHypercubeSampler$set_poisson_parameter()`](#method-LatinHypercubeSampler-set_poisson_parameter)

- [`LatinHypercubeSampler$set_lognormal_parameter()`](#method-LatinHypercubeSampler-set_lognormal_parameter)

- [`LatinHypercubeSampler$set_beta_parameter()`](#method-LatinHypercubeSampler-set_beta_parameter)

- [`LatinHypercubeSampler$set_truncnorm_parameter()`](#method-LatinHypercubeSampler-set_truncnorm_parameter)

- [`LatinHypercubeSampler$set_triangular_parameter()`](#method-LatinHypercubeSampler-set_triangular_parameter)

- [`LatinHypercubeSampler$generate_samples()`](#method-LatinHypercubeSampler-generate_samples)

- [`LatinHypercubeSampler$clone()`](#method-LatinHypercubeSampler-clone)

Inherited methods

- [`poems::GenericClass$new_clone()`](https://globalecologylab.github.io/poems/reference/GenericClass.html#method-new_clone)

------------------------------------------------------------------------

### Method `new()`

Initialization method sets parameter names when provided.

#### Usage

    LatinHypercubeSampler$new(parameter_names = NULL, ...)

#### Arguments

- `parameter_names`:

  Optional vector of sample parameter names.

- `...`:

  Additional parameters passed individually.

------------------------------------------------------------------------

### Method `set_class_parameter()`

Sets a parameter to sampled from a vector of classes.

#### Usage

    LatinHypercubeSampler$set_class_parameter(parameter_name, classes)

#### Arguments

- `parameter_name`:

  Character string name of sample parameter.

- `classes`:

  Vector of class values.

------------------------------------------------------------------------

### Method `set_uniform_parameter()`

Sets a parameter to be sampled from a
[`uniform`](https://rdrr.io/r/stats/Uniform.html) distribution with
lower and upper bounds, optionally rounded to a specified number of
decimal places.

#### Usage

    LatinHypercubeSampler$set_uniform_parameter(
      parameter_name,
      lower = 0,
      upper = 1,
      decimals = NULL
    )

#### Arguments

- `parameter_name`:

  Character string name of sample parameter.

- `lower`:

  Lower bound of the uniform distribution (default = 0).

- `upper`:

  Upper bound of the uniform distribution (default = 1).

- `decimals`:

  Optional number of decimals applied to generated samples.

------------------------------------------------------------------------

### Method `set_normal_parameter()`

Sets a parameter to be sampled from a
[`normal`](https://rdrr.io/r/stats/Normal.html) distribution with mean
and standard deviation, optionally rounded to a specified number of
decimal places.

#### Usage

    LatinHypercubeSampler$set_normal_parameter(
      parameter_name,
      mean = 0,
      sd = 1,
      decimals = NULL
    )

#### Arguments

- `parameter_name`:

  Character string name of sample parameter.

- `mean`:

  Mean parameter for the normal distribution (default = 0).

- `sd`:

  Standard deviation parameter for the normal distribution (default =
  1).

- `decimals`:

  Optional number of decimals applied to generated samples.

------------------------------------------------------------------------

### Method `set_poisson_parameter()`

Sets a parameter to be sampled from a
[`Poisson`](https://rdrr.io/r/stats/Poisson.html) distribution with
lambda parameter. Produces integers.

#### Usage

    LatinHypercubeSampler$set_poisson_parameter(parameter_name, lambda = 1)

#### Arguments

- `parameter_name`:

  Character string name of sample parameter.

- `lambda`:

  Lambda parameter for the Poisson distribution. Must be positive
  (default = 1).

------------------------------------------------------------------------

### Method `set_lognormal_parameter()`

Sets a parameter to be sampled from a
[`lognormal`](https://rdrr.io/r/stats/Lognormal.html) distribution with
log mean and log standard deviation, optionally expressed as regular
mean and SD (overriding log mean/sd), and optionally rounded to a
specified number of decimal places.

#### Usage

    LatinHypercubeSampler$set_lognormal_parameter(
      parameter_name,
      meanlog = 0,
      sdlog = 1,
      mean = NULL,
      sd = NULL,
      decimals = NULL
    )

#### Arguments

- `parameter_name`:

  Character string name of sample parameter.

- `meanlog`:

  Log mean parameter for the lognormal distribution (default = 0).

- `sdlog`:

  Log standard deviation parameter for the lognormal distribution
  (default = 1).

- `mean`:

  Optional (overriding) regular mean parameter for the lognormal
  distribution (default = NULL).

- `sd`:

  Optional (overriding) standard deviation parameter for the lognormal
  distribution (default = NULL).

- `decimals`:

  Optional number of decimals applied to generated samples.

------------------------------------------------------------------------

### Method `set_beta_parameter()`

Sets a parameter to be sampled from a
[`beta`](https://rdrr.io/r/stats/Beta.html) distribution configured with
alpha and beta parameters, or optionally with mean and standard
deviation (overriding alpha and beta), and optionally rounded to a
specified number of decimal places.

#### Usage

    LatinHypercubeSampler$set_beta_parameter(
      parameter_name,
      alpha = 1,
      beta = 1,
      mean = NULL,
      sd = NULL,
      decimals = NULL
    )

#### Arguments

- `parameter_name`:

  Character string name of sample parameter.

- `alpha`:

  Shaping (towards 1) parameter (\> 0) for the beta distribution
  (default = 1).

- `beta`:

  Shaping (towards 0) parameter (\> 0) for the beta distribution
  (default = 1).

- `mean`:

  Optional (overriding) mean parameter for the beta distribution
  (default = NULL).

- `sd`:

  Optional (overriding) standard deviation parameter for the beta
  distribution (default = NULL).

- `decimals`:

  Optional number of decimals applied to generated samples.

------------------------------------------------------------------------

### Method `set_truncnorm_parameter()`

Sets a parameter to be sampled from a
[`truncated normal`](https://rdrr.io/pkg/truncnorm/man/dtruncnorm.html)
distribution with mean, standard deviation, and lower and upper bounds,
optionally rounded to a specified number of decimal places.

#### Usage

    LatinHypercubeSampler$set_truncnorm_parameter(
      parameter_name,
      mean = 0,
      sd = 1,
      lower = -Inf,
      upper = Inf,
      decimals = NULL
    )

#### Arguments

- `parameter_name`:

  Character string name of sample parameter.

- `mean`:

  Mean parameter of the truncated normal distribution (default = 0).

- `sd`:

  Standard deviation of the truncated normal distribution (default = 1).

- `lower`:

  Lower bound of the truncated normal distribution (default = -Inf,
  meaning no lower bound).

- `upper`:

  Upper bound of the truncated normal distribution (default = Inf,
  meaning no upper bound).

- `decimals`:

  Optional number of decimals that generated samples are rounded to.

------------------------------------------------------------------------

### Method `set_triangular_parameter()`

Sets a parameter to be sampled from a
[`triangular`](https://rdrr.io/pkg/metRology/man/dtri.html) distribution
with lower and upper bounds and mode (peak), optionally rounded to a
specified number of decimal places.

#### Usage

    LatinHypercubeSampler$set_triangular_parameter(
      parameter_name,
      lower = 0,
      upper = 1,
      mode = (lower + upper)/2,
      decimals = NULL
    )

#### Arguments

- `parameter_name`:

  Character string name of sample parameter.

- `lower`:

  Lower bound of the triangular distribution (default = 0).

- `upper`:

  Upper bound of the triangular distribution (default = 1).

- `mode`:

  Mode (or peak) of the triangular distribution (default = (lower +
  upper)/2).

- `decimals`:

  Optional number of decimals applied to generated samples.

------------------------------------------------------------------------

### Method `generate_samples()`

Generates Latin hypercube sample data (via
[`randomLHS`](https://rdrr.io/pkg/lhs/man/randomLHS.html)) for the set
parameters using corresponding distributions.

#### Usage

    LatinHypercubeSampler$generate_samples(number = 10, random_seed = NULL)

#### Arguments

- `number`:

  Number of samples to generate (default = 10).

- `random_seed`:

  Optional seed for the random generation of samples.

#### Returns

A data frame of generated sample values.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LatinHypercubeSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
lhs_gen <- LatinHypercubeSampler$new(parameter_names = c("size", "age", "km", "price"))
lhs_gen$set_class_parameter("size", c("small", "medium", "large"))
lhs_gen$set_uniform_parameter("age", lower = 18, upper = 70, decimals = 0)
lhs_gen$set_poisson_parameter("offspring", lambda = 2)
lhs_gen$set_normal_parameter("km", mean = 50000, sd = 20000, decimals = 0)
lhs_gen$set_truncnorm_parameter("kg", mean = 75, sd = 20, lower = 0, upper = Inf, decimals = 2)
lhs_gen$set_lognormal_parameter("price", mean = 30000, sd = 10000, decimals = 0)
lhs_gen$set_beta_parameter("tread", mean = 0.7, sd = 0.1, decimals = 2)
lhs_gen$set_triangular_parameter("rating",
  lower = 0, upper = 10, mode = 5,
  decimals = 1
)
lhs_gen$generate_samples(number = 10, random_seed = 123)
#>      size age    km price offspring     kg tread rating
#> 1  medium  58 81395 27974         2  40.79  0.62    4.4
#> 2   small  40 67785 35994         0  68.14  0.66    8.0
#> 3   small  50 57527 48720         2  81.81  0.63    5.5
#> 4   large  64 66229 40466         2  75.74  0.91    7.1
#> 5  medium  28 37868 28792         3 127.10  0.72    5.8
#> 6   large  35 46512 22199         1  58.06  0.47    4.5
#> 7   small  32 30269 33404         1  73.68  0.81    6.6
#> 8  medium  48 54963 24981         0  87.93  0.73    2.4
#> 9   large  66 10421 19036         3  62.94  0.78    3.5
#> 10 medium  19 40146 16062         5  93.44  0.69    1.0
```
