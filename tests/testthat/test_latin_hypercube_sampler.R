context("Latin Hypercube Sampler")

test_that("initialization and parameter setting", {
  lhs_generator <- LatinHypercubeSampler$new(parameter_names = c("param_1", "param_2"))
  # Class, uniform and normal distributions
  lhs_generator$set_class_parameter("param_class", c("a", "b", "c", "d", "e"))
  lhs_generator$set_uniform_parameter("param_uniform", upper = 2)
  lhs_generator$set_normal_parameter("param_norm", mean = 100, sd = 10, decimals = 0)
  expect_equal(lhs_generator$parameter_names, c("param_1", "param_2", "param_class", "param_uniform", "param_norm"))
  expect_equal(lhs_generator$parameter_distributions,
               list(param_class = list(type = "class", classes = c("a", "b", "c", "d", "e")),
                    param_uniform = list(type = "uniform", lower = 0, upper = 2, decimals = NULL),
                    param_norm = list(type = "normal", mean = 100, sd = 10, decimals = 0)))
  # Lognormal distribution
  lhs_generator <- LatinHypercubeSampler$new()
  lhs_generator$set_lognormal_parameter("param_lognorm", decimals = 4)
  expect_equal(lhs_generator$parameter_distributions,
               list(param_lognorm = list(type = "lognormal", meanlog = 0, sdlog = 1, decimals = 4)))
  expect_error(lhs_generator$set_lognormal_parameter("param_lognorm", sdlog = -1),
               "Lognormal distribution standard deviation (sdlog) parameter must be non-negative", fixed = TRUE)
  expect_error(lhs_generator$set_lognormal_parameter("param_lognorm", mean = 10, sd = 0),
               "Lognormal distribution regular (overriding) mean and standard deviation parameters must be greater than zero", fixed = TRUE)
  lhs_generator$set_lognormal_parameter("param_lognorm", mean = 10, sd = 2)
  expect_equal(lhs_generator$parameter_distributions,
               list(param_lognorm = list(type = "lognormal", meanlog = log(10^2/sqrt(10^2 + 2^2)),
                                         sdlog = sqrt(log(1 + 2^2/10^2)), decimals = NULL)))
  # Beta distribution
  lhs_generator <- LatinHypercubeSampler$new()
  lhs_generator$set_beta_parameter("param_beta", alpha = 2, beta = 3, decimals = 4)
  expect_equal(lhs_generator$parameter_distributions,
               list(param_beta = list(type = "beta", alpha = 2, beta = 3, decimals = 4)))
  expect_error(lhs_generator$set_beta_parameter("param_beta", alpha = 0),
               "Beta distribution alpha and beta shaping parameters must be greater than zero")
  expect_error(lhs_generator$set_beta_parameter("param_beta", mean = 0, sd = 1),
               "Beta distribution (overriding) mean and standard deviation parameters must be between zero and one (exclusively)", fixed = TRUE)
  lhs_generator$set_beta_parameter("param_beta", mean = 0.5, sd = 0.5)
  expect_equal(lhs_generator$parameter_distributions,
               list(param_beta = list(type = "beta", alpha = 0.5*(0.5*(1 - 0.5)/(0.5*0.999)^2 - 1),
                                      beta = (1 - 0.5)*(0.5*(1 - 0.5)/(0.5*0.999)^2 - 1), decimals = NULL)))
  # Poission distribution
  lhs_generator <- LatinHypercubeSampler$new()
  expect_error(lhs_generator$set_poisson_parameter("param_pois", lambda = -1),
               "Poisson distribution lambda parameter must be non-negative")
  lhs_generator$set_poisson_parameter("param_pois", lambda = 2)
  expect_equal(lhs_generator$parameter_distributions,
               list(param_pois = list(type = "poisson", lambda = 2)))

  # Triangular distribution
  lhs_generator <- LatinHypercubeSampler$new()
  expect_error(lhs_generator$set_triangular_parameter("param_triang", lower = 2, upper = 0),
               "Triangular distribution parameters must comply with: lower <= mode <= upper")
  lhs_generator$set_triangular_parameter("param_triang", lower = 2, upper = 6, decimals = 2)
  expect_equal(lhs_generator$parameter_distributions,
               list(param_triang = list(type = "triangular", lower = 2, upper = 6, mode = (6 + 2)/2, decimals = 2)))
})

test_that("Latin hypercube sample generation", {
  lhs_generator <- LatinHypercubeSampler$new()
  # Incomplete errors
  expect_error(lhs_generator$generate_samples(), "Parameter distributions need to be set before generating samples.")
  lhs_generator$parameter_names <- c("param_class", "param_uniform")
  expect_error(lhs_generator$generate_samples(),
               "Parameter distributions need to be set before generating samples: param_class, param_uniform")
  # Set distributions and generate samples
  lhs_generator$set_class_parameter("param_class", c("a", "b", "c", "d", "e"))
  lhs_generator$set_uniform_parameter("param_uniform", upper = 2)
  lhs_generator$set_normal_parameter("param_norm", mean = 100, sd = 10, decimals = 0)
  lhs_generator$set_lognormal_parameter("param_lognorm", decimals = 4)
  lhs_generator$set_beta_parameter("param_beta", alpha = 2, beta = 3, decimals = 4)
  lhs_generator$set_triangular_parameter("param_triang", lower = 2, upper = 6, decimals = 2)
  sample_data <- lhs_generator$generate_samples(random_seed = 1234)
  expect_is(sample_data, "data.frame")
  expect_named(sample_data, c("param_class", "param_uniform", "param_norm", "param_lognorm", "param_beta", "param_triang"))
  expect_equal(nrow(sample_data), 10)
  expect_true(all(sample_data$param_class %in% c("a", "b", "c", "d", "e")))
  expect_true(all(sample_data$param_uniform >= 0 & sample_data$param_uniform <= 2))
  expect_true(all(sample_data$param_norm >= (100 - 4*10) & sample_data$param_norm <= (100 + 4*10)))
  expect_true(all(sample_data$param_lognorm > 0))
  expect_true(all(sample_data$param_beta > 0 & sample_data$param_beta < 1))
  expect_true(all(sample_data$param_triang >= 2 & sample_data$param_triang <= 6))
  # Repeat with same seed and no rounding on triangular
  lhs_generator$parameter_distributions$param_triang$decimals <- NULL
  sample_data_repeat <- lhs_generator$generate_samples(random_seed = 1234)
  expect_equal(sample_data_repeat[, 1:5], sample_data[, 1:5])
  expect_equal(round(sample_data_repeat$param_triang, 2), sample_data$param_triang)
})
