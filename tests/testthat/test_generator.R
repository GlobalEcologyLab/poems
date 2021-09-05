context("Generator")

test_that("initialization and parameter setting", {
  coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))
  generator <- Generator$new(description = "Test generator",
                             region = Region$new(coordinates = coordinates),
                             decimals = 4,
                             attr0 = 0, attr1 = 1)
  expect_equal(generator$attribute_aliases,
               list(attr0 = "template_attached$attr0", attr1 = "template_attached$attr1"))
  expect_equal(generator$template_attached, list(attr0 = 0, attr1 = 1))
  expect_true(generator$generate_rasters)
  expect_error(generator$occupancy_mask <- array(c(1, 1, 0, 0, 1, 1, 1)),
               "Occupancy mask must be a raster layer, stack or brick consistent with the defined region")
  generator$region$use_raster <- FALSE
  expect_false(generator$generate_rasters)
  expect_silent(generator$occupancy_mask <- array(c(1, 1, 0, 0, 1, 1, 1)))
  expect_equal(generator$get_attributes(c("description", "coordinates", "decimals", "occupancy_mask")),
               list(description = "Test generator", coordinates = coordinates, decimals = 4,
                    occupancy_mask = array(c(1, 1, 0, 0, 1, 1, 1))))
  generator$set_attributes(attr1 = 11, attr2 = 12)
  expect_equal(generator$get_attributes(c("attr0", "attr1", "attr2")), list(attr0 = 0, attr1 = 11, attr2 = 12))
  expect_equal(generator$template_attached, list(attr0 = 0, attr1 = 11))
  expect_equal(generator$attached, list(attr2 = 12))
})

test_that("file template reading data frame", {
  TEST_DIRECTORY <- test_path("test_inputs")
  coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))
  occupancy_mask <- array(c(1, 1, 0, 0, 1, 1, 1))
  generator <- Generator$new(description = "Test generator", decimals = 4,
                             region = Region$new(coordinates = coordinates, use_raster = FALSE),
                             occupancy_mask = occupancy_mask)
  expect_warning(generator$add_generative_requirements(attr3 = "file", dummy3 = "unknown"),
                 "Generative requirements must be added to model attributes only as")
  expect_equal(generator$generative_requirements_satisfied(), list(attr3 = FALSE, file_templates = FALSE))
  generator$set_attributes(attr1 = 1, attr2 = 2)
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s_no_such_file.csv"),
                              path_params = c("attr1","attr2"), file_type = "CSV")
  expect_equal(generator$generative_requirements_satisfied(), list(attr3 = TRUE))
  expect_null(generator$read_file("attr3"))
  expect_match(generator$error_messages, "Error reading file")
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.csv"),
                              path_params = c("attr1", "attr2"), file_type = "CSV")
  expect_null(generator$read_file("none1"))
  test_data <- generator$read_file("attr3")
  expect_is(test_data, "data.frame")
  expect_equal(generator$attached$attr3, test_data)
  generator$attached$attr3 <- test_data*2
  expect_equal(generator$get_attribute("attr3"), test_data*2)
  generator$attached$attr3 <- NULL # clear
  expect_equal(generator$get_attribute("attr3"), test_data)
  expect_equal(generator$attached$attr3, test_data)
  generator$attached$attr3 <- NULL # clear
  generator$outputs <- "attr3"
  expect_equal(generator$get_attribute("attr3"), round(test_data*occupancy_mask, 4))
  generator$attached$attr3 <- NULL # clear
  generator$occupancy_mask <- array(1, c(7, 10))
  generator$occupancy_mask[3:4, 5:8] <- 0
  expect_equal(generator$get_attribute("attr3"), round(test_data*generator$occupancy_mask, 4))
  generator$attached$attr3 <- NULL # clear
  # Missing path param
  generator$set_attributes(attr1 = NULL)
  expect_null(generator$read_file("attr3"))
  expect_equal(generator$error_messages[2], "Missing parameter(s) for file template:  attr1")
  # Via nested aliases
  generator$set_attributes(attr0 = list(first = 1, second = 2), attr1 = 1)
  generator$attribute_aliases <- list(a1 = "attr0$first", a2 = "attr0$second", a3 = "attr3", a3_1 = "attr3[1]")
  expect_equal(generator$get_attribute("a3"), round(test_data*generator$occupancy_mask, 4))
  generator$attached$attr3 <- NULL # clear
  generator$set_attributes(attr1 = NULL, attr2 = NULL)
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.csv"),
                              path_params = c("a1", "a2"), file_type = "CSV")
  expect_equal(generator$get_attribute("a3"), round(test_data*generator$occupancy_mask, 4))
  generator$attached$attr3 <- NULL # clear
  expect_equal(generator$get_attribute("a3_1"), test_data[1])
  # Generate rasters
  region = Region$new(coordinates = coordinates, use_raster = TRUE)
  generator <- Generator$new(description = "Test generator", decimals = 4, outputs = "attr3",
                             region = region, occupancy_mask = region$raster_from_values(occupancy_mask))
  generator$add_generative_requirements(attr3 = "file")
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.csv"),
                              path_params = c("attr1", "attr2"), file_type = "CSV")
  generator$set_attributes(attr1 = 1, attr2 = 2)
  expect_equal(unname(generator$get_attribute("attr3")[region$region_indices]),
               unname(round(as.matrix(test_data*occupancy_mask), 4)))
})

test_that("file template reading raster", {
  TEST_DIRECTORY <- test_path("test_inputs")
  coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))
  region = Region$new(coordinates = coordinates, use_raster = TRUE)
  occupancy_mask <- array(1, c(7, 10))
  occupancy_mask[3:4, 5:8] <- 0
  raster_occupancy_mask <- raster::stack(replicate(10, raster::raster(vals = region$region_raster[],
                                                                      nrows = 4, ncol = 4,
                                                                      xmn = 0, xmx = 4000, ymn = 0, ymx = 4000,
                                                                      crs = "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
  raster_occupancy_mask[][region$region_indices, ] <- occupancy_mask[raster_occupancy_mask[][region$region_indices, 1], ]
  generator <- Generator$new(description = "Test generator", decimals = 4,
                             region = Region$new(coordinates = coordinates, use_raster = TRUE))
  # Raster occupancy mask consistency
  expect_error(generator$occupancy_mask <- raster_occupancy_mask,
               "Occupancy mask raster must be consistent with the defined region raster")
  raster::crs(raster_occupancy_mask) <- raster::crs(region$region_raster)
  raster::extent(raster_occupancy_mask) <- raster::extent(region$region_raster)
  generator$occupancy_mask <- raster_occupancy_mask
  expect_equal(unname(generator$occupancy_mask[region$region_indices]), occupancy_mask)
  generator$set_attributes(attr1 = 1, attr2 = 2)
  # Via GRD (default)
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.grd"),
                              path_params = c("attr1", "attr2"))
  test_raster <- generator$read_file("attr3")
  expect_equal(generator$attached$attr3[], test_raster[])
  generator$attached$attr3 <- test_raster*2
  expect_equal(generator$get_attribute("attr3")[], test_raster[]*2)
  generator$attached$attr3 <- NULL # clear
  expect_is(test_raster, "RasterBrick")
  expect_equal(generator$file_templates$attr3$file_type, "GRD")
  expect_equal(generator$get_attribute("attr3")[], test_raster[])
  generator$attached$attr3 <- NULL # clear
  # Via RDS
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.RData"),
                              path_params = c("attr1", "attr2"), file_type = "RDS")
  test_raster <- generator$read_file("attr3")
  generator$attached$attr3 <- NULL # clear
  expect_is(test_raster, "RasterBrick")
  expect_equal(generator$get_attribute("attr3")[], test_raster[])
  generator$attached$attr3 <- NULL # clear
  # Via nested aliases
  generator$set_attributes(attr0 = list(first = 1, second = 2))
  generator$attribute_aliases <- list(a1 = "attr0$first", a2 = "attr0$second", a3 = "attr3", a3_8 = "attr3[8]",
                                      a3_X5 = "attr3[][, 5]")
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.RData"),
                              path_params = c("a1", "a2"), file_type = "RDS")
  generator$outputs <- "attr3"
  expect_equal(generator$get_attribute("a3")[], (round(test_raster, 4)*raster_occupancy_mask)[])
  generator$attached$attr3 <- NULL # clear
  expect_equal(generator$get_attribute("a3_8"), test_raster[8])
  generator$attached$attr3 <- NULL # clear
  expect_equal(generator$get_attribute("a3_X5"), test_raster[][, 5])
  # Generate arrays from rasters
  generator <- Generator$new(description = "Test generator", decimals = 4, outputs = "attr3",
                             region = region, generate_rasters = FALSE,
                             occupancy_mask = raster_occupancy_mask)
  expect_equal(unname(generator$occupancy_mask), occupancy_mask)
  generator$add_generative_requirements(attr3 = "file")
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.grd"),
                              path_params = c("attr1", "attr2"))
  generator$set_attributes(attr1 = 1, attr2 = 2)
  expect_equal(unname(generator$get_attribute("attr3")),
               unname(round(as.matrix(test_raster[region$region_indices]*occupancy_mask), 4)))
})

test_that("function execution template", {
  TEST_DIRECTORY <- test_path("test_inputs")
  generator <- Generator$new(description = "Test generator", decimals = 4, outputs = "attr4")
  generator$set_attributes(attr1 = 1.012345, attr2 = 2.111111)
  generator$add_generative_requirements(attr4 = "function")
  expect_equal(generator$generative_requirements_satisfied(), list(attr4 = FALSE, function_templates = FALSE))
  expect_error(generator$add_function_template("attr4", function_def = file.path(TEST_DIRECTORY, "no_test_function.R"),
                                               call_params = c("attr1", "attr2")),
               "Could not assign function")
  expect_error(generator$add_function_template("attr4", function_def = file.path(TEST_DIRECTORY, "test_invalid_code.R"),
                                               call_params = c("attr1", "attr2")),
               "Error loading function from file")
  expect_error(generator$add_function_template("attr4", function_def = file.path(TEST_DIRECTORY, "test_not_a_function.R"),
                                               call_params = c("attr1", "attr2")),
               "Could not assign function")
  expect_error(generator$add_function_template("attr4", function_def = file.path(TEST_DIRECTORY, "test_function_0.R"),
                                               call_params = c("attr1", "attr2")),
               "The function definition should have one argument, e.g. function(params)", fixed = TRUE)
  expect_error(generator$add_function_template("attr4", function_def = file.path(TEST_DIRECTORY, "test_function_2.R"),
                                               call_params = c("attr1", "attr2")),
               "The function definition should have one argument, e.g. function(params)", fixed = TRUE)

  generator$add_function_template("attr4", function_def = file.path(TEST_DIRECTORY, "test_function_1.R"),
                                  call_params = c("attr1", "attr2"))
  expect_true(is.function(generator$function_templates$attr4$function_def))
  expect_equal(generator$generative_requirements_satisfied(), list(attr4 = TRUE))
  expect_equal(generator$run_function("attr4"), 1.012345 + 2.111111)
  expect_equal(generator$attached$attr4, 1.012345 + 2.111111)
  generator$attached$attr4 <- NULL # clear
  generator$add_function_template("attr4", function_def = function (l) {l[[1]] + l[[2]]},
                                  call_params = c("attr1", "attr2"))
  generator$attached$attr4 <- 4.444
  expect_equal(generator$get_attribute("attr4"), 4.444)
  generator$attached$attr4 <- NULL # clear
  expect_equal(generator$get_attribute("attr4"), 3.1235)
  expect_null(generator$run_function("none2"))
  # Via nested aliases
  generator$set_attributes(attr0 = list(first = 3.012345, second = 2.54321))
  generator$attribute_aliases <- list(a1 = "attr0$first", a2 = "attr0$second", a4 = "attr4")
  generator$function_templates$attr4$call_params <- c("a1", "a2")
  expect_equal(generator$get_attribute("attr4"), 3.1235) # attached
  generator$attached$attr4 <- NULL # clear
  expect_equal(generator$get_attribute("a4"), 5.5556)
  generator$attached$attr4 <- NULL # clear
  # Missing call param
  generator$set_attributes(a1 = NULL)
  expect_null(generator$get_attribute("a4"))
  expect_equal(generator$error_messages, "Missing parameter(s) for function template:  a1")
})

test_that("sample distribution template (uniform)", {
  TEST_DIRECTORY <- test_path("test_inputs")
  generator <- Generator$new(description = "Test generator", th20 = 20, decimals = 4, outputs = "attr5")
  generator$add_generative_requirements(attr5 = "distribution")
  expect_equal(generator$generative_requirements_satisfied(), list(attr5 = FALSE, distribution_templates = FALSE))
  expect_error(generator$add_distribution_template("attr5", distr_type = "unknown"),
               "The distribution type should be one of: \"uniform\", \"normal\", \"lognormal\", \"beta\", \"triangular\"",
               fixed = TRUE)
  expect_error(generator$add_distribution_template("attr5", distr_type = "uniform",
                                                   distr_params = list(dummy1 = 0, dummy2 = "attr1")),
               "The distribution parameters should be a list containing uniform distribution parameters: lower, upper")
  generator$add_distribution_template("attr5", distr_type = "uniform",
                                      distr_params = list(lower = 0, upper = "attr1"),
                                      sample = "attr2", random_seed = 123)
  expect_equal(generator$generative_requirements_satisfied(), list(attr5 = TRUE))
  expect_null(generator$sample_distribution("attr5"))
  expect_equal(generator$error_messages, "The distribution sample for attr5 utilizes missing parameter(s): attr2")
  generator$set_attributes(attr2 = 0.6)
  expect_null(generator$sample_distribution("attr5"))
  expect_equal(generator$error_messages[2], "Values for uniform distribution not obtained for parameters: upper (via attr1)")
  generator$set_attributes(attr1 = 10)
  expect_equal(generator$sample_distribution("attr5"), 6)
  expect_equal(generator$attached$attr5, 6)
  generator$attached$attr5 <- NULL # clear
  generator$distribution_templates$attr5 <- NULL
  generator$add_distribution_template("attr5", distr_type = "uniform",
                                      distr_params = list(lower = 0, upper = "attr1"),
                                      sample = c(0.2, 0.4), random_seed = 123)
  expect_equal(generator$sample_distribution("attr5"), {set.seed(123); stats::runif(1, min = 0.2, max = 0.4)*10})
  generator$attached$attr5 <- NULL # clear
  generator$distribution_templates$attr5 <- NULL
  generator$add_distribution_template("attr5", distr_type = "uniform",
                                      distr_params = list(lower = 0, upper = "attr1"),
                                      sample = c(0.2, "attr2"), random_seed = 123)
  expect_equal(generator$sample_distribution("attr5"), {set.seed(123); stats::runif(1, min = 0.2, max = 0.6)*10})
  generator$attached$attr5 <- NULL # clear
  generator$distribution_templates$attr5$sample <- NULL
  expect_equal(generator$sample_distribution("attr5"), {set.seed(123); stats::runif(1)*10})
  generator$attached$attr5 <- NULL # clear
  expect_equal(generator$get_attribute("attr5"), {set.seed(123); round(stats::runif(1)*10, 4)})
  generator$attached$attr5 <- NULL # clear
  # Sample as a window
  generator$distribution_templates$attr5 <- NULL
  generator$add_distribution_template("attr5", distr_type = "uniform",
                                      distr_params = list(lower = 0, upper = "attr1"),
                                      sample = list(mid = 0.5, window = 0.2), random_seed = 123)
  expect_equal(generator$sample_distribution("attr5"), {set.seed(123); stats::runif(1, min = 0.4, max = 0.6)*10})
  generator$attached$attr5 <- NULL # clear
  generator$distribution_templates$attr5 <- NULL
  generator$add_distribution_template("attr5", distr_type = "uniform",
                                      distr_params = list(lower = 0, upper = "attr1"),
                                      sample = list(mid = "attr2", window = 0.4), random_seed = 123)
  expect_equal(generator$sample_distribution("attr5"), {set.seed(123); stats::runif(1, min = 0.4, max = 0.8)*10})
  generator$attached$attr5 <- NULL # clear
  generator$distribution_templates$attr5$sample <- list(mid = 0.2, window = "attr2")
  expect_equal(generator$sample_distribution("attr5"), {set.seed(123); stats::runif(1, min = 0, max = 0.5)*10})
  generator$attached$attr5 <- NULL # clear
  generator$set_attributes(attr3 = 1)
  generator$distribution_templates$attr5$sample <- list(mid = "attr2", window = "attr3")
  expect_equal(generator$sample_distribution("attr5"), {set.seed(123); stats::runif(1, min = 0.1, max = 1)*10})
  generator$attached$attr5 <- NULL # clear
  generator$distribution_templates$attr5$sample <- list(mid = "attr2", window = "attr4")
  expect_null(generator$sample_distribution("attr5"))
  expect_equal(generator$error_messages[3], "The distribution sample for attr5 utilizes missing parameter(s): attr4")
  # Add normalization threshold
  generator$distribution_templates$attr5$normalize_threshold <- 20
  generator$distribution_templates$attr5$sample <- "attr2"
  expect_equal(generator$sample_distribution("attr5"), 6/20)
  generator$attached$attr5 <- NULL # clear
  generator$distribution_templates$attr5$normalize_threshold <- 5
  expect_equal(generator$sample_distribution("attr5"), 1)
  generator$add_distribution_template("attr5", distr_type = "uniform",
                                      distr_params = list(lower = 0, upper = "attr1"),
                                      sample = "attr2", random_seed = 123,
                                      normalize_threshold = "th20")
  expect_equal(generator$sample_distribution("attr5"), 6/20)
})

test_that("sample distribution template with rasters (uniform)", {
  TEST_DIRECTORY <- test_path("test_inputs")
  coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))
  region = Region$new(coordinates = coordinates, use_raster = TRUE)
  raster_occupancy_mask <- raster::stack(replicate(10, region$region_raster))
  raster_occupancy_mask[region$region_indices] <- array(1, c(7, 10))
  raster_occupancy_mask[region$region_indices][3:4, 5:8] <- 0
  generator <- Generator$new(description = "Test generator", decimals = 4,
                             region = region, occupancy_mask = raster_occupancy_mask)
  generator$add_generative_requirements(attr5 = "distribution")
  # Single raster
  generator$add_distribution_template("attr5", distr_type = "uniform",
                                      distr_params = list(lower = 0, upper = "attr1"),
                                      random_seed = 123)
  generator$set_attributes(attr1 = 10)
  expect_equal(generator$sample_distribution("attr5")[region$region_indices],
               {set.seed(123); stats::runif(7)*10})
  expect_equal(generator$attached$attr5[region$region_indices], {set.seed(123); stats::runif(7)*10})
  generator$attached$attr5 <- 4.4444
  expect_equal(generator$get_attribute("attr5"), 4.4444)
  generator$attached$attr5 <- NULL # clear
  expect_equal(generator$get_attribute("attr5")[region$region_indices],
               {set.seed(123); stats::runif(7)*10})
  generator$attached$attr5 <- NULL # clear
  generator$outputs <- "attr5" # round (& mask)
  expect_equal(generator$get_attribute("attr5")[region$region_indices],
               {set.seed(123); round(stats::runif(7)*10, 4)})
  generator$attached$attr5 <- NULL # clear
  # Raster stack
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.RData"),
                              path_params = c("a1", "a2"), file_type = "RDS")
  generator$set_attributes(a1 = 1, a2 = 2) # used to read attr3 from file
  generator$distribution_templates$attr5$distr_params <- list(lower = region$region_raster*0,
                                                              upper = "attr3")
  expect_is(generator$sample_distribution("attr5"), "RasterBrick")
  generator$attached$attr5 <- NULL # clear
  attr3_matrix <- unname(generator$get_attribute("attr3")[region$region_indices])
  expect_equal(unname(generator$sample_distribution("attr5")[region$region_indices]),
               {set.seed(123); stats::qunif(stats::runif(7), min = 0, max = attr3_matrix)})
  generator$attached$attr3[][3, 1:4] <- c(NA, NaN, Inf, 0)
  attr3_matrix[which(region$region_indices == 3), 1:4] <- c(NA, NA, NA, 0)
  set.seed(123)
  expected_attr5_matrix <- stats::qunif(stats::runif(7), min = 0, max = attr3_matrix)
  generator$attached$attr5 <- NULL # clear
  expect_equal(unname(generator$sample_distribution("attr5")[region$region_indices]), expected_attr5_matrix)
  generator$attached$attr5 <- NULL # clear
  expect_equal(unname(generator$get_attribute("attr5")[region$region_indices]),
               round(expected_attr5_matrix*unname(raster_occupancy_mask[region$region_indices]), 4))
  generator$attached$attr5 <- NULL # clear
  # Add normalization threshold
  generator$distribution_templates$attr5$normalize_threshold <- 0.8
  expected_attr5_matrix <- expected_attr5_matrix*unname(raster_occupancy_mask[region$region_indices])/0.8
  expected_attr5_matrix[which(expected_attr5_matrix > 1)] <- 1
  expect_equal(unname(generator$get_attribute("attr5")[region$region_indices]),
               round(expected_attr5_matrix, 4))
  # Generate arrays from rasters
  generator$generate_rasters <- FALSE
  generator$occupancy_mask <- raster_occupancy_mask
  expect_equal(generator$occupancy_mask, raster_occupancy_mask[region$region_indices])
  expect_equal(unname(generator$get_attribute("attr5")), round(expected_attr5_matrix, 4))
})

test_that("sample distribution template (normal)", {
  TEST_DIRECTORY <- test_path("test_inputs")
  coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))
  region = Region$new(coordinates = coordinates, use_raster = TRUE)
  generator <- Generator$new(description = "Test generator", a1 = 1, a2 = 2, region = region)
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.RData"),
                              path_params = c("a1", "a2"), file_type = "RDS")
  a3 <- generator$get_attributes("attr3") # attached
  generator$attached$attr3[][3, 1:4] <- c(NA, NaN, Inf, 0)
  generator$add_generative_requirements(attr5 = "distribution")
  expect_error(generator$add_distribution_template("attr5", distr_type = "normal",
                                                   distr_params = list(dummy1 = 0, dummy2 = "attr1")),
               "The distribution parameters should be a list containing normal distribution parameters: mean, sd")
  generator$add_distribution_template("attr5", distr_type = "normal", random_seed = 123,
                                      distr_params = list(mean = "attr3", sd = 0.2))
  attr3_matrix <- unname(generator$get_attribute("attr3")[region$region_indices])
  attr3_matrix[which(region$region_indices == 3), 1:4] <- c(NA, NA, Inf, 0)
  expect_equal(unname(generator$sample_distribution("attr5")[region$region_indices]),
               {set.seed(123); stats::qnorm(stats::runif(7), mean = attr3_matrix, sd = 0.2)})
})

test_that("sample distribution template (lognormal)", {
  TEST_DIRECTORY <- test_path("test_inputs")
  coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))
  region = Region$new(coordinates = coordinates, use_raster = TRUE)
  generator <- Generator$new(description = "Test generator", a1 = 1, a2 = 2, region = region)
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.RData"),
                              path_params = c("a1", "a2"), file_type = "RDS")
  a3 <- generator$get_attributes("attr3") # attached
  generator$attached$attr3[][3, 1:4] <- c(NA, NaN, Inf, 0)
  generator$add_generative_requirements(attr5 = "distribution")
  expect_error(generator$add_distribution_template("attr5", distr_type = "lognormal",
                                                   distr_params = list(dummy1 = 0, dummy2 = "attr1")),
               "The distribution parameters should be a list containing lognormal distribution parameters: meanlog, sdlog (or mean, sd)",
               fixed = TRUE)
  generator$add_distribution_template("attr5", distr_type = "lognormal", random_seed = 123,
                                      distr_params = list(meanlog = "attr3", sdlog = 0.2))
  attr3_matrix <- unname(generator$get_attribute("attr3")[region$region_indices])
  attr3_matrix[which(region$region_indices == 3), 1:4] <- c(NA, NA, Inf, 0)
  expect_equal(unname(generator$sample_distribution("attr5")[region$region_indices]),
               {set.seed(123); stats::qlnorm(stats::runif(7), meanlog = attr3_matrix, sdlog = 0.2)})
  expect_equal(unname(generator$attached$attr5[region$region_indices]),
               {set.seed(123); stats::qlnorm(stats::runif(7), meanlog = attr3_matrix, sdlog = 0.2)})
  generator$attached$attr5 <- NULL # clear
  # Via mean and sd
  generator$add_distribution_template("attr5", distr_type = "lognormal", random_seed = 123,
                                      distr_params = list(mean = "attr3", sd = 0.2))
  set.seed(123)
  expected_values <- stats::qlnorm(stats::runif(7),
                                   meanlog = log(attr3_matrix^2/sqrt(attr3_matrix^2 + 0.2^2)),
                                   sdlog = sqrt(log(1 + 0.2^2/attr3_matrix^2)))
  expected_values[which(!is.finite(expected_values))] <- attr3_matrix[which(!is.finite(expected_values))]
  expect_equal(unname(generator$sample_distribution("attr5")[region$region_indices]), expected_values)
})

test_that("sample distribution template (beta)", {
  TEST_DIRECTORY <- test_path("test_inputs")
  coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))
  region = Region$new(coordinates = coordinates, use_raster = TRUE)
  generator <- Generator$new(description = "Test generator", a1 = 1, a2 = 2, region = region)
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.RData"),
                              path_params = c("a1", "a2"), file_type = "RDS")
  a3 <- generator$get_attributes("attr3") # attached
  generator$attached$attr3[][3, 1:4] <- c(NA, NaN, Inf, 0)
  generator$add_generative_requirements(attr5 = "distribution")
  expect_error(generator$add_distribution_template("attr5", distr_type = "beta",
                                                   distr_params = list(dummy1 = 0, dummy2 = "attr1")),
               "The distribution parameters should be a list containing beta distribution parameters: alpha, beta")
  generator$add_distribution_template("attr5", distr_type = "beta", random_seed = 123,
                                      distr_params = list(alpha = "attr3", beta = 0.2))
  attr3_matrix <- unname(generator$get_attribute("attr3")[region$region_indices])
  attr3_matrix[which(region$region_indices == 3), 1:4] <- c(NA, NA, Inf, 0)
  expect_equal(unname(generator$sample_distribution("attr5")[region$region_indices]),
               {set.seed(123); stats::qbeta(stats::runif(7), shape1 = attr3_matrix, shape2 = 0.2)})
  expect_equal(unname(generator$attached$attr5[region$region_indices]),
               {set.seed(123); stats::qbeta(stats::runif(7), shape1 = attr3_matrix, shape2 = 0.2)})
  generator$attached$attr5 <- NULL # clear
  # Via mean and sd
  generator$attached$attr3[region$region_indices] <- generator$attached$attr3[region$region_indices]*0.8
  generator$attached$attr3[region$region_indices][6, 5] <- 1
  attr3_matrix <- unname(generator$get_attribute("attr3")[region$region_indices])
  generator$add_distribution_template("attr5", distr_type = "beta", random_seed = 123,
                                      distr_params = list(mean = "attr3", sd = 0.3))
  available_indices <- which(is.finite(generator$attached$attr3[region$region_indices]) &
                               generator$attached$attr3[region$region_indices] > 0 &
                               generator$attached$attr3[region$region_indices] < 1)
  limited_sd <- array(0.3, dim(attr3_matrix)) # limit to < sqrt(mean*(1 - mean))
  limited_sd[available_indices] <- pmin(limited_sd[available_indices],
                                        sqrt(attr3_matrix[available_indices]*(1 - attr3_matrix[available_indices]))*0.999)
  set.seed(123)
  expected_values <- array(NA, dim(attr3_matrix))
  expected_values[available_indices] <- stats::qbeta(array(stats::runif(7), dim(attr3_matrix))[available_indices],
                                                     shape1 = attr3_matrix[available_indices]*(attr3_matrix[available_indices]*(1 - attr3_matrix[available_indices])/limited_sd[available_indices]^2 - 1),
                                                     shape2 = (1 - attr3_matrix[available_indices])*(attr3_matrix[available_indices]*(1 - attr3_matrix[available_indices])/limited_sd[available_indices]^2 - 1))
  expect_equal(unname(generator$sample_distribution("attr5")[region$region_indices]), expected_values)
})

test_that("sample distribution template (triangular)", {
  TEST_DIRECTORY <- test_path("test_inputs")
  coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))
  region = Region$new(coordinates = coordinates, use_raster = TRUE)
  generator <- Generator$new(description = "Test generator", a1 = 1, a2 = 2, region = region)
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.RData"),
                              path_params = c("a1", "a2"), file_type = "RDS")
  a3 <- generator$get_attributes("attr3") # attached
  generator$attached$attr3[][3, 1:4] <- c(NA, NaN, Inf, 0)
  generator$add_generative_requirements(attr5 = "distribution")
  expect_error(generator$add_distribution_template("attr5", distr_type = "triangular",
                                                   distr_params = list(dummy1 = 0, dummy2 = "attr1")),
               "The distribution parameters should be a list containing triangular distribution parameters: lower, mode, upper")
  generator$add_distribution_template("attr5", distr_type = "triangular", random_seed = 123,
                                      distr_params = list(lower = region$region_raster*0, upper = "attr3", mode = 0))
  attr3_matrix <- unname(generator$get_attribute("attr3")[region$region_indices])
  attr3_matrix[which(region$region_indices == 3), 1:4] <- c(NA, NA, NA, 0)
  finite_indices <- which(is.finite(attr3_matrix) & attr3_matrix > 0)
  expected_attr5_matrix <- attr3_matrix
  set.seed(123)
  expected_attr5_matrix[finite_indices] <- metRology::qtri(array(stats::runif(7), c(7, 10))[finite_indices],
                                                           min = 0, max = attr3_matrix[finite_indices], mode = 0)
  expect_equal(unname(generator$sample_distribution("attr5")[region$region_indices]), expected_attr5_matrix)
})

test_that("sample distribution template with correlation (uniform)", {
  TEST_DIRECTORY <- test_path("test_inputs")
  coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))
  region = Region$new(coordinates = coordinates, use_raster = TRUE)
  generator <- Generator$new(description = "Test generator", a1 = 1, a2 = 2, region = region)
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.RData"),
                              path_params = c("a1", "a2"), file_type = "RDS")
  a3 <- generator$get_attributes("attr3") # attached
  generator$attached$attr3[][3, 1:4] <- c(NA, NaN, Inf, 0)
  generator$add_generative_requirements(attr5 = "distribution")
  generator$add_distribution_template("attr5", distr_type = "uniform", random_seed = 123,
                                      distr_params = list(lower = 0, upper = "attr3"))
  attr3_matrix <- unname(generator$get_attribute("attr3")[region$region_indices])
  attr3_matrix[which(region$region_indices == 3), 1:4] <- c(NA, NA, NA, 0)
  generator$uses_correlations <- TRUE
  expect_null(generator$sample_distribution("attr5"))
  expect_equal(generator$error_messages,
               "The spatial correlation object for generating sample deviates needs to be set first")
  expect_error(generator$spatial_correlation <- c(1, 2, 3),
               "Spatial correlation must be a SpatialCorrelation or inherited class object")
  expect_error(generator$spatial_correlation <- SpatialCorrelation$new(region = region$clone()),
               "The spatial correlation object must have the same region class object as this generator")
  spatial_correlation <- SpatialCorrelation$new(region = region, correlation_amplitude = 0.6,
                                                correlation_breadth = 200)
  generator$spatial_correlation <- spatial_correlation
  expect_null(generator$sample_distribution("attr5"))
  expect_equal(generator$error_messages[2],
               "The compact correlation decomposition needs to be calculated before correlated normal deviates can be generated")
  generator$spatial_correlation$calculate_compact_decomposition()
  correlated_samples <- pnorm(spatial_correlation$generate_correlated_normal_deviates(random_seed = 123))
  expect_equal(unname(generator$sample_distribution("attr5")[region$region_indices]),
               stats::qunif(correlated_samples, min = 0, max = attr3_matrix))
  expect_equal(unname(generator$attached$attr5[region$region_indices]),
               stats::qunif(correlated_samples, min = 0, max = attr3_matrix))
  generator$attached$attr5 <- NULL # clear
  generator$temporal_correlation <- 0.99
  generator$time_steps <- 10
  correlated_samples <- pnorm(spatial_correlation$generate_correlated_normal_deviates(random_seed = 123, time_steps = 10,
                                                                                      temporal_correlation = 0.99))
  expect_equal(unname(generator$sample_distribution("attr5")[region$region_indices]),
               correlated_samples*attr3_matrix)
})

test_that("sample distribution template with nested aliases", {
  TEST_DIRECTORY <- test_path("test_inputs")
  coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))
  region = Region$new(coordinates = coordinates, use_raster = TRUE)
  raster_occupancy_mask <- region$region_raster
  raster_occupancy_mask[region$region_indices] <- c(1, 1, 0, 1, 1, 1, 1)
  generator <- Generator$new(description = "Test generator", a1 = 1, a2 = 2, region = region,
                             outputs = "attr5", decimals = 4, occupancy_mask = raster_occupancy_mask)
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.RData"),
                              path_params = c("a1", "a2"), file_type = "RDS")
  attr3 <- generator$get_attribute("attr3")
  generator$add_generative_requirements(attr5 = "distribution")
  generator$set_attributes(attr0 = list(lower = region$region_raster*0, upper = attr3, mode = attr3*0.4))
  generator$attribute_aliases <- list(a0_l = "attr0$lower", a0_u = "attr0$upper", a0_m = "attr0$mode", a5 = "attr5")
  generator$add_distribution_template("attr5", distr_type = "triangular", random_seed = 123,
                                      distr_params = list(lower = "a0_l", upper = "a0_u", mode = "a0_m"))
  attr3_matrix <- unname(attr3[region$region_indices])
  set.seed(123)
  expected_attr5_matrix <- metRology::qtri(array(stats::runif(7), c(7, 10)),
                                           min = 0, max = attr3_matrix, mode = attr3_matrix*0.4)
  expect_equal(unname(generator$get_attribute("a5")[region$region_indices]),
               round(expected_attr5_matrix*raster_occupancy_mask[region$region_indices], 4))
  expect_equal(unname(generator$attached$attr5[region$region_indices]), expected_attr5_matrix)
  generator$attached$attr5 <- NULL # clear
  generator$set_attributes(a0_l = NULL)
  expect_null(generator$get_attribute("a5"))
  expect_equal(generator$error_messages,
               "Values for triangular distribution not obtained for parameters: lower (via a0_l)")
})

test_that("cloning and generation", {
  TEST_DIRECTORY <- test_path("test_inputs")
  coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))
  region = Region$new(coordinates = coordinates, use_raster = TRUE)
  generator <- Generator$new(description = "Test generator", region = region, decimals = 4,
                             attribute_aliases = list(a1 = "attr1", a2 = "attr2", a3 = "attr3",
                                                      a4 = "attr4", a5 = "attr5"),
                             inputs = c("a1", "a2"), outputs = c("a3", "a4", "a5"))
  generator$attribute_aliases <- c(generator$attribute_aliases, list(a6 = "template_attached$attr6"))
  generator$set_attributes(a6 = 66)
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.RData"),
                              path_params = c("a1", "a2"), file_type = "RDS")
  generator$add_function_template("attr4", function_def = function (l) {l[[1]] + l[[2]]},
                                  call_params = c("a1", "a2"))
  generator$add_distribution_template("attr5", distr_type = "uniform", random_seed = 123,
                                      distr_params = list(lower = 0, upper = "a3"))
  generator$add_generative_requirements(attr3 = "file", attr4 = "function", attr5 = "distribution")
  # Cloning
  generator_clone <- generator$new_clone(params = list(attr1 = 1, attr2 = 2))
  expect_equal(generator_clone$attribute_aliases, generator$attribute_aliases)
  expect_equal(generator_clone$generative_requirements, generator$generative_requirements)
  expect_equal(generator_clone$inputs, generator$inputs)
  expect_equal(generator_clone$outputs, generator$outputs)
  expect_equal(generator_clone$file_templates, generator$file_templates)
  expect_equal(generator_clone$function_templates, generator$function_templates)
  expect_equal(generator_clone$distribution_templates, generator$distribution_templates)
  expect_named(generator_clone$get_attributes(), c("region", "coordinates", "attr1", "attr2"))
  expect_named(generator_clone$get_attributes(c("a1", "a2", "a3", "a4", "a5", "a6")), c("a1", "a2", "a6", "a3", "a4", "a5"))
  expect_null(generator_clone$error_messages)
  # Generation
  expect_equal(generator$generate(), list(error_messages = "Test generator generation requires inputs: a1, a2"))
  expect_equal(generator$generate(input_values = list(a1 = 5, a2 = 7)),
               list(a4 = 12, error_messages = c(paste("Error reading file", file.path(test_path("test_inputs"), "Test_5_7.RData")),
                                                "Values for uniform distribution not obtained for parameters: upper (via a3)")))
  generated_outputs <- generator$generate(input_values = list(a1 = 1, a2 = 2))
  expect_named(generated_outputs, c("a3", "a4", "a5"))
  expect_equal(generated_outputs$a4, 3)
  set.seed(123)
  samples <- stats::runif(7)
  expect_equal(unname(generated_outputs$a5[region$region_indices]),
               unname(round(generated_outputs$a3[region$region_indices]*samples, 4)))
})

test_that("inheritance", {
  TEST_DIRECTORY <- test_path("test_inputs")
  TestGenerator <- R6::R6Class("TestGenerator", inherit = Generator,
                               public = list(),
                               private = list(.model_attributes = c("attr1", "attr2", "attr3"),
                                              .active_attributes = c("region", "description", "inputs", "outputs", "file_templates", "function_templates",
                                                                     "distribution_templates", "uses_correlations", "spatial_correlation", "temporal_correlation",
                                                                     "time_steps", "decimals", "occupancy_mask", "template_attached",
                                                                     "attr1", "attr2", "attr3"),
                                              .attr1 = NULL, .attr2 = NULL, .attr3 = NULL),
                               active = list(attr1 = function(value) {if (missing(value)) { private$.attr1 } else { private$.attr1 <- value }},
                                             attr2 = function(value) {if (missing(value)) { private$.attr2 } else { private$.attr2 <- value }},
                                             attr3 = function(value) {if (missing(value)) { private$.attr3 } else { private$.attr3 <- value }})
  )
  coordinates <- data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3))
  region = Region$new(coordinates = coordinates, use_raster = TRUE)
  generator <- TestGenerator$new(description = "Test generator 2",
                                 object_generator = TestGenerator,
                                 region = region, params = list(attr1 = 1, attr2 = 2))
  # File reading via templates
  generator$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s.RData"),
                              path_params = c("attr1", "attr2"), file_type = "RDS")
  expect_is(generator$read_file("attr3"), "RasterBrick")
  expect_null(generator$attached$attr3) # not attached
  expect_null(generator$attr3) # not set automatically
  expect_equal(generator$get_attribute("attr3")[], generator$read_file("attr3")[])
  # Aliases
  generator$template_attached$attr0 <- 10
  generator$attribute_aliases <- list(a0 = "template_attached$attr0", a1 = "attr1", a2 = "attr2", a3 = "attr3")
  expect_equal(generator$get_attributes(c("a0", "a1", "a2")), list(a0 = 10, a1 = 1, a2 = 2))
  expect_equal(generator$get_attribute("a3")[], generator$read_file("attr3")[])
  # Cloning
  generator_clone <- generator$new_clone()
  expect_equal(generator_clone$get_attributes(c("a0", "a1", "a2")), list(a0 = 10))
  generator_clone$set_attributes(a1 = 1, a2 = 2)
  expect_equal(generator_clone$get_attribute("a3")[], generator$read_file("attr3")[])
  # Generation
  expect_true(generator$generative_requirements_satisfied()) # default with no requirements set
  expect_equal(generator$generate(),
               list(error_messages = "Test generator 2 generation requires settings for: outputs"))
  generator$inputs <- c("a1", "a2")
  generator$outputs <- c("a3")
  expect_equal(generator$generate(),
               list(error_messages = "Test generator 2 generation requires inputs: a1, a2"))
  expect_equal(generator$generate(input_values = list(a1 = 2, a2 = 1)),
               list(error_messages = paste("Error reading file", file.path(test_path("test_inputs"), "Test_2_1.RData"))))
  expect_equal(generator$generate(input_values = list(a1 = 1, a2 = 2)), list(a3 = generator$read_file("attr3")))
  generator$add_generative_requirements(attr3 = "function")
  expect_equal(generator$generative_requirements_satisfied(), list(attr3 = FALSE, function_templates = FALSE))
  expect_equal(generator$generate(input_values = list(a1 = 1, a2 = 2)),
               list(error_messages = "Test generator 2 generation requires further settings for output(s): attr3, function_templates"))
  generator$add_generative_requirements(attr3 = "file")
  expect_equal(generator$generative_requirements_satisfied(), list(attr3 = TRUE))
  expect_equal(generator$generate(input_values = list(a1 = 1, a2 = 2)), list(a3 = generator$read_file("attr3")))
})
