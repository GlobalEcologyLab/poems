context("Spatial Correlation")

test_that("initialization and parameter setting", {
  expect_warning(spatial_correlation <- SpatialCorrelation$new(region = Region$new()),
                 "Spatial region has not been defined within the region object")
  expect_error(spatial_correlation$region <- array(1:10, c(5, 2)),
               "Region should be a Region (or inherited class) object", fixed = TRUE)
  expect_error(spatial_correlation$correlation_amplitude <- -1,
               "Correlation function parameter amplitude must be between 0 and 1 inclusively")
  expect_error(spatial_correlation$correlation_amplitude <- 2,
               "Correlation function parameter amplitude must be between 0 and 1 inclusively")
  expect_error(spatial_correlation$correlation_amplitude <- "0",
               "Correlation function parameter amplitude must be numeric")
  expect_error(spatial_correlation$correlation_breadth <- -1,
               "Correlation function parameter breadth must be positive/non-zero")
  expect_error(spatial_correlation$correlation_breadth <- "0",
               "Correlation function parameter breadth must be numeric")
  expect_silent(spatial_correlation$region <- NULL)
  expect_silent(spatial_correlation$correlation_amplitude <- NULL)
  expect_silent(spatial_correlation$correlation_breadth <- NULL)
})

test_that("calculate distance matrix", {
  # Latitude/longitude coordinates
  coordinates1 <- array(c(1:4, 4:1), c(7, 2))
  distance_matrix1 <- geosphere::distm(coordinates1, coordinates1, fun = geosphere::distGeo)
  spatial_correlation <- SpatialCorrelation$new()
  expect_error(spatial_correlation$calculate_distance_matrix(),
               "Distance matrix calculation requires region to be set first")
  expect_error(spatial_correlation$calculate_correlations(),
               "Correlation calculations require region and function parameter settings amplitude and breadth")
  expect_warning(spatial_correlation$region <- Region$new(use_raster = FALSE),
                 "Spatial region has not been defined within the region object")
  expect_error(spatial_correlation$calculate_distance_matrix(),
               "Distance matrix calculation requires the region to be defined with coordinates or a raster first")
  spatial_correlation$coordinates = coordinates1
  expect_equal(spatial_correlation$calculate_distance_matrix(), distance_matrix1)
  spatial_correlation$region$use_raster <- TRUE
  expect_equal(spatial_correlation$calculate_distance_matrix(), distance_matrix1)
  # Scaling factor for km
  spatial_correlation$distance_scale <- 1000
  expect_equal(spatial_correlation$calculate_distance_matrix(), distance_matrix1/1000)
  # Coordinates in meters
  spatial_correlation$distance_scale <- 1
  raster2 <- raster::raster(vals = spatial_correlation$region$region_raster[], nrows = 4, ncol = 4,
                            xmn = 0, xmx = 4000, ymn = 0, ymx = 4000,
                            crs = "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  coordinates2 <- data.frame(x = c(0:3, 3:1), y = c(0, 0:3, 3:2))*1000 + 500 # meters
  distance_matrix2 <- as.matrix(stats::dist(coordinates2))
  expect_warning(spatial_correlation$region <- Region$new(region_raster = raster2, use_raster = FALSE),
                 "Spatial region has not been defined within the region object")
  spatial_correlation$region$use_raster <- TRUE
  expect_equal(spatial_correlation$calculate_distance_matrix(), distance_matrix2)
  spatial_correlation$distance_scale <- 1000
  expect_equal(spatial_correlation$calculate_distance_matrix(), distance_matrix2/1000)
})

test_that("calculate correlations", {
  # Default threshold (0.0000001)
  coordinates <- array(c(1:4, 4:1), c(7, 2))
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)/1000 # km
  correlation_matrix <- 0.6*exp(-1*distance_matrix/200)
  diag(correlation_matrix) <- 1
  spatial_correlation <- SpatialCorrelation$new(correlation_amplitude = 0.6, correlation_breadth = 200)
  expect_error(spatial_correlation$calculate_correlations(),
               "Correlation calculations require region and function parameter settings amplitude and breadth")
  spatial_correlation$coordinates <- coordinates
  expect_error(spatial_correlation$calculate_correlations(distance_matrix = distance_matrix[1:6, 1:6]),
               "Distance matrix dimensions must be consistent with the number of region cells")
  spatial_correlation$calculate_correlations(distance_matrix = distance_matrix)
  expect_equal(spatial_correlation$correlation_matrix, correlation_matrix)
  # Protect existing correlation integrity
  expect_error(spatial_correlation$region <- Region$new(),
               "Calculated correlations/decompositions are already associated with the existing region")
  expect_error(spatial_correlation$correlation_amplitude <- 0.7,
               "Calculated correlations/decompositions are already associated with the existing correlation parameters")
  expect_error(spatial_correlation$correlation_breadth <- 300,
               "Calculated correlations/decompositions are already associated with the existing correlation parameters")
  # Rounding
  spatial_correlation$calculate_correlations(distance_matrix = distance_matrix, decimals = 3)
  expect_equal(spatial_correlation$correlation_matrix, round(correlation_matrix, 3))
})

test_that("calculate Cholesky decomposition", {
  coordinates <- array(c(1:4, 4:1), c(7, 2))
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)/1000 # km
  correlation_matrix <- 0.6*exp(-1*distance_matrix/200)
  diag(correlation_matrix) <- 1
  spatial_correlation <- SpatialCorrelation$new(region = Region$new(coordinates = coordinates), compact_only = FALSE,
                                            correlation_amplitude = 0.6, correlation_breadth = 200)
  spatial_correlation$calculate_cholesky_decomposition(distance_matrix = distance_matrix)
  expect_equal(spatial_correlation$t_decomposition_matrix, chol(correlation_matrix))
  spatial_correlation$correlation_matrix[1, 1] <- -100
  expect_error(spatial_correlation$calculate_cholesky_decomposition(),
               "Cholesky decomposition could not be calculated because the leading minor of order 1 is not positive definite")
})

test_that("calculate compact decomposition", {
  coordinates <- array(c(1:4, 4:1), c(7, 2))
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)/1000 # km
  correlation_matrix <- 0.6*exp(-1*distance_matrix/32)
  diag(correlation_matrix) <- 1
  spatial_correlation <- SpatialCorrelation$new(region = Region$new(coordinates = coordinates), compact_only = FALSE,
                                            correlation_amplitude = 0.6, correlation_breadth = 200)
  spatial_correlation$t_decomposition_matrix <- round(chol(correlation_matrix), 3)
  spatial_correlation$calculate_compact_decomposition()
  original_indices <- which(spatial_correlation$t_decomposition_matrix > 0, arr.ind = TRUE)
  compact_indices <- which(spatial_correlation$t_decomposition_compact_matrix > 0, arr.ind = TRUE)
  expect_equal(spatial_correlation$t_decomposition_compact_matrix[compact_indices],
               spatial_correlation$t_decomposition_matrix[original_indices])
  expect_equal(spatial_correlation$t_decomposition_compact_map[compact_indices], original_indices[, 1])
  expect_named(spatial_correlation$get_compact_decomposition(), c("matrix", "map"))
})

test_that("generate correlated normal deviates", {
  coordinates <- array(c(1:4, 4:1), c(7, 2))
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)/1000 # km
  correlation_matrix <- 0.6*exp(-1*distance_matrix/32)
  diag(correlation_matrix) <- 1
  spatial_correlation <- SpatialCorrelation$new(region = Region$new(coordinates = coordinates), compact_only = FALSE,
                                            correlation_amplitude = 0.6, correlation_breadth = 200)
  spatial_correlation$t_decomposition_matrix <- round(chol(correlation_matrix), 3)
  spatial_correlation$calculate_compact_decomposition()
  correlated_normal_deviates <- spatial_correlation$generate_correlated_normal_deviates(random_seed = 1234)
  expect_equal(spatial_correlation$generate_correlated_normal_deviates(random_seed = 1234),
               correlated_normal_deviates)
  expect_false(all(spatial_correlation$generate_correlated_normal_deviates(random_seed = 4321) ==
                     correlated_normal_deviates))
  # Temporal correlation
  expect_equal(spatial_correlation$generate_correlated_normal_deviates(random_seed = 1234, time_steps = 5),
               array(correlated_normal_deviates, c(7, 5))) # default temporal_correlation = 1
  deviates_temporal_99 <- spatial_correlation$generate_correlated_normal_deviates(random_seed = 1234, temporal_correlation = 0.99, time_steps = 5)
  deviates_temporal_80 <- spatial_correlation$generate_correlated_normal_deviates(random_seed = 1234, temporal_correlation = 0.8, time_steps = 5)
  deviates_temporal_0 <- spatial_correlation$generate_correlated_normal_deviates(random_seed = 1234, temporal_correlation = 0, time_steps = 5)
  expect_equal(deviates_temporal_99[, 1], correlated_normal_deviates)
  expect_equal(deviates_temporal_80[, 1], correlated_normal_deviates)
  expect_equal(deviates_temporal_0[, 1], correlated_normal_deviates)
  expect_true(mean(apply(deviates_temporal_99, 1, stats::sd)) < mean(apply(deviates_temporal_80, 1, stats::sd)))
  expect_true(mean(apply(deviates_temporal_80, 1, stats::sd)) < mean(apply(deviates_temporal_0, 1, stats::sd)))
})
