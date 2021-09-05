context("Dispersal Generator")

test_that("initialization and parameter setting", {
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  # Default initialization
  dispersal_gen <- DispersalGenerator$new()
  expect_equal(dispersal_gen$get_attributes(c("description", "inputs", "outputs")),
               list(description = "dispersal", inputs = c("dispersal_proportion", "dispersal_breadth", "dispersal_max_distance"),
                    outputs = c("dispersal_data")))
  dispersal_gen <- DispersalGenerator$new(coordinates = coordinates)
  expect_warning(dispersal_gen <- DispersalGenerator$new(region = Region$new()),
                 "Spatial region has not been defined within the region object")
  expect_error(dispersal_gen <- DispersalGenerator$new(region = coordinates),
               "Region should be a Region (or inherited class) object", fixed = TRUE)
  expect_silent(dispersal_gen <- DispersalGenerator$new(region = Region$new(coordinates = coordinates, use_raster = FALSE)))
  # Dispersal function parameters
  dispersal_gen$dispersal_function_data <- data.frame(breadth = seq(130, 100, -10), max_distance = seq(200, 350, 50))
  dispersal_gen$dispersal_proportion <- 0.8
  expect_equal(dispersal_gen$dispersal_proportion, 0.8)
  dispersal_gen$dispersal_index <- 3
  expect_equal(dispersal_gen$dispersal_breadth, 110)
  expect_equal(dispersal_gen$dispersal_max_distance, 300)
  dispersal_gen$dispersal_index <- NULL
  dispersal_gen$dispersal_max_distance <- 275
  expect_equal(dispersal_gen$dispersal_index, 2)
  expect_equal(dispersal_gen$dispersal_breadth, 120)
  expect_equal(dispersal_gen$dispersal_max_distance, 250)
  # Fixed function parameters
  dispersal_gen <- DispersalGenerator$new(coordinates = coordinates, inputs = c("dispersal_r"),
                                          dispersal_proportion = 0.4, dispersal_breadth = 130)
  expect_equal(dispersal_gen$generative_template$dispersal_proportion, 0.4)
  expect_equal(dispersal_gen$generative_template$dispersal_breadth, 130)
})

test_that("calculate distance matrix and classes", {
  # Region with longlat coordinates
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)
  dispersal_gen <- DispersalGenerator$new()
  expect_error(dispersal_gen$calculate_distance_matrix(),
               "Distance matrix calculation requires region/coordinates to be set first")
  dispersal_gen$coordinates <- coordinates
  expect_equal(dispersal_gen$calculate_distance_matrix(), distance_matrix)
  # Scaling factor for km
  dispersal_gen$distance_scale <- 1000
  expect_equal(dispersal_gen$calculate_distance_matrix(), distance_matrix/1000)
  # Raster region with coordinates in meters
  region <-  Region$new(region_raster = raster::raster(vals = 1:16, nrows = 4, ncol = 4,
                                                       xmn = 0, xmx = 400000, ymn = 0, ymx = 400000,
                                                       crs = "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  dispersal_gen <- DispersalGenerator$new(region = region)
  expect_equal(dispersal_gen$calculate_distance_matrix(), as.matrix(stats::dist(region$coordinates)))
  # Distance classes
  dispersal_gen$dispersal_max_distance <- 2222
  expect_equal(dispersal_gen$distance_classes, seq(2, 2222, 2))
  dispersal_gen$distance_classes <- NULL
  dispersal_gen$dispersal_max_distance <- 333
  expect_equal(dispersal_gen$distance_classes, 1:333)
  dispersal_gen$dispersal_max_distance <- NULL
  dispersal_gen$set_distance_classes(minimum = 100, maximum = 400, interval = 20)
  expect_equal(dispersal_gen$distance_classes, seq(100, 400, 20))
  expect_equal(dispersal_gen$dispersal_max_distance, 400)
  dispersal_gen$set_distance_classes(maximum = 400)
  expect_equal(dispersal_gen$distance_classes, 1:400)
})

test_that("calculate distance data", {
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)/1000
  dispersal_gen <- DispersalGenerator$new()
  # Manually calculate expected distance data (no dispersal friction)
  distance_data <- which(distance_matrix > 0 & distance_matrix <= 400, arr.ind = TRUE)
  distance_class <- as.numeric(cut(distance_matrix[distance_data], breaks = c(1, seq(100, 400, 20))))
  distance_data <- data.frame(target_pop = distance_data[, 1], source_pop = distance_data[, 2], distance_class)
  # Error cases
  expect_error(dispersal_gen$calculate_distance_data(),
               "Distance data calculation requires region/coordinates and distance classes to be set first")
  dispersal_gen$coordinates <- coordinates
  dispersal_gen$set_distance_classes(minimum = 100, maximum = 400, interval = 20)
  expect_error(dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix[1:15, 1:15]),
               "Distance matrix dimensions must be consistent with region/coordinates")
  expect_error(dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix*10),
               "No distance data was generated with the current distance classes")
  # Calculate distance data
  dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix)
  expect_named(dispersal_gen$distance_data, "base")
  expect_named(dispersal_gen$distance_data[["base"]], c("target_pop", "source_pop", "compact_row", "distance_class"))
  expect_equal(dispersal_gen$distance_data$base[c("target_pop", "source_pop", "distance_class")], distance_data)
  expect_true(all(distance_matrix[as.matrix(dispersal_gen$distance_data$base[c("target_pop", "source_pop")])] <
                    dispersal_gen$distance_classes[dispersal_gen$distance_data$base$distance_class]))
  expect_true(all(distance_matrix[as.matrix(dispersal_gen$distance_data$base[c("target_pop", "source_pop")])] >
                    dispersal_gen$distance_classes[dispersal_gen$distance_data$base$distance_class - 1]))
  # Compact
  in_range_matrix <- array(0, c(16, 16))
  in_range_matrix[as.matrix(dispersal_gen$distance_data$base[c("target_pop", "source_pop")])] <- 1
  expect_equal(max(dispersal_gen$distance_data$base$compact_row), max(colSums(in_range_matrix)))
  # Consistency
  expect_error(dispersal_gen$set_distance_classes(minimum = 100, maximum = 450, interval = 20),
               "Dispersal generator distance data is already associated with the existing distance classes")
})

test_that("calculate dispersals", {
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)/1000
  dispersal_gen <- DispersalGenerator$new(coordinates = coordinates, distance_classes = seq(100, 400, 20))
  # Pre-calculation required
  expect_equal(dispersal_gen$calculate_dispersals(),
               "Dispersal distance data needs to be calculated before dispersals can be generated")
  dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix)
  # Manually calculate expected dispersals
  dispersal_data <- dispersal_gen$distance_data$base[, c("target_pop", "source_pop", "distance_class")]
  dispersal_rate_classes <- c(0.4*exp(-1*seq(100, 300, 20)/110), rep(0, 5))
  dispersal_data$dispersal_rate <- dispersal_rate_classes[dispersal_data$distance_class]
  dispersal_data <- dispersal_data[which(dispersal_data$dispersal_rate > 0), c("target_pop", "source_pop", "dispersal_rate")]
  dispersal_matrix <- array(0, c(16, 16))
  dispersal_matrix[as.matrix(dispersal_data[c("target_pop", "source_pop")])] <- dispersal_data$dispersal_rate
  dispersal_matrix <- dispersal_matrix*matrix(0.4/colSums(dispersal_matrix), nrow = 16, ncol = 16, byrow = TRUE)
  dispersal_data$dispersal_rate <- dispersal_matrix[as.matrix(dispersal_data[c("target_pop", "source_pop")])]
  # Parameters required
  expect_equal(dispersal_gen$calculate_dispersals(),
               "Dispersal calculation requires sample parameter settings for proportion, breadth & maximum distance (look-up data may be missing)")
  dispersal_gen$set_attributes(proportion = 0.4, breadth = 110, max_distance = 300)
  dispersal_gen$calculate_dispersals()
  # Dispersal data
  expect_is(dispersal_gen$dispersal_data, "list")
  expect_length(dispersal_gen$dispersal_data, 1)
  expect_named(dispersal_gen$dispersal_data[[1]], c("target_pop", "source_pop", "emigrant_row", "immigrant_row", "dispersal_rate"))
  expect_equal(dispersal_gen$dispersal_data[[1]][c("target_pop", "source_pop", "dispersal_rate")], dispersal_data)
  # Compact
  expect_equal(max(dispersal_gen$dispersal_data[[1]][c("emigrant_row", "immigrant_row")]),
               max(colSums(dispersal_matrix > 0), rowSums(dispersal_matrix > 0)))
  # Dispersal matrix
  dispersal_gen$calculate_dispersals(type = "matrix")
  expect_equal(dispersal_gen$dispersal_matrix, dispersal_matrix)
  expect_equal(colSums(dispersal_gen$dispersal_matrix), rep(0.4, 16))
  # Re-calculate with rounding
  dispersal_gen$decimals <- 3
  dispersal_gen$calculate_dispersals()
  expect_equal(dispersal_gen$dispersal_data[[1]][["dispersal_rate"]], round(dispersal_data$dispersal_rate, 3))
})

test_that("connect dispersal friction object", {
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)/1000
  dispersal_gen <- DispersalGenerator$new(coordinates = coordinates, distance_classes = seq(100, 400, 20))
  dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix)
  # Errors, warnings, consistency checks
  expect_error(dispersal_gen$dispersal_friction <- "dummy",
               "Dispersal friction must be a DispersalFriction or inherited class object")
  expect_warning(dispersal_gen$dispersal_friction <- DispersalFriction$new(),
                 "Dispersal generator distance data will need to be re-calculated with the dispersal friction object")
  expect_null(dispersal_gen$distance_data)
  dispersal_gen$distance_data <- 1234 # dummy data
  expect_error(dispersal_gen$dispersal_friction <- DispersalFriction$new(),
               "Dispersal generator distance data is already associated with the existing dispersal friction object")
  dispersal_gen$distance_data <- NULL
  expect_error(dispersal_gen$dispersal_friction <- DispersalFriction$new(coordinates = coordinates[1:15,]),
               "Dispersal friction object is inconsistent with the dispersal generator region/coordinates")
  expect_error(dispersal_gen$dispersal_friction <- DispersalFriction$new(conductance = array(1, c(15, 10))),
               "Conductance matrix dimensions are inconsistent with the dispersal generator region/coordinates")
  dispersal_gen$region$use_raster <- TRUE
  raster2 <- raster::raster(vals = rep(1, 16), nrows = 4, ncol = 4,
                            xmn = 0, xmx = 400000, ymn = 0, ymx = 400000,
                            crs = "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  expect_error(dispersal_gen$dispersal_friction <- DispersalFriction$new(conductance = raster::stack(replicate(10, raster2))),
               "Conductance raster is inconsistent with the dispersal generator region")
  conductance_raster <- raster::stack(replicate(10, dispersal_gen$region$region_raster))
  conductance_raster[dispersal_gen$region$region_indices] <- array(1, c(16, 10))
  dispersal_gen$region$use_raster <- FALSE
  expect_error(dispersal_gen$dispersal_friction <-
                 DispersalFriction$new(conductance = conductance_raster),
               "Conductance raster is inconsistent with the dispersal generator region")
  # Copy region/coordinates
  region <- dispersal_gen$region
  dispersal_gen$dispersal_friction <- DispersalFriction$new()
  expect_equal(dispersal_gen$dispersal_friction$coordinates, region$coordinates)
  dispersal_gen$region <- NULL
  expect_null(dispersal_gen$coordinates)
  dispersal_gen$dispersal_friction <- DispersalFriction$new(coordinates = region$coordinates)
  expect_equal(dispersal_gen$coordinates, region$coordinates)
  dispersal_gen$region <- NULL
  region$use_raster <- TRUE
  dispersal_gen$dispersal_friction <- DispersalFriction$new(region = region,
                                                            conductance = conductance_raster)
  expect_true(dispersal_gen$region$raster_is_consistent(conductance_raster))
})

test_that("calculate distance data with dispersal friction object", {
  # Region and conductance values with longlat coordinates
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)/1000
  # Distance data base as before
  dispersal_gen <- DispersalGenerator$new(coordinates = coordinates, distance_classes = seq(100, 400, 20))
  dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix)
  distance_data_base <- dispersal_gen$distance_data$base
  dispersal_gen$distance_data <- NULL
  # Dispersal friction
  conductance_matrix = array(1, c(16, 10))
  conductance_matrix[1, 1] <- 0.5
  conductance_matrix[c(2, 3, 5, 6, 7, 9, 10, 11), 2] <- 0 # isolate coordinate (1, 1)
  conductance_matrix[, 4] = array(c(0, 0.2, 0, 0.8), 16) # frictional landscape columns
  dispersal_friction = DispersalFriction$new(coordinates = coordinates,
                                             conductance = conductance_matrix)
  dispersal_gen$dispersal_friction <- dispersal_friction
  # Manually calculate expected distance data changes
  distance_multipliers <- dispersal_friction$calculate_distance_multipliers(as.matrix(distance_data_base[, 1:2]))
  in_range_distances <- distance_matrix[as.matrix(distance_data_base[, 1:2])]
  distance_data_changes <- list(10)
  for (i in c(1, 2, 4)) { # changes present (note: changes at t = 1 is a subset of those at t = 2)
    changed_indices <- which(distance_multipliers[[i]] != 1)
    distance_data_changes[[i]] <- distance_data_base[changed_indices,]
    distance_data_changes[[i]]$distance_class <- as.numeric(cut((in_range_distances*distance_multipliers[[i]])[changed_indices],
                                                                breaks = c(1, seq(100, 400, 20), Inf)))
  }
  distance_data_changes[[2]] <- distance_data_changes[[2]][-c(13, 179),] # > Dmax in both t = 1 and 2
  for (i in c(3, 5)) { # reverse changes
    distance_data_changes[[i]] <- distance_data_base[which(distance_multipliers[[i - 1]] != 1),]
  }
  for (i in c(6, 7, 8, 9, 10)) { # no changes
    distance_data_changes[[i]] <- distance_data_base[which(distance_multipliers[[i]] != 1),]
  }
  # Calculate distance data
  dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix)
  expect_equal(dispersal_gen$distance_data$base, distance_data_base)
  expect_equal(dispersal_gen$distance_data$changes, distance_data_changes)
  # Dispersal friction writing distance multipliers to file (memory performance strategy)
  dispersal_friction$write_to_dir <- tempdir()
  dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix)
  expect_equal(dispersal_gen$distance_data$base, distance_data_base)
  expect_equal(dispersal_gen$distance_data$changes, distance_data_changes)
})

test_that("calculate dispersals with dispersal friction", {
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)/1000
  conductance_matrix = array(1, c(16, 10))
  conductance_matrix[1, 1] <- 0.5
  conductance_matrix[c(2, 3, 5, 6, 7, 9, 10, 11), 2] <- 0 # isolate coordinate (1, 1)
  conductance_matrix[, 4] = array(c(0, 0.2, 0, 0.8), 16) # frictional landscape columns
  dispersal_friction <- DispersalFriction$new(conductance = conductance_matrix)
  dispersal_gen <- DispersalGenerator$new(coordinates = coordinates, distance_classes = seq(100, 400, 20),
                                          dispersal_friction = dispersal_friction,
                                          proportion = 0.4, breadth = 110, max_distance = 300)
  dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix)
  # Manually calculate expected sequence of dispersals
  dispersal_data <- dispersal_gen$distance_data$base[, c("target_pop", "source_pop", "distance_class")]
  dispersal_rate_classes <- c(0.4*exp(-1*seq(100, 300, 20)/110), rep(0, 6))
  dispersal_data$dispersal_rate <- dispersal_rate_classes[dispersal_data$distance_class]
  original_distance_class_map <- array(NA, c(16, 16))
  original_distance_class_map[as.matrix(dispersal_gen$distance_data$base[, c("target_pop", "source_pop")])] <-
    dispersal_gen$distance_data$base$distance_class
  dispersal_data <- dispersal_data[which(dispersal_data$dispersal_rate > 0), c("target_pop", "source_pop", "dispersal_rate")]
  dispersal_matrix <- array(0, c(16, 16))
  dispersal_matrix[as.matrix(dispersal_data[c("target_pop", "source_pop")])] <- dispersal_data$dispersal_rate
  multipliers <- 0.4/colSums(dispersal_matrix)
  dispersal_matrix <- dispersal_matrix*matrix(multipliers, nrow = 16, ncol = 16, byrow = TRUE)
  #   apply first change
  dispersal_matrix[as.matrix(dispersal_gen$distance_data$changes[[1]][, c("target_pop", "source_pop")])] <-
    dispersal_rate_classes[dispersal_gen$distance_data$changes[[1]]$distance_class]*multipliers[dispersal_gen$distance_data$changes[[1]]$source_pop]
  dispersal_data$dispersal_rate = dispersal_matrix[as.matrix(dispersal_data[c("target_pop", "source_pop")])]
  dispersal_data <- list(dispersal_data)
  change_1_dispersal_matrix <- dispersal_matrix
  #    apply subsequent changes
  for (i in 2:10) {
    dispersal_matrix[as.matrix(dispersal_gen$distance_data$changes[[i]][, c("target_pop", "source_pop")])] <-
      dispersal_rate_classes[dispersal_gen$distance_data$changes[[i]]$distance_class]*multipliers[dispersal_gen$distance_data$changes[[i]]$source_pop]
    nonzero_dispersal_indices <- which(original_distance_class_map[as.matrix(dispersal_gen$distance_data$changes[[i]][, c("target_pop", "source_pop")])] <= 11)
    dispersal_data[[i]] <- dispersal_gen$distance_data$changes[[i]][nonzero_dispersal_indices, c("target_pop", "source_pop")]
    dispersal_data[[i]]$dispersal_rate = dispersal_matrix[as.matrix(dispersal_data[[i]])]
  }
  # Calculate dispersal data
  dispersal_gen$calculate_dispersals()
  expect_is(dispersal_gen$dispersal_data, "list")
  expect_length(dispersal_gen$dispersal_data, 10)
  for (i in 2:10) {
    expect_equal(dispersal_gen$dispersal_data[[i]][c("target_pop", "source_pop", "dispersal_rate")], dispersal_data[[i]])
  }
  # Calculate dispersal matrix (t = 1 friction only)
  dispersal_gen$calculate_dispersals(type = "matrix")
  expect_equal(dispersal_gen$dispersal_matrix, change_1_dispersal_matrix)
  # Calculate dispersal data (t = 1 friction via NAs)
  region <- Region$new(coordinates = coordinates[-c(2, 5, 6),]) # NA cells to isolate (1, 1)
  dispersal_gen <- DispersalGenerator$new(region = region, distance_classes = seq(100, 400, 20),
                                          proportion = 0.4, breadth = 110, max_distance = 300)
  distance_matrix <- geosphere::distm(region$coordinates, region$coordinates, fun = geosphere::distGeo)/1000
  dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix)
  dispersal_gen$calculate_dispersals()
  expected_dispersal_data <- dispersal_gen$dispersal_data[[1]]
  expected_dispersal_data <- expected_dispersal_data[-which(expected_dispersal_data$target_pop == 1 |
                                                              expected_dispersal_data$source_pop == 1),]
  dispersal_matrix <- array(0, c(13, 13))
  dispersal_matrix[as.matrix(expected_dispersal_data[, 1:2])] <- expected_dispersal_data$dispersal_rate
  dispersal_matrix[, 2:13] <- dispersal_matrix[, 2:13]*array(rep(0.4/colSums(dispersal_matrix)[2:13], each = 13), c(13, 12))
  expected_dispersal_data$dispersal_rate <- dispersal_matrix[as.matrix(expected_dispersal_data[, 1:2])]
  dispersal_gen$distance_data <- NULL
  dispersal_gen$dispersal_friction <- DispersalFriction$new(region = region)
  dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix)
  dispersal_gen$calculate_dispersals()
  expect_equal(dispersal_gen$dispersal_data[[1]]$dispersal_rate, expected_dispersal_data$dispersal_rate)
})

test_that("cloning and generation", {
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)/1000
  dispersal_gen <- DispersalGenerator$new(coordinates = coordinates, distance_classes = seq(100, 400, 20))
  expect_equal(dispersal_gen$generative_requirements, list(dispersal_data = "default"))
  expect_false(dispersal_gen$generative_requirements_satisfied()[[1]])
  dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix)
  # Cloning
  dispersal_clone <- dispersal_gen$new_clone(proportion = 0.4, breadth = 110, max_distance = 300)
  expect_true(dispersal_clone$generative_requirements_satisfied()[[1]])
  expect_is(dispersal_clone$dispersal_data[[1]], "data.frame") # generated
  expect_equal(dim(dispersal_clone$dispersal_data[[1]]), c(164, 5))
  expect_null(dispersal_clone$error_messages)
  # Generation
  expect_equal(dispersal_gen$generate(),
               list(error_messages = "dispersal generation requires inputs: dispersal_proportion, dispersal_breadth, dispersal_max_distance"))
  generated_output <- dispersal_gen$generate(input_values = list(proportion = 0.4, breadth = 110, max_distance = 300))
  expect_named(generated_output, "dispersal_data")
  expect_equal(generated_output$dispersal_data, dispersal_clone$dispersal_data)
  # Fixed function parameters
  dispersal_gen <- DispersalGenerator$new(coordinates = coordinates, distance_classes = seq(100, 400, 20),
                                          inputs = "breadth", dispersal_proportion = 0.4)
  dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix)
  # Cloning
  dispersal_clone <- dispersal_gen$new_clone(breadth = 120)
  expect_true(dispersal_clone$generative_requirements_satisfied()[[1]])
  expect_is(dispersal_clone$dispersal_data[[1]], "data.frame") # generated
  expect_equal(dim(dispersal_clone$dispersal_data[[1]]), c(228, 5))
  expect_null(dispersal_clone$error_messages)
  # Generation
  expect_equal(dispersal_gen$generate(),
               list(error_messages = "dispersal generation requires inputs: breadth"))
  generated_output <- dispersal_gen$generate(input_values = list(breadth = 120))
  expect_named(generated_output, "dispersal_data")
  expect_equal(generated_output$dispersal_data, dispersal_clone$dispersal_data)
})
