context("Dispersal Friction")

test_that("initialization and parameter setting", {
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  # Region defined as coordinates not raster
  expect_silent(dispersal_friction <- DispersalFriction$new(coordinates = coordinates))
  expect_false(dispersal_friction$region$use_raster)
  expect_error(dispersal_friction <- DispersalFriction$new(region = coordinates),
    "Region should be a Region (or inherited class) object",
    fixed = TRUE
  )
  expect_warning(
    dispersal_friction <- DispersalFriction$new(region = Region$new()),
    "Spatial region has not been defined within the region object"
  )
  # Region defined as raster
  dispersal_friction <- DispersalFriction$new(region = Region$new(coordinates = coordinates))
  expect_true(dispersal_friction$region$use_raster)
  # Consistency of region/coordinates and conductance matrix values
  expect_error(
    dispersal_friction$conductance <- array(1, c(15, 10)),
    "Conductance matrix dimensions must be consistent with region/coordinates"
  )
  expect_silent(dispersal_friction$conductance <- array(1, c(16, 10)))
  expect_error(
    dispersal_friction$coordinates <- coordinates[1:15, ],
    "Region coordinates must be consistent with conductance matrix dimensions"
  )
  expect_equal(dispersal_friction$coordinates, coordinates) # unchanged
  expect_error(
    dispersal_friction$region <- Region$new(coordinates = coordinates[1:15, ]),
    "Region must be consistent with conductance matrix dimensions"
  )
  expect_equal(dispersal_friction$coordinates, coordinates) # unchanged
  expect_equal(nrow(dispersal_friction$conductance), dispersal_friction$region$region_cells)
  # Consistency of region/coordinates and conductance raster values
  dispersal_friction$region$use_raster <- TRUE
  raster2 <- raster::raster(
    vals = rep(1, 16), nrows = 4, ncol = 4,
    xmn = 0, xmx = 400000, ymn = 0, ymx = 400000,
    crs = "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
  expect_error(
    dispersal_friction$conductance <- raster::stack(replicate(10, raster2)),
    "Conductance raster must be consistent with the defined region raster"
  )
  dispersal_friction$conductance <- raster::stack(replicate(10, +(dispersal_friction$region$region_raster > 0)))
  expect_error(
    dispersal_friction$coordinates <- coordinates[1:15, ],
    "Region coordinates must be consistent with the conductance raster"
  )
  expect_error(
    dispersal_friction$region <- Region$new(coordinates = coordinates[1:15, ]),
    "Region must be consistent with the conductance raster"
  )
  expect_equal(raster::ncell(dispersal_friction$conductance), dispersal_friction$region$region_cells)
  # Write to directory
  expect_error(dispersal_friction$write_to_dir <- test_path("no_such_dir"),
    "Dispersal friction: write_to_dir must be a existing directory path (string)",
    fixed = TRUE
  )
  expect_silent(dispersal_friction$write_to_dir <- test_path("test_inputs"))
})

test_that("distance multiplier calculation errors", {
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  # No coordinates or conductance matrix or dispersal indices
  dispersal_friction <- DispersalFriction$new()
  expect_error(
    dispersal_friction$calculate_distance_multipliers(),
    "Distance multipliers calculation requires region/coordinates to be set first"
  )
  dispersal_friction$coordinates <- data.frame(x = 1, y = 1)[-1, ] # no cells
  expect_error(
    dispersal_friction$calculate_distance_multipliers(),
    "Distance multipliers calculation requires region/coordinates to be set first"
  )
  # Dispersal indices incorrect or not consistent with coordinates
  distance_matrix <- fossil::earth.dist(coordinates, dist = FALSE)*1000
  dispersal_indices <- which(distance_matrix > 0 & distance_matrix <= 350000, arr.ind = TRUE)
  colnames(dispersal_indices) <- c("target_pop", "source_pop")
  dispersal_friction <- DispersalFriction$new(
    coordinates = coordinates,
    conductance = array(1, c(16, 10))
  )
  expect_error(
    dispersal_friction$calculate_distance_multipliers(dispersal_indices = "wrong"),
    "Dispersal indices must be a two-column matrix representing the
        target and source coordinate index for each in-range migration, or a
        data.frame or array that can be converted to such a two-column matrix"
  )
  expect_error(
    dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices - 1),
    "Dispersal indices must be a two-column matrix representing the
        target and source coordinate index for each in-range migration, or a
        data.frame or array that can be converted to such a two-column matrix"
  )
  expect_error(
    dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices + 1),
    "Dispersal indices must be a two-column matrix representing the
        target and source coordinate index for each in-range migration, or a
        data.frame or array that can be converted to such a two-column matrix"
  )
  expect_error(
    dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices / 3),
    "Dispersal indices must be a two-column matrix representing the
        target and source coordinate index for each in-range migration, or a
        data.frame or array that can be converted to such a two-column matrix"
  )
  expect_error(
    dispersal_friction$calculate_distance_multipliers(dispersal_indices = as.vector(dispersal_indices)),
    "Dispersal indices must be a two-column matrix representing the
        target and source coordinate index for each in-range migration, or a
        data.frame or array that can be converted to such a two-column matrix"
  )
  expect_error(
    dispersal_friction$calculate_distance_multipliers(dispersal_indices = cbind(dispersal_indices, dispersal_indices[, 1])),
    "Dispersal indices must be a two-column matrix representing the
        target and source coordinate index for each in-range migration, or a
        data.frame or array that can be converted to such a two-column matrix"
  )
  dispersal_indices <- which(distance_matrix > 0, arr.ind = TRUE)
  expect_error(
    dispersal_friction$calculate_distance_multipliers(dispersal_indices = rbind(dispersal_indices, dispersal_indices)),
    "Dispersal indices must be a two-column matrix representing the
        target and source coordinate index for each in-range migration, or a
        data.frame or array that can be converted to such a two-column matrix"
  )
})

test_that("distance multiplier calculations", {
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  distance_matrix <- fossil::earth.dist(coordinates, dist = FALSE)
  dispersal_indices <- which(distance_matrix > 1 & distance_matrix <= 350, arr.ind = TRUE)
  colnames(dispersal_indices) <- c("target_pop", "source_pop")
  conductance_matrix <- array(1, c(16, 10))
  conductance_matrix[c(2, 3, 5, 6, 7, 9, 10, 11), 2] <- 0 # isolate coordinate (1, 1)
  conductance_matrix[, 3] <- array(c(0, 0.2, 0, 0.8), 16) # frictional landscape columns
  # Non-raster region and conductance values
  dispersal_friction <- DispersalFriction$new(
    coordinates = coordinates,
    conductance = conductance_matrix
  )
  distance_multipliers <- dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices)
  expect_equal(length(distance_multipliers), 10)
  expect_equal(distance_multipliers[[1]], rep(1, nrow(dispersal_indices)))
  # Isolated coordinate (1, 1) at t = 2
  neighbour_indices <- which(dispersal_indices[, "source_pop"] == 1 & dispersal_indices[, "target_pop"] %in% c(2, 5, 6))
  other_indices <- which(dispersal_indices[, "source_pop"] == 1 & !dispersal_indices[, "target_pop"] %in% c(2, 5, 6))
  expect_equal(distance_multipliers[[2]][neighbour_indices], rep(2, length(neighbour_indices)))
  expect_equal(distance_multipliers[[2]][other_indices], rep(Inf, length(other_indices)))
  # Indirect route from coordinate 8:(4, 2) to 14:(2, 4) at t = 2
  route_8_to_14_index <- which(dispersal_indices[, "source_pop"] == 8 & dispersal_indices[, "target_pop"] == 14)
  expect_true(distance_multipliers[[2]][route_8_to_14_index] > 1 && distance_multipliers[[2]][route_8_to_14_index] < 1.5)
  # Frictional landscape columns at t = 3
  route_1_to_2_index <- which(dispersal_indices[, "source_pop"] == 1 & dispersal_indices[, "target_pop"] == 2)
  expect_equal(distance_multipliers[[3]][route_1_to_2_index], 1 / mean(c(0, 0.2)))
  route_2_to_6_index <- which(dispersal_indices[, "source_pop"] == 2 & dispersal_indices[, "target_pop"] == 6)
  expect_equal(distance_multipliers[[3]][route_2_to_6_index], 1 / 0.2)
  route_2_to_14_index <- which(dispersal_indices[, "source_pop"] == 2 & dispersal_indices[, "target_pop"] == 14)
  expect_equal(distance_multipliers[[3]][route_2_to_14_index], 1 / 0.2)
  route_3_to_8_index <- which(dispersal_indices[, "source_pop"] == 3 & dispersal_indices[, "target_pop"] == 8)
  expect_equal(distance_multipliers[[3]][route_3_to_8_index], 1 / mean(c(0, 0.8)))
  route_4_to_12_index <- which(dispersal_indices[, "source_pop"] == 4 & dispersal_indices[, "target_pop"] == 12)
  expect_equal(distance_multipliers[[3]][route_4_to_12_index], 1 / 0.8)
  # Raster region and conductance values with longlat coordinates
  region <- Region$new(coordinates = coordinates)
  conductance <- raster::stack(replicate(10, region$region_raster))
  conductance[region$region_indices] <- conductance_matrix
  dispersal_friction <- DispersalFriction$new(region = region, conductance = conductance)
  expect_equal(
    dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices),
    distance_multipliers
  )
  # Raster region and conductance values with coordinates in meters
  region <- Region$new(region_raster = raster::raster(
    vals = 1:16, nrows = 4, ncol = 4,
    xmn = 0, xmx = 400000, ymn = 0, ymx = 400000,
    crs = "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  ))
  conductance <- raster::stack(replicate(10, region$region_raster))
  conductance[region$region_indices] <- conductance_matrix
  dispersal_friction <- DispersalFriction$new(region = region, conductance = conductance)
  distance_multipliers2 <- dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices)
  expect_equal(distance_multipliers2[-c(2, 3)], distance_multipliers[-c(2, 3)])
  expect_false(all(unlist(distance_multipliers2[c(2, 3)]) == unlist(distance_multipliers[c(2, 3)])))
  # Isolated coordinate (1, 1) at t = 2
  expect_equal(distance_multipliers2[[2]][neighbour_indices], rep(2, length(neighbour_indices)))
  expect_equal(distance_multipliers2[[2]][other_indices], rep(Inf, length(other_indices)))
  # Indirect route from coordinate 8:(4, 2) to 14:(2, 4) at t = 2
  expect_true(distance_multipliers2[[2]][route_8_to_14_index] > 1 && distance_multipliers2[[2]][route_8_to_14_index] < 1.5)
  # Frictional landscape columns at t = 3
  expect_equal(distance_multipliers2[[3]][route_1_to_2_index], 1 / mean(c(0, 0.2)))
  expect_equal(distance_multipliers2[[3]][route_2_to_6_index], 1 / 0.2)
  expect_equal(distance_multipliers2[[3]][route_2_to_14_index], 1 / 0.2)
  expect_equal(distance_multipliers2[[3]][route_3_to_8_index], 1 / mean(c(0, 0.8)))
  expect_equal(distance_multipliers2[[3]][route_4_to_12_index], 1 / 0.8)
  # Write multipliers to directory
  dispersal_friction$write_to_dir <- tempdir()
  distance_multipliers3 <- dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices)
  expect_equal(
    distance_multipliers3,
    lapply(1:10, function(i) file.path(tempdir(), sprintf("multipliers_%s.RData", i)))
  )
  expect_equal(
    lapply(distance_multipliers3, function(p) readRDS(p)),
    distance_multipliers2
  )
  # No conductance values uses region raster set to 1 for non-NA values
  coordinates <- coordinates[-c(2, 5, 6), ] # isolate (1, 1)
  region <- Region$new(coordinates = coordinates)
  distance_matrix <- fossil::earth.dist(coordinates, dist = FALSE)
  dispersal_indices <- which(distance_matrix > 0 & distance_matrix <= 350, arr.ind = TRUE)
  colnames(dispersal_indices) <- c("target_pop", "source_pop")
  dispersal_friction <- DispersalFriction$new(region = region)
  distance_multipliers3 <- dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices)
  expect_true(all(is.infinite(distance_multipliers3[[1]][which(dispersal_indices[, 1] == 1)])))
  expect_true(all(distance_multipliers3[[1]][which(dispersal_indices[, 1] != 1)] >= 1))
  expect_true(distance_multipliers3[[1]][which(dispersal_indices[, 1] == 2 & dispersal_indices[, 2] == 6)] > 1)
  expect_true(distance_multipliers3[[1]][which(dispersal_indices[, 1] == 6 & dispersal_indices[, 2] == 2)] > 1)
})
