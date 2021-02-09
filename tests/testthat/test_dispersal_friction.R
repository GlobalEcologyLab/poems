context("Dispersal Friction")

test_that("initialization and parameter setting", {
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  # Region defined as coordinates not raster
  expect_silent(dispersal_friction <- DispersalFriction$new(coordinates = coordinates))
  expect_false(dispersal_friction$region$use_raster)
  expect_error(dispersal_friction <- DispersalFriction$new(region = coordinates),
               "Region should be a Region (or inherited class) object", fixed = TRUE)
  expect_warning(dispersal_friction <- DispersalFriction$new(region = Region$new()),
                 "Spatial region has not been defined within the region object")
  # Region defined as raster
  dispersal_friction <- DispersalFriction$new(region = Region$new(coordinates = coordinates))
  expect_true(dispersal_friction$region$use_raster)
  # Consistency of region/coordinates and friction matrix values
  expect_error(dispersal_friction$friction_values <- array(1, c(16, 10)),
               "Friction values must be a raster layer, stack or brick consistent with the defined region")
  dispersal_friction$region$use_raster <- FALSE
  expect_error(dispersal_friction$friction_values <- array(1, c(15, 10)),
               "Friction values matrix dimensions must be consistent with region/coordinates")
  expect_silent(dispersal_friction$friction_values <- array(1, c(16, 10)))
  expect_error(dispersal_friction$coordinates <- coordinates[1:15,],
               "Region coordinates must be consistent with friction matrix dimensions")
  expect_equal(dispersal_friction$coordinates, coordinates) # unchanged
  expect_error(dispersal_friction$region <-  Region$new(coordinates = coordinates[1:15,]),
               "Region must be consistent with friction matrix dimensions")
  expect_equal(dispersal_friction$coordinates, coordinates) # unchanged
  expect_equal(nrow(dispersal_friction$friction_values), dispersal_friction$region$region_cells)
  # Consistency of region/coordinates and friction raster values
  dispersal_friction$region$use_raster <- TRUE
  raster2 <- raster::raster(vals = rep(1, 16), nrows = 4, ncol = 4,
                            xmn = 0, xmx = 400000, ymn = 0, ymx = 400000, crs = "+proj=utm +ellps=GRS80 +datum=WGS84")
  expect_error(dispersal_friction$friction_values <- raster::stack(replicate(10, raster2)),
               "Friction values raster must be consistent with the defined region raster")
  dispersal_friction$friction_values <- raster::stack(replicate(10, +(dispersal_friction$region$region_raster > 0)))
  expect_error(dispersal_friction$coordinates <- coordinates[1:15,],
               "Region coordinates must be consistent with the friction values raster")
  expect_error(dispersal_friction$region <-  Region$new(coordinates = coordinates[1:15,]),
               "Region must be consistent with the friction values raster")
  expect_equal(raster::ncell(dispersal_friction$friction_values), dispersal_friction$region$region_cells)
})

test_that("distance multiplier calculation errors", {
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  # No coordinates or friction matrix or dispersal indices
  dispersal_friction <- DispersalFriction$new()
  expect_error(dispersal_friction$calculate_distance_multipliers(),
               "Distance multipliers calculation requires region/coordinates to be set first")
  dispersal_friction$coordinates <- data.frame(x = 1, y = 1)[-1,] # no cells
  expect_error(dispersal_friction$calculate_distance_multipliers(),
               "Distance multipliers calculation requires region/coordinates to be set first")
  # Dispersal indices incorrect or not consistent with coordinates
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)
  dispersal_indices <- which(distance_matrix > 0 & distance_matrix <= 350000, arr.ind = TRUE)
  colnames(dispersal_indices) <- c("target_pop", "source_pop")
  dispersal_friction <- DispersalFriction$new(coordinates = coordinates,
                                         friction_values = array(1, c(16, 10)))
  expect_error(dispersal_friction$calculate_distance_multipliers(dispersal_indices = "wrong"),
               "Dispersal indices must be a two-column matrix representing the target and source coordinate index for each in-range migration")
  expect_error(dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices - 1),
               "Dispersal indices must be a two-column matrix representing the target and source coordinate index for each in-range migration")
  expect_error(dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices + 1),
               "Dispersal indices must be a two-column matrix representing the target and source coordinate index for each in-range migration")
  expect_error(dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices/3),
               "Dispersal indices must be a two-column matrix representing the target and source coordinate index for each in-range migration")
  expect_error(dispersal_friction$calculate_distance_multipliers(dispersal_indices = as.vector(dispersal_indices)),
               "Dispersal indices must be a two-column matrix representing the target and source coordinate index for each in-range migration")
  expect_error(dispersal_friction$calculate_distance_multipliers(dispersal_indices = cbind(dispersal_indices, dispersal_indices[,1])),
               "Dispersal indices must be a two-column matrix representing the target and source coordinate index for each in-range migration")
  dispersal_indices <- which(distance_matrix > 0, arr.ind = TRUE)
  expect_error(dispersal_friction$calculate_distance_multipliers(dispersal_indices = rbind(dispersal_indices, dispersal_indices)),
               "Dispersal indices must be a two-column matrix representing the target and source coordinate index for each in-range migration")
})

test_that("distance multiplier calculations", {
  coordinates <- data.frame(x = rep(1:4, 4), y = rep(1:4, each = 4))
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)/1000
  dispersal_indices <- which(distance_matrix > 0 & distance_matrix <= 350, arr.ind = TRUE)
  colnames(dispersal_indices) <- c("target_pop", "source_pop")
  barrier_friction_matrix = array(1, c(16, 10))
  barrier_friction_matrix[c(2, 3, 5, 6, 7, 9, 10, 11), 2] <- 0 # isolate coordinate (1, 1)
  barrier_friction_matrix[, 3] = array(c(0, 0.2, 0, 0.8), 16) # frictional landscape columns
  # Non-raster region and barrier friction values
  dispersal_friction <- DispersalFriction$new(coordinates = coordinates,
                                         friction_values = barrier_friction_matrix)
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
  expect_equal(distance_multipliers[[3]][route_1_to_2_index], 1/mean(c(0, 0.2)))
  route_2_to_6_index <- which(dispersal_indices[, "source_pop"] == 2 & dispersal_indices[, "target_pop"] == 6)
  expect_equal(distance_multipliers[[3]][route_2_to_6_index], 1/0.2)
  route_2_to_14_index <- which(dispersal_indices[, "source_pop"] == 2 & dispersal_indices[, "target_pop"] == 14)
  expect_equal(distance_multipliers[[3]][route_2_to_14_index], 1/0.2)
  route_3_to_8_index <- which(dispersal_indices[, "source_pop"] == 3 & dispersal_indices[, "target_pop"] == 8)
  expect_equal(distance_multipliers[[3]][route_3_to_8_index], 1/mean(c(0, 0.8)))
  route_4_to_12_index <- which(dispersal_indices[, "source_pop"] == 4 & dispersal_indices[, "target_pop"] == 12)
  expect_equal(distance_multipliers[[3]][route_4_to_12_index], 1/0.8)
  # Raster region and barrier friction values with longlat coordinates
  region <-  Region$new(coordinates = coordinates)
  friction_values <- raster::stack(replicate(10, region$region_raster))
  friction_values[region$region_indices] <- barrier_friction_matrix
  dispersal_friction <- DispersalFriction$new(region = region, friction_values = friction_values)
  expect_equal(dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices),
               distance_multipliers)
  # Raster region and barrier friction values with coordinates in metres
  region <-  Region$new(region_raster = raster::raster(vals = 1:16, nrows = 4, ncol = 4,
                                                       xmn = 0, xmx = 400000, ymn = 0, ymx = 400000,
                                                       crs = "+proj=utm +ellps=GRS80 +datum=WGS84"))
  friction_values <- raster::stack(replicate(10, region$region_raster))
  friction_values[region$region_indices] <- barrier_friction_matrix
  dispersal_friction <- DispersalFriction$new(region = region, friction_values = friction_values)
  distance_multipliers2 <- dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices)
  expect_equal(distance_multipliers2[-c(2, 3)], distance_multipliers[-c(2, 3)])
  expect_false(all(unlist(distance_multipliers2[c(2, 3)]) == unlist(distance_multipliers[c(2, 3)])))
  # Isolated coordinate (1, 1) at t = 2
  expect_equal(distance_multipliers2[[2]][neighbour_indices], rep(2, length(neighbour_indices)))
  expect_equal(distance_multipliers2[[2]][other_indices], rep(Inf, length(other_indices)))
  # Indirect route from coordinate 8:(4, 2) to 14:(2, 4) at t = 2
  expect_true(distance_multipliers2[[2]][route_8_to_14_index] > 1 && distance_multipliers2[[2]][route_8_to_14_index] < 1.5)
  # Frictional landscape columns at t = 3
  expect_equal(distance_multipliers2[[3]][route_1_to_2_index], 1/mean(c(0, 0.2)))
  expect_equal(distance_multipliers2[[3]][route_2_to_6_index], 1/0.2)
  expect_equal(distance_multipliers2[[3]][route_2_to_14_index], 1/0.2)
  expect_equal(distance_multipliers2[[3]][route_3_to_8_index], 1/mean(c(0, 0.8)))
  expect_equal(distance_multipliers2[[3]][route_4_to_12_index], 1/0.8)
  # No friction values uses region raster set to 1 for non-NA values
  coordinates <- coordinates[-c(2, 5, 6),] # isolate (1, 1)
  region <-  Region$new(coordinates = coordinates)
  distance_matrix <- geosphere::distm(coordinates, coordinates, fun = geosphere::distGeo)/1000
  dispersal_indices <- which(distance_matrix > 0 & distance_matrix <= 350, arr.ind = TRUE)
  colnames(dispersal_indices) <- c("target_pop", "source_pop")
  dispersal_friction <- DispersalFriction$new(region = region)
  distance_multipliers3 <- dispersal_friction$calculate_distance_multipliers(dispersal_indices = dispersal_indices)
  expect_true(all(is.infinite(distance_multipliers3[[1]][which(dispersal_indices[, 1] == 1)])))
  expect_true(all(distance_multipliers3[[1]][which(dispersal_indices[, 1] != 1)] >= 1))
  expect_true(distance_multipliers3[[1]][which(dispersal_indices[, 1] == 2 & dispersal_indices[, 2] == 6)] > 1)
  expect_true(distance_multipliers3[[1]][which(dispersal_indices[, 1] == 6 & dispersal_indices[, 2] == 2)] > 1)
})
