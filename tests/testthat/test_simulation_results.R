context("Simulation Results")

test_that("initialization and parameter setting", {
  TEST_DIRECTORY <- test_path("test_results")
  # Default initialization
  sim_results <- SimulationResults$new()
  expect_is(sim_results$all, "SimulationResults")
  expect_null(sim_results$all$all)
  expect_null(sim_results$parent)
  expect_is(sim_results$all$parent, "SimulationResults")
  # Attributes
  attributes <- c("region", "time_steps", "burn_in_steps", "occupancy_mask", "error_messages", "warning_messages")
  expect_equal(sim_results$get_attribute_names(), attributes)
  expect_equal(sim_results$get_attribute_names(all = TRUE), c(attributes, paste0("all$", attributes)))
  # Invalid/missing results file
  expect_error(sim_results <- SimulationResults$new(results = "dummy"), "Could not read results from dummy")
  expect_error(sim_results <- SimulationResults$new(results = 1:5), "Could not read results from type/class integer")
  expect_error(
    sim_results <- SimulationResults$new(results = file.path(TEST_DIRECTORY, "no_such_file.RData")),
    paste("Could not read results from", file.path(TEST_DIRECTORY, "no_such_file.RData"))
  )
  # Valid results file
  sim_results <- SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_1_results.RData"))
  expect_true("abundance" %in% sim_results$get_attribute_names())
  expect_false("all$abundance" %in% sim_results$get_attribute_names(all = TRUE))
  expect_named(sim_results$attached, "abundance")
  expect_equal(sim_results$all$attached, list())
  abundance <- sim_results$attached$abundance
  # Initialize from list
  sim_results <- SimulationResults$new(results = list(abundance = abundance))
  expect_named(sim_results$get_attributes(), "abundance")
  # Dynamically generate abundance across all cells
  all_abundance <- colSums(abundance)
  expect_equal(sim_results$all$get_attributes(), list(abundance = all_abundance)) # method 1
  expect_equal(sim_results$all$attached, list(abundance = all_abundance))
  expect_true("all$abundance" %in% sim_results$get_attribute_names(all = TRUE))
  sim_results$all$attached$abundance <- NULL # reset
  expect_equal(sim_results$all$get_attributes("abundance"), list(abundance = all_abundance)) # method 2
  sim_results$all$attached$abundance <- NULL # reset
  expect_equal(sim_results$all$get_attribute("abundance"), all_abundance) # method 3
  sim_results$all$attached$abundance <- NULL # reset
  expect_equal(sim_results$get_attributes("all$abundance"), list("all$abundance" = all_abundance)) # method 4
  expect_equal(sim_results$all$attached, list(abundance = all_abundance))
  sim_results$all$attached$abundance <- NULL # reset
  expect_equal(sim_results$get_attribute("all$abundance"), all_abundance) # method 5
  # Set (primative function) default
  sim_results$default <- 55 # value
  expect_equal(sim_results$default, 55)
  sim_results$default <- "all$abundance" # attribute
  expect_equal(sim_results$default, all_abundance)
})

test_that("apply occupancy mask and burn-in", {
  TEST_DIRECTORY <- test_path("test_results")
  sim_results <- SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_1_results.RData"))
  abundance <- sim_results$attached$abundance
  # Check occupancy mask
  occupancy_mask <- array(1, c(5, 10))
  occupancy_mask[2:3, 1:5] <- 0
  expect_error(
    sim_results <- SimulationResults$new(time_steps = 12, occupancy_mask = occupancy_mask),
    "The number of occupancy mask layers/columns must be one or match the number of time steps"
  )
  sim_results <- SimulationResults$new(
    time_steps = 10, occupancy_mask = occupancy_mask,
    results = list(abundance = abundance)
  )
  expect_equal(sim_results$get_attributes(), list(
    time_steps = 10, occupancy_mask = occupancy_mask,
    abundance = abundance * occupancy_mask
  ))
  expect_equal(sim_results$all$get_attributes(), list(
    time_steps = 10,
    abundance = colSums(abundance * occupancy_mask)
  ))
  expect_equal(sim_results$attached, list(abundance = abundance)) # unmasked
  expect_equal(sim_results$all$attached, list(abundance = colSums(abundance * occupancy_mask))) # sum of masked
  sim_results$attached$abundance <- abundance[, 1:8]
  expect_named(sim_results$get_attributes(), c("time_steps", "occupancy_mask", "abundance", "error_messages"))
  expect_equal(
    sim_results$get_attributes()$error_messages,
    "The column/layer dimension of the occupancy mask and the abundance result are inconsistent"
  )
  sim_results$attached$abundance <- abundance
  sim_results$error_messages <- NULL
  # Checks burn-in
  expect_error(
    sim_results <- SimulationResults$new(time_steps = 10, burn_in_steps = 10),
    "Burn-in must be less than the number of simulation time steps"
  )
  sim_results <- SimulationResults$new(
    time_steps = 10, occupancy_mask = occupancy_mask, burn_in_steps = 2,
    results = list(abundance = abundance)
  )
  expect_equal(sim_results$get_attributes(), list(
    time_steps = 10, burn_in_steps = 2, occupancy_mask = occupancy_mask,
    abundance = (abundance * occupancy_mask)[, 3:10]
  ))
  expect_equal(sim_results$all$get_attributes(), list(
    time_steps = 10, burn_in_steps = 2,
    abundance = colSums(abundance * occupancy_mask)[3:10]
  ))
  expect_equal(sim_results$attached, list(abundance = abundance)) # unmasked with burn-in steps included
  expect_equal(sim_results$all$attached, list(abundance = colSums(abundance * occupancy_mask))) # sum of masked with burn-in included
})

test_that("set (region) raster results", {
  TEST_DIRECTORY <- test_path("test_results")
  coordinates <- data.frame(x = c(1, 3, 2, 2, 3), y = c(3, 3, 2, 1, 1))
  region <- Region$new(coordinates = coordinates)
  sim_results <- SimulationResults$new(time_steps = 10, results = file.path(TEST_DIRECTORY, "sample_1_results.RData"))
  abundance <- sim_results$attached$abundance
  # Non-raster data (invalid)
  sim_results <- SimulationResults$new(
    region = region, time_steps = 10,
    results = file.path(TEST_DIRECTORY, "sample_1_results.RData")
  )
  expect_equal(
    sim_results$error_messages,
    "The abundance result must be a raster layer, stack or brick (consistent with the defined region)"
  )
  expect_equal(sim_results$attached, list()) # empty
  sim_results$error_messages <- NULL
  # Invalid set raster attribute
  sim_results$set_attributes(list(abundance = abundance))
  expect_equal(
    sim_results$error_messages,
    "The abundance result must be a raster layer, stack or brick (consistent with the defined region)"
  )
  abundance_raster <- raster::stack(replicate(10, 1 * (region$region_raster > 0)))
  raster::crs(abundance_raster) <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  abundance_raster[region$region_indices] <- abundance
  sim_results$set_attributes(list(abundance = abundance_raster))
  expect_equal(
    sim_results$error_messages[2],
    "The abundance result is not consistent with the defined region raster"
  )
  raster::crs(abundance_raster) <- raster::crs(region$region_raster)
  sim_results$set_attributes(list(abundance = raster::stack(replicate(12, abundance_raster[[1]]))))
  expect_equal(
    sim_results$error_messages[3],
    "The number of raster layers in the abundance result must be one or match the number of time steps"
  )
  expect_equal(sim_results$attached, list()) # empty
  sim_results$error_messages <- NULL
  # Valid set raster attribute
  sim_results$set_attributes(list(abundance = abundance_raster))
  expect_null(sim_results$error_messages)
  expect_equal(unname(sim_results$attached$abundance[region$region_indices]), abundance)
})

test_that("set (region) raster occupancy mask", {
  coordinates <- data.frame(x = c(1, 3, 2, 2, 3), y = c(3, 3, 2, 1, 1))
  region <- Region$new(coordinates = coordinates)
  # Invalid occupancy masks
  occupancy_mask <- array(1, c(5, 10))
  occupancy_mask[2:3, 1:5] <- 0
  expect_error(
    sim_results <- SimulationResults$new(region = region, time_steps = 10, occupancy_mask = occupancy_mask),
    "Occupancy mask must be a raster layer, stack or brick consistent with the defined region"
  )
  raster_occupancy_mask <- raster::raster(
    vals = +(region$region_raster[] > 0),
    nrows = 3, ncol = 3,
    xmn = 0, xmx = 3000, ymn = 0, ymx = 3000,
    crs = "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
  expect_error(
    sim_results <- SimulationResults$new(
      region = region, time_steps = 10,
      occupancy_mask = raster_occupancy_mask
    ),
    "Occupancy mask raster must be consistent with the defined region raster"
  )
  raster_occupancy_mask <- 1 * (region$region_raster > 0)
  expect_error(
    sim_results <- SimulationResults$new(
      region = region, time_steps = 10,
      occupancy_mask = raster::stack(replicate(12, raster_occupancy_mask))
    ),
    "The number of occupancy mask layers/columns must be one or match the number of time steps"
  )
  region$use_raster <- FALSE
  expect_error(sim_results <- SimulationResults$new(region = region, time_steps = 10, occupancy_mask = occupancy_mask[1:4, ]),
    "The number of occupancy mask rows must be consistent with the region (finite) cells",
    fixed = TRUE
  )
  # Valid occupancy masks
  sim_results <- SimulationResults$new(region = region, time_steps = 10, occupancy_mask = occupancy_mask)
  region$use_raster <- TRUE
  raster_occupancy_mask[region$region_indices] <- occupancy_mask[, 1]
  sim_results <- SimulationResults$new(region = region, time_steps = 10, occupancy_mask = raster_occupancy_mask)
  raster_occupancy_mask <- raster::stack(replicate(10, 1 * (region$region_raster > 0)))
  raster_occupancy_mask[region$region_indices] <- occupancy_mask
  sim_results <- SimulationResults$new(region = region, time_steps = 10, occupancy_mask = raster_occupancy_mask)
  expect_equal(unname(sim_results$occupancy_mask[region$region_indices]), occupancy_mask)
})


test_that("apply occupancy mask to raster results", {
  TEST_DIRECTORY <- test_path("test_results", "raster")
  coordinates <- data.frame(x = c(1, 3, 2, 2, 3), y = c(3, 3, 2, 1, 1))
  region <- Region$new(coordinates = coordinates)
  sim_results <- SimulationResults$new(time_steps = 10, results = file.path(test_path("test_results"), "sample_1_results.RData"))
  abundance <- sim_results$attached$abundance # matrix abundance result
  occupancy_mask <- array(1, c(5, 10))
  occupancy_mask[2:3, 1:5] <- 0
  # Get raster attributes with raster occupancy masks
  raster_occupancy_mask <- 1 * (region$region_raster > 0)
  raster_occupancy_mask[region$region_indices] <- occupancy_mask[, 1]
  sim_results <- SimulationResults$new(
    region = region, time_steps = 10, occupancy_mask = raster_occupancy_mask,
    results = file.path(TEST_DIRECTORY, "sample_1_raster_results.RData")
  )
  expect_equal(unname(sim_results$get_attribute("abundance")[region$region_indices]), abundance * occupancy_mask[, 1])
  expect_equal(sim_results$all$get_attribute("abundance"), colSums(abundance * occupancy_mask[, 1]))
  expect_null(sim_results$error_messages)
  raster_occupancy_mask <- raster::stack(replicate(10, 1 * (region$region_raster > 0)))
  raster_occupancy_mask[region$region_indices] <- occupancy_mask
  sim_results <- SimulationResults$new(
    region = region, time_steps = 10, occupancy_mask = raster_occupancy_mask,
    results = file.path(TEST_DIRECTORY, "sample_1_raster_results.RData")
  )
  expect_equal(unname(sim_results$get_attribute("abundance")[region$region_indices]), abundance * occupancy_mask)
  expect_equal(sim_results$all$get_attribute("abundance"), colSums(abundance * occupancy_mask))
  expect_null(sim_results$error_messages)
  # Get non-raster attributes with raster occupancy mask
  region$use_raster <- FALSE
  abundance_raster <- sim_results$attached$abundance
  sim_results$attached$abundance <- abundance
  expect_equal(sim_results$get_attribute("abundance")[], abundance) # no mask applied
  expect_equal(
    sim_results$error_messages[1],
    "The row/cell dimension of the occupancy mask and the abundance result are inconsistent"
  )
  sim_results$error_messages <- NULL
  # Get raster attributes with non-raster occupancy mask
  sim_results$occupancy_mask <- occupancy_mask
  sim_results$attached$abundance <- abundance_raster
  expect_equal(sim_results$get_attribute("abundance")[], abundance_raster[]) # no mask applied
  expect_equal(
    sim_results$error_messages[1],
    "The row/cell dimension of the occupancy mask and the abundance result are inconsistent"
  )
  sim_results$error_messages <- NULL
  # Get raster attribute with layer inconsistency with occupancy mask
  region$use_raster <- TRUE
  sim_results$occupancy_mask <- raster_occupancy_mask
  sim_results$attached$abundance <- abundance_raster[[1:8]]
  expect_equal(sim_results$get_attribute("abundance")[], abundance_raster[[1:8]][]) # no mask applied
  expect_equal(
    sim_results$error_messages[1],
    "The column/layer dimension of the occupancy mask and the abundance result are inconsistent"
  )
  sim_results$error_messages <- NULL
})

test_that("apply burn-in to raster results", {
  TEST_DIRECTORY <- test_path("test_results", "raster")
  coordinates <- data.frame(x = c(1, 3, 2, 2, 3), y = c(3, 3, 2, 1, 1))
  region <- Region$new(coordinates = coordinates)
  sim_results <- SimulationResults$new(
    region = region, time_steps = 10, burn_in_steps = 2,
    results = file.path(TEST_DIRECTORY, "sample_1_raster_results.RData")
  )
  abundance_raster <- sim_results$attached$abundance
  abundance <- unname(sim_results$attached$abundance[region$region_indices])
  extra1 <- c(1, 2, 3, 4)
  sim_results$all$attached <- list(extra1 = extra1)
  # get attributes with burn-in
  expect_equal(sim_results$get_attribute("abundance")[], abundance_raster[][, 3:10])
  expect_equal(
    sim_results$all$get_attributes(c("abundance", "extra1")),
    list(abundance = colSums(abundance)[3:10], extra1 = extra1)
  ) # burn-in: Yes, No
  expect_null(sim_results$error_messages)
  sim_results$time_steps <- NULL # no steps info
  expect_equal(sim_results$get_attribute("abundance")[], abundance_raster[][, 3:10])
  expect_equal(
    sim_results$all$get_attributes(c("abundance", "extra1")),
    list(abundance = colSums(abundance)[3:10], extra1 = extra1[3:4])
  ) # burn-in: Yes, Yes
  expect_null(sim_results$error_messages)
  sim_results$time_steps <- 12 # inconsistent steps (would error when abundance set)
  expect_equal(sim_results$get_attribute("abundance")[], abundance_raster[][, 3:10])
  expect_equal(
    sim_results$all$get_attributes(c("abundance", "extra1")),
    list(abundance = colSums(abundance), extra1 = extra1)
  ) # burn-in: No, No
  expect_null(sim_results$error_messages)
  # get attributes with occupancy mask (used to derive time-steps) and burn-in
  sim_results$time_steps <- NULL
  occupancy_mask <- array(1, c(5, 10))
  occupancy_mask[2:3, ] <- 0
  raster_occupancy_mask <- raster::stack(replicate(10, 1 * (region$region_raster > 0)))
  raster_occupancy_mask[region$region_indices] <- occupancy_mask
  sim_results$occupancy_mask <- raster_occupancy_mask[[1]] # no step info
  sim_results$all$attached$abundance <- NULL # reset
  expect_equal(sim_results$get_attribute("abundance")[], (abundance_raster * raster_occupancy_mask[[1]])[][, 3:10])
  expect_equal(
    sim_results$all$get_attributes(c("abundance", "extra1")),
    list(abundance = colSums(abundance * occupancy_mask[, 1])[3:10], extra1 = extra1[3:4])
  ) # burn-in: Yes, Yes
  expect_null(sim_results$error_messages)
  sim_results$occupancy_mask <- raster::stack(replicate(12, raster_occupancy_mask[[1]])) # inconsistent steps
  sim_results$all$attached$abundance <- NULL # reset
  expect_equal(sim_results$get_attribute("abundance")[], abundance_raster[][, 3:10]) # applied burn-in but no mask
  expect_equal(
    sim_results$all$get_attributes(c("abundance", "extra1")),
    list(abundance = colSums(abundance), extra1 = extra1)
  ) # burn-in: No, No
  expect_equal(
    sim_results$error_messages[1],
    "The column/layer dimension of the occupancy mask and the abundance result are inconsistent"
  )
  sim_results$error_messages <- NULL
  sim_results$occupancy_mask <- raster_occupancy_mask # consistent steps with raster mask
  sim_results$all$attached$abundance <- NULL # reset
  expect_equal(sim_results$get_attribute("abundance")[], (abundance_raster * raster_occupancy_mask)[][, 3:10])
  expect_equal(
    sim_results$all$get_attributes(c("abundance", "extra1")),
    list(abundance = colSums(abundance * occupancy_mask)[3:10], extra1 = extra1)
  ) # burn-in: Yes, No
  expect_null(sim_results$error_messages)
})

test_that("new cloning", {
  coordinates <- data.frame(x = c(1, 3, 2, 2, 3), y = c(3, 3, 2, 1, 1))
  region <- Region$new(coordinates = coordinates, use_raster = FALSE)
  sim_results <- SimulationResults$new(
    region = region, time_steps = 10,
    occupancy_mask = array(c(1, 0, 0, 1, 1), c(5, 10)), burn_in_steps = 2
  )
  sample_results <- list(abundance = array(50:1, c(5, 10)), all = list(extra1 = c(1, 2, 3, 4)))
  results_clone <- sim_results$new_clone(results = sample_results)
  clone_attributes <- results_clone$get_attributes()
  expect_named(clone_attributes, c("region", "time_steps", "burn_in_steps", "occupancy_mask", "abundance"))
  expect_equal(
    clone_attributes[c("time_steps", "burn_in_steps", "occupancy_mask")],
    sim_results$get_attributes(c("time_steps", "burn_in_steps", "occupancy_mask"))
  )
  expect_equal(clone_attributes[["abundance"]], (sample_results$abundance * sim_results$occupancy_mask)[, 3:10])
  clone_all_attributes <- results_clone$all$get_attributes()
  expect_named(clone_all_attributes, c("region", "time_steps", "burn_in_steps", "extra1", "abundance"))
  expect_equal(
    clone_all_attributes[c("time_steps", "burn_in_steps")],
    sim_results$all$get_attributes(c("time_steps", "burn_in_steps"))
  )
  expect_equal(clone_all_attributes[["abundance"]], colSums(sample_results$abundance * sim_results$occupancy_mask)[3:10])
  expect_equal(clone_all_attributes[["extra1"]], sample_results$all$extra1)
  expect_is(results_clone$all, "SimulationResults")
  expect_null(results_clone$all$all)
  expect_null(results_clone$parent)
  expect_is(results_clone$all$parent, "SimulationResults")
})
