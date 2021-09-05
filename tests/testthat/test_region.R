context("Region")

test_that("non-raster coordinates", {
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)), use_raster = FALSE)
  expect_is(region$coordinates, "data.frame")
  expect_equal(region$coordinates, data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3)))
  expect_null(region$region_raster)
  expect_equal(region$region_cells, 7)
  expect_error(region$region_cells <- 6, "Cannot set dynamically calculated number of region cells")
  expect_equal(region$region_indices, 1:7)
  expect_error(region$region_indices <- 1:4, "Cannot set dynamically calculated region cell indices")
})

test_that("raster via coordinates", {
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)), use_raster = TRUE)
  expect_equal(region$coordinates, data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3)))
  expect_is(region$region_raster, "RasterLayer")
  expect_equal(region$region_raster[region$region_indices], 1:7)
  expect_equal(region$region_cells, 7)
  expect_true(region$raster_is_consistent(region$region_raster))
  expect_silent(raster1 <- region$region_raster)
  raster1[1] <- 8
  expect_false(region$raster_is_consistent(raster1))
  raster1[1:3] <- NA
  expect_true(region$raster_is_consistent(raster1)) # subset ok
  raster2 <- raster::stack(replicate(10, region$region_raster))
  expect_true(region$raster_is_consistent(raster2))
  raster2[][1, 3] <- 8
  expect_true(region$strict_consistency) # default
  region$strict_consistency <- FALSE
  expect_true(region$raster_is_consistent(raster2))
  region$strict_consistency <- TRUE
  expect_false(region$raster_is_consistent(raster2))
  raster2[][1:3, 3] <- NA
  expect_true(region$raster_is_consistent(raster2)) # subset ok
  expect_error(region$coordinates <- array(1:10, c(5, 2)),
               "Region is already associated with a raster (and its coordinates)", fixed = TRUE)
  raster3 <- raster::raster(vals = c(NA, NA, 4, 6, NA, 2, NA, 7, NA, NA, 5, NA, 1, 3, NA, NA),
                            nrows = 4, ncol = 4, xmn = 0, xmx = 4000, ymn = 0, ymx = 4000,
                            crs = "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  expect_false(region$raster_is_consistent(raster3))
  expect_error(region$region_raster <- raster3, "Region is already associated with a set of coordinates")
  expect_silent(region$coordinates <- NULL)
  expect_false(region$raster_is_consistent(raster3))
})

test_that("via template raster", {
  raster2 <- raster::raster(vals = c(NA, NA, 1, 1, NA, 1, NA, 1, NA, NA, 1, NA, 1, 1, NA, NA),
                            nrows = 4, ncol = 4, xmn = 0, xmx = 4000, ymn = 0, ymx = 4000,
                            crs = "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  expect_error(region <- Region$new(template_raster = array(1:10, c(5, 2))),
               "Template raster should be a raster::RasterLayer (or inherited class) object", fixed = TRUE)
  expect_error(region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)), template_raster = raster2),
               "Region should be specified with a set of coordinates or a raster, not both")
  region <- Region$new(template_raster = raster2)
  expect_is(region$region_raster, "RasterLayer")
  expect_equal(region$region_raster[region$region_indices], 1:7)
  expect_equal(region$region_cells, 7)
  expect_equal(region$coordinates, as.data.frame(raster::coordinates(region$region_raster)[region$region_indices,]))
})

test_that("via region raster", {
  raster2 <- raster::raster(vals = c(NA, NA, 4, 6, NA, 2, NA, 7, NA, NA, 5, NA, 1, 3, NA, NA),
                            nrows = 4, ncol = 4, xmn = 0, xmx = 4000, ymn = 0, ymx = 4000,
                            crs = "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  expect_error(region <- Region$new(region_raster = array(1:10, c(5, 2))),
               "Region raster should be a raster::RasterLayer (or inherited class) object", fixed = TRUE)
  expect_error(region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)), region_raster = raster2),
               "Region should be specified with a set of coordinates or a raster, not both")
  region <- Region$new(region_raster = raster2)
  expect_is(region$region_raster, "RasterLayer")
  expect_equal(region$region_raster[region$region_indices], 1:7)
  expect_equal(region$region_cells, 7)
  expect_equal(region$coordinates, as.data.frame(raster::coordinates(region$region_raster)[region$region_indices,]))
  # via a raster grid file loads values into memory
  region <- Region$new(region_raster = raster::raster(file.path(test_path("test_inputs"), "Test_1_2.grd")))
  expect_false(raster::fromDisk(region$region_raster))
})

test_that("raster from values", {
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)), use_raster = FALSE)
  expect_error(region$raster_from_values(1:7),
               "Raster (or stack) can only be generated when the use_raster parameter is TRUE", fixed = TRUE)
  region$use_raster <- TRUE
  expect_error(region$raster_from_values(array(1:3, c(3, 2))),
               "Values must have a length or dimensions consistent with the number of region (non-NA) cells", fixed = TRUE)
  # Array
  value_raster <- region$raster_from_values(1:7)
  expect_is(value_raster, "RasterLayer")
  expect_equal(value_raster[region$region_indices], 1:7)
  expect_true(region$raster_is_consistent(value_raster))
  # Matrix
  expect_error(region$raster_from_values(array(1:35, c(5, 7))),
               "Values must have a length or dimensions consistent with the number of region (non-NA) cells", fixed = TRUE)
  value_matrix <- array(1:35, c(7, 5), dimnames = list(NULL, paste0("t", 1:5)))
  value_raster <- region$raster_from_values(value_matrix)
  expect_is(value_raster, "RasterBrick")
  expect_equal(value_raster[region$region_indices], value_matrix)
  expect_true(region$raster_is_consistent(value_raster))
})
