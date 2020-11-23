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
  expect_false(region$raster_is_consistent(raster2))
  raster2[][1:3, 3] <- NA
  expect_true(region$raster_is_consistent(raster2)) # subset ok
  expect_error(region$coordinates <- array(1:10, c(5, 2)),
               "Region is already associated with a raster (and its coordinates)", fixed = TRUE)
  raster3 <- raster::raster(vals = c(NA, NA, 4, 6, NA, 2, NA, 7, NA, NA, 5, NA, 1, 3, NA, NA),
                            nrows = 4, ncol = 4, xmn = 0, xmx = 4000, ymn = 0, ymx = 4000,
                            crs = "+proj=utm +ellps=GRS80")
  expect_false(region$raster_is_consistent(raster3))
  expect_error(region$region_raster <- raster3, "Region is already associated with a set of coordinates")
  expect_silent(region$coordinates <- NULL)
  expect_false(region$raster_is_consistent(raster3))
})

test_that("via template raster", {
  raster2 <- raster::raster(vals = c(NA, NA, 1, 1, NA, 1, NA, 1, NA, NA, 1, NA, 1, 1, NA, NA),
                            nrows = 4, ncol = 4, xmn = 0, xmx = 4000, ymn = 0, ymx = 4000,
                            crs = "+proj=utm +ellps=GRS80")
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
                            crs = "+proj=utm +ellps=GRS80")
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
