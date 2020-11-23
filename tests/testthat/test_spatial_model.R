context("Spatial Model")

test_that("initialization", {
  expect_warning(spatial_model <- SpatialModel$new(region = Region$new()),
                 "Spatial region has not been defined within the region object")
  expect_error(spatial_model$region <- array(1:10, c(5, 2)),
               "Region should be a Region (or inherited class) object", fixed = TRUE)
  spatial_model$region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
  expect_equal(spatial_model$coordinates, data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3)))
  expect_true(spatial_model$region$use_raster)
  expect_silent(spatial_model$region <- NULL)
  spatial_model <- SpatialModel$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
  expect_is(spatial_model$region, "Region")
  expect_false(spatial_model$region$use_raster)
  expect_equal(spatial_model$region$coordinates, data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3)))
})

test_that("aliases", {
  spatial_model <- SpatialModel$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
  spatial_model$attribute_aliases <- list(coords = "region$coordinates")
  expect_equal(spatial_model$get_attribute("coords"), spatial_model$region$coordinates)
  spatial_model$region$use_raster <- TRUE
  expect_error(spatial_model$set_attributes(coords = array(c(2:5, 5:2), c(7, 2))),
               "Region is already associated with a raster (and its coordinates)", fixed = TRUE)
  spatial_model$region$use_raster <- FALSE
  expect_silent(spatial_model$set_attributes(coords = array(c(2:5, 5:2), c(7, 2))))
  expect_equal(spatial_model$coordinates, data.frame(x = c(2:5, 5:3), y = c(2, 2:5, 5:4)))
  expect_equal(spatial_model$attached, list())
})

test_that("inheritance and cloning", {
  TestSpatialModel <- R6::R6Class("TestSpatialModel", inherit = SpatialModel,
                                  private = list(.model_attributes = c("region", "attr1"), .active_attributes = c("region", "attr1"), .attr1 = NULL),
                                  active = list(attr1 = function(value) {if (missing(value)) { private$.attr1 } else { private$.attr1 <- value }})
  )
  spatial_model <- TestSpatialModel$new(object_generator = TestSpatialModel, # for check only,
                                        region = Region$new(coordinates = array(c(1:4, 4:1), c(7, 2))),
                                        attr1 = 33)
  expect_equal(spatial_model$attr1, 33)
  expect_silent(spatial_clone <- spatial_model$new_clone())
  expect_null(spatial_clone$attr1)
  expect_silent(spatial_clone <- spatial_model$new_clone(attr1 = 44))
  expect_equal(spatial_clone$attr1, 44)
  expect_is(spatial_clone$region, "Region")
  expect_equal(spatial_clone$coordinates, data.frame(x = c(1:4, 4:2), y = c(1, 1:4, 4:3)))
})
