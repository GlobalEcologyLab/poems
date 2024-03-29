context("Simulation Model")

test_that("initialization, get and set", {
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
  attribute_aliases <- list(coords = "region$coordinates", a1_a = "attr1$a", a1_b = "attr1$b", a2 = "attr2")
  sim_model <- SimulationModel$new(
    params = list(time_steps = 10, attr1 = list(a = 2)), region = region,
    attribute_aliases = attribute_aliases
  )
  sim_model$set_attributes(a2 = 55)
  expect_equal(sim_model$attached, list(attr1 = list(a = 2), attr2 = 55))
  expect_equal(
    sim_model$get_attributes(params = c("coords", "time_steps", "a1_a", "a1_b", "a2")),
    list(coords = region$coordinates, time_steps = 10, a1_a = 2, a2 = 55)
  )
  expect_equal(sim_model$get_attribute_aliases(), c(sim_model$get_attribute_names(), names(sim_model$attribute_aliases)))
})

test_that("template nesting via list", {
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
  model_attributes <- c("region", "coordinates", "time_steps", "attr1", "attr2")
  required_attributes <- c("region", "time_steps", "attr1")
  attribute_aliases <- list(coords = "region$coordinates", a1_a = "attr1$a", a1_b = "attr1$b", a2 = "attr2")
  nested_model <- SimulationModel$new(
    template = list(
      time_steps = 10, region = region, attr1 = list(a = 2),
      attr2 = 55
    ),
    model_attributes = model_attributes, required_attributes = required_attributes,
    attribute_aliases = attribute_aliases
  )
  expect_is(nested_model$template_model, "SimulationModel")
  expect_equal(nested_model$template_model$model_attributes, model_attributes)
  expect_equal(nested_model$template_model$required_attributes, required_attributes)
  expect_equal(nested_model$template_model$attribute_aliases, attribute_aliases)
  expect_equal(
    nested_model$get_attributes(params = c("coords", "time_steps", "a1_a", "a1_b", "a2")),
    list(coords = region$coordinates, time_steps = 10, a1_a = 2, a2 = 55)
  )
  expect_equal(nested_model$template_model$attached, list(attr1 = list(a = 2), attr2 = 55))
  expect_equal(nested_model$attached, list())
})

test_that("template nesting via object", {
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
  model_attributes <- c("region", "coordinates", "time_steps", "attr1", "attr2")
  required_attributes <- c("region", "time_steps", "attr1")
  attribute_aliases <- list(coords = "region$coordinates", a1_a = "attr1$a", a1_b = "attr1$b", a2 = "attr2")
  template_model <- SimulationModel$new(
    time_steps = 10, region = region, model_attributes = model_attributes,
    required_attributes = required_attributes, attribute_aliases = attribute_aliases,
    attr1 = list(a = 2), attr2 = 55
  )
  nested_model <- SimulationModel$new(template = template_model)
  expect_equal(nested_model$model_attributes, model_attributes)
  expect_equal(nested_model$required_attributes, required_attributes)
  expect_equal(nested_model$attribute_aliases, attribute_aliases)
  expect_named(nested_model$template_model$attached, c("attr1", "attr2"))
  expect_equal(nested_model$attached, list())
})

test_that("set nested samples", {
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
  attribute_aliases <- list(coords = "region$coordinates", a1_a = "attr1$a", a1_b = "attr1$b", a2 = "attr2")
  nested_model <- SimulationModel$new(template = SimulationModel$new(
    time_steps = 10, region = region,
    attribute_aliases = attribute_aliases,
    attr1 = list(a = 2), attr2 = 55
  ))
  expect_null(nested_model$sample_attributes)
  nested_model$set_sample_attributes(params = c("a1_b", "a2", "extra1"))
  expect_equal(nested_model$sample_attributes, c("attr1", "attr2", "extra1"))
  expect_equal(nested_model$attached, list(attr1 = list(a = 2), extra1 = NA))
  nested_model$set_sample_attributes(a1_a = 3, a1_b = 4, a2 = 66)
  expect_equal(nested_model$attached, list(attr1 = list(a = 3, b = 4), extra1 = NA, attr2 = 66))
  expect_equal(
    nested_model$get_attributes(params = c("coords", "time_steps", "a1_a", "a1_b", "a2")),
    list(coords = region$coordinates, time_steps = 10, a1_a = 3, a1_b = 4, a2 = 66)
  )
  nested_model$set_sample_attributes(params = c("a1_b"))
  expect_equal(nested_model$sample_attributes, c("attr1"))
  expect_equal(
    nested_model$get_attributes(params = c("coords", "time_steps", "a1_a", "a1_b", "a2")),
    list(coords = region$coordinates, time_steps = 10, a1_a = 3, a1_b = 4, a2 = 55)
  )
})

test_that("consistency and completeness", {
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
  model_attributes <- c("region", "coordinates", "time_steps", "attr1", "attr2")
  required_attributes <- c("region", "time_steps", "attr1", "attr2")
  attribute_aliases <- list(coords = "region$coordinates", a1_a = "attr1$a", a1_b = "attr1$b", a2 = "attr2")
  nested_model <- SimulationModel$new(
    template = SimulationModel$new(), model_attributes = model_attributes,
    required_attributes = required_attributes,
    attribute_aliases = attribute_aliases
  )
  expect_equal(nested_model$list_consistency(), list(
    region = NA, coordinates = NA, time_steps = NA,
    attr1 = NA, attr2 = NA
  ))
  expect_null(nested_model$inconsistent_attributes())
  expect_equal(
    nested_model$inconsistent_attributes(include_nas = TRUE),
    list(inconsistent = NULL, not_available = c("region", "coordinates", "time_steps", "attr1", "attr2"))
  )
  expect_equal(nested_model$list_completeness(), list(
    region = FALSE, coordinates = NA, time_steps = FALSE,
    attr1 = FALSE, attr2 = FALSE
  ))
  expect_true(nested_model$is_consistent())
  expect_equal(nested_model$incomplete_attributes(), c("region", "time_steps", "attr1", "attr2"))
  expect_equal(
    nested_model$incomplete_attributes(include_nas = TRUE),
    list(incomplete = c("region", "time_steps", "attr1", "attr2"), not_available = c("coordinates"))
  )
  expect_false(nested_model$is_complete())
  nested_model$region <- region
  nested_model$time_steps <- 10
  nested_model$set_attributes(
    a1_a = array(1:6, c(6, 10)), a1_b = 1:11,
    a2 = raster::stack(replicate(7, region$region_raster))
  )
  expect_equal(nested_model$inconsistent_attributes(), c("attr1", "attr2"))
  expect_equal(nested_model$incomplete_attributes(), c("attr1", "attr2"))
  nested_model$set_attributes(
    a1_a = array(1:7, c(7, 9)), a1_b = 1:10,
    a2 = raster::stack(replicate(10, region$region_raster))
  )
  expect_equal(nested_model$inconsistent_attributes(), c("attr1"))
  expect_false(nested_model$is_consistent())
  nested_model$set_attributes(a1_a = array(1:7, c(7, 10)), a2 = 11:17)
  expect_true(nested_model$is_consistent())
  expect_true(nested_model$is_complete())
})
