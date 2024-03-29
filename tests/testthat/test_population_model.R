test_that("active get and set", {
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
  population_model <- PopulationModel$new(time_steps = 10)
  expect_null(population_model$populations)
  population_model$initial_abundance <- seq(10, 60, by = 10)
  expect_equal(population_model$populations, 6)
  population_model$region <- region
  expect_equal(population_model$populations, 7)
  expect_equal(
    population_model$get_attributes(
      c(
        "stages",
        "stage_matrix",
        "fecundity_mask",
        "density_affects",
        "density_stages",
        "dispersal_stages",
        "result_stages"
      )
    ),
    list(density_affects = "all")
  )
  population_model$stage_matrix <- array(c(0, 0.5, 0, 3, 0, 0.7, 4, 0, 0.8), c(3, 3))
  expect_equal(population_model$stages, 3)
  expect_equal(population_model$fecundity_mask, array(c(0, 0, 0, 1, 0, 0, 1, 1, 0), c(3, 3)))
  expect_equal(population_model$density_affects, array(c(0, 1, 0, 1, 0, 1, 1, 0, 1), c(3, 3)))
  expect_equal(
    population_model$get_attributes(c(
      "density_stages", "dispersal_stages", "result_stages"
    )),
    list(
      density_stages = array(1, 3),
      dispersal_stages = array(1, 3),
      result_stages = array(1, 3)
    )
  )
  expect_equal(
    population_model$get_attribute_aliases(
      c(
        "dispersal_source_n_k",
        "dispersal_target_k",
        "dispersal_target_n"
      )
    ),
    c(
      "dispersal_source_n_k",
      "dispersal_target_k",
      "dispersal_target_n",
      "dispersal_n_k_cutoff",
      "dispersal_n_k_threshold",
      "dispersal_k_threshold",
      "dispersal_n_threshold",
      "dispersal_n_cutoff",
      "dispersal_target_n_k_threshold",
      "dispersal_target_n_k_cutoff"
    )
  )
  population_model$set_attributes(list(
    dispersal_n_k_cutoff = -0.5,
    dispersal_n_k_threshold = 1.5
  ))
  expect_equal(
    population_model$dispersal_source_n_k,
    list(cutoff = -0.5, threshold = 1.5)
  )
  population_model$set_attributes(list(
    dispersal_n_threshold = 1,
    dispersal_n_cutoff = 2
  ))
  expect_equal(
    population_model$dispersal_target_n,
    list(threshold = 1, cutoff = 2)
  )
})

test_that("Detects incompleteness", {
  # Incomplete
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)), use_raster = FALSE)
  population_model <- PopulationModel$new(
    region = region,
    time_steps = 10,
    stage_matrix = array(c(0, 0.5, 0, 3, 0, 0.7, 4, 0, 0.8), c(3, 3))
  )
  expect_null(population_model$inconsistent_attributes())
  expect_equal(
    population_model$inconsistent_attributes(include_nas = TRUE)$not_available,
    c(
      "random_seed", "fecundity_max", "simulation_order",
      "results_selection", "initial_abundance", "standard_deviation",
      "correlation", "carrying_capacity",
      "density_dependence", "growth_rate_max", "translocation", "harvest",
      "mortality", "dispersal", "dispersal_source_n_k", "dispersal_target_k",
      "dispersal_target_n", "dispersal_target_n_k", "abundance_threshold"
    )
  )
  expect_true(population_model$is_consistent())
  expect_equal(
    population_model$incomplete_attributes(),
    c("initial_abundance", "carrying_capacity")
  )
  expect_false(population_model$is_complete())
})

test_that("Detects inconsistency in initial abundance and carrying capacity", {
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)), use_raster = FALSE)
  population_model <- PopulationModel$new(
    region = region,
    time_steps = 10,
    stage_matrix = array(c(0, 0.5, 0, 3, 0, 0.7, 4, 0, 0.8), c(3, 3))
  )
  # Using arrays/matrices
  population_model$initial_abundance <- seq(10, 60, by = 10)
  population_model$carrying_capacity <- seq(20, 70, by = 10)
  expect_equal(
    population_model$inconsistent_attributes(),
    c("initial_abundance", "carrying_capacity")
  )
  expect_false(population_model$is_consistent())
  population_model$initial_abundance <- seq(10, 70, by = 10)
  population_model$carrying_capacity <- seq(20, 80, by = 10)
  expect_true(population_model$is_consistent())
  population_model$initial_abundance <- array(seq(10, 70, by = 10), c(7, 2))
  population_model$carrying_capacity <- array(seq(20, 80, by = 10), c(7, 8))
  expect_equal(
    population_model$inconsistent_attributes(),
    c("initial_abundance", "carrying_capacity")
  )
  expect_false(population_model$is_consistent())
  population_model$initial_abundance <- array(seq(10, 70, by = 10), c(7, 3))
  population_model$carrying_capacity <- array(seq(20, 80, by = 10), c(7, 10))
  expect_true(population_model$is_consistent())
  expect_true(population_model$is_complete())
  # Using rasters
  region$use_raster <- TRUE
  population_model$initial_abundance <- region$region_raster * 10
  population_model$carrying_capacity <- region$region_raster * 15
  expect_true(population_model$is_consistent())
  population_model$initial_abundance[1] <- 80
  population_model$carrying_capacity[1] <- 100
  expect_equal(
    population_model$inconsistent_attributes(),
    c("initial_abundance", "carrying_capacity")
  )
  population_model$initial_abundance <- raster::stack(replicate(2, region$region_raster * 10))
  population_model$carrying_capacity <- raster::stack(replicate(8, region$region_raster * 15))
  expect_equal(
    population_model$inconsistent_attributes(),
    c("initial_abundance", "carrying_capacity")
  )
  population_model$initial_abundance <- raster::stack(replicate(3, region$region_raster * 10))
  population_model$carrying_capacity <- raster::stack(replicate(10, region$region_raster * 15))
  expect_true(population_model$is_consistent())
})

test_that("Detects inconsistency in life cycle stages", {
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)), use_raster = FALSE)
  population_model <- PopulationModel$new(
    region = region,
    time_steps = 10,
    stage_matrix = array(c(0, 0.5, 0, 3, 0, 0.7, 4, 0, 0.8), c(3, 3)),
    initial_abundance = array(seq(10, 70, by = 10), c(7, 3)),
    carrying_capacity = array(seq(20, 80, by = 10), c(7, 10))
  )
  population_model$dispersal_stages <- c(0, 1, 1, 1)
  population_model$result_stages <- c(0, 1, 1, 1)
  expect_equal(
    population_model$inconsistent_attributes(),
    c("dispersal_stages", "result_stages")
  )
  expect_false(population_model$is_consistent())
})

test_that("Test for private attributes", {
  population_model <- PopulationModel$new()
  expect_equal(population_model$get_attribute(".model_attributes"), c(
    "region", "coordinates", "random_seed", "replicates", "time_steps", "years_per_step",
    "populations", "stages", "initial_abundance", "stage_matrix", "fecundity_mask",
    "fecundity_max", "demographic_stochasticity", "standard_deviation", "correlation",
    "carrying_capacity", "density_dependence", "growth_rate_max", "density_affects",
    "density_stages", "translocation", "harvest", "mortality", "dispersal",
    "dispersal_stages", "dispersal_source_n_k", "dispersal_target_k",
    "dispersal_target_n", "dispersal_target_n_k", "abundance_threshold",
    "simulation_order", "results_selection", "result_stages"
  ))
  expect_equal(population_model$get_attribute(".simulation_function"), "population_simulator")
})

test_that("Test for attribute alias setting during initialization", {
  population_model <- PopulationModel$new(attribute_aliases = list(test_alias = "dispersal"))
  expect_equal(population_model$attribute_aliases$test_alias, "dispersal")

  population_model <- PopulationModel$new(attribute_aliases = list(test_alias = "dispersal_source_n_k$cutoff"))
  expect_equal(population_model$attribute_aliases$test_alias, "dispersal_source_n_k$cutoff")

  population_model <- PopulationModel$new(attribute_aliases = list(test_alias = "dispersal_target_k"))
  expect_equal(population_model$attribute_aliases$test_alias, "dispersal_target_k")

  population_model <- PopulationModel$new(attribute_aliases = list(test_alias = "dispersal_target_n$threshold"))
  expect_equal(population_model$attribute_aliases$test_alias, "dispersal_target_n$threshold")

  population_model <- PopulationModel$new(attribute_aliases = list(test_alias = "dispersal_target_n_k$threshold"))
  expect_equal(population_model$attribute_aliases$test_alias, "dispersal_target_n_k$threshold")
})
