context("Population Dispersal (function)")

test_that("setup user-defined function", {
  simulator <- SimulatorReference$new()
  # User-defined dispersal as a function
  dispersal_function <- population_dispersal(
    replicates = 4,
    time_steps = 10,
    years_per_step = 1,
    populations = 7,
    demographic_stochasticity = TRUE,
    density_stages = c(0, 1, 1),
    dispersal = function(params) {
      0.33
    },
    dispersal_stages = c(0, 1, 0.5),
    dispersal_source_n_k = list(cutoff = -0.5, threshold = 1.5),
    dispersal_target_k = 5,
    dispersal_target_n = list(threshold = 10, cutoff = 15),
    simulator = simulator
  )
  expect_is(dispersal_function, "function")
  expect_named(
    formals(dispersal_function),
    c(
      "r",
      "tm",
      "carrying_capacity",
      "stage_abundance",
      "occupied_indices"
    )
  )
  expect_named(
    environment(dispersal_function)[["params"]],
    c(
      "replicates",
      "time_steps",
      "years_per_step",
      "populations",
      "stages",
      "demographic_stochasticity",
      "density_stages",
      "dispersal_stages",
      "dispersal_source_n_k",
      "dispersal_target_k",
      "dispersal_target_n",
      "dispersal_target_n_k",
      "simulator"
    )
  )
  expect_equal(
    environment(dispersal_function)[["params"]][c(
      "replicates",
      "time_steps",
      "years_per_step",
      "populations",
      "stages",
      "demographic_stochasticity",
      "density_stages",
      "dispersal_stages",
      "dispersal_source_n_k",
      "dispersal_target_k",
      "dispersal_target_n"
    )],
    list(
      replicates = 4,
      time_steps = 10,
      years_per_step = 1,
      populations = 7,
      stages = 3,
      demographic_stochasticity = TRUE,
      density_stages = c(0, 1, 1),
      dispersal_stages = c(0, 1, 0.5),
      dispersal_source_n_k = list(cutoff = -0.5, threshold = 1.5),
      dispersal_target_k = 5,
      dispersal_target_n = list(threshold = 10, cutoff = 15)
    )
  )
  # User-defined dispersal as list with function
  dispersal_function <- population_dispersal(
    replicates = 4,
    time_steps = 10,
    years_per_step = 1,
    populations = 7,
    demographic_stochasticity = TRUE,
    density_stages = c(0, 1, 1),
    dispersal = list(a = 1, function(params) {
      0.33
    }, b = 2),
    dispersal_stages = c(0, 1, 0.5),
    dispersal_source_n_k = list(cutoff = -0.5, threshold = 1.5),
    dispersal_target_k = 5,
    dispersal_target_n = list(threshold = 10, cutoff = 15),
    simulator = simulator
  )
  expect_is(dispersal_function, "function")
  expect_named(
    environment(dispersal_function)[["params"]],
    c(
      "replicates",
      "time_steps",
      "years_per_step",
      "populations",
      "stages",
      "demographic_stochasticity",
      "density_stages",
      "dispersal_stages",
      "dispersal_source_n_k",
      "dispersal_target_k",
      "dispersal_target_n",
      "dispersal_target_n_k",
      "simulator",
      "a",
      "b"
    )
  )
  expect_equal(environment(dispersal_function)[["params"]][c("a", "b")], list(a = 1, b = 2))
})

test_that("user-defined dispersal calculations", {
  simulator <- SimulatorReference$new()
  test_function <- function(params) {
    # mock dispersal
    params$simulator$attached$params <- params # attach to reference object
    emigrants <- round(params$stage_abundance * params$dispersal_stages * 0.4)
    return(params$stage_abundance - emigrants + emigrants[, c(7, 1:6)])
  }
  dispersal_function <- population_dispersal(
    replicates = 4,
    time_steps = 10,
    years_per_step = 1,
    populations = 7,
    demographic_stochasticity = TRUE,
    density_stages = c(0, 1, 1),
    dispersal = list(a = 1, test_function, b = 2),
    dispersal_stages = c(0, 1, 0.5),
    dispersal_source_n_k = list(cutoff = -0.5, threshold = 1.5),
    dispersal_target_k = 5,
    dispersal_target_n = list(threshold = 10, cutoff = 15),
    simulator = simulator
  )
  carrying_capacity <- rep(10, 7)
  stage_abundance <- matrix(
    c(
      7, 13, 0, 26, 0, 39, 47,
      2, 0, 6, 8, 0, 12, 13,
      0, 3, 4, 6, 0, 9, 10
    ),
    nrow = 3,
    ncol = 7,
    byrow = TRUE
  )
  occupied_indices <- (1:7)[-5]
  emigrants <- round(stage_abundance * c(0, 1, 0.5) * 0.4)
  expected_stage_abundance <- stage_abundance - emigrants + emigrants[, c(7, 1:6)]
  expect_equal(
    dispersal_function(
      r = 2,
      tm = 6,
      carrying_capacity,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance
  )
  expect_named(
    simulator$attached$params,
    c(
      "replicates",
      "time_steps",
      "years_per_step",
      "populations",
      "stages",
      "demographic_stochasticity",
      "density_stages",
      "dispersal_stages",
      "dispersal_source_n_k",
      "dispersal_target_k",
      "dispersal_target_n",
      "dispersal_target_n_k",
      "simulator",
      "a",
      "b",
      "r",
      "tm",
      "carrying_capacity",
      "stage_abundance",
      "occupied_indices"
    )
  )
  expect_equal(
    simulator$attached$params[c(
      "a",
      "b",
      "r",
      "tm",
      "carrying_capacity",
      "stage_abundance",
      "occupied_indices"
    )],
    list(
      a = 1,
      b = 2,
      r = 2,
      tm = 6,
      carrying_capacity = carrying_capacity,
      stage_abundance = stage_abundance,
      occupied_indices = occupied_indices
    )
  )
  # Errors and warnings
  test_function <- function(params) {
    stop("test error")
  }
  dispersal_function <- population_dispersal(
    replicates = 4,
    time_steps = 10,
    years_per_step = 1,
    populations = 7,
    demographic_stochasticity = TRUE,
    density_stages = c(0, 1, 1),
    dispersal = list(a = 1, test_function, b = 2),
    dispersal_stages = c(0, 1, 0.5),
    dispersal_source_n_k = list(cutoff = -0.5, threshold = 1.5),
    dispersal_target_k = 5,
    dispersal_target_n = list(threshold = 10, cutoff = 15),
    simulator = simulator
  )
  expect_error(
    dispersal_function(
      r = 2,
      tm = 6,
      carrying_capacity,
      stage_abundance,
      occupied_indices
    ),
    "Error produced within user-defined dispersal function"
  )
  test_function <- function(params) {
    params$stage_abundance
  }
  dispersal_function <- population_dispersal(
    replicates = 4,
    time_steps = 10,
    years_per_step = 1,
    populations = 7,
    demographic_stochasticity = TRUE,
    density_stages = c(0, 1, 1),
    dispersal = list(a = 1, test_function, b = 2),
    dispersal_stages = c(0, 1, 0.5),
    dispersal_source_n_k = list(cutoff = -0.5, threshold = 1.5),
    dispersal_target_k = 5,
    dispersal_target_n = list(threshold = 10, cutoff = 15),
    simulator = simulator
  )
  stage_abundance[1] <- NA
  expect_warning(
    dispersal_function(
      r = 2,
      tm = 6,
      carrying_capacity,
      stage_abundance,
      occupied_indices
    ),
    "Non-finite abundances returned by user-defined dispersal function"
  )
  stage_abundance[2] <- -1
  expect_warning(
    dispersal_function(
      r = 2,
      tm = 6,
      carrying_capacity,
      stage_abundance,
      occupied_indices
    ),
    "Negative abundances returned by user-defined dispersal function"
  )
})

test_that("setup default function", {
  simulator <- SimulatorReference$new()
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
  conductance_raster <- raster::stack(replicate(10, +(region$region_raster > 0)))
  conductance_raster[[2]][11] <- 0
  dispersal_friction <- DispersalFriction$new(
    region = region,
    conductance = conductance_raster
  )
  dispersal_gen <- DispersalGenerator$new(
    dispersal_friction = dispersal_friction,
    dispersal_proportion = 0.6,
    dispersal_breadth = 110,
    dispersal_max_distance = 300,
    distance_scale = 1000,
    distance_classes = seq(100, 400, 20)
  )
  dispersal_gen$calculate_distance_data()
  dispersal_gen$calculate_dispersals(type = "matrix")
  dispersal_gen$calculate_dispersals(type = "data")
  dispersal_data <- dispersal_gen$dispersal_data[[1]]
  expected_dispersal_data_changes <- dispersal_gen$dispersal_data
  expected_dispersal_data_changes[[1]] <- dispersal_data[NULL, ]
  expected_compact_rows <- max(dispersal_data[c("emigrant_row", "immigrant_row")])
  expected_compact_matrix <- array(0, c(expected_compact_rows, 7))
  expected_compact_matrix[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <-
    dispersal_data$dispersal_rate
  expected_target_pop_map <- array(0, c(expected_compact_rows, 7))
  expected_target_pop_map[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <-
    dispersal_data$target_pop
  compact_indices <- array(1:(expected_compact_rows * 7), c(expected_compact_rows, 7))
  expected_immigrant_map <- array(0, c(expected_compact_rows, 7))
  expected_immigrant_map[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <-
    compact_indices[as.matrix(dispersal_data[, c("immigrant_row", "target_pop")])]
  # Via dispersal data with temporal changes
  dispersal_function <- population_dispersal(
    replicates = 4,
    time_steps = 10,
    years_per_step = 1,
    populations = 7,
    demographic_stochasticity = TRUE,
    density_stages = c(0, 1, 1),
    dispersal = dispersal_gen$dispersal_data,
    dispersal_stages = c(0, 1, 0.5),
    dispersal_source_n_k = list(cutoff = -0.5, threshold = 1.5),
    dispersal_target_k = 5,
    dispersal_target_n = list(threshold = 10, cutoff = 15),
    simulator = simulator
  )
  expect_is(dispersal_function, "function")
  expect_equal(environment(dispersal_function)[["dispersal_compact_rows"]], expected_compact_rows)
  expect_equal(environment(dispersal_function)[["dispersal_compact_matrix"]], expected_compact_matrix)
  expect_true(environment(dispersal_function)[["dispersals_change_over_time"]])
  expect_true(environment(dispersal_function)[["dispersal_depends_on_source_pop_n_k"]])
  expect_true(environment(dispersal_function)[["dispersal_depends_on_target_pop_k"]])
  expect_true(environment(dispersal_function)[["dispersal_depends_on_target_pop_n"]])
  expect_equal(environment(dispersal_function)[["dispersal_target_pop_map"]], expected_target_pop_map)
  expect_equal(
    environment(dispersal_function)[["dispersal_data_changes"]],
    expected_dispersal_data_changes
  )
  expect_equal(environment(dispersal_function)[["dispersal_immigrant_map"]], expected_immigrant_map)
  # Via dispersal matrix (no temporal changes)
  dispersal_gen$distance_data <- NULL
  dispersal_gen$dispersal_friction <- DispersalFriction$new(region = region)
  dispersal_gen$calculate_distance_data()
  dispersal_gen$calculate_dispersals(type = "matrix")
  dispersal_gen$calculate_dispersals(type = "data")
  dispersal_data <- dispersal_gen$dispersal_data[[1]]
  expected_compact_rows <- max(dispersal_data[c("emigrant_row", "immigrant_row")])
  expected_compact_matrix <- array(0, c(expected_compact_rows, 7))
  expected_compact_matrix[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <- dispersal_data$dispersal_rate
  compact_indices <- array(1:(expected_compact_rows * 7), c(expected_compact_rows, 7))
  expected_immigrant_map <- array(0, c(expected_compact_rows, 7))
  expected_immigrant_map[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <- compact_indices[as.matrix(dispersal_data[, c("immigrant_row", "target_pop")])]
  dispersal_function <- population_dispersal(
    replicates = 4,
    time_steps = 10,
    years_per_step = 1,
    populations = 7,
    demographic_stochasticity = TRUE,
    density_stages = c(0, 1, 1),
    dispersal = dispersal_gen$dispersal_matrix,
    dispersal_stages = c(0, 1, 0.5),
    dispersal_source_n_k = NULL,
    dispersal_target_k = NULL,
    dispersal_target_n = NULL,
    simulator = simulator
  )
  expect_is(dispersal_function, "function")
  expect_equal(environment(dispersal_function)[["dispersal_compact_rows"]], expected_compact_rows)
  expect_equal(environment(dispersal_function)[["dispersal_compact_matrix"]], expected_compact_matrix)
  expect_false(environment(dispersal_function)[["dispersals_change_over_time"]])
  expect_false(environment(dispersal_function)[["dispersal_depends_on_source_pop_n_k"]])
  expect_false(environment(dispersal_function)[["dispersal_depends_on_target_pop_k"]])
  expect_false(environment(dispersal_function)[["dispersal_depends_on_target_pop_n"]])
  expect_equal(environment(dispersal_function)[["dispersal_immigrant_map"]], expected_immigrant_map)
  expect_warning(
    dispersal_function <- population_dispersal(
      replicates = 4,
      time_steps = 10,
      years_per_step = 1,
      populations = 7,
      demographic_stochasticity = TRUE,
      density_stages = c(0, 1, 1),
      dispersal = dispersal_gen$dispersal_matrix,
      dispersal_stages = c(0, 1, 0.5),
      dispersal_source_n_k = list(cutoff = 1.5, threshold = -0.5),
      dispersal_target_k = NULL,
      dispersal_target_n = NULL,
      simulator = simulator
    ),
    "Dispersal density dependence for source N/K threshold must be greater than cutoff => not used"
  )
  expect_false(environment(dispersal_function)[["dispersal_depends_on_source_pop_n_k"]])
})

test_that("default dispersal calculations", {
  simulator <- SimulatorReference$new()
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
  conductance_raster <- raster::stack(replicate(10, +(region$region_raster > 0)))
  conductance_raster[[2]][11] <- 0
  dispersal_friction <- DispersalFriction$new(
    region = region,
    conductance = conductance_raster
  )
  dispersal_gen <- DispersalGenerator$new(
    dispersal_friction = dispersal_friction,
    dispersal_proportion = 0.6,
    dispersal_breadth = 110,
    dispersal_max_distance = 300,
    distance_scale = 1000,
    distance_classes = seq(100, 400, 20)
  )
  dispersal_gen$calculate_distance_data()
  dispersal_gen$calculate_dispersals(type = "matrix")
  dispersal_gen$calculate_dispersals(type = "data")
  dispersal_matrix_tm <- list()
  dispersal_matrix_tm[[1]] <- dispersal_matrix_tm[[2]] <- dispersal_gen$dispersal_matrix
  dispersal_matrix_tm[[3]] <- dispersal_matrix_tm[[4]] <- dispersal_gen$dispersal_matrix
  dispersal_matrix_tm[[2]][as.matrix(dispersal_gen$dispersal_data[[2]][c("target_pop", "source_pop")])] <- 0
  dispersal_matrix_tm[[4]] <- dispersal_matrix_tm[[4]] * matrix(
    1 / colSums(dispersal_matrix_tm[[4]]),
    # high dispersal
    nrow = 7,
    ncol = 7,
    byrow = TRUE
  )
  dispersal_gen$dispersal_data[[4]] <- dispersal_gen$dispersal_data[[1]]
  dispersal_gen$dispersal_data[[4]]$dispersal_rate <- dispersal_matrix_tm[[4]][as.matrix(dispersal_gen$dispersal_data[[4]][c("target_pop", "source_pop")])]
  dispersal_function <- population_dispersal(
    replicates = 4,
    time_steps = 10,
    years_per_step = 1,
    populations = 7,
    demographic_stochasticity = TRUE,
    density_stages = c(0, 1, 1),
    dispersal = dispersal_gen$dispersal_data,
    dispersal_stages = c(0.5, 1, 0),
    dispersal_source_n_k = NULL,
    dispersal_target_k = 5,
    dispersal_target_n = NULL,
    simulator = simulator
  )
  carrying_capacity_tm_1 <- carrying_capacity_tm_2 <- carrying_capacity_tm_4 <- rep(10, 7)
  carrying_capacity_tm_3 <- c(10, 0, 3, 10, 10, 10, 10) # some target k < 5
  # previous_dispersal_totals <- colSums(dispersal_matrix_tm[[3]])
  dispersal_matrix_tm[[3]][2, ] <- 0
  dispersal_matrix_tm[[3]][3, ] <- 3 / 5 * dispersal_matrix_tm[[3]][3, ]
  # dispersal_matrix_tm[[3]] <- dispersal_matrix_tm[[3]]*matrix(previous_dispersal_totals/colSums(dispersal_matrix_tm[[3]]),
  #                                                             nrow = 7, ncol = 7, byrow = TRUE)
  stage_abundance <- matrix(
    c(
      7, 13, 0, 26, 0, 39, 47,
      2, 0, 6, 8, 0, 12, 13,
      0, 3, 4, 6, 0, 9, 10
    ),
    nrow = 3,
    ncol = 7,
    byrow = TRUE
  )
  occupied_indices <- (1:7)[-5]
  occupied_matrix <- array(0, c(7, 7))
  occupied_matrix[, occupied_indices] <- 1
  # Calculate expected abundances
  expected_stage_abundance_tm <- list()
  for (tm in 1:4) {
    expected_stage_abundance_tm[[tm]] <- stage_abundance
    dispersal_matrix_tm[[tm]][, 5] <- 0
    dispersal_indices <- which(dispersal_matrix_tm[[tm]] * occupied_matrix > 0)
    set.seed(123)
    for (i in 1:2) {
      dispersers <- array(0, c(7, 7))
      dispersers[dispersal_indices] <- stats::rbinom(
        length(dispersal_indices),
        stage_abundance[rep(i, 7), ][dispersal_indices],
        dispersal_matrix_tm[[tm]][dispersal_indices] *
          c(0.5, 1)[i]
      )
      # Handle excessive dispersers for tm = 3:4, stage = 2 (only)
      for (xi in which(colSums(dispersers) > stage_abundance[i, ])) {
        excessive_rows <- which(dispersers[, xi] > 0)
        excessive_dispersers <- dispersers[excessive_rows, xi]
        disperser_reduction <- colSums(dispersers)[xi] - stage_abundance[i, xi]
        for (r in sample(
          rep(excessive_rows, times = excessive_dispersers),
          disperser_reduction
        )) {
          dispersers[r, xi] <- dispersers[r, xi] - 1
        }
      }
      expected_stage_abundance_tm[[tm]][i, ] <- stage_abundance[i, ] - colSums(dispersers) + rowSums(dispersers)
    }
  }
  # Check dispersal function
  set.seed(123)
  expect_equal(
    dispersal_function(
      r = 2,
      tm = 1,
      carrying_capacity_tm_1,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance_tm[[1]]
  )
  set.seed(123) # barrier/friction changes
  expect_equal(
    dispersal_function(
      r = 2,
      tm = 2,
      carrying_capacity_tm_2,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance_tm[[2]]
  )
  set.seed(123) # barrier/friction changes reversed
  expect_equal(
    dispersal_function(
      r = 2,
      tm = 3,
      carrying_capacity_tm_3,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance_tm[[3]]
  )
  set.seed(123) # high dispersal => handle excessive dispersers
  expect_equal(
    dispersal_function(
      r = 2,
      tm = 4,
      carrying_capacity_tm_4,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance_tm[[4]]
  )
})

test_that("density dependent dispersal", {
  simulator <- SimulatorReference$new()
  region <- Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
  dispersal_gen <- DispersalGenerator$new(
    region = region,
    dispersal_proportion = 0.6,
    dispersal_breadth = 110,
    dispersal_max_distance = 300,
    distance_scale = 1000,
    distance_classes = seq(100, 400, 20)
  )
  distance_matrix <- fossil::earth.dist(region$coordinates, dist = F)
  distance_matrix[which(distance_matrix < 1)] <- 0 # ensure actual zero distance for self-referenced cells
  dispersal_gen$calculate_distance_data(distance_matrix = distance_matrix)
  dispersal_gen$calculate_dispersals(type = "matrix")
  dispersal_gen$calculate_dispersals(type = "data")
  # Target abundance N threshold < cutoff (avoids overcrowded cells)
  dispersal_function <- population_dispersal(
    replicates = 4,
    time_steps = 10,
    years_per_step = 1,
    populations = 7,
    demographic_stochasticity = TRUE,
    density_stages = c(0, 1, 1),
    dispersal = dispersal_gen$dispersal_data,
    dispersal_stages = c(0.5, 1, 0),
    dispersal_source_n_k = NULL,
    dispersal_target_k = c(5, 5, 5, 10, 15, 20, 25),
    dispersal_target_n = list(threshold = 10, cutoff = 15),
    simulator = simulator
  )
  carrying_capacity <- c(10, 0, 3, 10, 10, 10, 10) # some target k < 5
  stage_abundance <- matrix(
    c(
      7, 13, 0, 26, 0, 39, 47,
      2, 0, 6, 8, 0, 12, 13,
      0, 3, 4, 6, 0, 9, 10
    ),
    nrow = 3,
    ncol = 7,
    byrow = TRUE
  )
  occupied_indices <- (1:7)[-5]
  occupied_matrix <- array(0, c(7, 7))
  occupied_matrix[, occupied_indices] <- 1
  # Calculate expected abundances
  dispersal_matrix <- dispersal_gen$dispersal_matrix
  expected_stage_abundance <- stage_abundance
  dd_multipliers <- c(1, 0 / 5, 3 / 5, 1, 10 / 15, 10 / 20, 10 / 25) # k /
  # note: density abundance = c(2, 3, 10, 14, 0, 21, 23)
  dd_multipliers <- dd_multipliers * c(1, 1, 1, (15 - 14) / (15 - 10), 1, 0, 0) # n \
  dispersal_matrix <- dispersal_matrix * dd_multipliers
  # dispersal_matrix <- dispersal_matrix*matrix(colSums(dispersal_gen$dispersal_matrix)/colSums(dispersal_matrix),
  #                                             nrow = 7, ncol = 7, byrow = TRUE)
  dispersal_indices <- which(dispersal_matrix * occupied_matrix > 0)
  set.seed(123)
  for (i in 1:2) {
    dispersers <- array(0, c(7, 7))
    dispersers[dispersal_indices] <- stats::rbinom(
      length(dispersal_indices),
      stage_abundance[rep(i, 7), ][dispersal_indices],
      dispersal_matrix[dispersal_indices] *
        c(0.5, 1)[i]
    )
    expected_stage_abundance[i, ] <- stage_abundance[i, ] - colSums(dispersers) + rowSums(dispersers)
  }
  # Disperse overcrowding
  density_abundance <- colSums(expected_stage_abundance * c(0, 1, 1))
  excessive_indices <- which(density_abundance > 15)
  for (excessive_index in excessive_indices[sample(length(excessive_indices))]) {
    target_indices <- which(dispersal_matrix[, excessive_index] > 0 &
      density_abundance < 15)
    for (stage_i in sample(rep(2, times = expected_stage_abundance[2, excessive_index]),
      size = density_abundance[excessive_index] - 15
    )) {
      target_i <- target_indices[sample(length(target_indices),
        size = 1,
        prob = dispersal_matrix[target_indices, excessive_index]
      )]
      expected_stage_abundance[stage_i, excessive_index] <- expected_stage_abundance[stage_i, excessive_index] - 1 # emigrant
      expected_stage_abundance[stage_i, target_i] <- expected_stage_abundance[stage_i, target_i] + 1 # immigrant
    }
  }
  # Check dispersal function
  set.seed(123)
  expect_equal(
    dispersal_function(
      r = 2,
      tm = 1,
      carrying_capacity,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance
  )
  # Set abundance N cutoff as array
  dispersal_function <- population_dispersal(
    replicates = 4,
    time_steps = 10,
    years_per_step = 1,
    populations = 7,
    demographic_stochasticity = TRUE,
    density_stages = c(0, 1, 1),
    dispersal = dispersal_gen$dispersal_data,
    dispersal_stages = c(0.5, 1, 0),
    dispersal_source_n_k = NULL,
    dispersal_target_k = NULL,
    dispersal_target_n = list(
      threshold = 10,
      cutoff = c(15, 15, 15, 15, 20, 25, 30)
    ),
    simulator = simulator
  )
  # Calculate expected abundances
  dispersal_matrix <- dispersal_gen$dispersal_matrix
  expected_stage_abundance <- stage_abundance
  # note: density abundance = c(2, 3, 10, 14, 0, 21, 23)
  dd_multipliers <- c(1, 1, 1, (15 - 14) / (15 - 10), 1, (25 - 21) / (25 - 10), (30 - 23) /
    (30 - 10)) # n \
  dispersal_matrix <- dispersal_matrix * dd_multipliers
  dispersal_indices <- which(dispersal_matrix * occupied_matrix > 0)
  set.seed(123)
  for (i in 1:2) {
    dispersers <- array(0, c(7, 7))
    dispersers[dispersal_indices] <- stats::rbinom(
      length(dispersal_indices),
      stage_abundance[rep(i, 7), ][dispersal_indices],
      dispersal_matrix[dispersal_indices] *
        c(0.5, 1)[i]
    )
    expected_stage_abundance[i, ] <- stage_abundance[i, ] - colSums(dispersers) + rowSums(dispersers)
  }
  # Check dispersal function
  set.seed(123)
  expect_equal(
    dispersal_function(
      r = 2,
      tm = 1,
      carrying_capacity,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance
  )
  # Target abundance N threshold > cutoff (seeks company)
  dispersal_function <- population_dispersal(
    replicates = 4,
    time_steps = 10,
    years_per_step = 1,
    populations = 7,
    demographic_stochasticity = TRUE,
    density_stages = c(0, 1, 1),
    dispersal = dispersal_gen$dispersal_data,
    dispersal_stages = c(0.5, 1, 0),
    dispersal_source_n_k = NULL,
    dispersal_target_k = NULL,
    dispersal_target_n = list(
      threshold = c(5, 10, 15, 20, 20, 20, 25),
      cutoff = c(0, 5, 5, 5, 5, 10, 15)
    ),
    simulator = simulator
  )
  # Calculate expected abundances
  dispersal_matrix <- dispersal_gen$dispersal_matrix
  expected_stage_abundance <- stage_abundance
  # note: density abundance = c(2, 3, 10, 14, 0, 21, 23)
  dd_multipliers <- c(2 / 5, 0, (10 - 5) / (15 - 5), (14 - 5) / (20 - 5), 0, 1, (23 - 15) /
    (25 - 15)) # n /
  dispersal_matrix <- dispersal_matrix * dd_multipliers
  dispersal_indices <- which(dispersal_matrix * occupied_matrix > 0)
  set.seed(123)
  for (i in 1:2) {
    dispersers <- array(0, c(7, 7))
    dispersers[dispersal_indices] <- stats::rbinom(
      length(dispersal_indices),
      stage_abundance[rep(i, 7), ][dispersal_indices],
      dispersal_matrix[dispersal_indices] *
        c(0.5, 1)[i]
    )
    # Handle excessive dispersers for stage = 2 (only)
    for (xi in which(colSums(dispersers) > stage_abundance[i, ])) {
      excessive_rows <- which(dispersers[, xi] > 0)
      excessive_dispersers <- dispersers[excessive_rows, xi]
      disperser_reduction <- colSums(dispersers)[xi] - stage_abundance[i, xi]
      for (r in sample(
        rep(excessive_rows, times = excessive_dispersers),
        disperser_reduction
      )) {
        dispersers[r, xi] <- dispersers[r, xi] - 1
      }
    }
    expected_stage_abundance[i, ] <- stage_abundance[i, ] - colSums(dispersers) + rowSums(dispersers)
  }
  # Check dispersal function
  set.seed(123)
  expect_equal(
    dispersal_function(
      r = 2,
      tm = 1,
      carrying_capacity,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance
  )
  # Source abundance divide by carrying capacity N/K threshold > cutoff (leaves over-exploited cells)
  dispersal_function <- population_dispersal(
    replicates = 4,
    time_steps = 10,
    years_per_step = 1,
    populations = 7,
    demographic_stochasticity = TRUE,
    density_stages = c(0, 1, 1),
    dispersal = dispersal_gen$dispersal_data,
    dispersal_stages = c(0.5, 1, 0),
    dispersal_source_n_k = list(cutoff = -0.5, threshold = 1.5),
    dispersal_target_k = NULL,
    dispersal_target_n = NULL,
    simulator = simulator
  )

  # Calculate expected abundances
  dispersal_matrix <- dispersal_gen$dispersal_matrix
  expected_stage_abundance <- stage_abundance
  # note: density abundance = c(2, 3, 10, 14, 0, 21, 23)
  dd_multipliers <- c((2 / 10 - -0.5) * 1 / 2, 1, 1, (14 / 10 - -0.5) * 1 / 2, (0 / 10 - -0.5) *
    1 / 2, 1, 1)
  dispersal_matrix <- dispersal_matrix * matrix(dd_multipliers,
    nrow = 7,
    ncol = 7,
    byrow = TRUE
  )
  dispersal_indices <- which(dispersal_matrix * occupied_matrix > 0)
  set.seed(123)
  for (i in 1:2) {
    dispersers <- array(0, c(7, 7))
    dispersers[dispersal_indices] <- stats::rbinom(
      length(dispersal_indices),
      stage_abundance[rep(i, 7), ][dispersal_indices],
      dispersal_matrix[dispersal_indices] *
        c(0.5, 1)[i]
    )
    expected_stage_abundance[i, ] <- stage_abundance[i, ] - colSums(dispersers) + rowSums(dispersers)
  }
  # Check dispersal function
  set.seed(123)
  expect_equal(
    dispersal_function(
      r = 2,
      tm = 1,
      carrying_capacity,
      stage_abundance,
      occupied_indices
    ),
    expected_stage_abundance
  )
})
