context("Population Results (class)")

test_that("parameter generation with single replicate", {
  TEST_DIRECTORY <- test_path("test_results")
  abundance <- SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_1_results.RData"))$attached$abundance
  abundance_stages <- list(s1 = round(abundance * 0.6), s2 = abundance - round(abundance * 0.6))
  harvested <- round(abundance * 0.3)
  harvested_stages <- list(s1 = round(abundance_stages$s1 * 0.3), s2 = harvested - round(abundance_stages$s1 * 0.3))
  # Initialize from file
  results_model <- PopulationResults$new(results = file.path(TEST_DIRECTORY, "sample_1_results.RData"))
  expect_equal(
    results_model$get_attributes(),
    list(
      abundance = abundance,
      abundance_trend = array(apply(abundance, 1, function(a) trend::sens.slope(as.matrix(a))$estimates)),
      extirpation = apply(abundance, 1, function(a) min(which(a == 0))),
      occupancy = +(abundance > 0)
    )
  )
  expect_equal(
    results_model$all$get_attributes(),
    list(
      abundance = array(colSums(abundance)),
      abundance_trend = array(as.numeric(trend::sens.slope(as.matrix(colSums(abundance)))$estimates)),
      ema = array(colSums(abundance)),
      extirpation = max(apply(abundance, 1, function(a) min(which(a == 0)))),
      occupancy = array(colSums(abundance > 0))
    )
  )
  expect_null(results_model$all$extinction_location)
  coordinates <- data.frame(x = c(1, 3, 2, 2, 3), y = c(3, 3, 2, 1, 1))
  results_model$region <- Region$new(coordinates = coordinates, use_raster = FALSE)
  expect_equal(results_model$all$extinction_location, unlist(as.list(coordinates[1, ])))
  # Initialize from list
  results <- list(
    abundance = abundance, abundance_stages = abundance_stages, harvested = harvested,
    harvested_stages = harvested_stages
  )
  results_model <- PopulationResults$new(results = results)
  results_model$region <- Region$new(coordinates = coordinates, use_raster = FALSE)
  expect_equal(
    results_model$get_attributes(c("abundance", "abundance_stages", "harvested", "harvested_stages")),
    results
  )
  expect_equal(
    results_model$all$get_attributes(c("abundance", "abundance_stages", "harvested", "harvested_stages")),
    list(
      abundance = array(colSums(abundance)),
      abundance_stages = lapply(abundance_stages, function(s) array(colSums(s))),
      harvested = array(colSums(harvested)),
      harvested_stages = lapply(harvested_stages, function(s) array(colSums(s)))
    )
  )
  # With occupancy mask
  occupancy_mask <- c(0, 1, 1, 1, 1)
  results_model$occupancy_mask <- occupancy_mask
  expect_equal(
    results_model$get_attributes(c(
      "abundance", "abundance_stages", "extirpation", "harvested",
      "harvested_stages", "occupancy"
    )),
    list(
      abundance = abundance * occupancy_mask,
      abundance_stages = lapply(abundance_stages, function(s) s * occupancy_mask),
      extirpation = apply(abundance * occupancy_mask, 1, function(a) min(which(a == 0))),
      harvested = harvested * occupancy_mask,
      harvested_stages = lapply(harvested_stages, function(s) s * occupancy_mask),
      occupancy = +(abundance * occupancy_mask > 0)
    )
  )
  results_model$all$abundance_trend <- NULL # clear
  expect_equal(
    results_model$all$get_attributes(),
    list(
      abundance = array(colSums(abundance * occupancy_mask)),
      abundance_stages = lapply(abundance_stages, function(s) array(colSums(s * occupancy_mask))),
      abundance_trend = array(as.numeric(trend::sens.slope(as.matrix(colSums(abundance * occupancy_mask)))$estimates)),
      ema = array(colSums(abundance * occupancy_mask)),
      extirpation = max(apply(abundance * occupancy_mask, 1, function(a) min(which(a == 0)))),
      extinction_location = unlist(as.list(coordinates[2, ])),
      harvested = array(colSums(round(abundance * 0.3) * occupancy_mask)),
      harvested_stages = lapply(harvested_stages, function(s) array(colSums(s * occupancy_mask))),
      occupancy = array(colSums(abundance * occupancy_mask > 0))
    )
  )
  # With trend interval
  results_model$trend_interval <- 1:5
  results_model$all$abundance_trend <- NULL # clear
  expect_equal(
    results_model$all$abundance_trend,
    array(as.numeric(trend::sens.slope(as.matrix(colSums(abundance * occupancy_mask)[1:5]))$estimates))
  )
  results_model$trend_interval <- NULL
  # With burn-in
  results_model$burn_in_steps <- 2
  expect_equal(
    results_model$get_attributes(c(
      "abundance", "abundance_stages", "extirpation", "harvested",
      "harvested_stages", "occupancy"
    )),
    list(
      abundance = (abundance * occupancy_mask)[, 3:10],
      abundance_stages = lapply(abundance_stages, function(s) (s * occupancy_mask)[, 3:10]),
      extirpation = pmax(apply(abundance * occupancy_mask, 1, function(a) min(which(a == 0))) - 2, 0),
      harvested = (round(abundance * 0.3) * occupancy_mask)[, 3:10],
      harvested_stages = lapply(harvested_stages, function(s) (s * occupancy_mask)[, 3:10]),
      occupancy = +(abundance * occupancy_mask > 0)[, 3:10]
    )
  )
  results_model$all$abundance_trend <- NULL # clear
  expect_equal(
    results_model$all$get_attributes(),
    list(
      burn_in_steps = 2,
      abundance = array(colSums(abundance * occupancy_mask))[3:10],
      abundance_stages = lapply(abundance_stages, function(s) array(colSums(s * occupancy_mask))[3:10]),
      abundance_trend = array(as.numeric(trend::sens.slope(as.matrix(colSums(abundance * occupancy_mask))[3:10])$estimates)),
      ema = array(colSums(abundance * occupancy_mask))[3:10],
      extirpation = max(pmax(apply(abundance * occupancy_mask, 1, function(a) min(which(a == 0))) - 2, 0)),
      extinction_location = unlist(as.list(coordinates[2, ])),
      harvested = array(colSums(round(abundance * 0.3) * occupancy_mask))[3:10],
      harvested_stages = lapply(harvested_stages, function(s) array(colSums(s * occupancy_mask))[3:10]),
      occupancy = array(colSums(abundance * occupancy_mask > 0))[3:10]
    )
  )
  # With trend interval and burn-in
  results_model$trend_interval <- 1:5
  results_model$all$abundance_trend <- NULL # clear
  expect_equal(
    results_model$all$abundance_trend,
    array(as.numeric(trend::sens.slope(as.matrix(colSums(abundance * occupancy_mask))[3:7])$estimates))
  )
})

test_that("parameter generation with replicates", {
  TEST_DIRECTORY <- test_path("test_results")
  abundance <- array(
    c(
      SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_1_results.RData"))$attached$abundance,
      SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_2_results.RData"))$attached$abundance,
      SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_3_results.RData"))$attached$abundance
    ),
    c(5, 10, 3)
  )
  abundance_stages <- list(s1 = round(abundance * 0.6), s2 = abundance - round(abundance * 0.6))
  harvested <- round(abundance * 0.3)
  harvested_stages <- list(s1 = round(abundance_stages$s1 * 0.3), s2 = harvested - round(abundance_stages$s1 * 0.3))
  coordinates <- data.frame(x = c(1, 3, 2, 2, 3), y = c(3, 3, 2, 1, 1))
  occupancy_mask <- c(0, 1, 1, 1, 1)
  results_model <- PopulationResults$new(
    region = Region$new(coordinates = coordinates, use_raster = FALSE),
    occupancy_mask = occupancy_mask, burn_in_steps = 2,
    results = list(
      abundance = abundance, abundance_stages = abundance_stages,
      harvested = harvested, harvested_stages = harvested_stages
    )
  )
  expect_equal(
    results_model$get_attributes(c(
      "abundance", "abundance_stages", "extirpation", "harvested",
      "harvested_stages", "occupancy"
    )),
    list(
      abundance = (abundance * occupancy_mask)[, 3:10, ],
      abundance_stages = lapply(abundance_stages, function(s) (s * occupancy_mask)[, 3:10, ]),
      extirpation = pmax(apply(
        abundance * occupancy_mask, c(1, 3),
        function(a) {
          if (any(a == 0)) min(which(a == 0)) else NA
        }
      ) - 2, 0),
      harvested = (round(abundance * 0.3) * occupancy_mask)[, 3:10, ],
      harvested_stages = lapply(harvested_stages, function(s) (s * occupancy_mask)[, 3:10, ]),
      occupancy = +(abundance * occupancy_mask > 0)[, 3:10, ]
    )
  )
  expect_named(
    results_model$all$get_attributes(),
    c(
      "region", "coordinates", "burn_in_steps", "abundance", "abundance_stages", "abundance_trend", "ema",
      "extirpation", "extinction_location", "harvested", "harvested_stages", "occupancy"
    )
  )
  expect_equal(
    results_model$all$get_attributes(c(
      "abundance", "abundance_stages", "abundance_trend", "ema",
      "extirpation", "extinction_location", "harvested",
      "harvested_stages", "occupancy"
    )),
    list(
      abundance = colSums(abundance * occupancy_mask)[3:10, ],
      abundance_stages = lapply(abundance_stages, function(s) colSums(s * occupancy_mask)[3:10, ]),
      abundance_trend = array(apply(
        colSums(abundance * occupancy_mask)[3:10, ], 2,
        function(a) as.numeric(trend::sens.slope(a)$estimates)
      )),
      ema = array(rowMeans(colSums(abundance * occupancy_mask)[3:10, ])),
      extirpation = pmax(apply(
        colSums(abundance * occupancy_mask), 2,
        function(a) {
          if (any(a == 0)) min(which(a == 0)) else NA
        }
      ) - 2, 0),
      extinction_location = rbind(unlist(as.list(coordinates[2, ])), array(NA, c(2, 2))),
      harvested = colSums(round(abundance * 0.3) * occupancy_mask)[3:10, ],
      harvested_stages = lapply(harvested_stages, function(s) colSums(s * occupancy_mask)[3:10, ]),
      occupancy = colSums(abundance * occupancy_mask > 0)[3:10, ]
    )
  )
  # With trend interval
  results_model$trend_interval <- 1:5
  results_model$all$abundance_trend <- NULL # clear
  expect_equal(
    results_model$all$abundance_trend,
    array(apply(
      colSums(abundance * occupancy_mask)[3:7, ], 2,
      function(a) as.numeric(trend::sens.slope(a)$estimates)
    ))
  )
})

test_that("parameter generation with summaries", {
  TEST_DIRECTORY <- test_path("test_results")
  abundance <- SimulationResults$new(results = file.path(TEST_DIRECTORY, "sample_1_results.RData"))$attached$abundance
  abundance <- round(array(abundance, c(5, 10, 3)) * array(rep(c(0.7, 1, 1.3), each = 50), c(5, 10, 3)))
  abundance_stages <- list(s1 = round(abundance * 0.6), s2 = abundance - round(abundance * 0.6))
  harvested <- round(abundance * 0.3)
  harvested_stages <- list(s1 = round(abundance_stages$s1 * 0.3), s2 = harvested - round(abundance_stages$s1 * 0.3))
  abundance <- list(
    mean = apply(abundance, c(1, 2), mean), sd = apply(abundance, c(1, 2), sd),
    min = apply(abundance, c(1, 2), min), max = apply(abundance, c(1, 2), max)
  )
  abundance_stages <- lapply(
    abundance_stages,
    function(s) {
      list(
        mean = apply(s, c(1, 2), mean), sd = apply(s, c(1, 2), sd),
        min = apply(s, c(1, 2), min), max = apply(s, c(1, 2), max)
      )
    }
  )
  harvested <- list(
    mean = apply(harvested, c(1, 2), mean), sd = apply(harvested, c(1, 2), sd),
    min = apply(harvested, c(1, 2), min), max = apply(harvested, c(1, 2), max)
  )
  harvested_stages <- lapply(
    harvested_stages,
    function(s) {
      list(
        mean = apply(s, c(1, 2), mean), sd = apply(s, c(1, 2), sd),
        min = apply(s, c(1, 2), min), max = apply(s, c(1, 2), max)
      )
    }
  )
  coordinates <- data.frame(x = c(1, 3, 2, 2, 3), y = c(3, 3, 2, 1, 1))
  occupancy_mask <- c(0, 1, 1, 1, 1)
  results_model <- PopulationResults$new(
    region = Region$new(coordinates = coordinates, use_raster = FALSE),
    occupancy_mask = occupancy_mask, burn_in_steps = 2,
    results = list(
      abundance = abundance, abundance_stages = abundance_stages,
      harvested = harvested, harvested_stages = harvested_stages
    )
  )
  expect_equal(
    results_model$get_attributes(c(
      "abundance", "abundance_stages", "extirpation", "harvested",
      "harvested_stages", "occupancy"
    )),
    list(
      abundance = lapply(abundance, function(a) (a * occupancy_mask)[, 3:10]),
      abundance_stages = lapply(
        abundance_stages,
        function(s) lapply(s, function(a) (a * occupancy_mask)[, 3:10])
      ),
      extirpation = array(pmax(apply(
        abundance$mean * occupancy_mask, 1,
        function(a) {
          if (any(a == 0)) min(which(a == 0)) else NA
        }
      ) - 2, 0)),
      harvested = lapply(harvested, function(h) (h * occupancy_mask)[, 3:10]),
      harvested_stages = lapply(
        harvested_stages,
        function(s) lapply(s, function(h) (h * occupancy_mask)[, 3:10])
      ),
      occupancy = +(abundance$mean * occupancy_mask > 0)[, 3:10]
    )
  )
  expect_named(
    results_model$all$get_attributes(),
    c(
      "region", "coordinates", "burn_in_steps", "abundance", "abundance_stages", "abundance_trend", "ema",
      "extirpation", "extinction_location", "harvested", "harvested_stages", "occupancy"
    )
  )
  expect_equal(
    results_model$all$get_attributes(c(
      "abundance", "abundance_stages", "abundance_trend", "ema",
      "extirpation", "extinction_location", "harvested",
      "harvested_stages", "occupancy"
    )),
    list(
      abundance = list(mean = array(colSums(abundance$mean * occupancy_mask))[3:10]),
      abundance_stages = lapply(
        abundance_stages,
        function(s) list(mean = array(colSums(s$mean * occupancy_mask)[3:10]))
      ),
      abundance_trend = array(as.numeric(trend::sens.slope(as.matrix(colSums(abundance$mean * occupancy_mask))[3:10])$estimates)),
      ema = array(colSums(abundance$mean * occupancy_mask))[3:10],
      extirpation = max(pmax(apply(abundance$mean * occupancy_mask, 1, function(a) min(which(a == 0))) - 2, 0)),
      extinction_location = unlist(as.list(coordinates[2, ])),
      harvested = list(mean = array(colSums(harvested$mean * occupancy_mask))[3:10]),
      harvested_stages = lapply(
        harvested_stages,
        function(s) list(mean = array(colSums(s$mean * occupancy_mask)[3:10]))
      ),
      occupancy = array(colSums(abundance$mean * occupancy_mask > 0))[3:10]
    )
  )
  # With trend interval
  results_model$trend_interval <- 1:5
  results_model$all$abundance_trend <- NULL # clear
  expect_equal(
    results_model$all$abundance_trend,
    array(as.numeric(trend::sens.slope(as.matrix(colSums(abundance$mean * occupancy_mask))[3:7])$estimates))
  )
})
