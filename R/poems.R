#' poems: Pattern-oriented ensemble modeling and simulation
#'
#' The \emph{poems} package provides a framework of interoperable
#' \code{\link[R6:R6Class]{R6}} classes for building ensembles of viable models via the
#' pattern-oriented modeling (POM) approach (Grimm et al., 2005). The package includes
#' classes for encapsulating and generating model parameters, and managing the POM
#' workflow. The workflow includes: model setup; generating model parameters via
#' Latin hypercube sampling; running multiple sampled model simulations; collating
#' summary results; and validating and selecting an ensemble of models that best match
#' known patterns. By default, model validation and selection utilizes an approximate
#' Bayesian computation (ABC) approach (Beaumont, Zhang, & Balding, 2002), although
#' alternative user-defined functionality could be employed. The package also includes
#' a spatially explicit demographic population model simulation engine, which includes
#' default functionality for density dependence, correlated environmental stochasticity,
#' stage-based transitions, and distance-based dispersal. The user may customize the
#' simulator by defining functionality for translocations, harvesting, mortality,
#' and other processes, as well as defining the sequence order for the simulator
#' processes. The framework could also be adapted for use with other model simulators
#' by utilizing its extendable (inheritable) base classes.
#'
#' @section Framework and workflow:
#' The \emph{poems} framework utilizes a hierarchy  of extendable (inheritable)
#' \code{\link[R6:R6Class]{R6}} class objects that work together to manage a POM
#' workflow for building an ensemble of simulation models.
#' \if{latex}{\cr \figure{framework.pdf}{options: width=5.5in}}
#' \if{html}{\figure{framework.png}}
#' \enumerate{ The workflow is summarized in the following steps:
#'   \item Create a simulation model template (a \code{\link{SimulationModel}} or
#'     inherited class object) with appropriate fixed parameters for the study domain.
#'     Also define a study region via the \code{\link{Region}} class if the
#'     simulations are to be spatially explicit.
#'   \item Create generators (\code{\link{Generator}} or inherited class objects) for
#'     dynamically generating (non-singular) model parameters represented by data
#'     structures, such as arrays or lists.
#'   \item Generate a data frame of sampled variable model parameters using the
#'     \code{\link{LatinHypercubeSampler}}. This will include singular model parameter
#'     values as well as input parameters for the generators.
#'   \item Create a \code{\link{SimulationManager}} object configured with the
#'     simulation model (template), the generators, and the sample parameter data
#'     frame. Running this manager sets and runs the models via the simulator function
#'     for each set (row) of sampled parameters, utilising the generators when
#'     required. The results of each model simulation run are written to a file. A
#'     simulation log file is also created.
#'   \item Create a \code{\link{ResultsManager}} object configured with the sample
#'     parameter data and result file details. Running this manager constructs a data
#'     frame of configured summary metrics, one row for each simulation result file.
#'     The manager utilizes the \code{\link{SimulationResults}} (or inherited) class
#'     to encapsulate, and dynamically generate additional derived, results. The
#'     metrics are generated via user-defined specifications and functions for
#'     calculating each metric from the results (objects).
#'   \item Create a \code{\link{Validator}} object configured with the sample
#'     parameter data, summary metrics, and target (observed) pattern values for each
#'     metric. By default, the validator utilizes an approximate Bayesian computation
#'     (ABC) validation method via the \code{\link[abc:abc]{abc}} library, although
#'     the validator (call) function can be configured to utilize other library or
#'     user-defined functions. Running the validator (with appropriate call function
#'     configuration) produces an ensemble of models (indices to sampled parameters)
#'     that were found to best match the targets. Diagnostic outputs may also be
#'     produced (depending on the call function and its configuration).
#'   \item The selected models may then be utilized for further studies, such as
#'     alternative model scenarios or counter-factuals. This can be achieved by
#'     utilizing the selected subset of parameter samples to form inputs for further
#'     model simulations (by repeating the steps above).
#' }
#'
#' @section Population modeling components:
#' \itemize{ The customizable spatially explicit demographic population model
#'   simulation engine and its associated classes are summarized by the following:
#'   \item \code{\link{population_simulator}} function: The simulation engine's main
#'     function processes the model input parameters, controls the flow, calling other
#'     function modules as required, and returns the results of each simulation.
#'   \item \code{\link{population_density}} function: Module for configuring
#'     and performing density dependence calculations at each simulation time step.
#'     A user-defined function may be utilized.
#'   \item \code{\link{population_env_stoch}} function: Module for configuring and
#'     stochastically applying environmental variability to stage-based population
#'     transition rates at each simulation time step.
#'   \item \code{\link{population_transitions}} function: Module for configuring and
#'     performing stage-based demographic transitions of population abundances at each
#'     simulation time step.
#'   \item \code{\link{population_transformation}} function: Module for configuring and
#'     performing user-defined transformations to staged population abundances. This
#'     functionality is utilized when defining functions for \code{translocation},
#'     \code{harvest}, \code{mortality}, or other custom transformative functions.
#'   \item \code{\link{population_dispersal}} function: Module for configuring and
#'     performing dispersal calculations at each simulation time step. A user-defined
#'     function may be utilized.
#'   \item \code{\link{population_results}} function: Module for configuring,
#'     initializing, and collating simulation results.
#'   \item \code{\link{PopulationModel}} class: Inherited from
#'     \code{\link{SimulationModel}}, this class encapsulates the input parameters
#'     utilized by the \code{\link{population_simulator}}.
#'   \item \code{\link{SimulatorReference}} class: This simple
#'     \code{\link[R6:R6Class]{R6}} class enables user-defined functionality to maintain
#'     persistent (attached) attributes and to write to the simulator results.
#'   \item \code{\link{SpatialCorrelation}} class: Provides functionality for
#'     generating parameters that can be utilized when optionally applying a spatial
#'     correlation within the simulator's environmental variability calculations.
#'   \item \code{\link{DispersalGenerator}} class: Inherited from
#'     \code{\link{Generator}}, this class provides functionality for generating
#'     distance-based dispersal parameters that can be utilized when performing
#'     dispersal calculations.
#'   \item \code{\link{DispersalFriction}} class: Provides functionality for
#'     adjusting the (equivalent) distance between population cells given a
#'     spatiotemporal frictional landscape. These adjustments may be utilized by the
#'     \code{\link{DispersalGenerator}}.
#'   \item \code{\link{PopulationResults}} class: Inherited from
#'     \code{\link{SimulationResults}}, this class encapsulates the results generated
#'     by the \code{\link{population_simulator}}, as well as dynamically generating
#'     additional derived results.
#' }
#'
#' @section References:
#' Beaumont, M. A., Zhang, W., & Balding, D. J. (2002). 'Approximate Bayesian
#' computation in population genetics'. \emph{Genetics}, vol. 162, no. 4, pp, 2025–2035.
#'
#' Grimm, V., Revilla, E., Berger, U., Jeltsch, F., Mooij, W. M., Railsback, S. F.,
#' Thulke, H. H., Weiner, J., Wiegand, T., DeAngelis, D. L., (2005). 'Pattern-Oriented
#' Modeling of Agent-Based Complex Systems: Lessons from Ecology'. \emph{Science}
#' vol. 310, no. 5750, pp. 987–991.
#'
#' @examples
#' # Workflow demonstration with a simple example population model
#'
#' OUTPUT_DIR <- tempdir() # directory for output files
#'
#' # Step 1: Create a simulation model template with a study region
#'
#' # Demonstration example region (U Island)
#' coordinates <- data.frame(x = rep(seq(177.01, 177.05, 0.01), 5),
#'                           y = rep(seq(-18.01, -18.05, -0.01), each = 5))
#' template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
#' template_raster[][-c(7, 9, 12, 14, 17:19)] <- NA # make U Island
#' region <- Region$new(template_raster = template_raster)
#' raster::plot(region$region_raster, main = "Example region (cell indices)",
#'              xlab = "Longitude (degrees)", ylab = "Latitude (degrees)",
#'              colNA = "blue")
#'
#' # Distance-based environmental correlation (via a compacted Cholesky decomposition)
#' env_corr <- SpatialCorrelation$new(region = region, amplitude = 0.4, breadth = 500)
#' correlation <- env_corr$get_compact_decomposition(decimals = 2)
#'
#' # User-defined harvest function (list-nested)
#' harvest <- list(rate = NA, # sample later
#'                 function(params) round(params$stage_abundance*(1 - params$rate)))
#' harvest_rate_alias <- list(harvest_rate = "harvest$rate")
#'
#' # Population (simulation) model template for fixed parameters
#' stage_matrix <- matrix(c(0,   2.5, # Leslie/Lefkovitch matrix
#'                          0.8, 0.5), nrow = 2, ncol = 2, byrow = TRUE,
#'                        dimnames = list(c("juv", "adult"), c("juv", "adult")))
#' model_template <- PopulationModel$new(region = region,
#'                                       time_steps = 10, # years
#'                                       populations = region$region_cells, # 7
#'                                       stages = 2,
#'                                       stage_matrix = stage_matrix,
#'                                       demographic_stochasticity = TRUE,
#'                                       standard_deviation = 0.05,
#'                                       correlation = correlation,
#'                                       density_dependence = "logistic",
#'                                       harvest = harvest,
#'                                       results_selection = c("abundance", "harvested"),
#'                                       attribute_aliases = harvest_rate_alias)
#'
#' # Step 2: Create generators for initial abundance, carrying capacity, and dispersal
#'
#' # Initial abundance and carrying capacity generated via example habitat suitability
#' example_hs <- c(0.8, 1, 0.7, 0.9, 0.6, 0.7, 0.8 )
#' example_hs_raster <- region$region_raster
#' example_hs_raster[region$region_indices] <- example_hs
#' raster::plot(example_hs_raster, main = "Example habitat suitability", colNA = "blue")
#' capacity_gen <- Generator$new(description = "Capacity generator",
#'                               example_hs = example_hs, # template attached
#'                               inputs = c("initial_n", "density_max"),
#'                               outputs = c("initial_abundance", "carrying_capacity"))
#' capacity_gen$add_generative_requirements(list(initial_abundance = "function",
#'                                               carrying_capacity = "function"))
#' capacity_gen$add_function_template("initial_abundance",
#'                                    function_def = function(params) {
#'                                      stats::rmultinom(1, size = params$initial_n,
#'                                                       prob = params$example_hs)[,1]
#'                                    },
#'                                    call_params = c("initial_n", "example_hs"))
#' capacity_gen$add_function_template("carrying_capacity",
#'                                    function_def = function(params) {
#'                                      round(params$density_max*params$example_hs)
#'                                    },
#'                                    call_params = c("density_max", "example_hs"))
#' capacity_gen$generate(input_values = list(initial_n = 500, density_max = 100)) # test
#'
#' # Distance-based dispersal generator
#' dispersal_gen <- DispersalGenerator$new(region = region,
#'                                         dispersal_max_distance = 3000, # in m
#'                                         dispersal_friction = DispersalFriction$new(),
#'                                         inputs = c("dispersal_p", "dispersal_b"),
#'                                         decimals = 5)
#' dispersal_gen$calculate_distance_data()
#' dispersal_gen$generate(input_values = list(dispersal_p = 0.5,
#'                                            dispersal_b = 700))$dispersal_data # test
#'
#' # Step 3: Generate sampled values for variable model parameters via LHS
#'
#' lhs_gen <- LatinHypercubeSampler$new()
#' lhs_gen$set_uniform_parameter("growth_rate_max", lower = 0.4, upper = 0.6, decimals = 2)
#' lhs_gen$set_uniform_parameter("harvest_rate", lower = 0.05, upper = 0.15, decimals = 2)
#' lhs_gen$set_uniform_parameter("initial_n", lower = 400, upper = 600, decimals = 0)
#' lhs_gen$set_uniform_parameter("density_max", lower = 80, upper = 120, decimals = 0)
#' lhs_gen$set_uniform_parameter("dispersal_p", lower = 0.2, upper = 0.5, decimals = 2)
#' lhs_gen$set_uniform_parameter("dispersal_b", lower = 400, upper = 1000, decimals = 0)
#' sample_data <- lhs_gen$generate_samples(number = 12, random_seed = 123)
#' sample_data
#'
#' # Step 4: Create a simulation manager and run the sampled model simulations
#'
#' sim_manager <- SimulationManager$new(sample_data = sample_data,
#'                                      model_template = model_template,
#'                                      generators = list(capacity_gen, dispersal_gen),
#'                                      parallel_cores = 2,
#'                                      results_dir = OUTPUT_DIR)
#' run_output <- sim_manager$run()
#' run_output$summary
#' dir(OUTPUT_DIR) # 12 result files plus simulation log
#'
#' # Step 5: Create a results manager and generate example summary metrics for results
#' results_manager <- ResultsManager$new(simulation_manager = sim_manager,
#'                                       simulation_results = PopulationResults$new(),
#'                                       summary_metrics = c("trend_n", "total_h"),
#'                                       summary_matrices = c("n", "h"),
#'                                       summary_functions = list(
#'                                         trend_n = function(results) {
#'                                           round(results$all$abundance_trend, 2)
#'                                         },
#'                                         total_h = function(results) {
#'                                           sum(results$harvested)
#'                                         },
#'                                         n = "all$abundance",
#'                                         h = "all$harvested"),
#'                                       parallel_cores = 2)
#' gen_output <- results_manager$generate()
#' gen_output$summary
#' dir(OUTPUT_DIR) # plus generation log
#' results_manager$summary_metric_data
#' results_manager$summary_matrix_list
#'
#' # Step 6: Create a validator for selecting the 'best' example models
#'
#' validator <- Validator$new(simulation_parameters = sample_data,
#'                            simulation_summary_metrics =
#'                              results_manager$summary_metric_data[-1],
#'                            observed_metric_targets = c(trend_n = 0, total_h = 600),
#'                            output_dir = OUTPUT_DIR)
#' suppressWarnings(validator$run(tolerance = 0.25, output_diagnostics = TRUE))
#' dir(OUTPUT_DIR) # plus validation diagnostics (see abc library documentation)
#' validator$selected_simulations # top 3 models (stable abundance and high harvest)
#'
#' # Plot the simulation, targets, and selected metrics
#' graphics::plot(x = results_manager$summary_metric_data$total_h,
#'                y = results_manager$summary_metric_data$trend_n,
#'                main = "Example model validation",
#'                xlab = "Total harvested", ylab = "Abundance trend")
#' graphics::points(x = 600, y = 0, col = "red", pch = 4)
#' selected_indices <- validator$selected_simulations$index
#' graphics::points(x = results_manager$summary_metric_data$total_h[selected_indices],
#'                  y = results_manager$summary_metric_data$trend_n[selected_indices],
#'                  col = "blue", pch = 3)
#' graphics::legend("bottomleft", legend = c("Summary metrics", "Targets", "Selected"),
#'                  col = c(1, "red", "blue"), pch = c(1, 4, 3), cex = 0.8)
#'
#' # Also examine the diagnostics PDF file to gain insight into the influence of each input
#' # parameter on model selection (via prior distributions, etc.).
#'
#' # Try running with hundreds or thousands of sampled parameters.
#'
#' @docType package
#' @name poems
NULL
