#' @keywords internal
"_PACKAGE"
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
#'     alternative model scenarios or counterfactuals. This can be achieved by
#'     utilizing the selected subset of parameter samples to form inputs for further
#'     model simulations (by repeating the steps above).
#' }
#'
#' @section Population modeling components:
#' \itemize{ The spatially explicit demographic population model
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
#'     spatio-temporal frictional landscape. These adjustments may be utilized by the
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
#' # Here we demonstrate building and running a simple population model. For a
#' # demonstration of the POM workflow with the model, see vignette("simple_example").
#'
#' # Demonstration example region (U Island) and initial abundance
#' coordinates <- data.frame(
#'   x = rep(seq(177.01, 177.05, 0.01), 5),
#'   y = rep(seq(-18.01, -18.05, -0.01), each = 5)
#' )
#' template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
#' template_raster[][-c(7, 9, 12, 14, 17:19)] <- NA # make U Island
#' region <- Region$new(template_raster = template_raster)
#' initial_abundance <- seq(0, 300, 50)
#' raster::plot(region$raster_from_values(initial_abundance),
#'   main = "Initial abundance", xlab = "Longitude (degrees)",
#'   ylab = "Latitude (degrees)", zlim = c(0, 300), colNA = "blue"
#' )
#'
#' # Set population model
#' pop_model <- PopulationModel$new(
#'   region = region,
#'   time_steps = 5,
#'   populations = 7,
#'   initial_abundance = initial_abundance,
#'   stage_matrix = matrix(c(
#'     0, 2.5, # Leslie/Lefkovitch matrix
#'     0.8, 0.5
#'   ), nrow = 2, ncol = 2, byrow = TRUE),
#'   carrying_capacity = rep(200, 7),
#'   density_dependence = "logistic",
#'   dispersal = (!diag(nrow = 7, ncol = 7)) * 0.05,
#'   result_stages = c(1, 2)
#' )
#'
#' # Run single simulation
#' results <- population_simulator(pop_model)
#' results # examine
#' raster::plot(region$raster_from_values(results$abundance[, 5]),
#'   main = "Final abundance", xlab = "Longitude (degrees)",
#'   ylab = "Latitude (degrees)", zlim = c(0, 300), colNA = "blue"
#' )
#'
#' @name poems
NULL
