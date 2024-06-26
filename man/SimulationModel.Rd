% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimulationModel.R
\name{SimulationModel}
\alias{SimulationModel}
\title{R6 class representing a simulation model}
\description{
\code{\link[R6:R6Class]{R6}} class representing a spatially-explicit simulation
model. It extends the \code{\link{SpatialModel}} class with a range of common
simulation parameters and functionality for creating a nested model, whereby a nested
template model with fixed parameters is maintained when a model is cloned for various
sampled parameters. Also provided are methods for checking the consistency and
completeness of model parameters.
}
\examples{
# U Island example region
coordinates <- data.frame(
  x = rep(seq(177.01, 177.05, 0.01), 5),
  y = rep(seq(-18.01, -18.05, -0.01), each = 5)
)
template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
template_raster[][-c(7, 9, 12, 14, 17:19)] <- NA # make U Island
region <- Region$new(template_raster = template_raster)
# Model template
template_model <- SimulationModel$new(
  simulation_function = "test_simulator",
  region = region, time_steps = 10
)
template_model$model_attributes <- c(
  template_model$model_attributes,
  "a", "b", "c", "d"
)
template_model$model_attributes
template_model$required_attributes <- c(
  template_model$required_attributes[1:2],
  "a", "b", "c", "d"
)
template_model$required_attributes
template_model$get_attributes(template_model$required_attributes)
template_model$simulation_function
# Nested model
nested_model <- SimulationModel$new(template_model = template_model)
nested_model$region$region_cells
nested_model$set_sample_attributes(a = 1:7, b = 1:10, c = 1:15)
nested_model$sample_attributes
nested_model$get_attributes(c("a", "b", "c", "d"))
# Completeness and consistency
nested_model$is_complete()
nested_model$incomplete_attributes()
nested_model$is_consistent()
nested_model$inconsistent_attributes()
nested_model$set_attributes(c = array(1:70, c(7, 10)), d = 15)
nested_model$is_complete()
nested_model$is_consistent()
# Attached attributes
nested_model$attached
template_model$attached

}
\section{Super classes}{
\code{\link[poems:GenericClass]{poems::GenericClass}} -> \code{\link[poems:GenericModel]{poems::GenericModel}} -> \code{\link[poems:SpatialModel]{poems::SpatialModel}} -> \code{SimulationModel}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{attached}}{A list of dynamically attached attributes (name-value pairs).}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{simulation_function}}{Name (character string) or source path of the default simulation function, which takes a model as an input and returns the simulation results.}

\item{\code{model_attributes}}{A vector of model attribute names.}

\item{\code{region}}{A \code{\link{Region}} (or inherited class) object specifying the study region.}

\item{\code{coordinates}}{Data frame (or matrix) of X-Y population (WGS84) coordinates in longitude (degrees West) and latitude (degrees North) (get and set), or distance-based coordinates dynamically returned by region raster (get only).}

\item{\code{random_seed}}{Number to seed the random number generation for stochasticity.}

\item{\code{replicates}}{Number of replicate simulation runs.}

\item{\code{time_steps}}{Number of simulation time steps.}

\item{\code{years_per_step}}{Number of years per time step.}

\item{\code{results_selection}}{List of simulator-dependent attributes to be included in the returned results of each simulation run.}

\item{\code{attribute_aliases}}{A list of alternative alias names for model attributes (form: \code{alias = "attribute"}) to be used with the set and get attributes methods.}

\item{\code{template_model}}{Nested template model for fixed (non-sampled) attributes for shallow cloning.}

\item{\code{sample_attributes}}{Vector of sample attribute names (only).}

\item{\code{required_attributes}}{Vector of required attribute names (only), i.e. those needed to run a simulation.}

\item{\code{error_messages}}{A vector of error messages encountered when setting model attributes.}

\item{\code{warning_messages}}{A vector of warning messages encountered when setting model attributes.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SimulationModel-new}{\code{SimulationModel$new()}}
\item \href{#method-SimulationModel-new_clone}{\code{SimulationModel$new_clone()}}
\item \href{#method-SimulationModel-get_attribute_names}{\code{SimulationModel$get_attribute_names()}}
\item \href{#method-SimulationModel-get_attributes}{\code{SimulationModel$get_attributes()}}
\item \href{#method-SimulationModel-set_attributes}{\code{SimulationModel$set_attributes()}}
\item \href{#method-SimulationModel-set_sample_attributes}{\code{SimulationModel$set_sample_attributes()}}
\item \href{#method-SimulationModel-is_consistent}{\code{SimulationModel$is_consistent()}}
\item \href{#method-SimulationModel-list_consistency}{\code{SimulationModel$list_consistency()}}
\item \href{#method-SimulationModel-inconsistent_attributes}{\code{SimulationModel$inconsistent_attributes()}}
\item \href{#method-SimulationModel-is_complete}{\code{SimulationModel$is_complete()}}
\item \href{#method-SimulationModel-list_completeness}{\code{SimulationModel$list_completeness()}}
\item \href{#method-SimulationModel-incomplete_attributes}{\code{SimulationModel$incomplete_attributes()}}
\item \href{#method-SimulationModel-clone}{\code{SimulationModel$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="poems" data-topic="GenericModel" data-id="get_attribute"><a href='../../poems/html/GenericModel.html#method-GenericModel-get_attribute'><code>poems::GenericModel$get_attribute()</code></a></span></li>
<li><span class="pkg-link" data-pkg="poems" data-topic="GenericModel" data-id="get_attribute_aliases"><a href='../../poems/html/GenericModel.html#method-GenericModel-get_attribute_aliases'><code>poems::GenericModel$get_attribute_aliases()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationModel-new"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationModel-new}{}}}
\subsection{Method \code{new()}}{
Initialization method sets template model and sets given attributes individually and/or from a list.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationModel$new(template = NULL, required_attributes = NULL, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{template}}{Template simulation model (nested) containing fixed (non-sampled) attributes.}

\item{\code{required_attributes}}{Vector of required attribute names (only), i.e. those needed to run a simulation.}

\item{\code{...}}{Parameters passed via a \emph{params} list or individually.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationModel-new_clone"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationModel-new_clone}{}}}
\subsection{Method \code{new_clone()}}{
Creates a new (re-initialized) object of the current (inherited) object class with optionally passed parameters.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationModel$new_clone(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Parameters passed via the inherited class constructor (defined in initialize and run via new).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
New object of the current (inherited) class.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationModel-get_attribute_names"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationModel-get_attribute_names}{}}}
\subsection{Method \code{get_attribute_names()}}{
Returns a list of all attribute names including public and private model attributes, as well as attached attributes (including those from the template model).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationModel$get_attribute_names()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
List of all attribute names.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationModel-get_attributes"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationModel-get_attributes}{}}}
\subsection{Method \code{get_attributes()}}{
Returns a list of values for selected attributes or attribute aliases (when array of parameter names provided) or all attributes (when no params).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationModel$get_attributes(params = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{params}}{Array of attribute names to return (all when NULL).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
List of selected or all attributes values.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationModel-set_attributes"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationModel-set_attributes}{}}}
\subsection{Method \code{set_attributes()}}{
Sets given attributes (optionally via alias names) individually and/or from a list.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationModel$set_attributes(params = list(), ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{params}}{List of parameters/attributes.}

\item{\code{...}}{Parameters/attributes passed individually.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationModel-set_sample_attributes"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationModel-set_sample_attributes}{}}}
\subsection{Method \code{set_sample_attributes()}}{
Sets the names (only - when \emph{params} is a vector) and values (when \emph{params} is a list and/or when name-value pairs are provided) of the sample attributes for the model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationModel$set_sample_attributes(params = list(), ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{params}}{List of parameters/attributes (names and values) or array of names only.}

\item{\code{...}}{Parameters/attributes passed individually.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationModel-is_consistent"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationModel-is_consistent}{}}}
\subsection{Method \code{is_consistent()}}{
Returns a boolean to indicate if (optionally selected or all) model attributes (such as dimensions) are consistent/valid.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationModel$is_consistent(params = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{params}}{Optional array of parameter/attribute names.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Boolean to indicate consistency of selected/all attributes.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationModel-list_consistency"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationModel-list_consistency}{}}}
\subsection{Method \code{list_consistency()}}{
Returns a boolean to indicate if (optionally selected or all) model attributes (such as dimensions) are consistent/valid.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationModel$list_consistency(params = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{params}}{Optional array of parameter/attribute names.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
List of booleans (or NAs) to indicate consistency of selected/all attributes.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationModel-inconsistent_attributes"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationModel-inconsistent_attributes}{}}}
\subsection{Method \code{inconsistent_attributes()}}{
Returns a list of attributes necessary to simulate the model that are inconsistent/invalid.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationModel$inconsistent_attributes(include_nas = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{include_nas}}{Optional boolean indicating whether of not to include attributes with unknown consistency (NA).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
List of inconsistent attributes which prevent the model simulation (and optionally those where consistency is not available).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationModel-is_complete"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationModel-is_complete}{}}}
\subsection{Method \code{is_complete()}}{
Returns a boolean to indicate if all attributes necessary to simulate the model have been set and are consistent/valid.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationModel$is_complete()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Boolean to indicate model completeness (and consistency).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationModel-list_completeness"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationModel-list_completeness}{}}}
\subsection{Method \code{list_completeness()}}{
Returns a list of booleans (or NAs) for each parameter to indicate attributes that are necessary to simulate the model have been set and are consistent/valid.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationModel$list_completeness()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
List of booleans (or NAs) for each parameter to indicate to indicate completeness (and consistency).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationModel-incomplete_attributes"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationModel-incomplete_attributes}{}}}
\subsection{Method \code{incomplete_attributes()}}{
Returns a list of attributes necessary to simulate the model that are incomplete/inconsistent/invalid.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationModel$incomplete_attributes(include_nas = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{include_nas}}{Optional boolean indicating whether of not to include attributes with unknown completeness (NA).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
List of incomplete attributes which prevent the model simulation (and optionally those where completeness is not available).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationModel-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationModel-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationModel$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
