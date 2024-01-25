## Test environments
* local macOS (13.5) install, Intel chip, R 4.3.2
* Windows, R-devel (via win-builder)
* macOS, Apple Silicon chip, R 4.3.2 (via mac-builder)
* Ubuntu Linux 20.04.1, R 4.3.2 (via R-hub)

## R CMD check results

0 ERRORs, 0 WARNINGs, 1 NOTE.

## Past R CMD check results

Previously, `poems` was archived due to some tests failing on certain Apple Silicon systems due to a dependency on `geosphere`. We have revised those tests so they will pass on Apple Silicon. Tests on CRAN's mac-builder are passing.

## Other notes

The example for the `SimulationManager` object is in a `dontrun{}` wrapper because it takes ~13 seconds to run. I have experimented with the example and there is no way to cut it down to <5 secs and still show the full functionality of the object as a simulation manager.

The words marked as misspelled in the 1 NOTE are:
  Conover (18:33)
  Iman (18:26)
  al (14:81, 22:45)
  et (14:78, 22:42)
  stochasticity (26:68)
  translocations (28:49)

Conover, Iman, et and al are all from an academic citation. Stochasticity and translocations are words, indeed common ones in our field of ecology.
