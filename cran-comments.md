## Test environments
* win-builder (R-devel)
* macOS 13.3.1, M1 chip, R 4.3.1 (via macbuilder)
* local macOS (13.5) install, Intel chip, R 4.3.1
* Ubuntu Linux 20.04.1, R 4.3.1 (via R-hub)

## R CMD check results

0 ERRORs, 0 WARNINGs, 1 NOTE: Possibly misspelled words in DESCRIPTION

The words that failed spell check were "Conover", "Iman", "et al" (all part of the citations in the DESCRIPTION) and
"stochasticity" and "translocations" (common words in ecology referring to sources of randomness and relocating
animals for conservation purposes, respectively.)

## Past R CMD check results

Previously, poems was archived due to some namespace import issues, as well as error-catching tests failing to account for different error messages across operating systems. We have fixed the namespace issues, and we now test the package across all three major platforms to make sure our tests have the expected behavior.

## Other notes

The example for the `SimulationManager` object is in a `dontrun{}` wrapper because it takes ~13 seconds to run. I have experimented with the example and there is no way to cut it down to <5 secs and still show the full functionality of the object as a simulation manager.
