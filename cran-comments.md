## Test environments
* local macOS 15.6.1 install, Apple Silicon chip, R 4.5.1
* Windows, R-devel (via win-builder)
* macOS, Apple Silicon chip, R 4.5.0 (via mac-builder)
* Ubuntu 24.04, R-devel (via Github Actions)
* Ubuntu 24.04, R 4.5.2 (via Github Actions)
* Ubuntu 24.04, R 4.4.3 (via Github Actions)

## R CMD check results

0 ERRORs, 0 WARNINGs, 0 NOTEs.

## Changes in this version

This version (1.4.0) updates the dependency from the deprecated `qs` package to `qs2`. 

**Important note**: `qs2` is not backward-compatible with `qs`. Users with existing `qs` data files will need to convert them to `qs2` format or continue using an older version of `poems`.

New features:
* Added support for reading `.qs2` files in the Generator class via the file_type = "QS2" parameter
* Added comprehensive test coverage for QS2 file handling (21 new test assertions)
* Updated all documentation to reflect the qs2 migration

## Other notes

Several examples are given the `@examplesIf` tag from `roxygen2` because they run
for too long. They have all been tested locally and run without error.

The words marked as misspelled in the 1 NOTE are:
  Conover (18:33)
  Iman (18:26)
  al (14:81, 22:45)
  et (14:78, 22:42)
  stochasticity (26:68)
  translocations (28:49)

Conover, Iman, et and al are all from an academic citation. Stochasticity and translocations are words, indeed common ones in our field of ecology.

## revdepcheck results

I checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * I saw 0 new problems
 * I failed to check 0 packages