## Test environments
* local macOS 15 install, Intel chip, R 4.5.0
* Windows, R-devel (via win-builder)
* macOS, Apple Silicon chip, R 4.5.0 (via mac-builder)
* Ubuntu 24.04, R-devel (via Github Actions)
* Ubuntu 24.04, R 4.5.0 (via Github Actions)
* Ubuntu 24.04, R 4.4.3 (via Github Actions)

## R CMD check results

0 ERRORs, 0 WARNINGs, 0 NOTEs.

## Past R CMD check results

`poems` did not have a dependency on R 4.1.0 as it should have. I have now updated the DESCRIPTION file.

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