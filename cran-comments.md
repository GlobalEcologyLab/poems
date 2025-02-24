## Test environments
* local macOS 15 install, Intel chip, R 4.4.1
* Windows, R-devel (via win-builder)
* macOS, Apple Silicon chip, R 4.4.1 (via mac-builder)
* Ubuntu 24.04, R-devel (via Github Actions)
* Ubuntu 24.04, R 4.4.1 (via Github Actions)
* Ubuntu 24.04, R 3.6 (via Github Actions)

## R CMD check results

0 ERRORs, 0 WARNINGs, 0 NOTEs.

## Past R CMD check results

`poems` was erroring out in R-devel in certain Linux distros due to a faulty test. I have now fixed this.

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

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages