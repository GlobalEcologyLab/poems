## Test environments
* local macOS (13.5) install, Intel chip, R 4.3.3
* Windows, R-devel (via win-builder)
* macOS, Apple Silicon chip, R 4.3.3 (via mac-builder)

## R CMD check results

0 ERRORs, 0 WARNINGs, 1 NOTE.

## Past R CMD check results

Previously, `poems` was archived due to some tests failing on certain Apple Silicon systems due to a dependency on `geosphere`. We have revised those tests so they will pass on Apple Silicon. Tests on CRAN's mac-builder are passing.

`poems` has also been turned away from CRAN due to long vignette runtimes. I have cut down vignette runtime to a third of what it was previously, which should hopefully solve this issue.

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
