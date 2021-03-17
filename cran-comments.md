## Test environments
* win-builder (devel and release)
* local OS X (10.12.6) install, R 3.6.2

## R CMD check results
There were no ERRORs or WARNINGs.

For the win-builder (devel and release) checks there were 3 NOTEs reported
but only 2 present (due to new submission of package):

** running examples for arch 'i386' ... [67s] NOTE
Examples with CPU (user + system) or elapsed time > 10s
                   user system elapsed
DispersalFriction  6.77   0.17   19.94
poems              1.51   0.08   19.76
DispersalGenerator 0.97   0.00   13.53

** running examples for arch 'x64' ... [72s] NOTE
Examples with CPU (user + system) or elapsed time > 10s
user system elapsed
DispersalFriction  7.22   0.44   20.91
poems              1.81   0.14   21.25
DispersalGenerator 0.96   0.00   14.40

Note: The package performs substantial computations and/or parallel processing 
      in these examples. The examples utilize trivial/small data sets and cannot
      be reduced or simplified further to demonstrate the functionality 
      effectively.

The Mac OS X check produced a similar output for running example times:
**  checking examples (19.2s)
   Examples with CPU or elapsed time > 5s
                      user system elapsed
   poems             6.369  0.806   4.023
   DispersalFriction 4.586  0.256   5.254
but reported no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.
