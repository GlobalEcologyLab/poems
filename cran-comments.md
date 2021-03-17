## Test environments
* local OS X (10.12.6) install, R 3.6.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 3 NOTEs reported but only 2 present (due to new submission of package):

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

The package performs substantial computations and/or parallel processing 
in these examples. The examples utilize trivial/small data sets and cannot
be reduced or simplified to demonstrate their functionality.

## Downstream dependencies
There are currently no downstream dependencies for this package.
