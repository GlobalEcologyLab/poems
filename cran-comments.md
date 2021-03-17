## Test environments
* win-builder (devel and release)
* local OS X (10.12.6) install, R 3.6.2
* R-hub builder

## R CMD check results

### win-builder (devel and release) 

There were no ERRORs or WARNINGs.

There were 3 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Sean Haythorne <sean.haythorne@adelaide.edu.au>'

New submission

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

Response to note:
The package performs substantial computations and/or parallel processing 
in these examples. The examples utilize trivial/small data sets and cannot
be reduced or simplified further to demonstrate the functionality 
effectively.

### local OS X (10.12.6) install, R 3.6.2

There were no ERRORs, WARNINGs or NOTEs.

Though a similar output was present:
**  checking examples (19.2s)
   Examples with CPU or elapsed time > 5s
                      user system elapsed
   poems             6.369  0.806   4.023
   DispersalFriction 4.586  0.256   5.254
but reported no ERRORs, WARNINGs or NOTEs.

### R-hub builder

There were no ERRORs or WARNINGs.

There were 3 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Sean Haythorne <sean.haythorne@adelaide.edu.au>'

New submission

* checking sizes of PDF files under 'inst/doc' ... NOTE

Response to note:
The package has a substantial number of components to be demonstrated.
The vignettes provide useful tutorials for demonstrating the workflow
of the process that the package enables.

* checking examples ... NOTE
Examples with CPU (user + system) or elapsed time > 5s
                   user system elapsed
DispersalFriction  3.56   0.19   10.88
poems              0.77   0.14   11.35
DispersalGenerator 0.47   0.03    7.58

Response as before.

## Downstream dependencies
There are currently no downstream dependencies for this package.
