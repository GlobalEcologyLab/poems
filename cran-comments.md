## Resubmission
This is a resubmission. In this version I have:

* Altered CRS in raster definitions in package tests to avoid anticipated 
  errors due to upcoming PROJ 8.

* Added references to package description.

## Test environments
* win-builder (devel and release)
* local OS X (12.1) install, R 4.1.1

There were no ERRORs or WARNINGs.

There was 2+ NOTES (on win-builder only):

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Sean Haythorne <sean.haythorne@adelaide.edu.au>'

* Examples with CPU (user + system) or elapsed time > 10s
                  user system elapsed
DispersalFriction 5.06   0.17   10.64
(there was a repeat of this note in one of the win-builder logs)

Package update submission

## Downstream dependencies
There are currently no downstream dependencies for this package.
