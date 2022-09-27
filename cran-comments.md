## Resubmission
This is a resubmission. In this version I have:
* Updated my email as a maintainer, my previous email at the University of 
  Adelaide was deactivated. I am now based at the University of Melbourne.
* Change RoxygenNote: 7.1.2 in DESCRIPTION for devtools::check()
There were no ERRORS, WARNINGS or NOTES.

## Package update
This is a package update. In this version I have:

* Altered CRS in raster definitions in package tests to avoid anticipated 
  errors due to upcoming PROJ 8
  [#1](https://github.com/GlobalEcologyLab/poems/issues/1)

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

These examples or the functionality they utilize have not changed since the 
package was originally submitted and accepted on CRAN.

Package update submission

## Downstream dependencies

I have ran downstream dependencies via revdepcheck::revdep_check(), which
resulted in an OK status for the only dependent package: paleopop 2.1.2.
