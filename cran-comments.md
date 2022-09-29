## Resubmission
This is a resubmission. In this version I have:
* Updated my email as a maintainer, my previous email at the University of 
  Adelaide was deactivated. I am now based at the University of Melbourne.
* Changed RoxygenNote: 7.2.1 in DESCRIPTION for devtools::check() and
  regenerated man/*.Rd to solve new (non-local) NOTES referring to HTML
  validation problems.
* Updated version number to 1.0.4.
* Placed \dontrun{} around examples that ran over the recommended time on the 
  win-builder checking servers (albeit OK locally). I suspect the win-builder
  servers are not performing well, as they produce inconsistent timing results
  at different times (submissions).
* Otherwise no other code changes.

## Test environments
* win-builder (devel and release)
* local OS X (12.6) install, R 4.1.1

There were no ERRORs or WARNINGs.

There was 1 NOTE (on win-builder only):

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Sean Haythorne <sean.haythorne@unimelb.edu.au>'

  New submission

  Package was archived on CRAN

## Downstream dependencies

I cannot run downstream dependencies via revdepcheck::revdep_check(), which
did result in an OK status for the only dependent package: paleopop 2.1.2 
prior to both packages being archived due to my out-of-date email.
