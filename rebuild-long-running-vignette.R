old_wd <- getwd()

setwd("vignettes/")
knitr::knit("thylacine_example.Rmd.orig", output = "thylacine_example.Rmd")
knitr::purl("thylacine_example.Rmd.orig", output = "thylacine_example.R")
knitr::knit("translocation_example.Rmd.orig", output = "translocation_example.Rmd")
knitr::purl("translocation_example.Rmd.orig", output = "translocation_example.R")

setwd(old_wd)