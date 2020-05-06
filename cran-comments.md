## Test environments

* local OS X install, R 3.6.1
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

## rhub check results

There were no ERRORs

There were 1 WARNINGs:

We rhub excute my vignettes 'UsageOfMrSGUIDE.Rmd'. Inside the vignettes, I used one function `plotTree()` which has `Suggests` packages. 

It gives:

Error: processing vignette 'UsageOfMrSGUIDE.Rmd' failed with diagnostics:
Package "ggpubr" needed for this function to work. Please install it.

I do not know why it is the case.
