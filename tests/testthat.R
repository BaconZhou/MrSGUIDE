Sys.setenv("R_TESTS" = "")
library(testthat)
library(MrSGUIDE)

test_check("MrSGUIDE")
