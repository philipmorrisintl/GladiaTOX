library(testthat)
library(GladiaTOX)

options(testthat.junit.output_file="tests-out.xml")
test_dir("testthat")
test_check("GladiaTOX")
