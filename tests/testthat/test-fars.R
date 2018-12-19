# test-fars.R
# This R script will test one function for the fars_functions.R scrip, that
# are part of the package: farspkg
context("file name formation")

test_that( "test make_filename", {
  expect_that(  make_filename("2015"), equals("accident_2015.csv.bz2") )
} )

