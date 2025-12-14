# Testthat setup for shinybeez
# This file is run by rhino::test_r() and devtools::test()

box::use(
  testthat[...],
)

# Set up box module path for testing
options(box.path = getwd())

# Run all tests
test_check("shinybeez")
