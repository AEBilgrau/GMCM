context("Test that shiny reports are working")

test_that("shiny reports can be expanded and rendered", {
  skip_on_cran()

  # Expect no errors (i.e. fail to provide errors)
  expect_error(
    # capture_messages(capture_output( # Make render shut up
      source(system.file("shiny/run-reports.R", package = "GMCM")),
    # ))
    NA
  )
})



