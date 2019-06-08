context("Test that shiny reports are working")

test_that("shiny reports can be expanded and rendered", {
  skip_on_cran()

  # Expect no errors
  expect_failure(
    expect_error(
      nil <- capture_messages(capture_output( # Make render shut up
        source(system.file("shiny/run-reports.R", package = "GMCM"))
      ))
    )
  )
})



