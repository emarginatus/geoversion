context("valid_crs")
test_that(
  "valid_crs works only with characters",
  expect_error(
    valid_crs(pi)
  )
)
test_that(
  "valid_crs works correctly", {
    x <- c(
      NA, "+proj=longlat +ellps=WGS84", "+proj=longlat +datum=NAD27",
      "+init=epsg:26978", "junk"
    )
    expect_identical(valid_crs(x), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  }
)
