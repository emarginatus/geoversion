context("convert polygons")

test_that(
  "Polygon objects are converted", {
    expect_is(
      poly_c <- convert(polygon[[1]]),
      "list"
    )
    expect_named(
      poly_c,
      c("Coordinates", "Feature")
    )
    expect_is(poly_c$Coordinates, "data.frame")
    expect_is(poly_c$Feature, "data.frame")
    expect_named(
      poly_c$Coordinates,
      c("Hash", "Order", "X", "Y")
    )
    expect_named(
      poly_c$Feature,
      c("Hash", "Type")
    )
    expect_true(poly_c$Feature$Hash %in% poly_c$Coordinates$Hash)
    expect_true(all(poly_c$Coordinates$Hash %in% poly_c$Feature$Hash))
  }
)
