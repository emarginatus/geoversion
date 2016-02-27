context("convert polygons")

test_that(
  "Polygon objects are converted", {
    expect_is(
      poly_c <- convert(polygon[[1]]),
      "geoVersion"
    )
    expect_named(
      poly_c@Coordinates,
      c("hash", "order", "x", "y")
    )
    expect_named(
      poly_c@Feature,
      c("hash", "type")
    )
    expect_true(all(poly_c@Feature$hash %in% poly_c@Coordinates$hash))
    expect_true(all(poly_c@Coordinates$hash %in% poly_c@Feature$hash))
  }
)
test_that(
  "Polygons objects are converted", {
    expect_is(
      poly_c <- convert(polygons[[1]]),
      "geoVersion"
    )
    expect_named(
      poly_c@Coordinates,
      c("hash", "order", "x", "y")
    )
    expect_named(
      poly_c@Feature,
      c("hash", "type")
    )
    expect_true(all(poly_c@Feature$hash %in% poly_c@Coordinates$hash))
    expect_true(all(poly_c@Coordinates$hash %in% poly_c@Feature$hash))
  }
)
test_that(
  "lists of Polygon objects are converted", {
    expect_is(
      poly_c <- convert(polygon),
      "geoVersion"
    )
    expect_named(
      poly_c@Coordinates,
      c("hash", "order", "x", "y")
    )
    expect_named(
      poly_c@Feature,
      c("hash", "type")
    )
    expect_true(all(poly_c@Feature$hash %in% poly_c@Coordinates$hash))
    expect_true(all(poly_c@Coordinates$hash %in% poly_c@Feature$hash))
  }
)
test_that(
  "lists of Polygons objects are converted", {
    expect_is(
      poly_c <- convert(polygons),
      "geoVersion"
    )
    expect_named(
      poly_c@Coordinates,
      c("hash", "order", "x", "y")
    )
    expect_named(
      poly_c@Feature,
      c("hash", "type")
    )
    expect_true(all(poly_c@Feature$hash %in% poly_c@Coordinates$hash))
    expect_true(all(poly_c@Coordinates$hash %in% poly_c@Feature$hash))
  }
)
test_that(
  "SpatialPolygonsDataFrame objects are converted", {
    expect_is(
      poly_c <- convert(sppolydf, stable.id = "PermanentID"),
      "geoVersion"
    )
    expect_is(
      poly_c <- convert(sppolydf2, stable.id = "PermanentID"),
      "geoVersion"
    )
    expect_named(
      poly_c@Coordinates,
      c("hash", "order", "x", "y")
    )
    expect_named(
      poly_c@Feature,
      c("hash", "type")
    )
    expect_true(all(poly_c@Feature$hash %in% poly_c@Coordinates$hash))
    expect_true(all(poly_c@Coordinates$hash %in% poly_c@Feature$hash))
  }
)
test_that(
  "different CRS return an error", {
    poly_c2 <- poly_c <- convert(polygon[[1]])
    poly_c2@CRS <- sp::CRS("+proj=longlat +datum=WGS84")
    expect_error(
      combine(poly_c, poly_c2),
      "CRS not unique"
    )
  }
)
