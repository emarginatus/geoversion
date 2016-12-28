context("convert polygons")

test_that(
  "Polygon objects are converted", {
    expect_is(
      poly_c <- convert(polygon[[1]]),
      "geoVersion"
    )
    expect_named(
      poly_c@Coordinates,
      c("hash", "succession", "x", "y")
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
      c("hash", "succession", "x", "y")
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
      c("hash", "succession", "x", "y")
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
      c("hash", "succession", "x", "y")
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
      c("hash", "succession", "x", "y")
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
  "the crs field takes precedence", {
    sppolydf$junk <- "+proj=longlat +ellps=WGS84"
    expect_is(
      output <- convert(
        object = sppolydf,
        stable.id = "PermanentID",
        crs.id = "junk"
      ),
      "geoVersion"
    )
    expect_identical(output@LayerElement$crs, sppolydf$junk)
    expect_identical(
      output@Attribute %>%
        filter_(~name == "junk") %>%
        nrow(),
      0L
    )
    expect_is(
      output <- convert(
        object = sppolydf_90ccw,
        stable.id = "PermanentID",
        crs.id = "crs",
        reference = reference_90ccw,
        transformation = transformation_90ccw
      ),
      "geoVersion"
    )
  }
)
