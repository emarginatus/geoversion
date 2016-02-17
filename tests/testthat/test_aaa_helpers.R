context("helpers")

test_that(
  "polygon exists",
  expect_is(polygon, "list")
)
test_that(
  "polygon exists",
  expect_is(polygon[[1]], "Polygon")
)
test_that(
  "polygons exists",
  expect_is(polygons, "list")
)
test_that(
  "polygons exists",
  expect_is(polygons[[1]], "Polygons")
)
test_that(
  "sppoly exists",
  expect_is(sppolydf, "SpatialPolygons")
)
test_that(
  "sppolydf exists",
  expect_is(sppolydf, "SpatialPolygonsDataFrame")
)
