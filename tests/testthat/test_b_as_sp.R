context("as_sp")
test_that(
  "as_sp converts SpatialPolygonsDataFrame", {
    gv <- convert(sppolydf, "PermanentID")
    expect_is(as_sp(gv), "SpatialPolygonsDataFrame")
  }
)
