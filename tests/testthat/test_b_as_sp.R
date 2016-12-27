context("as_sp")
test_that(
  "as_sp converts SpatialPolygonsDataFrame", {
    gv <- convert(sppolydf, "PermanentID")
    expect_is(gv_sp <- as_sp(gv), "SpatialPolygonsDataFrame")
    current <- gv_sp@data %>%
      select_(~-PermanentID) %>%
      sapply(class)
    old <- gv@Attribute[["type"]]
    names(old) <- gv@Attribute[["name"]]
    expect_true(all.equal(current[names(old)], old))
  }
)
