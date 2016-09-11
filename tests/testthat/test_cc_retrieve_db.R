context("retrieve geoVersion from a database")
test_that("it tests if the layer exists", {
  expect_error(
    retrieve(name = "junk", connection = connection),
    "There is no layer named 'junk'"
  )
})

layername <- "test"
test_that("it retrieves a geoVersion correctly", {
  expect_is(
    output <- retrieve(name = layername, connection = connection),
    "SpatialPolygonsDataFrame"
  )

  sppolydf.bis@data$Text[1] <- "C"
  sppolydf.bis@data$Factor[2] <- NA
  sppolydf.bis@data$Logical <- NULL
  sppolydf.bis@data$Extra <- TRUE
  sppolydf.bis@data$Factor <- factor(sppolydf.bis@data$Factor)
  expect_equal(
    sppolydf.bis@data,
    output@data[, colnames(sppolydf.bis@data)]
  )
  expect_identical(
    sp::coordinates(output) %>%
      unname(),
    sp::coordinates(sppolydf.bis) %>%
      unname()
  )
  for (i in seq_along(sppolydf.bis@polygons)) {
    x <- sppolydf.bis@polygons[[i]]
    y <- output@polygons[[i]]

    for (j in seq_along(x@Polygons)) {
      expect_identical(
        x@Polygons[[j]]@coords,
        y@Polygons[[j]]@coords %>%
          unname()
      )
      expect_identical(
        x@Polygons[[j]]@hole,
        y@Polygons[[j]]@hole
      )
      expect_identical(
        x@Polygons[[j]]@ringDir,
        y@Polygons[[j]]@ringDir
      )
      expect_identical(
        x@Polygons[[j]]@labpt,
        y@Polygons[[j]]@labpt
      )
      expect_identical(
        x@Polygons[[j]]@area,
        y@Polygons[[j]]@area
      )
    }
  }
})
