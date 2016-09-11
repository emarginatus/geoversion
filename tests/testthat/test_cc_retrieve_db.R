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
    rgeos::gCovers(sppolydf.bis, output, byid = TRUE) %>%
      unname(),
    sppolydf %>%
      length() %>%
      diag() %>%
      `==`(1L)
  )
  expect_identical(
    rgeos::gCovers(output, sppolydf.bis, byid = TRUE) %>%
      unname(),
    sppolydf %>%
      length() %>%
      diag() %>%
      `==`(1L)
  )
})

test_that("retrieve handles holes in polygons", {
  expect_is(
    output <- retrieve(name = paste0(layername, 2), connection = connection),
    "SpatialPolygonsDataFrame"
  )

  sppolydf@data$Factor <- factor(sppolydf@data$Factor)
  expect_equal(
    sppolydf@data,
    output@data[, colnames(sppolydf@data)]
  )
  expect_identical(
    rgeos::gCovers(sppolydf, output, byid = TRUE) %>%
      unname(),
    sppolydf %>%
      length() %>%
      diag() %>%
      `==`(1L)
  )
  expect_identical(
    rgeos::gCovers(output, sppolydf, byid = TRUE) %>%
      unname(),
    sppolydf %>%
      length() %>%
      diag() %>%
      `==`(1L)
  )
})
