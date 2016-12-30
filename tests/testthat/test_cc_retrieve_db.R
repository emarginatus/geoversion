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
    "geoVersion"
  )

  sppolydf.bis@data$Text[1] <- "C"
  sppolydf.bis@data$Factor[2] <- NA
  sppolydf.bis@data$Logical <- NULL
  sppolydf.bis@data$Extra <- TRUE
  sppolydf.bis@data$Factor <- factor(sppolydf.bis@data$Factor)
  output_sp <- as_sp(output)

  expect_true(all(colnames(output_sp@data) %in% colnames(sppolydf.bis@data)))
  expect_true(all(colnames(sppolydf.bis@data) %in% colnames(output_sp@data)))
  expect_identical(
    output_sp@data %>%
      anti_join(sppolydf.bis@data, by = "PermanentID") %>%
      nrow(),
    0L
  )
  expect_identical(
    nrow(output_sp@data),
    nrow(sppolydf.bis@data)
  )
  joined <- output_sp@data %>%
    select_(.dots = colnames(sppolydf.bis@data)) %>%
    inner_join(sppolydf.bis@data, by = "PermanentID")
  expect_identical(
    nrow(joined),
    nrow(output_sp@data)
  )
  expect_equal(
    joined %>%
      select_(dplyr:::ends_with(".x")) %>%
      unname(),
    joined %>%
      select_(dplyr:::ends_with(".y")) %>%
      unname()
  )
  expect_identical(
    rgeos::gCovers(sppolydf.bis, output_sp, byid = TRUE) %>%
      unname(),
    sppolydf %>%
      length() %>%
      diag() %>%
      `==`(1L)
  )
  expect_identical(
    rgeos::gCovers(output_sp, sppolydf.bis, byid = TRUE) %>%
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
    "geoVersion"
  )

  sppolydf@data$Factor <- factor(sppolydf@data$Factor)
  output_sp <- as_sp(output)

  expect_true(all(colnames(output_sp@data) %in% colnames(sppolydf@data)))
  expect_true(all(colnames(sppolydf@data) %in% colnames(output_sp@data)))
  expect_identical(
    output_sp@data %>%
      anti_join(sppolydf@data, by = "PermanentID") %>%
      nrow(),
    0L
  )
  expect_identical(
    nrow(output_sp@data),
    nrow(sppolydf.bis@data)
  )
  joined <- output_sp@data %>%
    select_(.dots = colnames(sppolydf@data)) %>%
    inner_join(sppolydf@data, by = "PermanentID")
  expect_identical(
    nrow(joined),
    nrow(output_sp@data)
  )
  expect_equal(
    joined %>%
      select_(dplyr:::ends_with(".x")) %>%
      unname(),
    joined %>%
    select_(dplyr:::ends_with(".y")) %>%
      unname()
  )
  expect_identical(
    rgeos::gCovers(sppolydf, output_sp, byid = TRUE) %>%
      unname(),
    rgeos::gCovers(output_sp, sppolydf, byid = TRUE) %>%
      unname()
  )
})

test_that("it returns the reference and transformation", {
  layername <- "test trans"
  expect_is(
    output <- retrieve(name = layername, connection = connection),
    "geoVersion"
  )
})
