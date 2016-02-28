context("store geoVersion in a database")
test_that("it stores a geoVersion correctly in an empty database", {
  gv <- convert(object = sppolydf, stable.id = "PermanentID")
  store(x = gv, connection = connection)
  element <- dbReadTable(connection, "element") #nolint
  expect_identical(
    element %>% select_(~id, ~features),
    gv@LayerElement
  )
  element <- element %>%
    distinct_(~spawn, ~destroy)
  expect_identical(nrow(element), 1L)
  expect_false(is.na(element$spawn))
  expect_true(is.na(element$destroy))

  features <- dbReadTable(connection, "features") #nolint
  expect_identical(features, gv@Features)

  feature <- dbReadTable(connection, "feature") #nolint
  expect_identical(feature, gv@Feature)

  coordinates <- dbReadTable(connection, "coordinates") #nolint
  expect_identical(coordinates, gv@Coordinates)

  attribute <- dbReadTable(connection, "attribute") #nolint
  expect_identical(
    attribute %>%
      arrange_(~id),
    gv@Attribute %>%
      select_(~id, ~name, ~type) %>%
      arrange_(~id)
  )

  attributevalue <- dbReadTable(connection, "attributevalue") #nolint
  expect_identical(
    attributevalue %>%
      select_(~element, ~attribute, ~value),
    gv@AttributeValue
  )
  attributevalue <- attributevalue %>%
    distinct_(~spawn, ~destroy)
  expect_identical(nrow(attributevalue), 1L)
  expect_false(is.na(attributevalue$spawn))
  expect_true(is.na(attributevalue$destroy))

  expect_identical(element$spawn, attributevalue$spawn)
})
test_that(
  "no change when the geoVersion equals the current version in the database", {
  gv <- convert(object = sppolydf, stable.id = "PermanentID")
  timestamp <- as.numeric(Sys.time())
  store(x = gv, connection = connection)
  element <- dbReadTable(connection, "element") #nolint
  expect_identical(
    element %>% select_(~id, ~features),
    gv@LayerElement
  )
  element <- element %>%
    distinct_(~spawn, ~destroy)
  expect_identical(nrow(element), 1L)
  expect_false(is.na(element$spawn))
  expect_true(is.na(element$destroy))
  expect_less_than(element$spawn, timestamp)

  features <- dbReadTable(connection, "features") #nolint
  expect_identical(features, gv@Features)

  feature <- dbReadTable(connection, "feature") #nolint
  expect_identical(feature, gv@Feature)

  coordinates <- dbReadTable(connection, "coordinates") #nolint
  expect_identical(coordinates, gv@Coordinates)

  attribute <- dbReadTable(connection, "attribute") #nolint
  expect_identical(
    attribute %>%
      arrange_(~id),
    gv@Attribute %>%
      select_(~id, ~name, ~type) %>%
      arrange_(~id)
  )

  attributevalue <- dbReadTable(connection, "attributevalue") #nolint
  expect_identical(
    attributevalue %>%
      select_(~element, ~attribute, ~value),
    gv@AttributeValue
  )
  attributevalue <- attributevalue %>%
    distinct_(~spawn, ~destroy)
  expect_identical(nrow(attributevalue), 1L)
  expect_false(is.na(attributevalue$spawn))
  expect_true(is.na(attributevalue$destroy))
  expect_less_than(attributevalue$spawn, timestamp)

  expect_identical(element$spawn, attributevalue$spawn)
})
test_that("it handles the removal of elements", {
  gv <- convert(object = sppolydf[1, ], stable.id = "PermanentID")
  store(x = gv, connection = connection)
})
