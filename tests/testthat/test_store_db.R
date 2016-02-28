context("store geoVersion in a database")
test_that("it stores a geoVersion correctly in an empty database", {
  gv <- convert(object = sppolydf, stable.id = "PermanentID")
  store(x = gv, connection = connection)
  element <- dbReadTable(connection, "element")
  expect_identical(
    element %>% select_(~id, ~features),
    gv@LayerElement
  )
  element <- element %>%
    distinct_(~spawn, ~destroy)
  expect_identical(nrow(element), 1L)
  expect_false(is.na(element$spawn))
  expect_true(is.na(element$destroy))

  features <- dbReadTable(connection, "features")
  expect_identical(features, gv@Features)

  feature <- dbReadTable(connection, "feature")
  expect_identical(feature, gv@Feature)

  coordinates <- dbReadTable(connection, "coordinates")
  expect_identical(coordinates, gv@Coordinates)

  attribute <- dbReadTable(connection, "attribute")
  expect_identical(
    attribute %>%
      arrange_(~id),
    gv@Attribute %>%
      select_(~id, ~name, ~type) %>%
      arrange_(~id)
  )

  attributevalue <- dbReadTable(connection, "attributevalue")
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
