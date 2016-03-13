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
  expect_lt(element$spawn, timestamp)

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
  expect_lt(attributevalue$spawn, timestamp)

  expect_identical(element$spawn, attributevalue$spawn)
})

test_that("it handles the removal of elements", {
  gv <- convert(object = sppolydf[1, ], stable.id = "PermanentID")
  timestamp <- as.numeric(Sys.time())
  store(x = gv, connection = connection)

  element <- dbReadTable(connection, "element") #nolint
  expect_identical(
    element %>%
      filter_(~is.na(destroy)) %>%
      select_(~id, ~features),
    gv@LayerElement
  )
  element <- element %>%
    distinct_(~spawn, ~destroy)
  expect_identical(nrow(element), 2L)
  expect_false(unique(is.na(element$spawn)))
  expect_identical(
    is.na(element$destroy),
    c(TRUE, FALSE)
  )
  expect_gt(element$destroy[2], timestamp)

  features <- dbReadTable(connection, "features") %>% #nolint
    semi_join(
      element %>%
        filter_(~is.na(destroy)),
      by = c("hash" = "features")
    )
  expect_identical(features, gv@Features)

  feature <- dbReadTable(connection, "feature") %>% #nolint
    semi_join(features, by = c("hash" = "feature"))
  expect_identical(feature, gv@Feature)

  coordinates <- dbReadTable(connection, "coordinates") %>% #nolint
    semi_join(feature, by = "hash")
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
      anti_join(element, by = c("element" = "id", "spawn", "destroy")) %>%
      nrow(),
    0L
  )
  expect_identical(
    attributevalue %>%
      filter_(~is.na(destroy)) %>%
      select_(~element, ~attribute, ~value),
    gv@AttributeValue
  )
  attributevalue <- attributevalue %>%
    distinct_(~spawn, ~destroy)
  expect_identical(nrow(attributevalue), 2L)
  expect_false(unique(is.na(attributevalue$spawn)))
  expect_identical(
    is.na(attributevalue$destroy),
    c(TRUE, FALSE)
  )
  expect_gt(attributevalue$destroy[2], timestamp)

  expect_identical(element$spawn, attributevalue$spawn)
  expect_identical(element$destroy, attributevalue$destroy)
})

test_that("store() re-uses features", {
  gv <- convert(object = sppolydf, stable.id = "PermanentID")
  timestamp <- as.numeric(Sys.time())
  store(x = gv, connection = connection)

  element <- dbReadTable(connection, "element") #nolint
  expect_identical(
    element %>%
      group_by_(~features) %>%
      summarise_(
        N = ~n(),
        destroyed = ~sum(is.na(destroy))
      ) %>%
      filter_(~N > 1) %>%
      nrow(),
    1L
  )
  expect_identical(
    element %>%
      filter_(~is.na(destroy)) %>%
      select_(~id, ~features),
    gv@LayerElement
  )
  element <- element %>%
    distinct_(~spawn, ~destroy)
  expect_identical(nrow(element), 3L)
  expect_false(unique(is.na(element$spawn)))
  expect_identical(
    is.na(element$destroy),
    c(TRUE, FALSE, TRUE)
  )

  features <- dbReadTable(connection, "features") %>% #nolint
    semi_join(
      element %>%
        filter_(~is.na(destroy)),
      by = c("hash" = "features")
    )
  expect_identical(features, gv@Features)

  feature <- dbReadTable(connection, "feature") %>% #nolint
    semi_join(features, by = c("hash" = "feature"))
  expect_identical(feature, gv@Feature)

  coordinates <- dbReadTable(connection, "coordinates") %>% #nolint
    semi_join(feature, by = "hash")
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
      anti_join(element, by = c("element" = "id", "spawn", "destroy")) %>%
      nrow(),
    0L
  )
  expect_identical(
    attributevalue %>%
      filter_(~is.na(destroy)) %>%
      select_(~element, ~attribute, ~value) %>%
      arrange_(~element, ~attribute),
    gv@AttributeValue %>%
      arrange_(~element, ~attribute)
  )
  attributevalue <- attributevalue %>%
    distinct_(~spawn, ~destroy)
  expect_identical(nrow(attributevalue), 3L)
  expect_false(unique(is.na(attributevalue$spawn)))
  expect_identical(
    is.na(attributevalue$destroy),
    c(TRUE, FALSE, TRUE)
  )
  expect_gt(attributevalue$spawn[3], timestamp)

  expect_identical(element$spawn, attributevalue$spawn)
  expect_identical(element$destroy, attributevalue$destroy)
})

test_that("handle elements with change in features", {
  gv <- convert(object = sppolydf.bis, stable.id = "PermanentID")
  timestamp <- as.numeric(Sys.time())
  store(x = gv, connection = connection)

  element <- dbReadTable(connection, "element") #nolint
  expect_identical(
    element %>%
      group_by_(~features) %>%
      summarise_(
        N = ~n(),
        destroyed = ~sum(is.na(destroy))
      ) %>%
      filter_(~N > 1) %>%
      nrow(),
    1L
  )
  expect_identical(
    element %>%
      filter_(~is.na(destroy)) %>%
      select_(~id, ~features),
    gv@LayerElement
  )
  element2 <- element %>%
    distinct_(~spawn, ~destroy)
  expect_identical(nrow(element2), 4L)
  expect_false(unique(is.na(element2$spawn)))
  expect_identical(
    is.na(element2$destroy),
    c(TRUE, FALSE, FALSE, TRUE)
  )

  features <- dbReadTable(connection, "features") %>% #nolint
    semi_join(
      element %>%
        filter_(~is.na(destroy)),
      by = c("hash" = "features")
    )
  expect_identical(features, gv@Features)

  feature <- dbReadTable(connection, "feature") %>% #nolint
    semi_join(features, by = c("hash" = "feature"))
  expect_identical(feature, gv@Feature)

  coordinates <- dbReadTable(connection, "coordinates") %>% #nolint
    semi_join(feature, by = "hash")
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
      filter_(~is.na(destroy)) %>%
      select_(~element, ~attribute, ~value) %>%
      arrange_(~element, ~attribute),
    gv@AttributeValue %>%
      arrange_(~element, ~attribute)
  )
  attributevalue <- attributevalue %>%
    distinct_(~spawn, ~destroy)
  expect_identical(nrow(attributevalue), 3L)
  expect_false(unique(is.na(attributevalue$spawn)))
  expect_identical(
    is.na(attributevalue$destroy),
    c(TRUE, FALSE, TRUE)
  )
})

test_that("handle elements with change in attributevalues", {
  sppolydf.bis@data$Text[1] <- "C"
  sppolydf.bis@data$Factor[2] <- NA
  sppolydf.bis@data$Logical <- NULL
  sppolydf.bis@data$Extra <- TRUE
  gv <- convert(object = sppolydf.bis, stable.id = "PermanentID")
  timestamp <- as.numeric(Sys.time())
  store(x = gv, connection = connection)

  element <- dbReadTable(connection, "element") #nolint
  expect_identical(
    element %>%
      group_by_(~features) %>%
      summarise_(
        N = ~n(),
        destroyed = ~sum(is.na(destroy))
      ) %>%
      filter_(~N > 1) %>%
      nrow(),
    1L
  )
  expect_identical(
    element %>%
      filter_(~is.na(destroy)) %>%
      select_(~id, ~features),
    gv@LayerElement
  )
  element2 <- element %>%
    distinct_(~spawn, ~destroy)
  expect_identical(nrow(element2), 4L)
  expect_false(unique(is.na(element2$spawn)))
  expect_identical(
    is.na(element2$destroy),
    c(TRUE, FALSE, FALSE, TRUE)
  )

  features <- dbReadTable(connection, "features") %>% #nolint
    semi_join(
      element %>%
        filter_(~is.na(destroy)),
      by = c("hash" = "features")
    )
  expect_identical(features, gv@Features)

  feature <- dbReadTable(connection, "feature") %>% #nolint
    semi_join(features, by = c("hash" = "feature"))
  expect_identical(feature, gv@Feature)

  coordinates <- dbReadTable(connection, "coordinates") %>% #nolint
    semi_join(feature, by = "hash")
  expect_identical(coordinates, gv@Coordinates)

  attribute <- dbReadTable(connection, "attribute") #nolint
  expect_identical(
    gv@Attribute %>%
      anti_join(attribute, by = c("id", "name", "type")) %>%
      nrow(),
    0L
  )

  attributevalue <- dbReadTable(connection, "attributevalue") #nolint
  expect_identical(
    attributevalue %>%
      filter_(~is.na(destroy)) %>%
      select_(~element, ~attribute, ~value) %>%
      arrange_(~element, ~attribute),
    gv@AttributeValue %>%
      arrange_(~element, ~attribute)
  )
  attributevalue <- attributevalue %>%
    distinct_(~spawn, ~destroy)
  expect_identical(nrow(attributevalue), 6L)
  expect_false(unique(is.na(attributevalue$spawn)))
  expect_identical(
    is.na(attributevalue$destroy),
    c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
  )
})
