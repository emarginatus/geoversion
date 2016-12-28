context("store geoVersion in a database")
layername <- "test"
test_that("it stores a geoVersion correctly in an empty database", {
  gv <- convert(object = sppolydf, stable.id = "PermanentID")
  store(x = gv, name = layername, connection = connection)
  element <- dbGetQuery( #nolint
    connection, "
    SELECT
      id,
      features,
      value AS crs,
      element.spawn,
      element.destroy
    FROM
      (
        layerelement
      INNER JOIN
        element
      ON
        layerelement.hash = element.hash
      )
    INNER JOIN
      crs
    ON
      layerelement.hash = crs.element"
  )
  expect_identical(
    element %>%
      select_(~id, ~features, ~crs) %>%
      arrange_(~id),
    gv@LayerElement
  )
  element <- element %>%
    distinct_(~spawn, ~destroy, .keep_all = TRUE)
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

  attributevalue <- dbGetQuery( #nolint
    connection, "
    SELECT
      id AS element,
      attribute,
      value,
      spawn,
      destroy
    FROM
      layerelement
    INNER JOIN
      attributevalue
    ON
      layerelement.hash = attributevalue.element"
  )
  expect_identical(
    attributevalue %>%
      select_(~element, ~attribute, ~value) %>%
      arrange_(~element, ~attribute),
    gv@AttributeValue %>%
      arrange_(~element, ~attribute)
  )
  attributevalue <- attributevalue %>%
    distinct_(~spawn, ~destroy, .keep_all = TRUE)
  expect_identical(nrow(attributevalue), 1L)
  expect_false(is.na(attributevalue$spawn))
  expect_true(is.na(attributevalue$destroy))

  expect_identical(element$spawn, attributevalue$spawn)
})

test_that(
  "no change when the geoVersion equals the current version in the database", {
  gv <- convert(object = sppolydf, stable.id = "PermanentID")
  timestamp <- as.numeric(Sys.time())
  store(x = gv, name = layername, connection = connection)
  element <- dbGetQuery( #nolint
    connection, "
    SELECT
      id,
      features,
      value AS crs,
      element.spawn,
      element.destroy
    FROM
      (
        layerelement
      INNER JOIN
        element
      ON
        layerelement.hash = element.hash
      )
    INNER JOIN
      crs
    ON
      layerelement.hash = crs.element"
  )
  expect_identical(
    element %>%
      select_(~id, ~features, ~crs) %>%
      arrange_(~id),
    gv@LayerElement
  )
  element <- element %>%
    distinct_(~spawn, ~destroy, .keep_all = TRUE)
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

  attributevalue <- dbGetQuery( #nolint
    connection, "
SELECT
  id AS element,
  attribute,
  value,
  spawn,
  destroy
FROM
  layerelement
INNER JOIN
  attributevalue
ON
  layerelement.hash = attributevalue.element"
  )
  expect_identical(
    attributevalue %>%
      select_(~element, ~attribute, ~value) %>%
      arrange_(~element, ~attribute),
    gv@AttributeValue %>%
      arrange_(~element, ~attribute)
  )
  attributevalue <- attributevalue %>%
    distinct_(~spawn, ~destroy, .keep_all = TRUE)
  expect_identical(nrow(attributevalue), 1L)
  expect_false(is.na(attributevalue$spawn))
  expect_true(is.na(attributevalue$destroy))
  expect_lt(attributevalue$spawn, timestamp)

  expect_identical(element$spawn, attributevalue$spawn)
})

test_that("it handles the removal of elements", {
  gv <- convert(object = sppolydf[1, ], stable.id = "PermanentID")
  timestamp <- as.numeric(Sys.time())
  store(x = gv, name = layername, connection = connection)

  element <- dbGetQuery( #nolint
    connection, "
    SELECT
      id,
      features,
      value AS crs,
      element.spawn,
      element.destroy
    FROM
      (
        layerelement
      INNER JOIN
        element
      ON
        layerelement.hash = element.hash
      )
    INNER JOIN
      crs
    ON
      layerelement.hash = crs.element"
  )
  expect_identical(
    element %>%
      filter_(~is.na(destroy)) %>%
      select_(~id, ~features, ~crs) %>%
      arrange_(~id),
    gv@LayerElement
  )
  element <- element %>%
    distinct_(~spawn, ~destroy, .keep_all = TRUE) %>%
    arrange_(~id)
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

  attributevalue <- dbGetQuery( #nolint
    connection, "
    SELECT
      id AS element,
      attribute,
      value,
      spawn,
      destroy
    FROM
      layerelement
    INNER JOIN
      attributevalue
    ON
      layerelement.hash = attributevalue.element"
  )
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
    distinct_(~spawn, ~destroy, .keep_all = TRUE) %>%
    arrange_(~element, ~attribute)
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
  store(x = gv, name = layername, connection = connection)

  element <- dbGetQuery( #nolint
    connection, "
    SELECT
      id,
      features,
      value AS crs,
      element.spawn,
      element.destroy
    FROM
      (
        layerelement
      INNER JOIN
        element
      ON
        layerelement.hash = element.hash
      )
    INNER JOIN
      crs
    ON
      layerelement.hash = crs.element"
  )
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
      select_(~id, ~features, ~crs) %>%
      arrange_(~id),
    gv@LayerElement %>%
      arrange_(~id)
  )
  element <- element %>%
    distinct_(~spawn, ~destroy, .keep_all = TRUE) %>%
    arrange_(~id, ~spawn)
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

  attributevalue <- dbGetQuery( #nolint
    connection, "
    SELECT
      id AS element,
      attribute,
      value,
      spawn,
      destroy
    FROM
      layerelement
    INNER JOIN
      attributevalue
    ON
      layerelement.hash = attributevalue.element"
  )
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
    distinct_(~spawn, ~destroy, .keep_all = TRUE) %>%
    arrange_(~element, ~attribute, ~spawn)
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

test_that("handle elements with changes in features", {
  gv <- convert(object = sppolydf.bis, stable.id = "PermanentID")
  timestamp <- as.numeric(Sys.time())
  store(x = gv, name = layername, connection = connection)

  element <- dbGetQuery( #nolint
    connection, "
    SELECT
      id,
      features,
      value AS crs,
      element.spawn,
      element.destroy
    FROM
      (
        layerelement
      INNER JOIN
        element
      ON
        layerelement.hash = element.hash
      )
    INNER JOIN
      crs
    ON
      layerelement.hash = crs.element"
  )
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
      select_(~id, ~features, ~crs) %>%
      arrange_(~id),
    gv@LayerElement %>%
      arrange_(~id)
  )
  element2 <- element %>%
    distinct_(~spawn, ~destroy, .keep_all = TRUE) %>%
    arrange_(~id, ~spawn)
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
    ) %>%
    arrange_(~hash, ~feature)
  expect_identical(
    features,
    gv@Features %>%
      arrange_(~hash, ~feature)
  )

  feature <- dbReadTable(connection, "feature") %>% #nolint
    semi_join(features, by = c("hash" = "feature")) %>%
    arrange_(~hash)
  expect_identical(
    feature,
    gv@Feature %>%
      arrange_(~hash)
  )

  coordinates <- dbReadTable(connection, "coordinates") %>% #nolint
    semi_join(feature, by = "hash") %>%
    arrange_(~hash, ~succession)
  expect_identical(
    coordinates,
    gv@Coordinates %>%
      arrange_(~hash, ~succession)
  )

  attribute <- dbReadTable(connection, "attribute") #nolint
  expect_identical(
    attribute %>%
      arrange_(~id),
    gv@Attribute %>%
      select_(~id, ~name, ~type) %>%
      arrange_(~id)
  )

  attributevalue <- dbGetQuery( #nolint
    connection, "
    SELECT
      id AS element,
      attribute,
      value,
      spawn,
      destroy
    FROM
      layerelement
    INNER JOIN
      attributevalue
    ON
      layerelement.hash = attributevalue.element"
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
    distinct_(~spawn, ~destroy, .keep_all = TRUE) %>%
    arrange_(~element, ~attribute, ~spawn)
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
  store(x = gv, name = layername, connection = connection)

  element <- dbGetQuery( #nolint
    connection, "
    SELECT
      id,
      features,
      value AS crs,
      element.spawn,
      element.destroy
    FROM
      (
        layerelement
      INNER JOIN
        element
      ON
        layerelement.hash = element.hash
      )
    INNER JOIN
      crs
    ON
      layerelement.hash = crs.element"
  )
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
      select_(~id, ~features, ~crs) %>%
      arrange_(~id),
    gv@LayerElement %>%
      arrange_(~id)
  )
  element2 <- element %>%
    distinct_(~spawn, ~destroy, .keep_all = TRUE) %>%
    arrange_(~id, ~spawn)
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
    ) %>%
    arrange_(~hash)
  expect_identical(
    features,
    gv@Features %>%
      arrange_(~hash)
  )

  feature <- dbReadTable(connection, "feature") %>% #nolint
    semi_join(features, by = c("hash" = "feature")) %>%
    arrange_(~hash)
  expect_identical(
    feature,
    gv@Feature %>%
      arrange_(~hash)
  )

  coordinates <- dbReadTable(connection, "coordinates") %>% #nolint
    semi_join(feature, by = "hash") %>%
    arrange_(~hash, ~succession)
  expect_identical(
    coordinates,
    gv@Coordinates %>%
      arrange_(~hash, ~succession)
  )

  attribute <- dbReadTable(connection, "attribute") #nolint
  expect_identical(
    gv@Attribute %>%
      anti_join(attribute, by = c("id", "name", "type")) %>%
      nrow(),
    0L
  )

  attributevalue <- dbGetQuery( #nolint
    connection, "
    SELECT
      id AS element,
      attribute,
      value,
      spawn,
      destroy
    FROM
      layerelement
    INNER JOIN
      attributevalue
    ON
      layerelement.hash = attributevalue.element"
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
    distinct_(~spawn, ~destroy, .keep_all = TRUE) %>%
    arrange_(~element, ~spawn)
  expect_identical(nrow(attributevalue), 6L)
  expect_false(unique(is.na(attributevalue$spawn)))
  expect_identical(
    is.na(attributevalue$destroy),
    c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
})

test_that("it re-uses features across layers", {
  gv <- convert(object = sppolydf, stable.id = "PermanentID")
  timestamp <- as.numeric(Sys.time())
  store(x = gv, name = paste0(layername, 2), connection = connection)

  element <- sprintf("
    SELECT
      id,
      features,
      value AS crs,
      element.spawn,
      element.destroy
    FROM
      (
        (
          layerelement
        INNER JOIN
          layer
        ON
          layerelement.layer = layer.hash
        )
      INNER JOIN
        element
      ON
        layerelement.hash = element.hash
      )
    INNER JOIN
      crs
    ON
      layerelement.hash = crs.element
    WHERE
      layer.name = %s",
    dbQuoteString(connection, paste0(layername, 2)) #nolint
  ) %>%
    dbGetQuery(conn = connection) #nolint
  expect_identical(
    element %>%
      group_by_(~features) %>%
      summarise_(
        N = ~n(),
        destroyed = ~sum(is.na(destroy))
      ) %>%
      filter_(~N > 1) %>%
      nrow(),
    0L
  )
  expect_identical(
    element %>%
      filter_(~is.na(destroy)) %>%
      select_(~id, ~features, ~crs) %>%
      arrange_(~id),
    gv@LayerElement %>%
      arrange_(~id)
  )
  element2 <- element %>%
    distinct_(~spawn, ~destroy, .keep_all = TRUE) %>%
    arrange_(~id, ~spawn)
  expect_identical(nrow(element2), 1L)
  expect_false(unique(is.na(element2$spawn)))
  expect_identical(
    is.na(element2$destroy),
    TRUE
  )

  features <- dbReadTable(connection, "features") %>% #nolint
    semi_join(
      element %>%
        filter_(~is.na(destroy)),
      by = c("hash" = "features")
    ) %>%
    arrange_(~hash)
  expect_identical(
    features,
    gv@Features %>%
      arrange_(~hash)
  )

  feature <- dbReadTable(connection, "feature") %>% #nolint
    semi_join(features, by = c("hash" = "feature")) %>%
    arrange_(~hash)
  expect_identical(
    feature,
    gv@Feature %>%
      arrange_(~hash)
  )

  coordinates <- dbReadTable(connection, "coordinates") %>% #nolint
    semi_join(feature, by = "hash") %>%
    arrange_(~hash, ~succession)
  expect_identical(
    coordinates,
    gv@Coordinates %>%
      arrange_(~hash, ~succession)
  )

  attribute <- dbReadTable(connection, "attribute") #nolint
  expect_identical(
    gv@Attribute %>%
      anti_join(attribute, by = c("id", "name", "type")) %>%
      nrow(),
    0L
  )

  attributevalue <- dbGetQuery( #nolint
    connection, sprintf("
    SELECT
      id AS element,
      attribute,
      value,
      attributevalue.spawn,
      attributevalue.destroy
    FROM
      (
        layer
      INNER JOIN
        layerelement
      ON
        layer.hash = layerelement.layer
      )
    INNER JOIN
      attributevalue
    ON
      layerelement.hash = attributevalue.element AND
      layer.name = '%s'",
    paste0(layername, 2)
    )
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
    distinct_(~spawn, ~destroy, .keep_all = TRUE) %>%
    arrange_(~element, ~spawn)
  expect_identical(nrow(attributevalue), 1L)
  expect_false(unique(is.na(attributevalue$spawn)))
  expect_identical(
    is.na(attributevalue$destroy),
    TRUE
  )
})

test_that("it returns an error when using the wrong type", {
  gv <- convert(object = sppolydf, stable.id = "PermanentID")
  gv@Feature$type <- "junk"
  expect_error(
    store(gv, name = "junk", connection = connection),
    "storing geoVersion with feature types junk not yet handled"
  )
})

test_that("it sets and updates the correct CRS", {
  gv <- convert(object = sppolydf, stable.id = "PermanentID")
  store(x = gv, name = paste0(layername, "crs"), connection = connection)
  stored <- sprintf("
    SELECT
      layer.spawn AS layerspawn,
      crs.value,
      crs.spawn,
      crs.destroy
    FROM
      (
        layer
      INNER JOIN
        layerelement
      ON
        layer.hash = layerelement.layer
      )
    INNER JOIN
      crs
    ON
      layerelement.hash = crs.element
    WHERE
      name = %s AND
      crs.destroy IS NULL
    GROUP BY
      layer.spawn, crs.value, crs.spawn, crs.destroy
    ",
    dbQuoteString(conn = connection, paste0(layername, "crs")) #nolint
  ) %>%
    dbGetQuery(con = connection) #nolint
  expect_identical(nrow(stored), 1L)
  expect_equal(stored$layerspawn, stored$spawn)
  expect_false(is.na(stored$spawn))
  expect_true(is.na(stored$destroy))
  expect_identical(stored$value, sppolydf@proj4string@projargs)

  store(x = gv, name = paste0(layername, "crs"), connection = connection)
  stored <- sprintf("
    SELECT
      layer.spawn AS layerspawn,
      crs.value,
      crs.spawn,
      crs.destroy
    FROM
      (
        layer
      INNER JOIN
        layerelement
      ON
        layer.hash = layerelement.layer
      )
    INNER JOIN
      crs
    ON
      layerelement.hash = crs.element
    WHERE
      name = %s AND
      crs.destroy IS NULL
    GROUP BY
      layer.spawn, crs.value, crs.spawn, crs.destroy
    ",
    dbQuoteString(conn = connection, paste0(layername, "crs")) #nolint
  ) %>%
    dbGetQuery(con = connection) #nolint
  expect_identical(nrow(stored), 1L)
  expect_equal(stored$layerspawn, stored$spawn)
  expect_false(is.na(stored$spawn))
  expect_true(is.na(stored$destroy))
  expect_identical(stored$value, sppolydf@proj4string@projargs)
  stored <- sprintf("
    SELECT
      layer.spawn AS layerspawn,
      crs.value,
      crs.spawn,
      crs.destroy
    FROM
      (
        layer
      INNER JOIN
        layerelement
      ON
        layer.hash = layerelement.layer
      )
    INNER JOIN
      crs
    ON
      layerelement.hash = crs.element
    WHERE
      name = %s AND
      crs.destroy IS NOT NULL
    GROUP BY
      layer.spawn, crs.value, crs.spawn, crs.destroy
    ",
    dbQuoteString(conn = connection, paste0(layername, "crs")) #nolint
  ) %>%
    dbGetQuery(con = connection) #nolint
  expect_identical(nrow(stored), 0L)

  sppolydf@proj4string <- CRS("+proj=longlat +datum=WGS84")
  gv <- convert(object = sppolydf, stable.id = "PermanentID")
  store(x = gv, name = paste0(layername, "crs"), connection = connection)
  stored <- sprintf("
    SELECT
      layer.spawn AS layerspawn,
      crs.value,
      crs.spawn,
      crs.destroy
    FROM
      (
        layer
      INNER JOIN
        layerelement
      ON
        layer.hash = layerelement.layer
      )
    INNER JOIN
      crs
    ON
      layerelement.hash = crs.element
    WHERE
      name = %s AND
      crs.destroy IS NULL
    GROUP BY
      layer.spawn, crs.value, crs.spawn, crs.destroy
    ",
    dbQuoteString(conn = connection, paste0(layername, "crs")) #nolint
  ) %>%
    dbGetQuery(con = connection) #nolint
  expect_identical(nrow(stored), 1L)
  expect_equal(stored$layerspawn, stored$spawn)
  expect_false(is.na(stored$spawn))
  expect_true(is.na(stored$destroy))
  expect_identical(stored$value, sppolydf@proj4string@projargs)
  stored <- sprintf("
    SELECT
      layer.spawn AS layerspawn,
      crs.value,
      crs.spawn,
      crs.destroy
    FROM
      (
        layer
      INNER JOIN
        layerelement
      ON
        layer.hash = layerelement.layer
      )
    INNER JOIN
      crs
    ON
      layerelement.hash = crs.element
    WHERE
      name = %s AND
      crs.destroy IS NOT NULL
    GROUP BY
      layer.spawn, crs.value, crs.spawn, crs.destroy
    ",
    dbQuoteString(conn = connection, paste0(layername, "crs")) #nolint
  ) %>%
    dbGetQuery(con = connection) #nolint
  expect_identical(nrow(stored), 1L)
})
