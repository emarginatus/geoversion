#' store a geoVersion object
#' @param x a geoVersion object
#' @param name the name of the layer
#' @param connection a DBIConnection
#' @export
#' @importFrom DBI dbGetQuery dbWriteTable dbReadTable dbIsValid dbQuoteString
#' @importFrom dplyr %>% full_join filter_ mutate_ select_ semi_join anti_join rowwise transmute_
#' @importFrom assertthat assert_that is.string
store <- function(x, name, connection){
  assert_that(inherits(x, "geoVersion"))
  assert_that(is.string(name))
  assert_that(inherits(connection, "DBIConnection"))
  assert_that(dbIsValid(connection)) #nolint

  timestamp <- as.numeric(Sys.time())

  if (all(x@Feature$type %in% c("S", "H"))) {
    type <- "S"
  } else {
    x@Feature$type %>%
      unique() %>%
      sort() %>%
      paste(collapse = ", ") %>%
      sprintf(
        fmt = "storing geoVersion with feature types %s not yet handled"
      ) %>%
      stop(call. = FALSE)
  }
  layerhash <- sprintf("
SELECT
  hash
FROM
  layer
WHERE
  name = %s AND
  type = %s AND
  destroy IS NULL",
    dbQuoteString(connection, name), #nolint
    dbQuoteString(connection, type) #nolint
  ) %>%
    dbGetQuery(conn = connection) #nolint
  if (nrow(layerhash) == 0) {
    layerhash <- sha1(list(Name = name, Type = type, Spawn = timestamp))
    data.frame(
      hash = layerhash,
      name = name,
      type = type,
      spawn = timestamp,
      destroy = NA_real_,
      stringsAsFactors = FALSE
    ) %>%
      dbWriteTable(conn = connection, name = "layer", append = TRUE) #nolint
  } else {
    layerhash <- layerhash$hash
  }

  # compare exisiting CRS
  compare.crs <- sprintf("
SELECT
  value,
  spawn
FROM
  crs
WHERE
  layer = %s AND
  destroy IS NULL",
    dbQuoteString(connection, layerhash) #nolint
  ) %>%
    dbGetQuery(conn = connection) %>% #nolint
    full_join(
      data.frame(
        value = x@CRS@projargs,
        stringsAsFactors = FALSE
      ),
      by = "value"
    )
  if (nrow(compare.crs) > 1) {
    sprintf("
UPDATE
  crs
SET
  destroy = %.21f
WHERE
  layer = %s AND
  spawn = %.21f AND
  destroy IS NULL
",
      timestamp,
      dbQuoteString(conn = connection, layerhash), #nolint
      max(compare.crs$spawn, na.rm = TRUE)
    ) %>%
      dbGetQuery(conn = connection) #nolint
    compare.crs <- compare.crs %>%
      filter_(~is.na(spawn))
  }
  if (is.na(compare.crs$spawn)) {
    data.frame(
      layer = layerhash,
      value = x@CRS@projargs,
      spawn = timestamp,
      destroy = NA_real_,
      stringsAsFactors = FALSE
    ) %>%
    dbWriteTable(conn = connection, name = "crs", append = TRUE) #nolint
  }

  sprintf(
    "SELECT id, hash FROM layerelement WHERE layer = %s",
    dbQuoteString(connection, layerhash) #nolint
  ) %>%
    dbGetQuery(conn = connection) %>% #nolint
    anti_join(x = x@LayerElement, by = "id") %>%
    rowwise() %>%
    transmute_(
      layer = ~layerhash,
      ~id,
      hash = ~sha1(list(Layer = layer, ID = id))
    ) %>%
    as.data.frame() %>%
    dbWriteTable( #nolint
      conn = connection,
      name = "layerelement",
      append = TRUE
    )

  element <- sprintf("
SELECT
  layer,
  id,
  element.hash,
  features,
  element.spawn,
  element.destroy
FROM
  (
    layer
  INNER JOIN
    layerelement
  ON
    layer.hash = layerelement.layer
  )
INNER JOIN
  element
ON
  layerelement.hash = element.hash
WHERE
  element.destroy IS NULL AND
  layer.name = %s",
      dbQuoteString(connection, name) #nolint
    ) %>%
    dbGetQuery(conn = connection) %>% #nolint
    full_join(x@LayerElement, by = "id") %>%
    rowwise() %>%
    mutate_(
      layer = ~ifelse(
        is.na(layer),
        layerhash,
        layer
      ),
      hash = ~ifelse(
        is.na(hash),
        sha1(list(Layer = layerhash, ID = id)),
        hash
      )
    ) %>%
    as.data.frame()
  old <- element %>%
    filter_(~is.na(features.y) | features.x != features.y) %>%
    mutate_(
      sql = ~sprintf("
UPDATE
  element
SET
  destroy = %.21f
WHERE
  hash = %s AND features = %s AND destroy IS NULL",
        timestamp,
        dbQuoteString(connection, hash), #nolint
        dbQuoteString(connection, features.x) #nolint
      )
    )
  sapply(
    old$sql,
    function(statement){
      dbGetQuery(connection, statement) #nolint
    }
  )
  new.element <- element %>%
    filter_(~is.na(features.x) | features.x != features.y) %>%
    mutate_(spawn = timestamp, destroy = NA_real_) %>%
    select_(~hash, features = ~features.y, ~spawn, ~destroy)
  dbWriteTable(connection, "element", new.element, append = TRUE) #nolint

  new.features <- x@Features %>%
    semi_join(new.element, by = c("hash" = "features"))
  dbWriteTable( #nolint
    new.features,
    conn = connection,
    name = "staging_features",
    overwrite = TRUE
  )
  dbGetQuery( #nolint
    connection, "
INSERT INTO
  features
    SELECT
      staging_features.hash, staging_features.feature
    FROM
      staging_features
    LEFT JOIN
      (
        SELECT
          hash, feature, 1 AS current
        FROM
          features
      ) AS current
    ON
      staging_features.hash = current.hash AND
      staging_features.feature = current.feature
    WHERE current IS NULL")

  new.feature <- x@Feature %>%
    semi_join(new.features, by = c("hash" = "feature"))
  dbWriteTable( #nolint
    new.feature,
    conn = connection,
    name = "staging_feature",
    overwrite = TRUE
  )
  dbGetQuery( #nolint
    connection, "
INSERT INTO
  feature
    SELECT
      staging_feature.hash, staging_feature.type
    FROM
      staging_feature
    LEFT JOIN
      (
        SELECT
          hash, 1 AS current
        FROM
          feature
      ) AS current
    ON
      staging_feature.hash = current.hash
    WHERE current IS NULL")

  x@Coordinates %>%
    semi_join(new.feature, by = "hash") %>%
    dbWriteTable( #nolint
      conn = connection,
      name = "staging_coordinates",
      overwrite = TRUE
    )
  dbGetQuery( #nolint
    connection, "
INSERT INTO
  coordinates
    SELECT
      staging_coordinates.hash, succession, x, y
    FROM
      staging_coordinates
    LEFT JOIN
      (
        SELECT DISTINCT
          hash, 1 AS current
        FROM
          coordinates
      ) AS current
    ON
      staging_coordinates.hash = current.hash
    WHERE current IS NULL")

  x@Attribute %>%
    select_(~id, ~name, ~type) %>%
    dbWriteTable( #nolint
      conn = connection,
      name = "staging_attribute",
      overwrite = TRUE
    )
  dbGetQuery( #nolint
    connection, "
INSERT INTO
  attribute
    SELECT
      staging_attribute.id, name, type
    FROM
      staging_attribute
    LEFT JOIN
      (
        SELECT DISTINCT
          id, 1 AS current
        FROM
          attribute
      ) AS current
    ON
      staging_attribute.id = current.id
    WHERE current IS NULL")


  attributevalue <- sprintf("
SELECT
  element,
  attribute,
  value,
  attributevalue.spawn AS spawn,
  attributevalue.destroy AS destroy
FROM
  attributevalue
INNER JOIN
  (
    layer
  INNER JOIN
    layerelement
  ON
    layer.hash = layerelement.layer
  )
ON
  attributevalue.element = layerelement.hash
WHERE
  attributevalue.destroy IS NULL AND
  layer.name = %s",
    dbQuoteString(connection, name) #nolint
  ) %>%
    dbGetQuery(conn = connection) %>% #nolint
    full_join(
      x@AttributeValue %>%
        rowwise() %>%
        mutate_(
          element = ~sha1(list(Layer = layerhash, ID = element))
        ),
      by = c("element", "attribute")
    )
  old <- attributevalue %>%
    filter_(~is.na(value.y) | value.x != value.y) %>%
    mutate_(
      sql = ~sprintf("
UPDATE
  attributevalue
SET
  destroy = %.21f
WHERE
  element = %s AND attribute = %s AND destroy IS NULL",
        timestamp,
        dbQuoteString(connection, element), #nolint
        dbQuoteString(connection, attribute) #nolint
      )
    )
  sapply(
    old$sql,
    function(statement){
      dbGetQuery(connection, statement) #nolint
    }
  )
  attributevalue %>%
    filter_(~is.na(value.x) | value.x != value.y) %>%
    mutate_(spawn = timestamp, destroy = NA_real_) %>%
    select_(~element, ~attribute, value = ~value.y, ~spawn, ~destroy) %>%
    dbWriteTable( #nolint
      conn = connection,
      name = "attributevalue",
      append = TRUE
    )
  return(invisible(NULL))
}
