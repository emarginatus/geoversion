#' store a geoVersion object
#' @param x a geoVersion object
#' @param connection a DBIConnection
#' @export
#' @importFrom DBI dbGetQuery dbWriteTable dbReadTable
#' @importFrom dplyr %>% full_join filter_ mutate_ select_ semi_join anti_join
store <- function(x, connection){
  timestamp <- as.numeric(Sys.time())
  element <- dbGetQuery( #nolint
    connection,
    "SELECT id, features, spawn, destroy FROM element WHERE destroy IS NULL"
  ) %>%
    full_join(x@LayerElement, by = "id")
  old <- element %>%
    filter_(~is.na(features.y)) %>%
    mutate_(
      sql = ~sprintf("
UPDATE
  element
SET
  destroy = %f
WHERE
  id = %i AND features = '%s' AND destroy IS NULL",
        timestamp,
        id,
        features.x
      )
    )
  sapply(
    old$sql,
    function(statement){
      dbGetQuery(connection, statement) #nolint
    }
  )
  new.element <- element %>%
    filter_(~is.na(features.x)) %>%
    mutate_(spawn = timestamp, destroy = NA) %>%
    select_(~id, features = ~features.y, ~spawn, ~destroy)
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

  attributevalue <- dbGetQuery( #nolint
    connection,
    "SELECT
      element, attribute, value, spawn, destroy
    FROM
      attributevalue
    WHERE
      destroy IS NULL"
  ) %>%
    full_join(x@AttributeValue, by = c("element", "attribute"))
  old <- attributevalue %>%
    filter_(~is.na(value.y)) %>%
    mutate_(
      sql = ~sprintf("
UPDATE
  attributevalue
SET
  destroy = %f
WHERE
  element = %i AND attribute = '%s' AND destroy IS NULL",
        timestamp,
        element,
        attribute
      )
    )
  sapply(
    old$sql,
    function(statement){
      dbGetQuery(connection, statement) #nolint
    }
  )
  attributevalue %>%
    filter_(~is.na(value.x)) %>%
    mutate_(spawn = timestamp, destroy = NA) %>%
    select_(~element, ~attribute, value = ~value.y, ~spawn, ~destroy) %>%
    dbWriteTable( #nolint
      conn = connection,
      name = "attributevalue",
      append = TRUE
    ) #nolint
  return(invisible(NULL))
}
