#' Retrieve a spatial object from the database
#' @inheritParams store
#' @param timestamp The point in time at which the object should be return. Default to NA which implies the latest status.
#' @export
#' @importFrom DBI dbQuoteIdentifier dbGetQuery
#' @importFrom dplyr %>% group_by_ arrange_ do_ select_ ungroup rename_ mutate_ mutate_at filter_ distinct_
#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame CRS
#' @importFrom tidyr spread_
#' @importFrom assertthat assert_that is.string is.number
retrieve <- function(name, connection, timestamp = NA_real_){
  assert_that(is.string(name))
  assert_that(inherits(connection, "DBIConnection"))
  assert_that(is.number(timestamp))

  timerange <- function(name, timestamp, connection){
    if (is.na(timestamp)) {
      timerange <- sprintf(
        "%s.destroy IS NULL",
        DBI::dbQuoteIdentifier(conn = connection, name)
      )
    } else {
      timerange <- sprintf(
        "%s.spawn >= %.21f AND\n%.21f < %s.destroy",
        DBI::dbQuoteIdentifier(conn = connection, name),
        timestamp,
        DBI::dbQuoteIdentifier(conn = connection, name),
        timestamp
      )
    }
  }

  layer <- sprintf(
    "SELECT hash FROM layer WHERE name = %s AND %s",
    DBI::dbQuoteIdentifier(connection, name),
    timerange("layer", timestamp, connection)
  ) %>%
    DBI::dbGetQuery(conn = connection) %>%
    "[["("hash") #nolint
  if (length(layer) == 0) {
    stop("There is no layer named '", name, "'")
  }

  layerelement <- sprintf("
    SELECT
      le.id,
      e.features,
      value AS crs
    FROM
      (
        layerelement AS le
      INNER JOIN
        element AS e
      ON
        le.hash = e.hash
      )
    INNER JOIN
      crs
    ON
      e.hash = crs.element
    WHERE
      layer = %s AND
      %s",
    DBI::dbQuoteString(connection, layer),
    timerange("e", timestamp, connection)
  ) %>%
    DBI::dbGetQuery(conn = connection)
  features <- sprintf("
    SELECT
      ff.hash AS hash,
      ff.feature AS feature
    FROM
      (
        SELECT
          e.features
        FROM
          layerelement AS le
        INNER JOIN
          element AS e
        ON
          le.hash = e.hash
        WHERE
          layer = %s AND
          %s
      ) AS e0
    INNER JOIN
      features AS ff
    ON
      e0.features = ff.hash",
    DBI::dbQuoteString(connection, layer),
    timerange("e", timestamp, connection)
  ) %>%
    DBI::dbGetQuery(conn = connection)
  feature <- sprintf("
    SELECT
      f.hash AS hash,
      f.type AS type
    FROM
      (
        (
          SELECT
            e.features
          FROM
            layerelement AS le
          INNER JOIN
            element AS e
          ON
            le.hash = e.hash
          WHERE
            layer = %s AND
            %s
        ) AS e0
      INNER JOIN
        features AS ff
      ON
        e0.features = ff.hash
      )
    INNER JOIN
      feature AS f
    ON
      ff.feature = f.hash",
    DBI::dbQuoteString(connection, layer),
    timerange("e", timestamp, connection)
  ) %>%
    DBI::dbGetQuery(conn = connection)

  coordinates <- sprintf("
    SELECT
      c.hash AS hash,
      c.succession AS succession,
      c.x AS x,
      c.y AS y
    FROM
      (
        (
          SELECT
            e.features
          FROM
            layerelement AS le
          INNER JOIN
            element AS e
          ON
            le.hash = e.hash
          WHERE
            layer = %s AND
            %s
        ) AS e0
      INNER JOIN
        features AS ff
      ON
        e0.features = ff.hash
      )
    INNER JOIN
      coordinates AS c
    ON
      ff.feature = c.hash",
    DBI::dbQuoteString(connection, layer),
    timerange("e", timestamp, connection)
  ) %>%
    DBI::dbGetQuery(conn = connection)

  attribute_value <- sprintf("
    SELECT
      le.id AS element,
      av.attribute,
      av.value
    FROM
      attributevalue AS av
    INNER JOIN
      layerelement AS le
    ON
      av.element = le.hash
    WHERE
      le.layer = %s AND
      %s
    ORDER BY
      le.id, av.attribute
    ",
    DBI::dbQuoteString(connection, layer),
    timerange("av", timestamp, connection)
  ) %>%
    DBI::dbGetQuery(conn = connection)
  attribute <- sprintf("
    SELECT
      a.id AS id,
      a.name AS name,
      a.type AS type
    FROM
      attribute AS a
    INNER JOIN
      (
        attributevalue AS av
      INNER JOIN
        layerelement AS le
      ON
        av.element = le.hash
      )
    ON
      a.id = av.attribute
    WHERE
      le.layer = %s AND
      %s
    GROUP BY
      a.id, name, type
    ",
    DBI::dbQuoteString(connection, layer),
    timerange("av", timestamp, connection)
  ) %>%
    DBI::dbGetQuery(conn = connection)

  new(
    "geoVersion",
    Coordinates = coordinates,
    Feature = feature,
    Features = features,
    LayerElement = layerelement,
    Attribute = attribute,
    AttributeValue = attribute_value
  )
}
