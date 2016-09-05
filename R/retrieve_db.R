#' Retrieve a spatial object from the database
#' @inheritParams store
#' @export
#' @importFrom DBI dbQuoteIdentifier dbGetQuery
#' @importFrom dplyr %>% group_by_ arrange_ do_ select_ ungroup rename_ mutate_ mutate_at filter_ distinct_
#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @importFrom tidyr spread_
retrieve <- function(name, connection){
  layer <- sprintf(
    "SELECT hash FROM layer WHERE name = %s AND destroy IS NULL",
    dbQuoteIdentifier(connection, name)
  ) %>%
    dbGetQuery(conn = connection) %>%
    "[["("hash")
  if (length(layer) == 0) {
    stop("There is no layer named '", name, "'")
  }

  sr <- sprintf("
SELECT
  l.id AS id,
  fs.feature AS feature,
  f.type AS type,
  c.x AS x,
  c.y AS y,
  c.succession as succession
FROM
  (
    (
      (
        SELECT
          le.id,
          e.features
        FROM
          layerelement AS le
        INNER JOIN
          element AS e
        ON
          le.hash = e.hash
        WHERE
          le.layer = %s
        AND
          e.destroy IS NULL
      ) AS l
    INNER JOIN
      features AS fs
    ON
      l.features = fs.hash
    )
  INNER JOIN
    feature AS f
  ON
    fs.feature = f.hash
  )
INNER JOIN
  coordinates AS c
ON
  f.hash = c.hash
",
    dbQuoteIdentifier(connection, layer)
  ) %>%
    dbGetQuery(conn = connection) %>%
    group_by_(~id, ~feature) %>%
    arrange_(~id, ~feature, ~succession) %>%
    do_(
      polygon = ~select_(., ~x, ~y) %>%
        Polygon()
    ) %>%
    ungroup() %>%
    group_by_(~id) %>%
    do_(
      polygons = ~Polygons(srl = .$polygon, ID = .$id)
    ) %>%
    ungroup() %>%
    do_(
      SP = ~SpatialPolygons(
        Srl = .$polygons,
        pO = seq_along(.$polygons),
        proj4string = CRS(as.character(NA))
      ) %>%
        list()
    ) %>%
    unlist() %>%
    "[["(1)

  rawdata <- sprintf("
SELECT
  l.id AS id,
  a.name AS name,
  a.type AS type,
  av.value AS value
FROM
    (
      (
        SELECT
          le.id,
          le.hash
        FROM
          layerelement AS le
        WHERE
          le.layer = %s
      ) AS l
    INNER JOIN
      attributevalue AS av
    ON
      l.hash = av.element
    )
  INNER JOIN
    attribute AS a
  ON
    av.attribute = a.id
WHERE
  av.destroy IS NULL
",
    dbQuoteIdentifier(connection, layer)
  ) %>%
    dbGetQuery(conn = connection)
  coltypes <- rawdata %>%
    distinct_(~name, ~type) %>%
    rename_(colname = ~name, fun = ~type) %>%
    mutate_(fun = ~paste0("as.", fun))
  rawdata <- rawdata %>%
    select_(~-type) %>%
    spread_(key_col = "name", value_col = "value")
  for (this.fun in unique(coltypes$fun)) {
    rawdata <- mutate_at(
      rawdata,
      coltypes %>%
        filter_(~fun == this.fun) %>%
        "[["("colname"),
      this.fun
    )
  }
  rownames(rawdata) <- rawdata$id
  return(
    SpatialPolygonsDataFrame(Sr = sr, data = rawdata)
  )
}
