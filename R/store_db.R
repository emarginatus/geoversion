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
  assert_that(DBI::dbIsValid(connection))

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
    DBI::dbQuoteString(connection, name),
    DBI::dbQuoteString(connection, type)
  ) %>%
    DBI::dbGetQuery(conn = connection)
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
      DBI::dbWriteTable(conn = connection, name = "layer", append = TRUE)
  } else {
    layerhash <- layerhash$hash
  }

  sprintf(
    "SELECT id, hash FROM layerelement WHERE layer = %s",
    DBI::dbQuoteString(connection, layerhash)
  ) %>%
    DBI::dbGetQuery(conn = connection) %>%
    anti_join(x = x@LayerElement, by = "id") %>%
    rowwise() %>%
    transmute_(
      layer = ~layerhash,
      ~id,
      hash = ~sha1(list(Layer = layer, ID = id))
    ) %>%
    as.data.frame() %>%
    DBI::dbWriteTable(
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
      DBI::dbQuoteString(connection, name)
    ) %>%
    DBI::dbGetQuery(conn = connection) %>%
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
        DBI::dbQuoteString(connection, hash),
        DBI::dbQuoteString(connection, features.x)
      )
    )
  sapply(
    old$sql,
    function(statement){
      DBI::dbGetQuery(connection, statement)
    }
  )
  new.element <- element %>%
    filter_(~is.na(features.x) | features.x != features.y) %>%
    mutate_(spawn = timestamp, destroy = NA_real_) %>%
    select_(~hash, features = ~features.y, ~spawn, ~destroy)
  DBI::dbWriteTable(connection, "element", new.element, append = TRUE)

  new.features <- x@Features %>%
    semi_join(new.element, by = c("hash" = "features"))
  DBI::dbWriteTable(
    new.features,
    conn = connection,
    name = "staging_features",
    overwrite = TRUE
  )
  DBI::dbGetQuery(
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
        WHERE current IS NULL"
  )

  new.feature <- x@Feature %>%
    semi_join(new.features, by = c("hash" = "feature"))
  DBI::dbWriteTable(
    new.feature,
    conn = connection,
    name = "staging_feature",
    overwrite = TRUE
  )
  DBI::dbGetQuery(
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
      WHERE current IS NULL"
  )

  x@Coordinates %>%
    semi_join(new.feature, by = "hash") %>%
    DBI::dbWriteTable(
      conn = connection,
      name = "staging_coordinates",
      overwrite = TRUE
    )
  DBI::dbGetQuery(
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
    WHERE current IS NULL"
  )

  x@Attribute %>%
    select_(~id, ~name, ~type) %>%
    DBI::dbWriteTable(
      conn = connection,
      name = "staging_attribute",
      overwrite = TRUE
    )
  DBI::dbGetQuery(
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
    DBI::dbQuoteString(connection, name)
  ) %>%
    DBI::dbGetQuery(conn = connection) %>%
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
        DBI::dbQuoteString(connection, element),
        DBI::dbQuoteString(connection, attribute)
      )
    )
  sapply(
    old$sql,
    function(statement){
      DBI::dbGetQuery(connection, statement)
    }
  )
  attributevalue %>%
    filter_(~is.na(value.x) | value.x != value.y) %>%
    mutate_(spawn = timestamp, destroy = NA_real_) %>%
    select_(~element, ~attribute, value = ~value.y, ~spawn, ~destroy) %>%
    DBI::dbWriteTable(
      conn = connection,
      name = "attributevalue",
      append = TRUE
    )

  crs <- sprintf("
    SELECT
      element,
      id,
      value AS crs,
      spawn
    FROM
      crs
    INNER JOIN
      layerelement AS le
    ON
      crs.element = le.hash
    WHERE
      le.layer = %s AND
      crs.destroy IS NULL",
    DBI::dbQuoteString(connection, layerhash)
  ) %>%
    DBI::dbGetQuery(conn = connection) %>%
    full_join(
      element %>%
        select_(~id, ~hash) %>%
        inner_join(x@LayerElement, by = "id"),
      by = "id"
    )
  old <- crs %>%
    filter_(~crs.x != crs.y | (is.na(crs.y) != is.na(crs.x))) %>%
    mutate_(
      sql = ~sprintf("
        UPDATE
          crs
        SET
          destroy = %.21f
        WHERE
          element = %s AND destroy IS NULL",
        timestamp,
        DBI::dbQuoteString(connection, element)
      )
    )
  sapply(
    old$sql,
    function(statement){
      DBI::dbGetQuery(connection, statement)
    }
  )
  new.crs <- crs %>%
    filter_(~
        crs.x != crs.y |
        (is.na(crs.x) & is.na(spawn)) |
        (is.na(crs.x) != is.na(crs.y))
    ) %>%
    mutate_(spawn = timestamp, destroy = NA_real_) %>%
    select_(element = ~hash, value = ~crs.y, ~spawn, ~destroy)
  DBI::dbWriteTable(connection, "crs", new.crs, append = TRUE)

  return(invisible(NULL))
}
