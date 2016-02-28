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
    name = "features",
    append = TRUE
  )
  new.feature <- x@Feature %>%
    semi_join(new.features, by = c("hash" = "feature"))
  dbWriteTable( #nolint
    new.feature,
    conn = connection,
    name = "feature",
    append = TRUE
  )
  x@Coordinates %>%
    semi_join(new.feature, by = "hash") %>%
    dbWriteTable(conn = connection, name = "coordinates", append = TRUE) #nolint
  x@Attribute %>%
    anti_join(
      dbReadTable(conn = connection, name = "attribute"), #nolint
      by = "id"
    ) %>%
    select_(~id, ~name, ~type) %>%
    dbWriteTable(conn = connection, name = "attribute", append = TRUE) #nolint
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
