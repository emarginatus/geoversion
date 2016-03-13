#' Convert a geoVersion object to a Spatial*DataFrame
#' @param x the geoVersion object
#' @export
#' @importFrom dplyr %>% select_ inner_join group_by_ arrange_ summarise_
#' @importFrom tidyr spread_
#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
as_sp <- function(x){
  attribute <- x@Attribute %>%
    select_(~id, ~name) %>%
    inner_join(
      x@AttributeValue,
      by = c("id" = "attribute")
    ) %>%
    select_(~-id) %>%
    spread_(key_col = "name", value_col = "value", convert = TRUE) %>%
    inner_join(x@LayerElement, by = c("element" = "id"))
  rownames(attribute) <- attribute$features
  polygons <- x@Feature %>%
    inner_join(x@Coordinates, by = "hash") %>%
    group_by_(~hash) %>%
    arrange_(~succession) %>%
    summarise_(
      polygon = ~list(
        Polygon(cbind(x, y), hole = type == "H")
      )
    ) %>%
    inner_join(x@Features, by = c("hash" = "feature")) %>%
    group_by_(~hash.y) %>%
    summarise_(
      polygons = ~list(Polygons(polygon, ID = unique(hash.y)))
    )
  SpatialPolygons(polygons$polygons, proj4string = x@CRS) %>% #nolint
    SpatialPolygonsDataFrame( #nolint
      data = attribute %>%
        select_(~-features, ID = ~element)
    )
}
