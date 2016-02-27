#' Convert a geoVersion object to a Spatial*DataFrame
#' @param x the geoVersion object
#' @export
#' @importFrom dplyr %>% select_ inner_join group_by_ arrange_ summarise_
#' @importFrom tidyr spread_
#' @importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
as_sp <- function(x){
  attribute <- x@Attribute %>%
    select_(~ID, ~Name) %>%
    inner_join(
      x@AttributeValue,
      by = c("ID" = "Attribute")
    ) %>%
    select_(~-ID) %>%
    spread_(key_col = "Name", value_col = "Value", convert = TRUE) %>%
    inner_join(x@LayerElement, by = c("Element" = "ID"))
  rownames(attribute) <- attribute$Features
  polygons <- x@Feature %>%
    inner_join(x@Coordinates, by = "Hash") %>%
    group_by_(~Hash) %>%
    arrange_(~Order) %>%
    summarise_(
      Polygon = ~list(
        Polygon(cbind(X, Y), hole = Type == "H")
      )
    ) %>%
    inner_join(x@Features, by = c("Hash" = "Feature")) %>%
    group_by_(~Hash.y) %>%
    summarise_(
      Polygons = ~list(Polygons(Polygon, ID = unique(Hash.y)))
    )
  SpatialPolygons(polygons$Polygons, proj4string = x@CRS) %>% #nolint
    SpatialPolygonsDataFrame( #nolint
      data = attribute %>%
        select_(~-Features, ID = ~Element)
    )
}
