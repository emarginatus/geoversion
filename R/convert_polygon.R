#' Convert a spatial object
#' @param object the object to convert
#' @param id the name of the variable that holds the stable ID
#' @name convert
#' @rdname convert
#' @exportMethod convert
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "convert",
  def = function(object, id){
    standard.generic("convert") #nocov
  }
)

#' @rdname convert
#' @importFrom methods setMethod
#' @importClassesFrom sp Polygon
#' @importFrom digest sha1
setMethod(
  f = "convert",
  signature = "Polygon",
  definition = function(object, id){
    hash <- sha1(list(Hole = object@hole, Coords = object@coords))
    coordinates <- data.frame(
      hash,
      seq_len(nrow(object@coords)),
      object@coords,
      stringsAsFactors = FALSE
    )
    colnames(coordinates) <- c("Hash", "Order", "X", "Y")
    return(
      new(
        "geoVersion",
        Coordinates = coordinates,
        Feature = data.frame(
          Hash = hash,
          Type = ifelse(object@hole, "H", "S"),
          stringsAsFactors = FALSE
        )
      )
    )
  }
)

#' @rdname convert
#' @importFrom methods setMethod
#' @importClassesFrom sp Polygons
setMethod(
  f = "convert",
  signature = "Polygons",
  definition = function(object, id){
    poly <- convert(object@Polygons)
    poly@Features <- data.frame(
      Hash = object@ID,
      Feature = poly@Feature$Hash,
      stringsAsFactors = FALSE
    )
    return(poly)
  }
)

#' @rdname convert
#' @importFrom methods setMethod
#' @importFrom dplyr %>% group_by_ summarise_ mutate_ inner_join select_
#' @importFrom digest sha1
#' @importClassesFrom sp SpatialPolygonsDataFrame
setMethod(
  f = "convert",
  signature = "SpatialPolygonsDataFrame",
  definition = function(object, id){
    poly <- convert(object@polygons)
    poly@CRS <- object@proj4string
    hash <- poly@Features %>%
      group_by_(~Hash) %>%
      summarise_(
        Features = ~sha1(sort(Feature))
      ) %>%
      mutate_(
        ID = ~object@data[Hash, id]
      )
    poly@Features <- poly@Features %>%
      inner_join(
        hash,
        by = "Hash"
      ) %>%
      select_(
        Hash = ~Features,
        ~Feature
      )
    poly@LayerElement <- hash %>%
      select_(~ID, ~Features) %>%
      as.data.frame()
    return(poly)
  }
)

#' @rdname convert
#' @importFrom methods setMethod
setMethod(
  f = "convert",
  signature = "list",
  definition = function(object, id){
    do.call(
      combine,
      lapply(object, convert)
    )
  }
)
