#' Convert a spatial object
#' @param object the object to convert
#' @name convert
#' @rdname convert
#' @exportMethod convert
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "convert",
  def = function(object){
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
  definition = function(object){
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
