#' Combine objects
#' @param ... the objects
#' @name combine
#' @rdname combine
#' @exportMethod combine
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "combine",
  def = function(...){
    standard.generic("combine") #nocov
  }
)

#' @rdname combine
#' @importFrom methods setMethod
#' @include geoversion_class.R
setMethod(
  f = "combine",
  signature = "geoVersion",
  definition = function(...){
    coordinates <- lapply(
      list(...),
      function(x){
        x@Coordinates
      }
    )
    feature <- lapply(
      list(...),
      function(x){
        x@Feature
      }
    )
    features <- lapply(
      list(...),
      function(x){
        x@Features
      }
    )
    layer.element <- lapply(
      list(...),
      function(x){
        x@LayerElement
      }
    )
    crs <- lapply(
      list(...),
      function(x){
        x@CRS
      }
    )
    crs <- unique(crs)
    if (length(crs) > 1) {
      stop("CRS not unique", call. = FALSE)
    }
    new(
      "geoVersion",
      Coordinates = do.call(rbind, coordinates),
      Feature = do.call(rbind, feature),
      Features = do.call(rbind, features),
      LayerElement = do.call(rbind, layer.element),
      CRS = crs[[1]]
    )
  }
)
